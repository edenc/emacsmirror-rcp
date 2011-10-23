
(setq el-get-epkg-path '("/Users/edenc/src/epkgs"))
(setq ec/el-get-sources nil)
(setq ec/el-get-epkg-dep-index nil)
(ec/el-get-read-all-epkg)

(defun ec/el-get-read-all-epkg ()
  "Return the list of all the recipes, formatted like `el-get-sources'.

Only consider any given recipe only once even if present in
multiple dirs from `el-get-recipe-path'. The first recipe found
is the one considered.  We first look in `el-get-sources' then in
each directory listed in `el-get-recipe-path' in order."
  (let ((packages (mapcar 'el-get-source-name el-get-sources)))
    (append
     el-get-sources
     (loop for dir in (ec/el-get-epkg-dirs)
	   nconc (loop for recipe in (directory-files dir nil "^[^.].*$")
		       for filename = (concat (file-name-as-directory dir) (file-name-as-directory recipe) "master")
		       for package = (file-name-sans-extension (file-name-nondirectory recipe))
		       unless (member package packages)
           unless (not (file-exists-p filename))
		       do (push package packages)
           and collect (ignore-errors (ec/el-get-read-epkg-file filename package)))))))

(defun ec/el-get-read-epkg-file (filename package)
  "Read given filename and return its content (a valid form is expected)"
  (condition-case err
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (let ((pkgdef (read (current-buffer)))
              (recipe nil))
          (setq recipe (plist-put recipe :name (intern package)))
          (setq recipe (plist-put recipe :description (plist-get pkgdef :summary)))
          (setq recipe (plist-put recipe :type 'emacsmirror))
          (setq recipe (plist-put recipe :pkgname package))
          (setq recipe (plist-put recipe :required (plist-get pkgdef :required)))
          (ec/el-get-epkg-update-dep-index package pkgdef)
          recipe))
    ((debug error)
     (error "Error reading recipe %s: %S" filename err))))

(defun ec/el-get-epkg-update-dep-index (package pkgdef)
  (loop for p in (plist-get pkgdef :provided)
        do (setq ec/el-get-epkg-dep-index
                 (plist-put ec/el-get-epkg-dep-index
                            p (intern package)))))

(ec/el-get-epkg-update-deps (ec/el-get-read-all-epkg))
(defun ec/el-get-epkg-update-deps (sources)
  (mapcar
   (lambda (p)
     (let (resolved-pkg deps)
       (mapc
        (lambda (d)
          (setq resolved-pkg
                (plist-get ec/el-get-epkg-dep-index (car (cdr d))))
          (if resolved-pkg (push resolved-pkg deps)))
        (car (plist-get p :required)))
       (if deps (plist-put p :depends deps) p)))
   sources))

(defun ec/el-get-epkg-dirs ()
  "Return the elements of el-get-recipe-path that actually exist.

Used to avoid errors when exploring the path for recipes"
  (reduce (lambda (dir result)
            (if (file-directory-p dir) (cons dir result) result))
          el-get-epkg-path :from-end t :initial-value nil))

(defun ec/el-get-list-all-packages ()
  (with-current-buffer (get-buffer-create "*el-get packages*")
	(setq buffer-read-only nil)
	(erase-buffer)
	(let ((packages (ec/el-get-epkg-update-deps (ec/el-get-read-all-epkg))))
	  (let ((selector (cond
					   ((string= el-get-package-menu-sort-key "Status")
						#'(lambda (package)
							(let ((package-name (el-get-as-string (plist-get package :name))))
							  (el-get-package-status package-name))))
					   ((string= el-get-package-menu-sort-key "Description")
						#'(lambda (package)
							(plist-get package :description)))
					   (t
						#'(lambda (package)
							(el-get-as-string (plist-get package :name)))))))
		(setq packages
			  (sort packages
					(lambda (left right)
					  (let ((vleft (funcall selector left))
							(vright (funcall selector right)))
						(string< vleft vright))))))
	  (mapc (lambda (package)
			  (let ((package-name (el-get-as-string (plist-get package :name))))
				(el-get-print-package package-name
									  (el-get-package-status package-name)
									  (plist-get package :description))))
			packages))
	(goto-char (point-min))
	(current-buffer)))

(defun ec/el-get-package-menu ()
  (with-current-buffer (ec/el-get-list-all-packages)
	(el-get-package-menu-mode)
	(setq header-line-format
		  (mapconcat
		   (lambda (pair)
			 (let ((column (car pair))
				   (name (cdr pair)))
			   (concat
				;; Insert a space that aligns the button properly.
				(propertize " " 'display (list 'space :align-to column)
							'face 'fixed-pitch)
				;; Set up the column button.
				(propertize name
							'column-name name
							'help-echo "mouse-1: sort by column"
							'mouse-face 'highlight
							'keymap el-get-package-menu-sort-button-map))))
		   '((2 . "Package")
			 (30 . "Status")
			 (41 . "Description"))
		   ""))
	(pop-to-buffer (current-buffer))))

(defun ec/el-get-list-packages ()
  "Display a list of packages."
  (interactive)
  (ec/el-get-package-menu))
(ec/el-get-list-packages)




(el-get-list-all-packages)

(defun el-get-read-all-recipes ()
  (ec/el-get-epkg-update-deps (ec/el-get-read-all-epkg)))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for PACKAGE."
  (let ((source (loop for src in (ec/el-get-epkg-update-deps (ec/el-get-read-all-epkg))
		      when (string= package (el-get-source-name src))
		      return src)))

    (cond ((or (null source) (symbolp source))
	   ;; not in `el-get-sources', or only mentioned by name
	   ;; (compatibility from pre 3.1 era)
	   (el-get-read-recipe package))

	  ((null (plist-get source :type))
	   ;; we got a list with no :type, that's an override plist
	   (loop with def = (el-get-read-recipe package)
		 for (prop override) on source by 'cddr
		 do (plist-put def prop override)
		 finally return def))

	  ;; none of the previous, must be a full definition
	  (t source))))


(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 4000)

(setq el-get-sources (ec/el-get-epkg-update-deps (ec/el-get-read-all-epkg)))
(defun ec/dump-sources ()
  (mapc (lambda (recipe)
	  (let ((name (symbol-name (plist-get recipe :name))))
	    (with-temp-buffer
	      (print recipe (current-buffer))
	      (write-file name)
	      (kill-buffer)))) el-get-sources))

(ec/dump-sources)
