;;; dir-alias.el --- Create emacs directory aliases -*- lexical-binding: t -*-
;; 
;; Filename: dir-alias.el
;; Description: Create emacs directory Alaises
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Mar 28 10:32:38 2016 (-0500)
;; Version: 0.1
;; Package-Requires: ((cl-lib 0.5))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'cl-lib)

(defvar dir-alias--regexp nil
  "Regular Expression for matching files.")

(defvar dir-alias--dirs-assoc nil
  "Association list of directories.")

(defun dir-alias--ensure-last-slash (name)
  "Ensure the last value is a slash for NAME."
  (when name
    (let ((last (aref (substring name -1) 0)))
      (cond
       ((char-equal ?/ last)
	name)
       ((char-equal ?\\ last)
	(concat (substring name 0 -1) "/"))
       (t (concat name "/"))))))

(defvar dir-alias--convert-file-name (make-hash-table :test 'equal)
  "Hash of converted file names.")

(defun dir-alias--convert-file-name (name)
  "Convert NAME to a regular file-name."
  (if (not dir-alias--regexp) name
    (or (gethash name dir-alias--convert-file-name)
	(save-match-data
	  (let ((ret name)
		file-name
		(case-fold-search t))
	    (when (and dir-alias--regexp ret (stringp ret)
		       (string-match dir-alias--regexp ret)
		       (setq file-name (assoc (match-string 1 ret) dir-alias--dirs-assoc)))
	      (setq ret (replace-match (dir-alias--ensure-last-slash (cdr file-name)) t t ret)))
	    (puthash name ret dir-alias--convert-file-name)
	    ret)))))

(defun dir-alias--file-name-handler (operation name &rest args)
  "Run OPERATION on NAME with ARGS.

Allows the directory aliaes to be used."
  (dir-alias--real-file-name-handler
   operation
   (cons (dir-alias--convert-file-name name)
         (if (stringp (car args))
             (cons (dir-alias--convert-file-name (car args))
                   (cdr args))
           args))))

(defun dir-alias--real-file-name-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
	 (cons 'dir-alias--file-name-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
                      (inhibit-file-name-operation operation))
                  (apply operation args)))

(defun dir-alias--rebuild-and-enable ()
  "Rebuild `dir-alias-regexp' and enables directory aliases.
The appropriate line is added to `file-name-handler-alist'."
  (setq dir-alias--convert-file-name (make-hash-table :test 'equal)
	dir-alias--regexp (format "^~%s[/\\\\]"
				  (regexp-opt
				   (mapcar
				    (lambda(x)
				      (nth 0 x))
				    dir-alias--dirs-assoc) 'paren))
	file-name-handler-alist (cl-remove-if (lambda(x) (eq (cdr x) 'dir-alias--file-name-handler)) file-name-handler-alist)
	file-name-handler-alist (cons `(,dir-alias--regexp . dir-alias--file-name-handler) file-name-handler-alist)
	directory-abbrev-alist (cl-sort
				`((,(concat "\\`" (regexp-quote (dir-alias--ensure-last-slash (expand-file-name (getenv "HOME"))))) . "~/")
				  ,@(mapcar
				     (lambda(x)
				       `(,(concat "\\`" (regexp-quote (replace-regexp-in-string "[/]*$" "/" (cdr x)))) . ,(concat "~" (car x) "/")))
				     dir-alias--dirs-assoc))
				(lambda(x1 x2) (> (length (car x1)) (length (car x2)))))))

(define-minor-mode dir-alias-mode
  "Enable directory aliaes."
  :global t
  (if dir-alias-mode
      (dir-alias--rebuild-and-enable)
    (setq file-name-handler-alist
	  (cl-remove-if (lambda(x) (eq (cdr x) 'dir-alias--file-name-handler)) file-name-handler-alist)
	  directory-abbrev-alist nil)))

(defun dir-alias--2 (alias directory)
  "Create an ALIAS for DIRECTORY.
The alias will be ~alias/ to refer to the full directory.
These aliaes are only honored in Emacs."
  (if (consp alias)
      (dolist (a alias)
	(dir-alias a directory))
    (when (file-exists-p directory)
      (unless (assoc alias dir-alias--dirs-assoc)
	(push (cons alias directory) dir-alias--dirs-assoc)))))

(defun dir-alias (&rest alias-syntax)
  "Create an alias for directories using ALIAS-SYNTAX.

The aliases can be specified as a group of arguments consising of
ALIAS DIRECTORY or ALIAS-LIST DIRECTORY.

This wrapper function then sends the 2 arguments to `dir-alias--2'.

When completed, the dir-alias variables if the mode is enabled by
`dir-alias--rebuild-and-enable'."
  (let ((args alias-syntax)
	arg1 arg2)
    (while args
      (setq arg1 (pop args)
	    arg2 (and args (pop args)))
      (dir-alias--2 arg1 arg2)))
  (when dir-alias-mode
    (dir-alias--rebuild-and-enable)))

(defun dir-alias-subdirs (dir &optional except)
  "Alias subdirectories of DIR, with the exception of EXCEPT directories."
  (let ((lst (cl-remove-if (lambda(x) (and (stringp (car x)) (string= (car x) "")))
			   (mapcar
			    (lambda(x)
			      (cons (format "%s"
					    (if (string-match "^\\(.*?\\)[0-9_.-]*$" x)
						(match-string 1 x) x))
				    (if (and except (not (string-match (regexp-opt (append '("."  "..") except) 'paren) x)))
					(format "%s/"
						(expand-file-name x dir))
				      (format "%s/"
					      (expand-file-name x dir)))))
			    (cl-remove-if
			     (lambda(x)
			       (or (not (file-directory-p (expand-file-name x dir)))
				   (string-match (format "^%s$"
							 (regexp-opt '("." "..") 'paren)) x)))
			     (directory-files (expand-file-name dir)))))))
    (dolist (item lst)
      (dir-alias--2 (car item) (cdr item)))
    (when dir-alias-mode
      (dir-alias--rebuild-and-enable))))

(defun dir-alias-env--1 (env)
  "Create a directory alias of ENV environmental variable."
  (let* ((env-name (upcase env))
	 (alias-name (downcase env))
	 (env (getenv env-name)))
    (dir-alias alias-name (expand-file-name env))))

(defun dir-alias-env (&rest envs)
  "Setup ENVS using `dir-alias-env--1' on each argument."
  (let ((args envs))
    (while args
      (dir-alias-env--1 (pop args))))
  (when dir-alias-mode
      (dir-alias--rebuild-and-enable)))


(defun dir-alias-test ()
  "Test directory aliases."
  (mapc (lambda(test)
	  (message "%s\t%s\t%s\t%s\t%s" test
		   (expand-file-name test) (abbreviate-file-name (expand-file-name test))
		   (expand-file-name (concat test "dummy")) (abbreviate-file-name (concat (expand-file-name test) "dummy"))))
	(mapcar (lambda(x) (concat "~" (car x) "/")) dir-alias--dirs-assoc)))

(defvar usb-drive-letter)
(when (boundp 'usb-app-dir)
  (dir-alias "ep" (expand-file-name (concat usb-app-dir "../"))
	     "site-lisp" (expand-file-name (concat usb-app-dir "site-lisp/"))
	     "app" (expand-file-name usb-app-dir)
	     "data" (expand-file-name (expand-file-name (or (getenv "EPDATA") (concat usb-app-dir "../Data"))))
	     "nsi" (expand-file-name (expand-file-name (concat usb-app-dir "../Other/source/nsi/")))
	     "ahk" (expand-file-name (expand-file-name (concat usb-app-dir "../Other/source/ahk/")))
	     "other" (expand-file-name (expand-file-name (concat usb-app-dir "../Other/")))
	     "start" (expand-file-name (expand-file-name (concat usb-app-dir "../Data/start/")))
	     "ini" (expand-file-name (expand-file-name (if (getenv "EPDATA") (concat (getenv "EPDATA") "/ini/")
							     (concat usb-app-dir "../Data/ini/"))))
	     "src" (expand-file-name (expand-file-name (if (getenv "EPDATA") (concat (getenv "EPDATA") "/src/")
							 (concat usb-app-dir "../Data/src/"))))
	     "usb" usb-drive-letter
	     "pa" (expand-file-name (concat usb-drive-letter "PortableApps"))
	     "doc" (expand-file-name (concat usb-drive-letter "Documents"))
	     "doc" (expand-file-name (concat usb-drive-letter "LiberKey/MyDocuments")))
  (dir-alias-subdirs (expand-file-name (expand-file-name (if (getenv "EPDATA") (concat (getenv "EPDATA") "/src/") (concat usb-app-dir "../Data/src/")))))
  (dir-alias-subdirs (expand-file-name (concat usb-app-dir "../Data/start")) '("user" "system" "shared")))

(dir-alias-env "mydoc"
	       "temp"
	       "ProgramData"
	       "ProgramFiles"
	       "userprofile"
	       "appdata"
	       "homepath"
	       "systemroot"
	       "localappdata")

(dir-alias "ed" (expand-file-name "~/.emacs.d")
	   "doc" (expand-file-name "~/Documents")
	   "db" (expand-file-name "db")
	   "git" (expand-file-name "~/git")
	   "git" (expand-file-name "~/github")
	   "org" (expand-file-name "~/org"))


(dir-alias-subdirs (expand-file-name "~/.emacs.d") '("eshell" "url" "var"))

(defvar recentf-filename-handlers)
;;; FIXME elpa el-get yasnippet
(setq recentf-filename-handlers (quote (abbreviate-file-name)))


(defadvice file-readable-p (around emacs-portable-advice activate)
  "Ignore c:/ files on Mac OSX."
  (if (and (eq system-type 'darwin)
             (save-match-data
               (string-match "^[A-Za-z]:[/\\]" (nth 0 (ad-get-args 0))))) nil
    ad-do-it))

(defadvice file-remote-p (around emacs-portable-advice activate)
  "Ignore c:/ files on Mac OSX."
  (if (and (eq system-type 'darwin)
           (save-match-data
             (string-match "^[A-Za-z]:[/\\]" (nth 0 (ad-get-args 0))))) t
    ad-do-it))

(defadvice file-exists-p (around emacs-portable-advice activate)
  "Ignore c:/ files on Mac OSX."
  (if (and (eq system-type 'darwin)
           (save-match-data
             (string-match "^[A-Za-z]:[/\\]" (nth 0 (ad-get-args 0))))) nil
    ad-do-it))

(provide 'dir-alias)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dir-alias.el ends here
