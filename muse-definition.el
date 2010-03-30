;;; muse-definition -- define words in a muse project

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Website: http://www.russet.org.uk

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This adds support for definitions in a muse project. There are two new tags
;; added. The first "<define>" adds a word into a page which should be defined
;; and, optionally an inline definition. The second "<definition>" is used to
;; provide an definition for when inline definitions are not enough.
;;
;; For example,
;; 
;; <define link="word">a set of letters surrounded by spaces</define>
;;
;; defines "word" as "a set of...".
;;
;; Alternatively, we could do
;; 
;; <define link="word"/>
;; 
;; <definition name="word">a set of letters surrounded by spaces</definition>
;;
;; The "<definition> tag can occur anywhere in the same muse project.
;; In general, they can be placed in a single file, which acts as a glossary.
;; Any other muse markup can be used in this file; when published the
;; "<definition>" tag is removed.
;;
;; Note the one limitation here -- the definition used by the "<define>" tag
;; will be the one present in the "<definition>" tag, the last time the latter
;; was published -- essentially, it's a two stage compiliation. You can use
;; a third tag to overcome this.
;;
;; <defdepend file="definitions.muse"/>
;;
;; will ensure that all definitions in "definitions.muse" are current. You can
;; include this tag into your header and footer if you like using like so...
;;
;; <markup>
;; <defdepend file="definitions.muse"/>
;; </markup>
;;
;; The <defdepend> tag needs to be on a new line or it doesn't work (for me at
;; least).

;;; History:
;; 

;;; Todo: 
;; 
;; putting defdepend into header.xml doesn't work

(require 'assoc)

;;; Code:
(add-to-list 'muse-publish-markup-tags
             '("define" t t t 'ignore))

(add-to-list 'muse-publish-markup-tags
             '("definition" t t t muse-definition-tag))

(add-to-list 'muse-publish-markup-tags
             '("defdepend" t t t muse-definition-depend))

;; a hash which will contain hash tables of definition for each project.
(defvar muse-definition-hash 
  (make-hash-table :test 'equal))

(defun muse-definition-project-hash()
  ;; and stick in lazy instantiation
  (let* ((project-name
         (car (muse-project)))
        (project-hash
         (gethash project-name
                  muse-definition-hash)))
             
    (if project-hash
        project-hash
      (puthash project-name
               (make-hash-table :test 'equal)
               muse-definition-hash)
      (muse-definition-project-hash))))

(defun muse-definition-project-hash-clear()
  (interactive)
  (puthash (car (muse-project)) nil muse-definition-hash))

(defun muse-definition-tag (beg end attrs)
  "Support for definition tag."
  (let ((definition-string
          (buffer-substring-no-properties beg end))
        (definition-name
          (cdr (assoc "name" attrs)))
        (definition-hash (muse-definition-project-hash)))
    (puthash definition-name definition-string definition-hash)))

(defun muse-definition-depend (beg end attrs)
  ;; recursion check!
  (when (not
         (equal muse-publishing-current-file
                (expand-file-name
                 (cdr (assoc  "file" attrs))
                 (file-name-directory muse-publishing-current-file))))
    
    (setq attrs (aput 'attrs "markup" "muse-definition-include-nothing"))
    (muse-publish-include-tag
     beg end attrs)))
   

(defun muse-definition-include-nothing (attrs)
  (muse-publish-markup-region (point-min) (point-max))
  (delete-region (point-min) (point-max)))

(defun muse-definition-uniquify-link(link)
  (concat (number-to-string (random)) link))



;;; HTML publishing support begins here.
(add-to-list 'muse-html-markup-tags
             '("define" t t t muse-definition-html-tag))


(defvar muse-definition-html-definition-script
    "<script type=\"text/javascript\" language=\"JavaScript\"><!--
      function hideShowToggle(d) {
      if(d.length < 1) { return; }
      if(document.getElementById(d).style.display == \"none\") { document.getElementById(d).style.display = \"block\"; }
      else { document.getElementById(d).style.display = \"none\"; }
      }
      //-->
    </script>")


(defvar muse-definition-html-definition-link
  "<a href=\"javascript:hideShowToggle('%s')\">%s</a>")

(defvar muse-definition-html-definition
  "<span id=\"%s\"
   style=\"display:none; 
      background-color: white;
      border: solid;
      border-color: black;
      padding: 2px;
      position:absolute;\">

   %s
</span>")


(defun muse-definition-html-tag(beg end attrs)
  (let ((link (cdr (assoc "link" attrs)))
        (definition (buffer-substring beg end)))
    (delete-region beg end)
    
    ;; if we have a definition use it
     (if (< 0 (length definition))
         (muse-definition-html-insert beg link definition)
       ;; we don't have a definition, so attempt to get from the hash
       ;; else, do nothing which should leave things blank.
       (if (gethash link (muse-definition-project-hash))
           (muse-definition-html-insert
            beg link
            (gethash link (muse-definition-project-hash)))
         (muse-display-warning
          (format "Definition for %s not found in %s"
                  link muse-publishing-current-file))))))


(defun muse-definition-html-insert(beg link definition)
  (let ((unique-id
         (muse-definition-uniquify-link link)))
    (save-excursion
      ;; from the start...
      (goto-char beg)
      ;; insert the link and definition
      (muse-insert-markup
       (format
        muse-definition-html-definition-link
        unique-id link))
      (muse-insert-markup
       (format
        muse-definition-html-definition
        unique-id
        definition)))))


(defun muse-definition-debug-print-hash()
  (interactive)
  (let ((hash (muse-definition-project-hash)))
    (switch-to-buffer "*muse-definition-debug*")
    (erase-buffer)
    (if hash
        (progn
          (pp hash (current-buffer))
          (insert "\n")
          (maphash
           (lambda(key value)
             (insert (concat "\nKEY: " key "\n"))
             (pp value (current-buffer)))
           hash)))))

(provide 'muse-definition)

;;; muse-definition.el ends here
