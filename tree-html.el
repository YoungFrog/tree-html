;;; tree-html.el --- Manipulating parse trees obtained from libxml  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nicolas Richard

;; Author: Nicolas Richard <theonewiththeevillook@yahoo.fr>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See also related :
;; http://www.emacswiki.org/emacs/XmlParser
;; https://github.com/tali713/esxml

;;; Code:

;;; Extension de libxml pour parser un fichier et une chaîne de
;;; caractères.
(defun libxml-parse-html-file (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (libxml-parse-html-region
       (point-min)
       (point-max)))))
(defun libxml-parse-html-string (string)
  "Example:
\(libxml-parse-html-string \"<a href=\\\"http://www/\\\">foo</a>\")"
  (with-temp-buffer
    (insert string)
    (libxml-parse-html-region
     (point-min)
     (point-max))))


;;; Low levels getters for HTML parse trees.

;; On pourrait croire que shr contient ce qu'il faut, mais shr commence par
;; transformer l'arbre analysé en une autre syntaxe (via shr-transform-dom, qui
;; accepte un arbre sorti par libxml), et puis descend dedans (via shr-descend,
;; cf `shr-insert-document').

(defun yf/tree-html-get-tag (tree)
  (and (listp tree) (car tree)))
(defun yf/tree-html-get-attributes (tree)
  (and (listp tree) (cadr tree)))
(defun yf/tree-html-get-content (tree)
  (cond ((listp tree)
         (cddr tree))
        ((stringp tree)
         nil)
        (t (error "Not a tree: %s" tree))))
(defun yf/tree-html-get-attribute-value (tree attr)
  (cdr (assq attr (yf/tree-html-get-attributes tree))))

(defun yf/tree-select (tree predicate descend &optional seen)
  "Recursively descend in a TREE and return a list of \(sub)trees
matching PREDICATE.

DESCEND is a function that takes a TREE as argument and return a
list of trees contained in it.

SEEN is used internally to check for circular structures."
  (or seen (setq seen (make-hash-table :test 'eq)))
  (if (gethash tree seen)
      (progn
        (error "Circular structure detected: %S" tree)
        nil)
    (puthash tree t seen)
    (if (funcall predicate tree)
        (list tree)
      (apply #'append
             (mapcar
              (lambda (it)
                (yf/tree-select it predicate descend seen))
              (funcall descend tree))))))

(defun yf/tree-html-select (tree predicate) 
  "Recursively descend in a TREE and return a list of (sub)trees
matching PREDICATE.

For getting information out of a tree, use the various
yf/tree-html-get-* functions."
  ;; A TREE in this context is either a string or a list: (tag alist TREE).
  (yf/tree-select tree predicate
                  #'yf/tree-html-get-content))

(defmacro yf/tree-html-aselect (tree &rest predicate-forms) 
  "Convenience macro for yf/tree-html-select"
  `(yf/tree-html-select ,tree
                        (lambda (it) ,@predicate-forms)))

(defun yf/tree-html-select-by-tag (tree tag)
  (yf/tree-html-select tree
                       (lambda (tree)
                         (eq tag (yf/tree-html-get-tag tree)))))


;;; Helpers function for manipulating the parse trees.

(defun yf/tree-html-get-sole-element (list)
  (require 'cl-macs)
  (cl-assert (and (consp list) (not (cdr list)))
             nil
             "List should contain excatly one element but has %d: %S"
             (length list) list)
  (car list))

(defun yf/tree-html-get-value (tree)
  "Get the value associated with a tree, making sure that there
is only one such value. See also `yf/tree-html-as-text'."
  (yf/tree-html-get-sole-element
   (yf/tree-html-get-content tree)))

(defun yf/tree-html-as-text (tree)
  (apply #'concat 
         (yf/tree-html-select
          tree
          (lambda (it) (stringp it)))))

(when (featurep 'ert)
  (ert-deftest yf/test/yf-tree-html-select ()
    (let ((yf/test2 '(top nil
                          (comment nil "[if lt IE 7]><html class=\"no-js lt-ie9 lt-ie8 lt-ie7\" lang=\"fr\"><![endif]")
                          (comment nil "[if IE 7]><html class=\"no-js lt-ie9 lt-ie8\" lang=\"fr\"><![endif]")
                          (comment nil "[if IE 8]><html class=\"no-js lt-ie9\" lang=\"fr\"><![endif]")
                          (comment nil "[if gt IE 8]><!")
                          (html
                           ((class . "no-js")
                            (lang . "fr"))
                           (comment nil "<![endif]")))))
      (should (equal (yf/tree-html-select yf/test2
                                          (lambda (item)
                                            (eq
                                             (car item)
                                             'comment)))
                     '((comment nil "[if lt IE 7]><html class=\"no-js lt-ie9 lt-ie8 lt-ie7\" lang=\"fr\"><![endif]") (comment nil "[if IE 7]><html class=\"no-js lt-ie9 lt-ie8\" lang=\"fr\"><![endif]") (comment nil "[if IE 8]><html class=\"no-js lt-ie9\" lang=\"fr\"><![endif]") (comment nil "[if gt IE 8]><!") (comment nil "<![endif]")))))
    ;; (should (equal (yf/tree-html-select '#1=(a b #1#) ; circular structure test.
    ;;                                     (lambda (item)
    ;;                                       (eq
    ;;                                        (car item)
    ;;                                        'a)))
    ;;                '#1=(a b #1#)))
    ))


(provide 'tree-html)
;;; tree-html.el ends here
