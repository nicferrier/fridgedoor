;;; fridgedoor.el --- fridgedoor.el - a place to stick things

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia
;; Package-Requires: ((elnode "0.9.9.7.6"))
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

;; This is my entry into the Lisp In Summer Projects competition.

;;; Code:

(require 'elnode)

(elnode-app fridgedoor-dir cl)

(defconst fridgedoor-data-dir "~/fridgedoor"
  "Where we store uploads.")

(defun fridgedoor-filename ()
  (let* ((webname (format-time-string "%Y%m%d/%H/%M/%S-%N" (current-time)))
         (filename
          (concat
           (file-name-as-directory fridgedoor-data-dir)
           webname)))
    (make-directory (file-name-directory filename) t)
    (list webname filename)))

(defun fridgedoor-handler (httpcon)
  (elnode-method httpcon
    (POST
     (let ((title (elnode-http-param httpcon "title"))
           (datatype (elnode-http-param httpcon "type"))
           (data (elnode-http-param httpcon "data")))
       (let* ((lst (list :title title :datatype datatype))
              (lsttxt (format "%S" lst)))
         (destructuring-bind (webname filename) (fridgedoor-filename)
           (with-temp-file filename
             (insert (format "%s\n" lst))
             (insert data)
             (write-file))
           (elnode-send-redirect httpcon (concat "/" webname))))))
    (GET
     (elnode-docroot-for fridgedoor-data-dir
         with target
         on httpcon
         do
         (with-current-buffer (generate-new-buffer "*fridgedoor*")
           (insert-file-contents target)
           ;; Read the sexp then spit the doc out
           (let ((meta (save-excursion
                         (goto-char (point-min))
                         (read (current-buffer)))))
             (forward-line 1)
             (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
             (elnode-http-return
              httpcon (buffer-substring (point) (point-max)))))))))

(defun fridgedoor-start ()
  (interactive)
  (elnode-start 'fridgedoor-handler :port 8092))

(provide 'fridgedoor)

;;; fridgedoor.el ends here
