;;; fridgedoor.el --- fridgedoor.el - a place to stick things -*- lexical-binding: t -*-

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

(defun elnode-js-server (httpcon jquery-p &rest scripts)
  (let ((host (elnode-http-header httpcon "Host"))
        (server (elnode-server-info httpcon)))
    (message "host header = %s" host)
    (noflet ((script-it (name)
               (format
                "{var e=document.createElement('script');
var s = \"http://%s/%s\";
e.setAttribute('src', s);
e.setAttribute('language', 'javascript');
document.body.appendChild(e);
console.log(\"tryin to download:\",s);}"
                host name))
             (script-tag (script)
               (if (equal (elt script 0) (elt "(" 0))
                   (format
                    "{var e=document.createElement('script');
e.appendChild(document.createTextNode('%s'));
document.body.appendChild(e);}"
                    script)
                   ;; Else it's a webname
                   (script-it script))))
      (elnode-http-start
       httpcon 200 '("Content-type" . "application/x-javascript"))
      (elnode-http-return
       httpcon
       (concat
        "(function () {"
        (if jquery-p 
            (concat
             (script-it "jquery-1.10.2.min.js")
             (format
              "var timer;timer=window.setInterval(function (){console.log(\"in timer\");if (!(typeof jQuery === \"undefined\")) {clearTimeout(timer);%s}}, 10);"
              (loop for script in scripts
                 concat (script-tag script))))) "})();")))))

(defun elnode-make-js-server (jquery-p &rest scripts)
  "Make a javascript server for the specified SCRIPTS.

If JQUERY-P is `t' then make JQuery the first script.

Each element of SCRIPTS can be either a web name or a script to
run.  Scripts to run must be wrapped in parens."
  (lambda (httpcon)
    (apply 'elnode-js-server httpcon jquery-p scripts)))

(defun fridgedoor/filename ()
  (let* ((webname (format-time-string "%Y%m%d/%H/%M/%S-%N" (current-time)))
         (filename
          (concat
           (file-name-as-directory fridgedoor-data-dir)
           webname)))
    (make-directory (file-name-directory filename) t)
    (list webname filename)))

(defun fridgedoor/template (template &rest args)
  "Apply the TEMPLATE against the ARGS."
  (apply 's-format
         (with-temp-buffer
           (insert-file-contents (concat fridgedoor-dir template))
           (buffer-string))
         args))

(defun fridgedoor/content (filename)
  "Get the contents of FILENAME as a propertized string.

The string content is returned propertized with `:meta' being the
meta data list."
  (with-current-buffer (generate-new-buffer "*fridgedoor*")
    (insert-file-contents filename)
    ;; Read the sexp then spit the doc out
    (let ((meta (save-excursion
                  (goto-char (point-min))
                  (read (current-buffer)))))
      (forward-line 1)
      (propertize (buffer-substring (point) (point-max)) :meta meta))))

(defun fridgedoor-handler (httpcon)
  (elnode-method httpcon
    (POST
     (let ((title (elnode-http-param httpcon "title"))
           (datatype (elnode-http-param httpcon "type"))
           (data (elnode-http-param httpcon "data")))
       (let* ((lst (list :title title :datatype datatype))
              (lsttxt (format "%S" lst)))
         (destructuring-bind (webname filename) (fridgedoor/filename)
           (with-temp-file filename
             (insert (format "%s\n" lst))
             (insert data))
           (elnode-send-redirect httpcon (concat "/magnet/" webname))))))
    (GET
     ;; we might need to override the cache functions to allow query
     ;; string to be cached because ff just returns internal cache to
     ;; ajax otherwise
     (elnode-docroot-for fridgedoor-data-dir
         with target
         on httpcon
         do
         (if (elnode-http-header httpcon "X-Requested-With")
             (let* ((doc (fridgedoor/content target))
                    (meta (plist-get (text-properties-at 0 doc) :meta))
                    (html-safe (htmlize-protect-string doc))
                    (data (append (kvplist->alist meta) (list (cons "doc" doc)))))
               (elnode-send-json httpcon data))
             ;; Else
             (elnode-send-file httpcon (concat fridgedoor-dir "page.html")))))))

(defconst fridgedoor-assets-server
  (elnode-webserver-handler-maker
   (file-name-as-directory
    (expand-file-name (concat fridgedoor-dir "www"))))
  "Webserver for static assets.")

(defun fridgedoor-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   `(("^[^/]*//magnet/\\(.*\\)" . fridgedoor-handler)
     ("^[^/]*//js$"
      . ,(elnode-make-js-server t "form.js" "logo.js" "data.js"))
     ("^[^/]*//\\(.*\\)" . ,fridgedoor-assets-server))))

(elnode-start 'fridgedoor-router :port 8096)

(provide 'fridgedoor)

;;; fridgedoor.el ends here
