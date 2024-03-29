;;; eel.el --- Completion client for Everything HTTP server  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  João Távora

;; Author: João Távora <joaotavora@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'external-completion)

(defun eel (re &optional dir)
  (let* ((dir (or dir default-directory))
         (headers '(("Connection" . "Close")))
         (fulldir (expand-file-name dir))
         (searcharg (format "%s.*%s"
                            (url-hexify-string
                             (replace-regexp-in-string "/" "\\\\\\\\"
                                                       (convert-standard-filename
                                                        (expand-file-name dir))))
                            (url-hexify-string
                             (replace-regexp-in-string "/" "\\\\\\\\" re))))
         (params `(("search" . ,searcharg)
                   ("regex" . "1")
                   ("json" . "1")
                   ("count" . 250)
                   ("path_column" . "1")))
         (query (format "GET /?%s HTTP/1.1"
                        (string-join (cl-loop for (q . v) in params
                                              collect (format "%s=%s" q v))
                                     "&")))
         (conn (make-network-process :name "EEL connection"
                                :host "localhost"
                                :service 3636
                                :buffer (generate-new-buffer " *eel-process*")))
         (cancelled nil))
    ;; (message "searcharg is %s" searcharg)
    (with-current-buffer (process-buffer conn)
      (erase-buffer))
    (catch 'done
      (set-process-sentinel
       conn
       (lambda (conn _change)
         (when (not (process-live-p conn))
           (unless cancelled
             (throw
              'done
              (with-current-buffer (process-buffer conn)
                (goto-char (point-min))
                (search-forward "\n\n")
                (cl-loop
                 for r across (plist-get (json-parse-buffer :object-type 'plist) 
                                         :results)
                 for cand = (cl-destructuring-bind (&key type name path) r
                              (let ((folderp (string= type "folder")))
                              (substring (expand-file-name (concat name (and folderp "//"))
                                                           path)
                                         (length fulldir))))
                 unless (or (string-empty-p cand)
                            (string-prefix-p "." cand))
                 do
                 (save-match-data
                   (when (string-match re cand)
                     (add-face-text-property (match-beginning 0) (match-end 0)
                                           'completions-common-part
                                           nil cand)))
                 and collect cand)))))))
      (process-send-string
       conn
       (cl-loop for (header . value) in headers
                concat (concat header ": " value "\r\n") into header-section
                finally return (format "%s\r\n%s\r\n" query header-section)))
      (let ((inhibit-quit t))
        (prog1
            (setq cancelled (if (sit-for 30) 'timeout 'user-input))
          (kill-process conn))))))

;;; Completion table

(cl-defmethod eel-completion-function (dir)
  (cl-labels ((lookup (pat _point)
                (let ((refreshed (eel pat dir)))
                  (cond ((listp refreshed) refreshed)
                        ((eq 'timeout refreshed)
                         (message "oops, timeout"))
                        ((eq 'user-input refreshed)
                         nil)))))
    (external-completion-table 'eel #'lookup)))

(defvar eel--find-file-history nil)

(defvar eel-finding-context nil)

(defun eel-find-file (dir)
  (interactive (list
                (if (>= (prefix-numeric-value current-prefix-arg) 2)
                    (read-directory-name "Eel find file in directory? ")
                    (let ((eel-finding-context t))
                      (project-root (project-current))))))
  (let ((default-directory (expand-file-name dir)))
    (find-file (completing-read (format "Eel find file in %s: " dir)
                                (eel-completion-function default-directory)
                                nil nil nil eel--find-file-history))))

(provide 'eel)
;;; eel.el ends here
