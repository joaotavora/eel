
(require 'cl-lib)


(defun eel (str &optional dir)
  (interactive (list "/hhh?path=1&json=1" default-directory))
  (let* ((dir (or dir default-directory))
         (headers '(("Connection" . "Close")))
         (searcharg (format "%s%s"
                            (url-hexify-string
                             (replace-regexp-in-string "/" "\\\\\\\\"
                                                       (convert-standard-filename
                                                        (expand-file-name dir))))
                            (replace-regexp-in-string "" ".*" str)))
         (params `(("search" . ,searcharg)
                   ("regex" . "1")
                   ("json" . "1")
                   ("count" . 25000)
                   ("path_column" . "1")))
         (query (format "GET /?%s HTTP/1.1"
                        (string-join (cl-loop for (q . v) in params
                                              collect (format "%s=%s" q v))
                                     "&")))
         (conn (make-network-process :name "EEL connection"
                                :host "localhost"
                                :service 3636
                                :buffer "*eel-process*"))
         (cancelled nil))
    (with-current-buffer (process-buffer conn)
      (erase-buffer))
    (catch 'done
      (set-process-sentinel
       conn
       (lambda (conn change)
         (when (not (process-live-p conn))
           (unless cancelled
             (throw 'done (with-current-buffer (process-buffer conn)
                            (goto-char (point-min))
                            (search-forward "\n\n")
                            (json-parse-buffer :object-type 'plist)))))))
      (process-send-string
       conn
       (cl-loop for (header . value) in headers
                concat (concat header ": " value "\r\n") into header-section
                finally return (format "%s\r\n%s\r\n" query header-section)))
      (let ((inhibit-quit t))
        (setq cancelled (eq (sit-for 30) nil))))))

(eel "raborabo" "c:/repo/tsw")


