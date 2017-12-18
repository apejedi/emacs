(defvar tekno-port 10000
  "port of the tekno server")

(defvar tekno-server-clients '()
  "alist where KEY is a client process and VALUE is the string")

(defvar tekno-server-handlers '()
  )

(defun tekno-server-start nil
  "starts an emacs tekno server"
  (interactive)
  (unless (process-status "tekno-server")
    (make-network-process :name "tekno-server"
                          :buffer "*tekno-server*"
                          :family 'ipv4
                          :service 10000
                          :sentinel 'tekno-server-sentinel
                          :filter 'tekno-server-filter
                          :server 't)
    )
  )

(defun tekno-server-stop nil
  "stop an emacs tekno server"
  (interactive)
  (while  tekno-server-clients
    (delete-process (car (car tekno-server-clients)))
    (setq tekno-server-clients (cdr tekno-server-clients)))
  (delete-process "tekno-server")
  )

(defun tekno-server-filter (proc string)
  ;; (with-current-buffer "*tekno-server*"
  ;;   (insert (format "got %s\n" string)))
  (if (string= "tekno-pattern" (buffer-name))
      (progn
        (setq cur-pos string)
        (highlight-pattern-pos string)))
  )

(defun tekno-server-sentinel (proc msg)
  (tekno-server-log (format "client %s %s" proc msg))
  (when (string= msg "connection broken by remote peer\n")
    (setq tekno-server-clients (assq-delete-all proc tekno-server-clients))
    (tekno-server-log (format "client %s has quit" proc))))

;;from server.el
(defun tekno-server-log (string &optional client)
  "If a *tekno-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*tekno-server*")
      (with-current-buffer "*tekno-server*"
        (goto-char (point-max))
        (insert (current-time-string)
                (if client (format " %s:" client) " ")
                string)
        (or (bolp) (newline)))))
