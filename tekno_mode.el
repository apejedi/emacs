(require 'clomacs)
(require 'cl-lib)

(clomacs-defun get-patterns techno.core/get-patterns :return-type :list)

(clomacs-defun get-patterns-from-string techno.core/get-patterns-from-string :return-type :list)

(clomacs-defun get-pattern-str techno.core/get-pattern-str)

(setq current-playing-patterns '())
(setq pattern-queue-add '())
(setq pattern-queue-rm '())
(setq pattern-print-list '())
(setq techno-patterns nil)

(set-face-foreground 'ctbl:face-row-select "white")
(set-face-background 'ctbl:face-row-select "blue5")
(set-face-bold-p 'ctbl:face-row-select t)
(set-face-foreground 'ctbl:face-cell-select "black")
(set-face-background 'ctbl:face-cell-select "yellow")
(set-face-bold-p 'ctbl:face-cell-select t)

(defvar tekno:uid 1)

(defun tekno:uid ()
  "[internal] Generate an unique number."
  (incf tekno:uid))



(defun ctbl:dest-ol-selection-set (dest cell-id)
  "[internal] Put a selection overlay on CELL-ID. The selection overlay can be
 put on some cells, calling this function many times.  This
 function does not manage the selections, just put the overlay."
  (lexical-let (ols (row-id (car cell-id)) (col-id (cdr cell-id)))
    (ctbl:dest-with-region dest
      (ctbl:find-all-by-row-id
       dest row-id
       (lambda (tcell-id begin end)
         (let* ((overlay (make-overlay begin end))
                (rows (if techno-patterns
                          (ctbl:component-sorted-data techno-patterns)))
                (pattern (if rows (nth (cdr tcell-id) (nth row-id rows))
                           ""))
               )
           (overlay-put overlay 'face
                        (if (= (cdr tcell-id) col-id)
                            'ctbl:face-cell-select
                          (if (member (intern pattern) current-playing-patterns)
                              'ctbl:face-row-select)
                          ))
           (push overlay ols)
           ))))
    (setf (ctbl:dest-select-ol dest) ols)))

(defun load-patterns-from-buffer (buf &optional sketch)
  (let* ((contents (with-current-buffer
                       (get-buffer buf)
                     (buffer-string)))
         (contents  (replace-regexp-in-string "\"" "\\\\\"" contents))
         (data (get-patterns-from-string contents sketch))
         (table (make-hash-table :test 'equal)))
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print contents))
    (dolist (el data)
      (puthash (car el) (car (cdr el)) table)
      )
    (setq pattern-data table))
  )


(defun load-patterns ()
  (interactive)
  (let* ((s (read-string "Sketch: ")))
    (load-patterns-from-buffer "sketches.clj" s)
    (update-pattern-view))
  )

(defun refresh-patterns ()
  (interactive)
  (let* ((s (read-string "type: "))
         (table (if pattern-data pattern-data (make-hash-table :test 'equal))))
    (dolist (p (get-patterns))
      (puthash (symbol-name p) (get-pattern-str p s) table)
      )
    (setq pattern-data table)
    (update-pattern-view))
  )

;; (load-patterns-from-buffer "sketches.clj" "track2")
;; (update-pattern-view)



(defun pattern-add-q ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns)))
    (if (not (member key pattern-queue-rm))
        (setq pattern-queue-add (cons key pattern-queue-add)))
    (setq pattern-queue-rm (delete key pattern-queue-rm))
    (update-pattern-view)
    )
  )
(defun pattern-rm-q ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns)))
    (if (not (member key pattern-queue-add))
        (setq pattern-queue-rm (cons key pattern-queue-rm)))
    (setq pattern-queue-add (delete key pattern-queue-add))
    (update-pattern-view)
    )
  )

(defun pattern-flush-q ()
  (interactive)
  (let* ()
    (dolist (a pattern-queue-add)
      (add-pattern-key a)
      )
    (dolist (a pattern-queue-rm)
      (rm-pattern-key a)
      )
    (setq pattern-queue-add '())
    (setq pattern-queue-rm '())
    (update-pattern-view)
    )
  )


(defun build-pattern-model ()
  (let* ((column-model ; column model
          (list (make-ctbl:cmodel
                 :title "A" :sorter 'ctbl:sort-number-lessp
                 :min-width 5 :align 'centerc)
                (make-ctbl:cmodel
                 :title "B" :align 'centerc
                 :sorter 'ctbl:sort-number-lessp)
                (make-ctbl:cmodel
                 :title "C" :align 'centerc)
                (make-ctbl:cmodel
                 :title "D" :align 'centerc)
                (make-ctbl:cmodel
                 :title "E" :align 'centerc)
                (make-ctbl:cmodel
                 :title "F" :align 'centerc)
                (make-ctbl:cmodel
                 :title "G" :align 'centerc)
                (make-ctbl:cmodel
                 :title "H" :align 'centerc)))
         (data '(()))
         (data2
          (cl-loop for k being the hash-keys of pattern-data
                   do
                  (if (< (length (car (last data))) 8)
                      (setf data
                            (append (butlast data)
                                    (list (append (car (last data)) (list k)))
                                    ))
                    (setf data (append data (list (list k)) )))
                  ))
         (model ; data model
          (make-ctbl:model
           :column-model column-model :data data)))
    model)
  )


(defun start-player ()
  (interactive)
  (nrepl-sync-request:eval
   "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
(if (or (nil? player) (not (node-active? player)))
      (def player (s/get-s
                   (/ 80 60)
                   )))"
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
  )
(defun set-player-sp ()
  (interactive)
  (let ((sp (read-string "Speed: ")))
      (nrepl-sync-request:eval
       (concat "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
(if (and (not (nil? player)) (node-active? player))
    (s/set-sp player (/ " sp " 60)))")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection))))
  )

(defun stop-player ()
  (interactive)
  (nrepl-sync-request:eval
   "(ns techno.core
        (:use [overtone.core]
              )
        (:require [techno.sequencer :as s]
                  [clojure.tools.reader.edn :as edn]
                  [clojure.tools.reader.reader-types :as readers]
                  [clojure.string :as string]))
(kill player)"
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
  )

(defun add-pattern-print (&optional key)
  (interactive)
  (setq pattern-print-list
        (cons (if key key
                  (ctbl:cp-get-selected-data-cell techno-patterns))
              pattern-print-list))
  (update-pattern-view)
  )


(defun clear-pattern-print ()
  (interactive)
  (setq pattern-print-list '())
  (update-pattern-view)
  )

(defun pattern-print-add-all ()
  (interactive)
  (cl-loop for k being the hash-keys of pattern-data
           do
           (add-pattern-print k)
           )
  (update-pattern-view)
  )

(defun rm-pattern-print (&optional key)
  (interactive)
  (let* ((key (if key key
                  (ctbl:cp-get-selected-data-cell techno-patterns))))
    (if (member key pattern-print-list)
        (setq pattern-print-list (delete key pattern-print-list)))
    (update-pattern-view)
    )
  )

(defun add-pattern ()
  (interactive)
  (add-pattern-key (ctbl:cp-get-selected-data-cell techno-patterns))
  )

(defun add-pattern-key (key)
  (let* ((pattern (gethash key pattern-data))
         (body (concat " (import java.util.concurrent.ThreadLocalRandom) (use '[overtone.core]
        '[overtone.inst.synth]
        '[techno.core :as core]
        '[techno.synths]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
         (require '[techno.sequencer :as s])
         (s/add-p core/player " pattern " " key ")")))
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print body))
    (nrepl-sync-request:eval
     body
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
      (update-pattern-view))
  )


(defun save-pattern ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns)))
    (puthash current-pattern (with-current-buffer
                       (get-buffer "tekno-pattern")
                     (buffer-string)) pattern-data)
    )
  )
(defun dec-amp ()
    (interactive)
  (let* ((pattern (ctbl:cp-get-selected-data-cell techno-patterns)))
    (nrepl-sync-request:eval
     (concat "(ns techno.core
        (:use [overtone.core]
              )
        (:require [techno.sequencer :as s]
                  [clojure.tools.reader.edn :as edn]
                  [clojure.tools.reader.reader-types :as readers]
                  [clojure.string :as string]))
(s/mod-amp player " pattern " -0.1)")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection))))
 )

(defun inc-amp ()
    (interactive)
  (let* ((pattern (ctbl:cp-get-selected-data-cell techno-patterns)))
    (nrepl-sync-request:eval
     (concat "(ns techno.core
        (:use [overtone.core]
              )
        (:require [techno.sequencer :as s]
                  [clojure.tools.reader.edn :as edn]
                  [clojure.tools.reader.reader-types :as readers]
                  [clojure.string :as string]))
(s/mod-amp player " pattern " 0.1)")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection))))
 )
(defun save-add-pattern ()
  (interactive)
  (save-pattern)
  (add-pattern-key current-pattern)
  )
(defun show-pattern-view ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns))
         (buf (get-buffer-create "tekno-pattern")))
    (with-current-buffer buf
      (funcall 'clojure-mode)
      (erase-buffer)
      (insert (replace-regexp-in-string "\\\\n" "
" (gethash key pattern-data)))
      ;(insert (concat ";;" key))
      (save-excursion
        (indent-region (point-min) (point-max) nil))
      (local-set-key (kbd "C-x C-z") 'save-pattern)
      (local-set-key (kbd "C-x C-a") 'save-add-pattern))
    (setq current-pattern key)
    (switch-to-buffer-other-window buf)
    )
  )

(defun rm-pattern ()
  (interactive)
  (rm-pattern-key (ctbl:cp-get-selected-data-cell techno-patterns))
  )
(defun rm-pattern-key (key)
  (interactive)
  (let* ()
    (nrepl-sync-request:eval
     (concat "(use
        '[techno.core :as core]
        '[techno.sequencer :as s]
        ) (s/rm-p core/player " key ")")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
      (update-pattern-view))
  )

;; (init-pattern-view)

;; (message "%s" current-playing-patterns)
;; (message "%s"  (member (intern ":sdst") current-playing-patterns))
(defun init-pattern-view ()
  (let* ((param (copy-ctbl:param ctbl:default-rendering-param))
         (meh (setf (ctbl:param-bg-colors param)
                    (lambda (model row-id col-id str)
                      (if (member (intern (replace-regexp-in-string "[\s-]+" "" str)) current-playing-patterns)
                          "blue5"
                        "black"))))
         (component (ctbl:create-table-component-buffer
                     :buffer (get-buffer-create "tekno") :model (build-pattern-model)
                     :custom-map (ctbl:define-keymap
                                  '(
                                    ("w" . ctbl:navi-move-up)
                                    ("s" . ctbl:navi-move-down)
                                    ("a" . ctbl:navi-move-left)
                                    ("d" . ctbl:navi-move-right)
                                    ("c" . ctbl:navi-jump-to-column)
                                    ("M-a" . add-pattern)
                                    ("C-M-a" . pattern-add-q)
                                    ("M-r" . rm-pattern)
                                    ("C-M-x" . pattern-rm-q)
                                    ("C-M-g" . pattern-flush-q)
                                    ("C-M-u" . update-pattern-view)
                                    ("C-M-<down>" . dec-amp)
                                    ("C-M-<up>" . inc-amp)

                                    ("C-e" . ctbl:navi-move-right-most)
                                    ("C-a" . ctbl:navi-move-left-most)
                                    ("<return>" . show-pattern-view)
                                    ("g" . ctbl:action-update-buffer)

                                    ([mouse-1] . ctbl:navi-on-click)
                                    ("C-m" . ctbl:navi-on-click)
                                    ("C-p" . add-pattern-print)
                                    ("M-p" . rm-pattern-print)
                                    ("C-M-r" . clear-pattern-print)
                                    ("C-M-p" . pattern-print-add-all)
                                    ))
                     :param param))
         )
    (setq techno-patterns component)
    (pop-to-buffer (ctbl:cp-get-buffer component))
    (goto-line 3)
    (forward-char 1)
    )
  )

(defun update-pattern-view ()
  (interactive)
  (let ((p (point)))
      (setq current-playing-patterns (get-patterns))
    (ctbl:cp-set-model techno-patterns (build-pattern-model))
                                        ;(pop-to-buffer (ctbl:cp-get-buffer techno-patterns))
    (with-current-buffer (ctbl:cp-get-buffer techno-patterns)
      (end-of-buffer)
      (read-only-mode -1)
      (insert "\n\n\n")
      (insert (format "Queue Add: %s \n" pattern-queue-add))
      (insert (format "Queue Rm: %s \n" pattern-queue-rm))
      (insert "\n\n")
      (insert "Showing: " (format "%s" pattern-print-list))
      (goto-char p)
      (read-only-mode t)
      )
    )
  )

(defun new-pattern (&optional type)
  (interactive)
  (let ((type (read-string "Type: ")))
      (cond ((string= type "phrase")
             (puthash (concat ":phrase" (format "%s" (tekno:uid)))
                      "(let []
     (s/phrase-p
      bass-synth
      []
      0.25 1 [:attack 0.1 :release 0.3])
    )" pattern-data))
            ((string= type "drum")
             (puthash (concat ":drum" (format "%s" (tekno:uid)))
                      "(let []
     (drum-p
      [:Kit4-Electro]
      [])
    )" pattern-data))
            (t (puthash (concat ":pattern" (format "%s" (tekno:uid)))
                        "(let []
    )" pattern-data))))
  (update-pattern-view)
  )
