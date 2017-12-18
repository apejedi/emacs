(require 'clomacs)
(require 'cl-lib)

(clomacs-defun get-patterns techno.core/get-patterns :return-type :list)

(clomacs-defun get-patterns-from-string techno.core/get-patterns-from-string :return-type :list)

(clomacs-defun get-pattern-str techno.core/get-pattern-str)
(clomacs-defun get-pattern-fx techno.core/get-pattern-fx)
(clomacs-defun player-active? techno.core/player-active?)
(clomacs-defun get-synths techno.core/get-synths :return-type :list)
(clomacs-defun get-annotated-pattern techno.core/get-annotated-pattern :return-type :list)

(setq current-playing-patterns '())
(setq pattern-queue-add '())
(setq pattern-queue-rm '())
(setq pattern-queue-mod '())
(setq pattern-print-list '())
(setq techno-patterns nil)
(setq use-player t)
(setq pattern-sizes (make-hash-table :test 'equal))
(setq tempo "80")
(setq synth-params (make-hash-table :test 'equal))
(setq synth-stack (make-hash-table :test 'equal))
(setq category-component nil)
(setq synth-component nil)
(setq stack-component nil)
(setq categories (make-hash-table :test 'equal))
(setq synths (make-hash-table :test 'equal))
(setq synth-defaults (make-hash-table :test 'equal))
(setq root-note "C4")
(setq scale-type "major")
(setq div "8")
(setq highlight-bounds (make-hash-table :test 'equal))
(setq cur-pos nil)

(set-face-foreground 'ctbl:face-row-select "white")
(set-face-background 'ctbl:face-row-select "blue5")
(set-face-bold-p 'ctbl:face-row-select t)
(set-face-foreground 'ctbl:face-cell-select "black")
(set-face-background 'ctbl:face-cell-select "yellow")
(set-face-bold-p 'ctbl:face-cell-select t)

(load-file "server.el")

(defvar tekno:uid 1)

(defun tekno:uid ()
  "[internal] Generate an unique number."
  (incf tekno:uid))


(defun use-player ()
  (interactive)
  (setq use-player t)
  )

(defun use-sequencer ()
  (interactive)
  (setq use-player nil)
  )
(defun ctbl:find-by-cell-id (dest cell-id)
  "[internal] Return a point where the text property `ctbl:cell-id'
is equal to cell-id in the current table view. If CELL-ID is not
found in the current view, return nil."
  (loop
   with pos = (ctbl:dest-point-min dest)
   ;with pos = (ctbl:find-position-fast dest cell-id)
   ;with pos = (point-min)
        with end = (ctbl:dest-point-max dest)
        for next = (next-single-property-change pos 'ctbl:cell-id nil end)
        for text-cell = (and next (ctbl:cursor-to-cell next))
        while (and next (< next end)) do
        (if (and text-cell (equal cell-id text-cell))
            (return next))
        (setq pos next)))

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
                          (if (and pattern (intern pattern)
                               (member (intern pattern) current-playing-patterns))
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
    ;;   (print (car el)))
    (setq tempo "80")
    (dolist (el data)
      (if (not (string= (car el) ":tempo"))
          (progn (puthash (car el) (car (cdr el)) table)
                 (get-pattern-size (car (cdr el)) (car el)))
        (setq tempo (car (cdr el))))
      )
    (setq pattern-data table))
  )


(defun load-patterns ()
  (interactive)
  (let* ((s (read-string "Sketch: "))
         (f (if use-player "sketches2.clj" "sketches.clj")))
    (load-patterns-from-buffer f s)
    (start-player)
    (update-pattern-view))
  )

(defun refresh-patterns ()
  (interactive)
  (let* ((key (read-string "pattern: "))
         (s (read-string "type: "))
         (table (if pattern-data pattern-data (make-hash-table :test 'equal))))
    (dolist (p (get-patterns))
      (if (or (string= key (symbol-name p)) (string= key ":all"))
          (puthash (symbol-name p) (get-pattern-str p s) table))
      )
    (setq pattern-data table)
    (update-pattern-view))
  )


;; (load-patterns-from-buffer "sketches.clj" "track2")
;; (update-pattern-view)



(defun pattern-mod-q ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns)))
    (if (not (member key pattern-queue-rm))
        (setq pattern-queue-mod (cons key pattern-queue-mod)))
    (update-pattern-view)
    )
  )
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
    (if (and (not (member key pattern-queue-add)) (not (member key pattern-queue-mod)))
        (setq pattern-queue-rm (cons key pattern-queue-rm)))
    (setq pattern-queue-add (delete key pattern-queue-add))
    (setq pattern-queue-mod (delete key pattern-queue-mod))
    (update-pattern-view)
    )
  )

(defun pattern-mute-q ()
  (interactive)
  (dolist (a pattern-queue-add)
    (add-pattern-key a nil t)
    )
  (setq pattern-queue-mod pattern-queue-add)
  (setq pattern-queue-add '())
  (update-pattern-view)
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
    (setq pattern-queue-mod '())
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

(defun set-tempo ()
  (interactive)
  (let* ((te (read-string "bpm: ")))
    (setq tempo te)
    )
  (update-pattern-view)
  )


(defun start-player ()
  (interactive)
  (let* ((init (if use-player (concat " (if (not (contains? (p/scheduled-jobs) player))
                     (def player (p/get-s 80 {:div 8})))"
                                      (if tempo (concat " (p/set-sp player " tempo ")") ""))
                "(if (or (nil? player) (not (node-active? player)))
                    (let [p (s/get-s
                      (/ 80 60)
                      )]
                    (def player p)
                    ))"
                ))
        (res (nrepl-sync-request:eval
              (concat "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [techno.player :as p]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
" init "
(def sync-to-midi (atom true))
(let [started (atom false)
              stopped (atom false)]
  (on-event
   [:midi nil]
   (fn [m]
       ;; (println (:status m))
       ;; (when (= (:status m) :song-position-pointer)
       ;;   (println \"pointer\"))
       (when (and (not @stopped)
                  ;; (= (:status m) :start)
                  (= (:status m) :song-position-pointer)
                  @sync-to-midi)
         (println \"syncing\")
         ;(techno.synths/o-kick)        ;
         (s/reset-s player)
         (reset! started true)
         )
       (when (= (:status m) :stop)
         (reset! stopped true)
         )
       (when (and (= (:status m) :song-position-pointer) @stopped)
         (println \"stopping\")
         (reset! started false)
         (reset! stopped false))
       ) :midi-clock))
")
            (cider-current-connection)
            (clomacs-get-session (cider-current-connection)))))
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print res))
(add-dummy-p)
(update-pattern-view)
)
)

(defun start-stop-player ()
  (interactive)
  (if (string= "true" (player-active?))
      (stop-player)
    (start-player))
  )
(defun play-pattern ()
  (interactive)
  (save-pattern)
  (let* ((pattern (with-current-buffer
                       (get-buffer "tekno-pattern")
                       (replace-regexp-in-string "^[ \n]*" "" (buffer-string))
                       ))
         (body (concat " (import java.util.concurrent.ThreadLocalRandom) (use '[overtone.core]
        '[techno.core :as core]
        '[techno.synths]
        '[overtone.inst.synth]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
         (require '[techno.sequencer :as s]
                  '[techno.player :as p])
         (p/play-p " pattern " " (if tempo tempo "80") ")"))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection)))))
    ;; (if t
    ;;     (with-output-to-temp-buffer "*scratch*"
    ;;       (print body)
    ;;       (print res)))
    )
  )

(defun sync-to-midi ()
  (interactive)
  (nrepl-sync-request:eval
           "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
(reset! sync-to-midi true)

"
            (cider-current-connection)
            (clomacs-get-session (cider-current-connection)))
  )
(defun stop-sync-to-midi ()
  (interactive)
  (nrepl-sync-request:eval
           "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
(reset! sync-to-midi false)

"
            (cider-current-connection)
            (clomacs-get-session (cider-current-connection)))
  )

(defun add-dummy-p ()
  (nrepl-sync-request:eval
           "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
  (s/add-p player {1.75 [nil []]} :sync {:no-group true})

"
            (cider-current-connection)
            (clomacs-get-session (cider-current-connection)))
  )

(defun start-midi-player2 ()
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
    (let [bus (control-bus)
          active (atom false)]
      (def player (s/midi-s bus)))
  )"
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
  )

(defun start-midi-player ()
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
    (let [bus (control-bus)
          active (atom false)]
      (on-event
       [:midi nil]
       (fn [m]
           (when (.contains (name (:status m)) \"start\")
             (reset! active true))
           (when (and (= (:status m) :timing-clock))
             (control-bus-set! bus 1)
             )
           )
       :midi-clock)
      (def player (s/midi-s bus)))
  )"
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
  )

(defun sync-to-midi ()
  (interactive)

  )

(defun set-player-sp ()
  (interactive)
  (let* ((sp (read-string "Speed: "))
         (p (if use-player "p/set-sp" "s/set-sp"))
         (req (concat "(ns techno.core
  (:use [overtone.core]
        )
  (:require [techno.sequencer :as s]
            [techno.player :as p]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as string]))
(if " (if use-player " (p/active? player) " "(and (not (nil? player)) (node-active? player))") "
    (" p " player " (if use-player sp (concat "(/ " sp " 60)")) "))"))
         (res (nrepl-sync-request:eval
           req
            (cider-current-connection)
            (clomacs-get-session (cider-current-connection)))))
(setq tempo sp)
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print req))

)
  )

(defun stop-player ()
  (interactive)
  (nrepl-sync-request:eval
   (concat "(ns techno.core
        (:use [overtone.core]
              )
        (:require [techno.sequencer :as s]
                  [techno.player :as p]
                  [clojure.tools.reader.edn :as edn]
                  [clojure.tools.reader.reader-types :as readers]
                  [clojure.string :as string]))
(" (if use-player "p/stop-s" "kill")" player)
(remove-event-handler :midi-clock)
")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
(update-pattern-view)
  )

(defun add-pattern-print (&optional key)
  (interactive)
  (let* ((key (if key key (ctbl:cp-get-selected-data-cell techno-patterns))))
    (setq pattern-print-list
          (if (member key pattern-print-list) pattern-print-list (cons key pattern-print-list)))
    (update-pattern-view)
    (show-pattern-struct)
    )
  )


(defun clear-pattern-print ()
  (interactive)
  (setq pattern-print-list '())
  (update-pattern-view)
  (show-pattern-struct)
  )

(defun pattern-print-add-all ()
  (interactive)
  (cl-loop for k being the hash-keys of pattern-data
           do
           (add-pattern-print k)
           )
  (update-pattern-view)
  (show-pattern-struct)
  )

(defun pattern-print-add-playing ()
  (interactive)
  (setq pattern-print-list '())
  (dolist (p (get-patterns))
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print p))
    (add-pattern-print (format "%s" p))
    )
  (update-pattern-view)
  (show-pattern-struct)
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

(defun queue-add-pattern ()
  (interactive)
  (add-pattern-key (ctbl:cp-get-selected-data-cell techno-patterns) t)
  )

(defun add-pattern-mute ()
  (interactive)
  (add-pattern-key (ctbl:cp-get-selected-data-cell techno-patterns) nil t)
  )

(defun get-pattern-struct (&optional keys)
  (let* ((keys (if keys keys pattern-print-list))
         (patterns (mapconcat
                    (function (lambda (p)
                                (gethash p pattern-data)))
                    keys
                    ""))
         (body (concat " (import java.util.concurrent.ThreadLocalRandom) (use '[overtone.core]
        '[overtone.inst.synth]
        '[techno.core :as core]
        '[techno.synths]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
         (require '[techno.sequencer :as s]
                   '[techno.sequencer :as p])
         (core/get-merged-str " patterns ")"))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection)))
              ))
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print body)
    ;;   (print res))
    (if (member "out" res)
        (car (nthcdr (+ 1 (cl-position "out" res :test 'equal)) res))
      "")
    )
  )

(defun add-pattern-key (key &optional add-at-1 mute)
  (let* ((pattern (gethash key pattern-data))
         (attrs "{:wrap true")
         (attrs (if add-at-1 (concat attrs " :add-at-1 true ") attrs))
         (attrs (if mute (concat attrs " :volume 0 ") attrs))
         (attrs (concat attrs "}"))
         (add-p (if use-player "p/add-p" "s/add-p"))
         (body (concat " (import java.util.concurrent.ThreadLocalRandom) (use '[overtone.core]
        '[techno.core :as core]
        '[techno.synths]
        '[overtone.inst.synth]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
         (require '[techno.sequencer :as s]
                  '[techno.player :as p])
         (" add-p " core/player " pattern " " key " " attrs ")"))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         (err (nrepl-dict-get res "err")))
    (if (stringp err)
        (with-output-to-temp-buffer "*scratch*"
          (print err)))
      (update-pattern-view))
  )

(defun save-pattern ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns)))
    (puthash current-pattern (with-current-buffer
                       (get-buffer "tekno-pattern")
                       (replace-regexp-in-string "^[ \n]*" "" (buffer-string))
                       ) pattern-data)
    (get-pattern-size (gethash current-pattern pattern-data) current-pattern)
    ;(cider--display-interactive-eval-result (get-pattern-struct (cons current-pattern '())))
    (update-pattern-view)
    (show-pattern-struct)
    (with-current-buffer
        (get-buffer "tekno-pattern")
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):|")
      (get-pattern-bounds))
    )
  )

(defun dec-amp-big () (interactive) (dec-amp t))
(defun inc-amp-big () (interactive) (inc-amp t))
(defun dec-amp (&optional big)
    (interactive)
    (let* ((pattern (ctbl:cp-get-selected-data-cell techno-patterns))
           (delta (if big "-0.1" "-0.01"))
           (mod-amp (if use-player "p/mod-amp" "s/mod-amp"))
           (pattern-str (if (> (length pattern-queue-mod) 0)
                            (apply 'concat
                                   (mapcar (lambda (p)
                                             (format "\n(%s player %s %s)\n" mod-amp p delta)
                                             ) pattern-queue-mod))
                          (format "(%s player %s %s)" mod-amp pattern delta)))
           )
      (nrepl-sync-request:eval
             (concat "(ns techno.core
        (:use [overtone.core]
              )
        (:require [techno.sequencer :as s]
                  [clojure.tools.reader.edn :as edn]
                  [clojure.tools.reader.reader-types :as readers]
                  [clojure.string :as string]))"
                     pattern-str)
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
)
 )

(defun inc-amp (&optional big)
    (interactive)
    (let* ((pattern (ctbl:cp-get-selected-data-cell techno-patterns))
           (delta (if big "0.1" "0.01"))
           (mod-amp (if use-player "p/mod-amp" "s/mod-amp"))
           (pattern-str (if (> (length pattern-queue-mod) 0)
                            (apply 'concat
                                   (mapcar (lambda (p)
                                             (format "\n(%s player %s %s)\n" mod-amp p delta)
                                             ) pattern-queue-mod))
                          (format "(%s player %s %s)" mod-amp pattern delta))))
    (nrepl-sync-request:eval
     (concat "(ns techno.core
        (:use [overtone.core]
              )
        (:require [techno.sequencer :as s]
                  [clojure.tools.reader.edn :as edn]
                  [clojure.tools.reader.reader-types :as readers]
                  [clojure.string :as string]))"
             pattern-str)
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection))))
 )



(defun save-add-pattern ()
  (interactive)
  (save-pattern)
  (add-pattern-key current-pattern)
  )

(defun show-pattern-struct ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns))
         (buf (get-buffer-create "tekno-pattern-struct")))
    (with-current-buffer buf
      (funcall 'clojure-mode)
      (funcall 'toggle-truncate-lines nil)
      (erase-buffer)
      (insert (get-pattern-struct))
      (save-excursion
        (indent-region (point-min) (point-max) nil)))
    ;(switch-to-buffer-other-window buf)
    )
  )

(defun show-pattern-view ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns))
         (buf (get-buffer-create "tekno-pattern")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (replace-regexp-in-string "\\\\n" "
" (gethash key pattern-data)))
      (funcall 'clojure-mode)
      ;(insert (concat ";;" key))
      (save-excursion
        (indent-region (point-min) (point-max) nil))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):|")
      (local-set-key (kbd "C-x C-z") 'save-pattern)
      (local-set-key (kbd "C-x C-a") 'save-add-pattern)
      (local-set-key (kbd "C-p") 'add-pattern-print)
      (local-set-key (kbd "M-p") 'play-pattern)
      (local-set-key (kbd "C-M-r") 'clear-pattern-print)
      (local-set-key (kbd "C-M-p") 'pattern-print-add-playing)
      (local-set-key (kbd "C-=") 'start-stop-player)
      )
    (setq current-pattern key)
    (switch-to-buffer-other-window buf)
    (get-pattern-bounds)
    )
  )

(defun rm-pattern ()
  (interactive)
  (rm-pattern-key (ctbl:cp-get-selected-data-cell techno-patterns))
  )
(defun rm-pattern-key (key)
  (interactive)
  (let* ((rm-p (if use-player "p/rm-p" "s/rm-p")) )
    (nrepl-sync-request:eval
     (concat "(use
        '[techno.core :as core]
        '[techno.sequencer :as s]
        '[techno.player :as p]
        ) (" rm-p " core/player " key ")")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
      (update-pattern-view))
  )

;; (init-pattern-view)
; (init-synth-page)

;; (message "%s" current-playing-patterns)
;; (message "%s"  (member (intern ":sdst") current-playing-patterns))
(defun init-pattern-view ()
  (let* ((param (copy-ctbl:param ctbl:default-rendering-param))
         (meh (setf (ctbl:param-bg-colors param)
                    (lambda (model row-id col-id str)
                      (if (and (stringp str) (member (intern (replace-regexp-in-string "[\s-]+" "" str)) current-playing-patterns))
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
                                    ("M-q" . queue-add-pattern)
                                    ("M-f" . add-pattern-mute)
                                    ("C-M-a" . pattern-add-q)
                                    ("M-r" . rm-pattern)
                                    ("C-M-x" . pattern-rm-q)
                                    ("C-M-g" . pattern-flush-q)
                                    ("C-M-f" . pattern-mute-q)
                                    ("C-M-m" . pattern-mod-q)
                                    ("C-M-u" . update-pattern-view)
                                    ("M-<down>" . dec-amp-big)
                                    ("M-<up>" . inc-amp-big)
                                    ("S-<down>" . dec-amp)
                                    ("S-<up>" . inc-amp)

                                    ("C-e" . ctbl:navi-move-right-most)
                                    ("C-a" . ctbl:navi-move-left-most)
                                    ("<return>" . show-pattern-view)
                                    ("g" . ctbl:action-update-buffer)

                                    ([mouse-1] . ctbl:navi-on-click)
                                    ("C-m" . ctbl:navi-on-click)
                                    ("C-p" . add-pattern-print)
                                    ("M-p" . rm-pattern-print)
                                    ("C-M-r" . clear-pattern-print)
                                    ("C-M-p" . pattern-print-add-playing)
                                    ("C-=" . start-stop-player)
                                    ))
                     :param param))
         )
    (setq techno-patterns component)
    (ctbl:cp-add-selection-change-hook component 'update-pattern-view)
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
    (display-pattern-info)
    )
  )

(defun quantize-recorded-pattern ()
  (interactive)
  (kill-new
   (let* ((quant (if use-player "mk-map-p" "quantize-time-pattern"))
          (res (nrepl-sync-request:eval
                  (concat "(ns techno.core
  ) (get-pattern-str (techno.recorder/" quant "))")
                  (cider-current-connection)
                  (clomacs-get-session (cider-current-connection))))
               (str (if (member "value" res)
                        (car (nthcdr (+ 1 (cl-position "value" res :test 'equal)) res))
                      "nothing loaded"))
               (str (replace-regexp-in-string "^nil" ""
                                   (replace-regexp-in-string "\\\\n" "
"
                                                             (replace-regexp-in-string "\"" "" str))))
               )
          str

          ))
  )

(defun record-pattern ()
  (interactive)
  (nrepl-sync-request:eval
   (concat "(ns techno.recorder
  ) (start-record-pattern)")
   (cider-current-connection)
   (clomacs-get-session (cider-current-connection)))
  )

(defun stop-record-pattern ()
  (interactive)
  (nrepl-sync-request:eval
   (concat "(ns techno.recorder
  ) (stop-record-pattern)")
   (cider-current-connection)
   (clomacs-get-session (cider-current-connection)))
  )

(defun play-recorded-pattern ()
  (interactive)
  (nrepl-sync-request:eval
   (concat "(ns techno.recorder
  ) (play-time-pattern)")
   (cider-current-connection)
   (clomacs-get-session (cider-current-connection)))
  )


(defun rename-pattern ()
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns))
         (to (read-string "To: ")))
    (puthash to
             (gethash key pattern-data) pattern-data)
    (remhash key pattern-data)
    (if (member (intern key) (get-patterns))
        (progn
            (rm-pattern-key key)
            (add-pattern-key to)))
    (update-pattern-view)
    )
  )



(defun new-pattern (&optional type)
  (interactive)
  (let (;(type (read-string "Type: "))
        (name (concat ":" (read-string "Name: "))))
    (puthash name
                        "(let []
    )" pattern-data)
  (update-pattern-view)
  )
  )

(defun export-queued ()
  (interactive)
  (kill-new
   (concat "(def sketch
{"  (apply 'concat (loop for k in pattern-queue-add
                         collect (concat k " "(gethash k pattern-data) "
")))
 "
})")
   )
  )

(defun display-pattern-info ()
  (interactive)
  (let*
      ((key (ctbl:cp-get-selected-data-cell techno-patterns))
       (pattern (gethash key pattern-data))

)
      (with-current-buffer (ctbl:cp-get-buffer techno-patterns)
        (end-of-buffer)
        (read-only-mode -1)
        (insert "\n\n\n")
        (insert (format "Queue Add: %s \n" pattern-queue-add))
        (insert (format "Queue Rm: %s \n" pattern-queue-rm))
        (insert (format "Queue Mod: %s \n" pattern-queue-mod))
        (insert (format "Tempo: %s \n" tempo))
        (insert (format "Player Active: %s \n" (player-active?)))
        (insert "\n\n")
        (insert (format "Pattern: %s %s" key (gethash key pattern-sizes)))
        (insert "\n\n")
        (insert "Showing: " (format "%s" pattern-print-list) "\n\n")

        ;; (insert "FX: \n" (replace-regexp-in-string "\\\\n" "
        ;; " (get-pattern-fx (ctbl:cp-get-selected-data-cell techno-patterns))) "\n\n")
                                        ;(insert (get-pattern-struct))
        (goto-char p)
        (read-only-mode t)
        ))
  )

(defun get-pattern-size (pattern &optional key)
  (let* ((body (concat " (import java.util.concurrent.ThreadLocalRandom) (use '[overtone.core]
        '[techno.core :as core]
        '[techno.synths]
        '[overtone.inst.synth]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
         (require '[techno.sequencer :as s]
                  '[techno.player :as p])
         (let [data " pattern "] (print (p/get-pos
(p/p-size data) (:div data))))"))
       (res (nrepl-dict-get (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))) "out")))
(if key (puthash key res pattern-sizes))
res
)

)

(defun switch-component ()
  (interactive)
  (let ((cur (if (not (get-text-property (point) 'ctbl:component))
                 stack-component
                 (ctbl:cp-get-component))))
    (if (eq cur category-component)
        (goto-char (ctbl:find-by-cell-id (ctbl:component-dest synth-component) (ctbl:cell-id 0 0)))
      (if (eq cur synth-component)
          (goto-char (ctbl:find-by-cell-id (ctbl:component-dest stack-component) (ctbl:cell-id 0 0)))
        (goto-char (ctbl:find-by-cell-id (ctbl:component-dest category-component) (ctbl:cell-id 0 0)))
        )
      )
    )
  )

(defun update-synth-stack ()
  (if (not (equal (ctbl:cp-get-selected-data-cell category-component) "samples"))
      (let* ((s (nth 0 (nth (car (ctbl:component-selected stack-component)) (ctbl:component-sorted-data stack-component))))
             (s (if (member s (hash-table-keys synth-stack)) s (car (hash-table-keys synth-stack))))
             (data (mapcar
                    (lambda (k)
                      (append (list k)
                              (if (eq s k)
                                  (progn
                                    (setq s-key k)
                                    (hash-table-values (gethash k synth-params)))
                                '()))
                      ) (hash-table-keys synth-stack))))
        (ctbl:cp-set-model
         stack-component
         (ctbl:make-model-from-list
          data
          (append (list "Synth")
                  (mapcar (lambda (k)
                            (format "%s %s" k (gethash k (gethash s-key synth-defaults))))
                          (hash-table-keys (gethash s-key synth-defaults))))
          ))
        )
    (let* ((k (ctbl:cp-get-selected-data-cell synth-component))
           (s (ctbl:cp-get-selected-data-cell stack-component))
           (body (format "(ns techno.core) ((get-in drum-kits [:%s :%s]))" k s))
           (res (nrepl-sync-request:eval
                 body
                 (cider-current-connection)
                 (clomacs-get-session (cider-current-connection)))))
        )
    )
  )


(defun init-synth-page ()
  (get-buffer-create "synth-page")
  (let* ((data (get-synths))
         (keymap (ctbl:define-keymap
                  '(
                    ("w" . ctbl:navi-move-up)
                    ("s" . ctbl:navi-move-down)
                    ("a" . ctbl:navi-move-left)
                    ("d" . ctbl:navi-move-right)
                    ("c" . ctbl:navi-jump-to-column)
                    ("M-a" . (lambda ()
                               (interactive)
                               (if (equal (ctbl:cp-get-selected-data-cell category-component) "sketches")
                                   (progn
                                     (load-patterns-from-buffer
                                      "sketches2.clj"
                                      (ctbl:cp-get-selected-data-cell synth-component))
                                     (start-player)
                                     (update-pattern-view)
                                     (switch-to-buffer-other-window (get-buffer "tekno")))
                                 (let ((s (ctbl:cp-get-selected-data-cell synth-component)))
                                   (if (not (gethash s synth-stack))
                                       (puthash s
                                                (+ (hash-table-count synth-stack) 1)
                                                synth-stack))
                                   (ctbl:cp-set-selected-cell stack-component (ctbl:cell-id 0 0))))
                               (update-synth-stack)
                               (set-synth-handler)
                               ))
                    ("M-r" . (lambda ()
                               (interactive)
                               (let ((s (ctbl:cp-get-selected-data-cell stack-component)))
                                 (remhash s synth-stack))
                               (update-synth-stack)
                               (set-synth-handler)
                               ))
                    ("M-e" . (lambda ()
                               (interactive)
                               (let* ((p (ctbl:component-selected stack-component))
                                      (s (nth 0 (nth
                                                 (car p)
                                                 (ctbl:component-sorted-data stack-component))))
                                      (v (read-string "val: ")))
                                 (puthash (nth (- (cdr p) 1)
                                               (hash-table-keys (gethash s synth-params)))
                                          (string-to-number v)
                                          (gethash s synth-params))
                                 (update-synth-stack)
                                 (set-synth-handler)
                                 )
                               ))
                    ))))
    (dolist (d data)
      (puthash (car d) (car (cdr d)) categories)
      (dolist (s (car (cdr d)))
        (puthash (car s) (car (cdr s)) synths)
        (let ((defaults  (make-hash-table :test 'equal))
              (params  (make-hash-table :test 'equal)))
          (dolist (p (car (cdr s)))
            (puthash (car p) (car (cdr p)) defaults)
            (puthash (car p) (car (cdr p)) params)
            )
          (puthash (car s) defaults synth-defaults)
          (puthash (car s) params synth-params)
          )
        )
      )
    (with-current-buffer "synth-page"
      (local-set-key (kbd "<tab>") 'switch-component)
      (erase-buffer)
      (goto-char (point-min))
                                        ;(insert (format "%s" (list (hash-table-keys categories))))
      (insert "Categories:\n")
      (setq category-component (ctbl:create-table-component-region
                                :model (ctbl:make-model-from-list
                                        (list (hash-table-keys categories)))
                                :keymap keymap))
      (insert "\n\n")
      (insert "Synths:\n\n")
      (setq synth-component
            (ctbl:create-table-component-region
             :model (ctbl:make-model-from-list
                     (seq-partition
                      (mapcar 'car (gethash (car (hash-table-keys categories)) categories))
                      8))
             :keymap keymap
             ))
      (insert "\n\n")
      (insert "Stack:\n\n")
      (setq stack-component
            (ctbl:create-table-component-region
             :model (ctbl:make-model-from-list '((1)))
             :keymap keymap
             ))
      (ctbl:cp-add-selection-change-hook
       category-component
       (lambda ()
         (ctbl:cp-set-model
          synth-component
          (ctbl:make-model-from-list
           (seq-partition
            (mapcar 'car (gethash (ctbl:cp-get-selected-data-cell category-component) categories))
            8)))
         (goto-char (ctbl:find-by-cell-id (ctbl:component-dest category-component) (ctbl:cp-get-selected category-component)))
         ))
      (ctbl:cp-add-selection-change-hook
       synth-component
       (lambda ()
         (if (equal (ctbl:cp-get-selected-data-cell category-component) "samples")
             (progn
               (ctbl:cp-set-model
                stack-component
                (ctbl:make-model-from-list
                 (seq-partition
                  (hash-table-keys (gethash (ctbl:cp-get-selected-data-cell synth-component) synth-params))
                  5))
                )
               (goto-char (ctbl:find-by-cell-id (ctbl:component-dest synth-component) (ctbl:cp-get-selected synth-component))))
           )
         ))

      (ctbl:cp-add-selection-change-hook
       stack-component
       'update-synth-stack)
      )
    )
  )

(defun set-scale ()
  (interactive)
  (setq root-note (read-string "root: "))
  (setq scale-type (read-string "type: "))
  )
(defun set-div ()
  (interactive)
  (setq div (read-string "div: "))
  )

(defun get-scale-p ()
  (interactive)
  (let* ((body (format "(ns techno.core
                          (:use [techno.recorder]))
                        (get-seq-p %s %s
                           #(degree-fn (scale %s %s) %%)
                           #(if (not (= :| %%)) (keyword (str \"0\" (name %%))) %%))"
                       tempo div root-note scale-type))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         (res (substring (nrepl-dict-get res "value") 3))
         (res (replace-regexp-in-string ":|" ":|
" res)))
    (kill-new
     (apply 'concat
            (loop for k being the hash-keys of synth-stack
                  collect
                  (format
                   "(p/scale-p
%s
%s %s
%s
1/%s 0 [%s])

"                  k root-note scale-type res div
                   (apply 'concat
                          (loop for p being the hash-keys of (gethash k synth-params)
                                collect (if (and (not (equal p "note"))
                                                 (not (equal p "freq"))
                                                 (not (equal p "out-bus")))
                                            (concat ":" p " "
                                                    (number-to-string
                                                     (gethash p (gethash k synth-params)))
                                                    " ")
                                          ""))))))
     )
    )
  )

(defun remove-synth-effects ()
  (interactive)
  (let* ((body "(ns techno.core)
                        (group-clear synth-grp)")
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         )
      )
  )

(defun add-synth-effects ()
  (interactive)
  (let* ((effects (buffer-substring-no-properties (region-beginning) (region-end)))
         (body (format "(ns techno.core)
                        (group-clear synth-grp)
                        (doseq [[k e] %s] (apply (first e)
                        (concat [[:head synth-grp] :audio-bus 0] (vec (rest e)))))"
                       effects))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         )
      )
  )



(defun set-synth-handler ()
  (let* ((synths (format "[%s]" (apply 'concat
                                       (loop for k being the hash-keys of synth-stack
                                             collect
                                             (concat " [" k " ["
                                                      (apply 'concat
                                                             (loop for p being the hash-keys of (gethash k synth-params)
                                                                   collect (if (and (not (equal p "note")) (not (equal p "freq")) (not (equal p "out-bus")))
                                                                               (concat ":" p " " (number-to-string (gethash p (gethash k synth-params))) " ")
                                                                             "")))
                                                      "] " "] ")))))
         (body (format
                "(ns techno.core)
(if (not (node-active? synth-grp))
    (def synth-grp (group :tail 2)))
(let [params %s]
    (on-event [:midi :note-on]
              (fn [m]
                  (let [args (fn [s] (cond (not (= -1 (.indexOf (vec (map (fn [p] (:name p)) (:params s))) \"note\"))) [:note (:data1 m)]
                                           (not (= -1 (.indexOf (vec (map (fn [p] (:name p)) (:params s))) \"freq\"))) [:freq (midi->hz (:data1 m))]
                                           (not (= -1 (.indexOf (vec (map (fn [p] (:name p)) (:params s))) \"freq1\"))) [:freq1 (midi->hz (:data1 m))]
                                           true []))
                      play (fn [synth args]
                               (techno.recorder/record-action [synth args (:data1 m)])
                               (apply synth (concat [[:head synth-grp]] (vec args)))
                             )]
                  (doseq [[s p] params]
                         (play s (concat p (args s))))
                  ))
              ::prophet-midi))" synths))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         )
    ;; (with-current-buffer "*scratch*"
    ;;   (insert (format "%s"  body))
    ;;   (insert (format "%s"  res))
    ;;   )
  ))


(defun get-pattern-bounds ()
  (let* ((text (with-current-buffer "tekno-pattern" (buffer-string)))
         (bounds (get-annotated-pattern text))
         (res (nrepl-sync-request:eval
               (format "(ns techno.player) (reset! send-offsets %s)
                        (if (nil? @tekno-client) (mk-tekno-client))" current-pattern)
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection)))))
    (clrhash highlight-bounds)
    (dolist (b bounds)
      (puthash (car b) (cons (car (car (cdr b))) (car (cdr (car (cdr b)))))
               highlight-bounds))
    )
  )


(defun highlight-pattern-pos (&optional string)
  (interactive)
  (setq cur-pos string)
  )

(progn
  (define-eval-sexp-fu-flash-command highlight-pattern-pos
    (eval-sexp-fu-flash (lambda ()
                          (gethash cur-pos highlight-bounds)
                          )))
  (tekno-server-start)
  )
