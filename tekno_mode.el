(require 'clomacs)
(require 'cl-lib)

(clomacs-defun get-patterns techno.core/get-patterns :return-type :list)

(clomacs-defun get-patterns-from-string techno.core/get-patterns-from-string :return-type :list)

(clomacs-defun get-pattern-str techno.core/get-pattern-str)
(clomacs-defun get-pattern-fx techno.core/get-pattern-fx)
(clomacs-defun player-active? techno.core/player-active?)
(clomacs-defun get-synths techno.core/get-synths :return-type :list)
(clomacs-defun get-annotated-pattern techno.core/get-annotated-pattern :return-type :list)
(clomacs-defun get-step-mode-bounds techno.core/get-step-mode-bounds :return-type :list)

(setq current-playing-patterns '())
(setq pattern-queue-add '())
(setq pattern-queue-rm '())
(setq pattern-queue-mod '())
(setq pattern-print-list '())
(setq techno-patterns nil)
(setq use-player t)
(setq pattern-sizes (make-hash-table :test 'equal))
(setq step-mode-hash (make-hash-table :test 'equal))
(setq tempo "80")
(setq synth-params (make-hash-table :test 'equal))
(setq synth-stack (make-hash-table :test 'equal))
(setq category-component nil)
(setq synth-component nil)
(setq stack-component nil)
(setq step-component nil)
(setq categories (make-hash-table :test 'equal))
(setq synths (make-hash-table :test 'equal))
(setq synth-defaults (make-hash-table :test 'equal))
(setq pattern-fx (make-hash-table :test 'equal))
(setq root-note "C4")
(setq scale-type "major")
(setq div "8")
(setq step-bars 4)
(setq highlight-bounds (make-hash-table :test 'equal))
(setq step-bounds (make-hash-table :test 'equal))
(setq pattern-positions (make-hash-table :test 'equal))
(setq pattern-step-mode nil)
(setq cur-pos nil)
(setq player-info nil)
(setq fx-chooser nil)
(setq fx-stack nil)
(setq fx-data (make-hash-table :test 'equal))
(setq tekno-components '(techno-patterns player-info fx-chooser fx-stack))
(nconc tekno-components tekno-components)

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
                          (if (or (and pattern (intern pattern)
                                       (member (intern pattern) current-playing-patterns)
                                       (string= "tekno" (buffer-name)))
                                  (and (gethash (format "[%s %s]" (+ 1 (car tcell-id))
                                                        (+ 1 (cdr tcell-id))) step-mode-hash)
                                       (string= "step-sequencer" (buffer-name)))
                                  )
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
         ;; (body (format  "(ns techno.core)
         ;;                   (get-patterns-from-string \"%s\" \"%s\")"
         ;;                contents sketch ))
         ;; (res (nrepl-sync-request:eval
         ;;       body
         ;;       (cider-current-connection)
         ;;       (clomacs-get-session (cider-current-connection))))
         (data (get-patterns-from-string contents sketch))
         (table (make-hash-table :test 'equal))
         (playing (get-patterns)))
    (setq tempo "80")
    (dolist (el data)
      (if (not (string= (car el) ":tempo"))
          (let* ((k (car el)))
            (progn (puthash k (car (cdr el)) table)
                   (get-pattern-size (car (cdr el)) k)))
        (setq tempo (car (cdr el))))
      )
    (dolist (p playing)
      (let* ((p (symbol-name p))
            (k (if (gethash p table) (concat p "-b") p)))
        (puthash k (gethash p pattern-data) table)
        (if (gethash p table)
            (nrepl-sync-request:eval
             (format "(ns techno.core) (p/rename-p player {%s %s})"
                     p k)
             (cider-current-connection)
             (clomacs-get-session (cider-current-connection))))
        ))
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
  (setq current-playing-patterns (get-patterns))
  (ctbl:cp-set-model techno-patterns (build-pattern-model))
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
                     (def player (p/get-s 80 {:div 16})))"
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
;; (def sync-to-midi (atom true))
;; (let [started (atom false)
;;               stopped (atom false)]
;;   (on-event
;;    [:midi nil]
;;    (fn [m]
;;        ;; (println (:status m))
;;        ;; (when (= (:status m) :song-position-pointer)
;;        ;;   (println \"pointer\"))
;;        (when (and (not @stopped)
;;                   ;; (= (:status m) :start)
;;                   (= (:status m) :song-position-pointer)
;;                   @sync-to-midi)
;;          (println \"syncing\")
;;          ;(techno.synths/o-kick)        ;
;;          (s/reset-s player)
;;          (reset! started true)
;;          )
;;        (when (= (:status m) :stop)
;;          (reset! stopped true)
;;          )
;;        (when (and (= (:status m) :song-position-pointer) @stopped)
;;          (println \"stopping\")
;;          (reset! started false)
;;          (reset! stopped false))
;;        ) :midi-clock))
")
            (cider-current-connection)
            (clomacs-get-session (cider-current-connection))))
)
    ;; (with-output-to-temp-buffer "*scratch*"
    ;;   (print res))
;(add-dummy-p)
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
         (p/play-p (assoc " pattern " :key " current-pattern ") " (if tempo tempo "80") ")"))
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
  (setq current-playing-patterns (get-patterns))
  (ctbl:cp-set-model techno-patterns (build-pattern-model))
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
         (body (format " (import java.util.concurrent.ThreadLocalRandom) (use '[overtone.core]
        '[techno.core :as core]
        '[techno.synths]
        '[overtone.inst.synth]
        '[techno.drum-patterns]
        '[techno.drums]
        '[techno.samples]
        '[techno.melody])
         (require '[techno.sequencer :as s]
                  '[techno.player :as p])
         (%s core/player %s %s %s)" add-p pattern key attrs))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         (err (nrepl-dict-get res "err")))
    (if (stringp err)
        (with-output-to-temp-buffer "*scratch*"
          (print err)))
    (update-fx-model key)
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
    (ignore-errors (init-step-sequencer))
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
      (update-fx-model pattern) (update-pattern-view)
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
     (clomacs-get-session (cider-current-connection)))
    (update-fx-model pattern) (update-pattern-view)
    )
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

(defun pattern-mode (step)
  (with-current-buffer "tekno-pattern"
    (get-pattern-bounds)
    (let* ((start (point))
           (pattern (buffer-substring-no-properties (+ 1 (car (gethash "pattern" highlight-bounds))) (+ 1 (cdr (gethash "pattern" highlight-bounds)))))
           (div (gethash "div" highlight-bounds))
           (body (format  "(ns techno.core)
                           (p/%s '%s %s \"%s\")"
                         (if step "step-mode-p" "text-mode-p")
                         pattern div (gethash "rest-regex" highlight-bounds)))
           (res (nrepl-sync-request:eval
                 body
                 (cider-current-connection)
                 (clomacs-get-session (cider-current-connection))))
           ;; (x    (with-current-buffer "*scratch*"
           ;;         (insert (format "%s"  body))
           ;;         (insert (format "%s"  res))
           ;;         ))
           (res (substring (nrepl-dict-get res "value") 3))
           (res (replace-regexp-in-string ":|" ":|
" res))
           ;; (res (replace-regexp-in-string ")" ")
;; " res))
           )

      (delete-region (+ 1 (car (gethash "pattern" highlight-bounds))) (+ 1 (cdr (gethash "pattern" highlight-bounds))))
      (insert res)
      (indent-region (point-min) (point-max))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):|")
      (get-pattern-bounds)
      (if step
          (bind-keys* ("w" . nav-up-p)
                      ("s" . nav-down-p)
                      ("d" . nav-right-p)
                      ("a" . nav-left-p)
                      ("p" . paste-action-p)
                      ("x" . delete-action-p))
        (bind-keys* ("w" . self-insert-command)
                    ("s" . self-insert-command)
                    ("d" . self-insert-command)
                    ("a" . self-insert-command)
                    ("p" . self-insert-command)
                    ("x" . self-insert-command)))
      (goto-char start)
      ))
  )
(defun text-mode-p ()
  (interactive)
  (setq pattern-step-mode nil)
  (pattern-mode nil)
  )
(defun step-mode-p ()
  (interactive)
  (init-step-sequencer)
  (switch-to-buffer-other-window "step-sequencer")
  )

(defun save-step-mode-p ()
  (interactive)
  (let* ((size (car (read-from-string (gethash current-pattern pattern-sizes))))
         (div (gethash "div" highlight-bounds))
         (body (format "(ns techno.core)
                                               (map (fn [b] (p/get-pos b %s)) (range 1 (inc (p/get-beat %s %s %s))))"
                       div (aref size 0) (aref size 1) div))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         (offsets (substring (nrepl-dict-get res "value") 3))
         (offsets (car (read-from-string offsets)))
         (pattern (loop for note in
                        offsets
                        concat (if (gethash (format "%s" note) step-mode-hash)
                                   (format " %s " (gethash (format "%s" note) step-mode-hash))
                                 " :01 ")))
         (pattern (format "[%s]" pattern))
         (start (+ 1 (car (gethash "pattern" highlight-bounds))))
         (end (+ 1 (cdr (gethash "pattern" highlight-bounds)))))
    (with-current-buffer "tekno-pattern"
      (delete-region start end)
      (goto-char start)
      (insert pattern))
    (text-mode-p)
    (init-step-sequencer)
    )
  )

(defun move-p (pos dir shift)
  (with-current-buffer "tekno-pattern"
    (get-pattern-bounds)
    (let* ((start (point))
           (pattern (buffer-substring-no-properties (+ 1 (car (gethash "pattern" highlight-bounds))) (+ 1 (cdr (gethash "pattern" highlight-bounds)))))
           (div (gethash "div" highlight-bounds))
           (reg (gethash "rest-regex" highlight-bounds))
           (body (format "(ns techno.core)
                        (p/%s (p/%s '%s %s %s %s \"%s\") %s \"%s\")"
                         (if pattern-step-mode "step-mode-p" "text-mode-p")
                         (if shift "shift-step-p" "move-step-p")
                         pattern div pos dir reg div reg))
           (res (nrepl-sync-request:eval
                 body
                 (cider-current-connection)
                 (clomacs-get-session (cider-current-connection))))
           ;; (x    (with-current-buffer "*scratch*"
           ;;         (insert (format "%s"  body))
           ;;         (insert (format "%s"  res))
           ;;         ))
           (res (substring (nrepl-dict-get res "value") 3))
           (res (replace-regexp-in-string ":|" ":|
" res)))
      (delete-region (+ 1 (car (gethash "pattern" highlight-bounds))) (+ 1 (cdr (gethash "pattern" highlight-bounds))))
      (insert res)
      (indent-region (point-min) (point-max))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):|")
      (get-pattern-bounds)
      (goto-char start)
      (init-step-sequencer)
      ))
  )
(defun shift-right-p ()
  (interactive)
  (with-current-buffer "tekno-pattern"
    (move-p (gethash (point) pattern-positions) ":right" t)
    )
  )
(defun shift-left-p ()
  (interactive)
  (with-current-buffer "tekno-pattern"
    (move-p (gethash (point) pattern-positions) ":left" t)
    )
  )
(defun nudge-left-p ()
  (interactive)
  (with-current-buffer "tekno-pattern"
    (move-p (gethash (point) pattern-positions) ":left" nil)
    )
  )
(defun nudge-right-p ()
  (interactive)
  (with-current-buffer "tekno-pattern"
    (move-p (gethash (point) pattern-positions) ":right" nil)
    )
  )
(defun nav-p (dir)
  (with-current-buffer "tekno-pattern"
    (let* ((pos (gethash (point) pattern-positions))
           (pos (replace-regexp-in-string
                 "\\]" ")" (replace-regexp-in-string
                  "\\[" "(" pos)))
           (pos (car (read-from-string pos)))
           (bar (car pos))
           (note (car (cdr pos)))
           (bar (+ bar (if (string= dir ":up") -1 (if (string= dir ":down") 1 0))))
           (note (+ note (if (string= dir ":left") -1 (if (string= dir ":right") 1 0))))
           (key (format "[%s %s]" bar note)))
      (if (gethash key highlight-bounds)
          (goto-char (+ 1 (car (gethash key highlight-bounds)))))
      ))
  )

(defun nav-right-p ()
  (interactive)
  (nav-p ":right")
  )
(defun nav-left-p ()
  (interactive)
  (nav-p ":left")
  )
(defun nav-up-p ()
  (interactive)
  (nav-p ":up")
  )
(defun nav-down-p ()
  (interactive)
  (nav-p ":down")
  )

(defun paste-action-p ()
  (interactive)
  (with-current-buffer "tekno-pattern"
      (let* ((text (car kill-ring))
             (pos (gethash (point) pattern-positions))
             (pos (gethash pos highlight-bounds)))
        (delete-region
         (car pos)
         (+ 1 (cdr pos)))
        (goto-char (+ 1 (car pos)))
        (insert (concat text " "))
        ))
  )

(defun delete-action-p ()
  (interactive)
  (with-current-buffer "tekno-pattern"
      (let* ((text (car kill-ring))
             (pos (gethash (point) pattern-positions))
             (pos (gethash pos highlight-bounds)))
        (delete-region
         (car pos)
         (+ 1 (cdr pos)))
        (goto-char (car pos))
        (insert " :01 ")
        ))
  )

(defun show-pattern-view (&optional switch)
  (interactive)
  (let* ((key (ctbl:cp-get-selected-data-cell techno-patterns))
         (buf (get-buffer-create "tekno-pattern")))
    (with-current-buffer buf
      (read-only-mode -1)
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
      ;(local-set-key (kbd "C-M-r") 'clear-pattern-print)
      ;(local-set-key (kbd "C-M-p") 'pattern-print-add-playing)
      (local-set-key (kbd "C-=") 'start-stop-player)
      (bind-keys* ("<C-right>" . nudge-right-p)
                  ("<C-left>" . nudge-left-p)
                  ("<S-left>" . shift-left-p)
                  ("<S-right>" . shift-right-p)
                  ("C-M-t" . text-mode-p)
                  ;("C-M-u" . step-mode-p)
                  )
      )
    (setq current-pattern key)
    (if switch (switch-to-buffer-other-window buf))
    (get-pattern-bounds)
    (ignore-errors (init-step-sequencer))
    )
  )

(defun rm-pattern ()
  (interactive)
  (rm-pattern-key (ctbl:cp-get-selected-data-cell techno-patterns))
  (setq current-playing-patterns (get-patterns))
  (ctbl:cp-set-model techno-patterns (build-pattern-model))
  )
(defun rm-pattern-key (key)
  (interactive)
  (let* ((rm-p (if use-player "p/rm-p" "s/rm-p")) )
    (nrepl-sync-request:eval
     (concat "(use
        '[techno.core :as core]
        ;'[techno.sequencer :as s]
        '[techno.player :as p]
        ) (" rm-p " core/player " key ")")
     (cider-current-connection)
     (clomacs-get-session (cider-current-connection)))
      (update-pattern-view))
  )

(defun switch-stack ()
  (interactive)
  (let* ((cur (if (not (get-text-property (point) 'ctbl:component))
                  techno-patterns
                  (ctbl:cp-get-component))
          ))
    (while (not (eq (symbol-value (car tekno-components)) cur))
      (setq tekno-components (cdr tekno-components)))
    (setq tekno-components (cdr tekno-components))
    (goto-char
     (ctbl:find-by-cell-id
      (ctbl:component-dest (symbol-value (car tekno-components)))
      (ctbl:cell-id 0 0)))
    ))
;; (init-pattern-view)
; (init-synth-page)

;; (message "%s" current-playing-patterns)
;; (message "%s"  (member (intern ":sdst") current-playing-patterns))


(defun update-fx-stack ()
  (let* ((cur (ctbl:cp-get-selected-data-cell techno-patterns))
         (cur (if cur cur 0))
         (data (gethash
                cur
                fx-data))
         (row (car (ctbl:component-selected fx-stack)))
         (selected (if data (gethash row data) nil))
         (header (if selected (cons "idx" (hash-table-keys selected)) '("idx"))))
                                        ;(message (format "%s" header))
    (if data
        (ctbl:cp-set-model
         fx-stack
         (ctbl:make-model-from-list
          (mapcar
           (lambda (idx)
                                        ;(message (format "%s" (gethash idx data)))
             (if (eq row idx)
                 (cons idx (hash-table-values (gethash idx data)))
               (list idx)))
           (sort (hash-table-keys data) '<))
          header)))))


(defun pause-node (pause)
  (interactive)
  (let* ((data (gethash
                (ctbl:cp-get-selected-data-cell
                 techno-patterns)
                fx-data))
         (data (if data
                   (gethash
                   (car (ctbl:component-selected fx-stack))
                   data)))
         (id (if data (gethash ":id" data)))
         (res (if data
                  (nrepl-sync-request:eval
                   (format "(ns techno.player) (%s %s)" (if pause "node-pause" "node-start") id)
                   (cider-current-connection)
                   (clomacs-get-session (cider-current-connection)))))
         (res (if data
                  (substring (nrepl-dict-get res "value") 3))))
    (if data
        (progn
          (puthash ":paused" (if pause "true" "false") data)
          (update-fx-stack)))
    )
  )

(defun init-pattern-view ()
  (interactive)
  (with-current-buffer (get-buffer-create "tekno")
    (global-set-key (kbd "C-M-;") (lambda () (interactive) (goto-buf "tekno")))
    (global-set-key (kbd "C-M-p") (lambda () (interactive) (goto-buf "tekno-pattern")))
    (global-set-key (kbd "C-M-u") (lambda () (interactive) (goto-buf "step-sequencer")))
    (read-only-mode -1)
    (erase-buffer)
    (insert "Patterns: \n\n")
    (let* ((param (copy-ctbl:param ctbl:default-rendering-param))
           (x (setf (ctbl:param-display-header param) nil))
           (meh (setf (ctbl:param-bg-colors param)
                      (lambda (model row-id col-id str)
                        (if (and (stringp str) (member (intern (replace-regexp-in-string "[\s-]+" "" str)) current-playing-patterns))
                            "blue5"
                          "black"))))
           (component (ctbl:create-table-component-region
                                        ;:buffer (get-buffer-create "tekno")
                       :model (build-pattern-model)
                       :keymap (ctbl:define-keymap
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
                                      ("C-M-u" . step-mode-p)
                                      ("M-<down>" . dec-amp-big)
                                      ("M-<up>" . inc-amp-big)
                                      ("S-<down>" . dec-amp)
                                      ("S-<up>" . inc-amp)

                                      ("C-e" . ctbl:navi-move-right-most)
                                      ("C-a" . ctbl:navi-move-left-most)
                                      ("<return>" . (lambda () (interactive) (show-pattern-view t)))
                                      ("M-v" . (lambda () (interactive) (show-pattern-view)))
                                      ("C-p" . add-pattern-print)
                                      ("M-p" . rm-pattern-print)
                                      ("C-M-r" . clear-pattern-print)
                                      ;("C-M-p" . pattern-print-add-playing)
                                      ("C-=" . start-stop-player)
                                      ("<tab>" . switch-stack)
                                      ("C-M-v" . (lambda ()
                                                   (interactive)
                                                   (update-fx-model
                                                    (ctbl:cp-get-selected-data-cell
                                                     techno-patterns))
                                                   (update-fx-stack)
                                                   ))
                                      ))
                       :param param))
           )
      (setq techno-patterns component)
      (insert "\n\nPlayer: \n\n")
      (setq player-info
            (ctbl:create-table-component-region
             :model (ctbl:make-model-from-list
                     (list (list (format "%s" pattern-queue-add) (format "%s" pattern-queue-rm) (format "%s" pattern-queue-mod) tempo (format "%s" (player-active?))))
                     '("Queue Add" "Queue Rm" "Queue Mod" "Tempo" "Player Active" "size"))
             :keymap (ctbl:define-keymap
                      '(("w" . ctbl:navi-move-up)
                        ("s" . ctbl:navi-move-down)
                        ("a" . ctbl:navi-move-left)
                        ("d" . ctbl:navi-move-right)
                        ("C-=" . start-stop-player)
                        ("<tab>" . switch-stack)))
             ))
      (insert "\n\nFX: \n\n")
      (setq fx-chooser
            (ctbl:create-table-component-region
             :model (ctbl:make-model-from-list
                     '(("p-delay" "p-reverb" "p-low-shelf" "p-hi-shelf" "p-pitch-shift" "p-compander" "p-peak-eq" "stuttertest" "scramble" "onsetDelay")))
             :keymap (ctbl:define-keymap
                      '(("w" . ctbl:navi-move-up)
                        ("s" . ctbl:navi-move-down)
                        ("a" . ctbl:navi-move-left)
                        ("d" . ctbl:navi-move-right)
                        ("C-=" . start-stop-player)
                        ("<tab>" . switch-stack)
                        ("M-a" . (lambda ()
                                   (interactive)
                                   (let* ((s (ctbl:cp-get-selected-data-cell fx-chooser))
                                          (p (ctbl:cp-get-selected-data-cell techno-patterns))
                                          (s (if (string-match-p
                                                  (regexp-quote "p-.*") s) (concat "p/" s) s))
                                          (out-bus (if (string-match-p
                                                        (regexp-quote "p-.*") p) "out-bus" "outBus"))
                                          (audio-bus (if (string-match-p
                                                        (regexp-quote "p-.*") p) "audio-bus" "audioBus"))
                                          (res (nrepl-sync-request:eval
                                                (format "(ns techno.core)
                                                            (let [fx (techno.sequencer/get-pattern-fx %s)
                                                                  grp (:group fx)
                                                                  mixer (to-sc-id (:mixer fx))
                                                                  bus (:bus fx)
                                                                  x (%s [:before mixer] :%s bus :%s bus)]
                                                              (techno.sequencer/add-pattern-fx %s
                                                                  (to-sc-id x)
                                                                  x))" p s audio-bus out-bus p)
                                                (cider-current-connection)
                                                (clomacs-get-session (cider-current-connection)))))
                                     (update-fx-model p)
                                     (update-fx-stack)
                                     )))))
             :param param
             ))
      (insert "\nStack: \n\n")
      (setq fx-stack
            (ctbl:create-table-component-region
             :model (ctbl:make-model-from-list
                     '((1 2 3)))
             :keymap (ctbl:define-keymap
                      '(("w" . ctbl:navi-move-up)
                        ("s" . ctbl:navi-move-down)
                        ("a" . ctbl:navi-move-left)
                        ("d" . ctbl:navi-move-right)
                        ("C-=" . start-stop-player)
                        ("<tab>" . switch-stack)
                        ("M-p" . (lambda () (interactive) (pause-node t)))
                        ("M-s" . (lambda () (interactive) (pause-node nil)))
                        ("M-r" . (lambda ()
                                   (interactive)
                                   (let* ((data (gethash
                                                 (ctbl:cp-get-selected-data-cell
                                                  techno-patterns)
                                                 fx-data))
                                          (data (if data
                                                    (gethash
                                                     (car (ctbl:component-selected fx-stack))
                                                     data)))
                                          (id (if data (gethash ":id" data)))
                                          (p (ctbl:cp-get-selected-data-cell techno-patterns))
                                          (body (format "(ns techno.core)
                                                            (let [fx (techno.sequencer/get-pattern-fx %s)
                                                                  x (some (fn [a] (if (and (map? (second a)) (= (:id (second a)) %s)) (first a))) fx)]
                                                            (techno.sequencer/rm-pattern-fx %s x))" p id p))
                                          (res (nrepl-sync-request:eval
                                                body
                                                (cider-current-connection)
                                                (clomacs-get-session (cider-current-connection))))
                                          )
                                     (update-fx-model p)
                                     (update-fx-stack)
                                     )))
                        ("M-e" . (lambda ()
                                   (interactive)
                                   (let* ((data (gethash
                                                 (ctbl:cp-get-selected-data-cell
                                                  techno-patterns)
                                                 fx-data))
                                          (data (if data
                                                    (gethash
                                                     (car (ctbl:component-selected fx-stack))
                                                     data)))
                                          (p (ctbl:component-selected fx-stack))
                                          (s (nth 0 (nth (car p)
                                                         (ctbl:component-sorted-data fx-stack))))
                                          (v (read-string "val: ")))
                                     (if data
                                         (progn
                                           (let* ((k (nth (- (cdr p) 1)
                                                          (hash-table-keys data)))
                                                  (id (gethash ":id" data))
                                                  (body (format "(ns techno.core) (ctl %s %s %s) (node-get-control %s %s)" id k (string-to-number v) id k))
                                                  (res (nrepl-sync-request:eval
                                                        body
                                                        (cider-current-connection)
                                                        (clomacs-get-session (cider-current-connection)))))
                                               (puthash k (string-to-number v) data)
                                             (update-fx-stack))))
                                     )
                                   ))))))
      (ctbl:cp-add-selection-change-hook
       fx-stack
       (lambda () (update-fx-stack)))
      (update-pattern-view)
      (ctbl:cp-add-selection-change-hook techno-patterns 'update-pattern-view)
      (pop-to-buffer (ctbl:cp-get-buffer component))
      (goto-line 3)
      (forward-char 1)
      (read-only-mode 1)
      ))
  )

(defun goto-buf (buf)
  (interactive)
  (pop-to-buffer buf)
  (let* ((dest (ctbl:find-by-cell-id
                 (ctbl:component-dest techno-patterns)
                 (ctbl:component-selected techno-patterns))))
    (goto-char
     (if dest dest (ctbl:find-by-cell-id
                    (ctbl:component-dest techno-patterns)
                    (cons 0 0)))
     ))
  )


(defun update-fx-model (cur)
  (interactive)
  (with-current-buffer "tekno"
   (let* ((body (format "(ns techno.core)
               (let [fx (techno.sequencer/get-pattern-fx %s)]
                  (map-indexed (fn [i [k v]]
                    (cond
                    (and (map? v) (contains? v :synth))
                    (conj (map seq (into '() (node-get-controls v (map (fn [c] (get c :name)) (:params (var-get (resolve (symbol (:synth v ))))))))) (list :id (:id v)) (list :paused (node-paused? v)) (list :synth (:synth v)) i)
                    (node? v) (list i (list k (to-sc-id v)))
                    true (list i (list k v))))
                (into '() fx))
                )
                " cur))
          (res (nrepl-sync-request:eval
                                body
                                (cider-current-connection)
                                (clomacs-get-session (cider-current-connection))))
          (res (substring (nrepl-dict-get res "value") 3))
          (res (read res)))
     (if (gethash cur fx-data)
         (clrhash (gethash cur fx-data))
         (puthash cur (make-hash-table :test 'equal) fx-data))
     (dolist (p res)
       (let* ((p-data (gethash cur fx-data))
              (k (car p))
              (v (cdr p))
              (k (if (symbolp k) (symbol-name k) k))
              (x (if (gethash k p-data)
                     (clrhash (gethash k p-data))
                   (puthash k (make-hash-table :test 'equal) p-data)))
              (data (gethash k p-data))
              (x
               (mapcar (lambda (d)
                         (if (listp d)
                             (puthash
                              (if (symbolp (car d)) (symbol-name (car d)) (car d))
                              (car (cdr d))
                              data)
                           (puthash
                            "val"
                            d
                            data)))
                       v)
                 ))
         )
       )
     ))
  )

(defun update-pattern-view ()
  (interactive)
  (with-current-buffer "tekno-pattern"
      (read-only-mode -1))
  (let ((p (point)))
      (setq current-playing-patterns (get-patterns))
      (ctbl:cp-set-model techno-patterns (build-pattern-model))
      (save-excursion
        (let* ((k (ctbl:cp-get-selected-data-cell techno-patterns))
               (vol (if (gethash k fx-data)
                        (catch 'vol
                          (mapc
                           (lambda (f)
                             (when (and (hash-table-p f)
                                        (string-match-p
                                         (regexp-quote "mixer") (gethash ":synth" f)))
                               (throw 'vol (gethash ":volume" f)))
                             )
                           (hash-table-values (gethash k fx-data)))))))
            (ctbl:cp-set-model
             player-info
             (ctbl:make-model-from-list
              (list (list (format "%s" pattern-queue-add)
                          (format "%s" pattern-queue-rm)
                          (format "%s" pattern-queue-mod) tempo
                          (format "%s" (player-active?))
                          (gethash k pattern-sizes)
                          vol))
              '("Queue Add" "Queue Rm" "Queue Mod" "Tempo" "Player Active" "Size" "Vol")))))
    ;(display-pattern-info)
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
        ;; (insert (format "Pattern: %s %s" key (gethash key pattern-sizes)))
        ;; (insert "\n\n")
        ;; (insert "Showing: " (format "%s" pattern-print-list) "\n\n")

        ;; (insert "FX: \n" (replace-regexp-in-string "\\\\n" "
        ;; " (get-pattern-fx (ctbl:cp-get-selected-data-cell techno-patterns))) "\n\n")
        ;;                                 (insert (get-pattern-struct))
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
  (interactive)
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
                                     (goto-buf "tekno"))
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
                    ("M-c" . (lambda ()
                               (interactive)
                               (if (equal (ctbl:cp-get-selected-data-cell category-component) "samples")
                                   (let* ((s (downcase (ctbl:cp-get-selected-data-cell stack-component)))
                                         (k (ctbl:cp-get-selected-data-cell synth-component))
                                         (d (string-match "\\([a-z]+\\)[^a-z0-9]*\\([0-9]+\\)*.wav" s))
                                         (v (format ":%s%s" (match-string 1 s) (substring (match-string 0 s) -5 -4))))
                                     (kill-new (format "[(drum-s [:%s] %s) []]" k v))
                                     ))
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

(defun init-step-sequencer ()
  (clrhash step-mode-hash)
  (clrhash step-bounds)
  (get-buffer-create "step-sequencer")
  (let* ((bounds (get-pattern-bounds))
         (text (with-current-buffer "tekno-pattern" (buffer-string)))
         (body (format "(ns techno.core)
                        (p/p-size %s)"
                       text))
         (div (gethash "div" highlight-bounds))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         (size (string-to-number (substring (nrepl-dict-get res "value") 3)))
         (keymap (ctbl:define-keymap
                  '(
                    ("w" . ctbl:navi-move-up)
                    ("s" . ctbl:navi-move-down)
                    ("a" . ctbl:navi-move-left)
                    ("d" . ctbl:navi-move-right)
                    ("c" . ctbl:navi-jump-to-column)
                    ("p" . (lambda ()
                             (interactive)
                             (let* ((bar (+ 1 (car (ctbl:component-selected step-component))))
                                    (note (+ 1 (cdr (ctbl:component-selected step-component))))
                                    (k (format "[%s %s]" bar note))
                                    (v (gethash k step-mode-hash))
                                    (text (substring-no-properties (car kill-ring)))
                                    (v (if v (replace-regexp-in-string "
" "" v) "nil"))
                                    (body (format " (let [a '%s b '%s] (if (vector? a) (if (vector? b) (into a b) (conj a b)) (if a (if (vector? b) (into b [a]) [a b]) b)))"
                                                  v text))
                                    (res (nrepl-sync-request:eval
                                          body
                                          (cider-current-connection)
                                          (clomacs-get-session (cider-current-connection))))
                                    (res (nrepl-dict-get res "value"))
                                    )
                               ;; (with-current-buffer "*scratch*"
                               ;;   (insert (format "%s" res)))
                               (puthash k
                                        res
                                        step-mode-hash)
                               (save-step-mode-p)
                               )
                             ))
                    ("x" . (lambda ()
                             (interactive)
                             (let* ((bar (+ 1 (car (ctbl:component-selected step-component))))
                                    (note (+ 1 (cdr (ctbl:component-selected step-component))))
                                    (k (format "[%s %s]" bar note)))
                               (remhash k
                                        step-mode-hash))
                             (save-step-mode-p)))
                    ("C-x C-z" . save-step-mode-p)
                    ("M-e" . (lambda ()
                               (interactive)
                               (let* ((k (format "[%s %s]"
                                                 (+ 1 (car (ctbl:component-selected step-component)))
                                                 (+ 1 (cdr (ctbl:component-selected step-component)))))
                                      (c (gethash k step-mode-hash))
                                      (v (read-string "val: " c)))
                                 (puthash k v step-mode-hash)
                                 (save-step-mode-p)
                                 )
                               ))
                    ("M-c" . (lambda ()
                               (interactive)
                               (let* ((k (format "[%s %s]"
                                                 (+ 1 (car (ctbl:component-selected step-component)))
                                                 (+ 1 (cdr (ctbl:component-selected step-component)))))
                                      (c (gethash k step-mode-hash)))
                                 (kill-new c))
                               ))
                    ("C-<right>" . (lambda ()
                               (interactive)
                               (let* ((k (format "[%s %s]"
                                                 (+ 1 (car (ctbl:component-selected step-component)))
                                                 (+ 1 (cdr (ctbl:component-selected step-component))))))
                                 (move-p k ":right" nil))
                               ))
                    ("C-<left>" . (lambda ()
                               (interactive)
                               (let* ((k (format "[%s %s]"
                                                 (+ 1 (car (ctbl:component-selected step-component)))
                                                 (+ 1 (cdr (ctbl:component-selected step-component))))))
                                 (move-p k ":left" nil))
                               ))
                    ("<S-right>" . (lambda ()
                               (interactive)
                               (let* ((k (format "[%s %s]"
                                                 (+ 1 (car (ctbl:component-selected step-component)))
                                                 (+ 1 (cdr (ctbl:component-selected step-component))))))
                                 (move-p k ":right" t))
                               ))
                    ("<S-left>" . (lambda ()
                               (interactive)
                               (let* ((k (format "[%s %s]"
                                                 (+ 1 (car (ctbl:component-selected step-component)))
                                                 (+ 1 (cdr (ctbl:component-selected step-component))))))
                                 (move-p k ":left" t))
                               ))
                    ("C-x C-a" . (lambda ()
                               (interactive)
                               (save-add-pattern)
                               ))
                    ("C-x C-z" . (lambda ()
                               (interactive)
                               (save-pattern)
                               ))
                    ("C-=" . (lambda ()
                               (interactive)
                               (start-stop-player)
                               ))
                    )))
         (param (copy-ctbl:param ctbl:default-rendering-param))
         (x (setf (ctbl:param-display-header param) nil))
         (meh (setf (ctbl:param-bg-colors param)
                    (lambda (model row-id col-id str)
                      ;; (with-current-buffer "*scratch*"
                      ;;   (insert (format "%s %s" row-id col-id)))
                      (if (gethash (format "[%s %s]" (+ 1 row-id)
                                           (+ 1 col-id)) step-mode-hash)
                          "blue5"
                        "black"))))
         (slots (loop for k in (number-sequence 1 (/ size div))
                      collect (number-sequence 1 div)))
         (x (dolist (b (number-sequence 1 (/ size div)))
              (dolist (n (number-sequence 1 div))
                (let* ((k (format "[%s %s]" b n))
                       (v (gethash k highlight-bounds))
                       (action (if v (with-current-buffer "tekno-pattern"
                                       (buffer-substring-no-properties
                                        (car v) (cdr v))) nil)))
                  (if action
                      (puthash k
                               action
                               step-mode-hash))
                  )
                )))
         )
    (with-current-buffer "step-sequencer"
      (let* ((key (if step-component (ctbl:component-selected step-component))))
        (erase-buffer)
        (setq step-component (ctbl:create-table-component-region
                              :model (ctbl:make-model-from-list
                                      slots)
                              :keymap keymap
                              :param param
                              ))
        (insert "\n\naction:\n")
        (setq step-mode-end (point-max))
        (ctbl:cp-add-selection-change-hook
         step-component
         (lambda ()
           (save-excursion
             (delete-region step-mode-end (point-max))
             (goto-char step-mode-end)
             (insert (format "%s"
                             (gethash (format "[%s %s]"
                                              (+ 1 (car (ctbl:component-selected step-component)))
                                              (+ 1 (cdr (ctbl:component-selected step-component))))
                                      step-mode-hash))))
           ))
        (if key
            (ctbl:cp-set-selected-cell step-component key))
        )
      )
    (dolist (b (number-sequence 1 (/ size div)))
      (dolist (n (number-sequence 1 div))
        (let* ((k (format "[%s %s]" b n))
               (step-a (with-current-buffer "step-sequencer"
                         (ctbl:find-by-cell-id
                          (ctbl:component-dest step-component)
                          (ctbl:cell-id (- b 1) (- n 1)))))
               (step-b (with-current-buffer "step-sequencer"
                         (next-single-property-change
                          step-a
                          'ctbl:cell-id nil
                          (ctbl:dest-point-max
                           (ctbl:component-dest step-component)))))
               )
          (puthash k (cons step-a step-b) step-bounds)
          )
        ))
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
                                                 (not (equal p "out-bus"))
                                                 (not (equal p "outBus")))
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
                                                                   collect (if (and (not (equal p "note")) (not (equal p "freq")) (not (equal p "out-bus")) (not (equal p "outBus")) (not (equal p "gate")))
                                                                               (concat ":" p " " (number-to-string (gethash p (gethash k synth-params))) " ")
                                                                             "")))
                                                      "] " "] ")))))
         (body (format
                "(ns techno.synths)
(if (not (node-active? synth-grp))
    (def synth-grp (group :tail 2)))
(let [params %s
      args (fn [s m] (cond (not (= -1 (.indexOf (vec (map (fn [p] (:name p)) (:params s))) \"note\"))) [:note (:data1 m)]
                         (not (= -1 (.indexOf (vec (map (fn [p] (:name p)) (:params s))) \"freq\"))) [:freq (midi->hz (:data1 m))]
                         (not (= -1 (.indexOf (vec (map (fn [p] (:name p)) (:params s))) \"freq1\"))) [:freq1 (midi->hz (:data1 m))]
                         true []))]
    (on-event [:midi :note-on]
              (fn [m]
                  (let [play (fn [synth args]
                               (techno.recorder/record-action [synth args (:data1 m)])
                               (apply synth (concat [[:head synth-grp]] (vec args)))
                             )]
                    (doseq [[s p] params]
                           (if (first (filter (fn [p] (= (keyword (:name p)) :gate)) (:params s)))
                               (swap! live-synths assoc-in [(:name s) (:data1 m)] (play s (concat p (args s m))))
                             (play s (concat p (args s m))))
                           )
                  ))
              ::prophet-midi)
    (on-event [:midi :note-off]
              (fn [m]
                  (let [n (:data1 m)]
                    (doseq [[s p] params]
                      (let [syn (get-in @live-synths [(:name s) (:data1 m)])]
                        (when (and syn (node-active? syn) (first (filter (fn [p] (= (keyword (:name p)) :gate)) (:params s))))
                          (techno.recorder/record-action [s (concat p (args s m) [:gate 0]) (:data1 m)])
                          (ctl syn :gate 0))))
                  ))
              ::prophet-midi-off)
    )" synths))
         (res (nrepl-sync-request:eval
               body
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         )
   ;; (with-current-buffer "*scratch*"
   ;;    (insert (format "%s" body))
   ;;   (insert (format "%s" res)))
  ))


(defun get-pattern-bounds ()
  (let* ((text (with-current-buffer "tekno-pattern" (buffer-string)))
         (text (replace-regexp-in-string "\"" "\\\\\"" text))
         (bounds (if pattern-step-mode (get-step-mode-bounds text)
                   (get-annotated-pattern text)))
         (res (nrepl-sync-request:eval
               (format "(reset! techno.player/send-offsets %s)
                        (if (nil? @techno.player/tekno-client) (techno.player/mk-tekno-client))" current-pattern)
               (cider-current-connection)
               (clomacs-get-session (cider-current-connection))))
         )
    (clrhash highlight-bounds)
    (clrhash pattern-positions)
    (dolist (b bounds)
      (if (listp (car (cdr b)))
        (puthash (car b) (cons (car (car (cdr b))) (car (cdr (car (cdr b)))))
                 highlight-bounds)
        (puthash (car b) (car (cdr b))
                 highlight-bounds))
      (if (and (not (string= "pattern" (car b))) (not (string= "div" (car b))) (not (string= "rest-regex" (car b))))
          (dolist (n (number-sequence (car (car (cdr b))) (car (cdr (car (cdr b))))))
            (puthash n (car b) pattern-positions)))
      ))
  )


(defun highlight-pattern-pos (&optional string)
  (interactive)
  )

(defun highlight-step-pos (&optional string)
  (interactive)
  )

(progn
  (define-eval-sexp-fu-flash-command highlight-pattern-pos
    (eval-sexp-fu-flash (lambda ()
                          (gethash cur-pos highlight-bounds)
                          )))
  (define-eval-sexp-fu-flash-command highlight-step-pos
    (eval-sexp-fu-flash (lambda ()
                          (gethash cur-pos step-bounds)
                          )))
  (tekno-server-start)
  )
