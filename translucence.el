;; set a few defaults
;; TODO: look at using defcustom to allow customization menu options
(setq transparency-step              1)
(setq default-active-transparency   90)
(setq default-inactive-transparency 80)
(setq transparency-diff              5)  ;; diff between active/inactive frame
(setq min-opacity                   20)  ;; emacs' lower limit
(setq max-opacity                  100)  ;; emacs' upper limit

(defun frame-opacity (&optional frame)
  (if frame (frame-parameter frame 'alpha)
    (frame-parameter (selected-frame) 'alpha)))

(defun active-frame-opacity ()
  (car (frame-opacity (selected-frame))))

(defun inactive-frame-opacity ()
  (cadr (frame-opacity (selected-frame))))

(defun opacity-all-frames ()
  (mapcar 'frame-opacity (frame-list)))

(defun set-frame-opacity (active inactive &optional all-frames-p)
  (if all-frames-p
      (eval `(modify-all-frames-parameters '((alpha . (,active ,inactive)))))
    (eval `(modify-frame-parameters nil '((alpha . (,active ,inactive)))))))

(defun transparency (active inactive &optional all-frames-p)
  "Custom set frame transparency"
  (interactive
   "nTransparency on active frame:\s
    nTransparency on inactive frame:\s")
  (if (y-or-n-p "Apply to all frames? ")
      (set-frame-opacity active inactive 't)
    (set-frame-opacity active inactive)))

(defun frame-transparent-p ()
  (< (car (frame-opacity)) 100))

(defun toggle-transparency (&optional all-frames-p)
  "Toggles the transparency of the frame(s)"
  (interactive)
  (if (frame-transparent-p)
      (set-frame-opacity max-opacity (- max-opacity transparency-diff) all-frames-p)
    (set-frame-opacity
     default-active-transparency default-inactive-transparency all-frames-p)))

(defun increase-opacity (&optional all-frames-p)
  "Decreases the transparency of the current frame"
  (interactive)
  (let ((active        (setq active       (active-frame-opacity)))
        (inactive      (setq inactive     (inactive-frame-opacity)))
        (differential  (setq differential (- active transparency-diff)))
        (max-opacity-p (>= active max-opacity)))
    (setq active (if max-opacity-p
                     max-opacity
                   (+ active transparency-step)))
    (if (< active differential)
        (setq inactive (+ inactive transparency-step))
      (setq inactive differential))
    (set-frame-opacity active inactive all-frames-p)))

(defun decrease-opacity (&optional all-frames-p)
  "Increases the transparency of the current frame"
  (interactive)
  (let ((active        (setq active   (active-frame-opacity)))
        (inactive      (setq inactive (inactive-frame-opacity)))
        (min-opacity-p (<= active min-opacity)))
    (setq active   (if min-opacity-p
                       min-opacity
                       (- active transparency-step)))
    (setq inactive (if min-opacity-p
                     min-opacity
                     (- inactive transparency-step)))
    (set-frame-opacity active inactive all-frames-p)))

;; ideally, when we increase or decrease opacity on all frames,
;; they'd shift up from their current values. At this time, they
;; all follow the current frame. Look at -map from dash.el passibly


;; *************************************************************************
;;    "Animate" Transparency Loop
;; *************************************************************************

(require 'timer)

(defun cancel-transparency-timers ()
  (mapcar 'cancel-function-timers
        '(increase-opacity decrease-opacity disappearing reappearing)))

(defun disappearing (&optional interval duration)
  (let ((interval (or interval .15))
        (duration (or duration   5)))
    (cancel-transparency-timers)
    (run-with-timer 0        interval 'decrease-opacity)
    (run-with-timer duration nil      'reappearing interval duration)))

(defun reappearing (&optional interval duration)
  (let ((interval (or interval .15))
        (duration (or duration   5)))    ;; when to call the next cycle
    (cancel-transparency-timers)
    (run-with-timer 0        interval 'increase-opacity)
    (run-with-timer duration nil      'disappearing interval duration)))

;; `disappearing` and `reappearing` recursively call each other incestuously.
;; `transparency-animation` allows us to specify what UPPER and LOWER bounds
;; on our frame-opacity. We determine how long the transition from UPPER to
;; LOWER should take given the INTERVAL.
(defun transparency-animation (&optional upper lower interval)
  "Specify the UPPER and LOWER bounds on the frame-opacity. Given the INTERVAL
in (sub-)seconds from UPPER to LOWER, we derive the duration from one disappearing
cycle to the next reappearing cycle.

    (transparency-animation 97 60 .025)
"
  (interactive)
  (let* ((upper      (or upper    98))
         (lower      (or lower    75))
         (interval   (or interval .08))
         (duration   (* interval (- upper lower))))
    (set-frame-opacity upper (- upper 20))
    (disappearing interval duration)))

(defun stop-transparency-animation ()
  (interactive)
  (cancel-transparency-timers)
  (set-frame-opacity default-active-transparency default-inactive-transparency))


(defun toggle-transparency-animation ()
  (interactive)                                             ;; check to see if our timers are running
  (if (or (eq 'increase-opacity (elt (car timer-list) 5))   ;; a bit of a hack
          (eq 'decrease-opacity (elt (car timer-list) 5)))  ;; a local state var would be more robust here
      (stop-transparency-animation)
    (transparency-animation)))


;; Key Bindings

(global-set-key (kbd "C-M-<wheel-up>")            'increase-opacity)
(global-set-key (kbd "C-M-<double-wheel-up>")     'increase-opacity)
(global-set-key (kbd "C-M-<triple-wheel-up>")     'increase-opacity)

(global-set-key (kbd "C-M-<wheel-down>")          'decrease-opacity)
(global-set-key (kbd "C-M-<double-wheel-down>")   'decrease-opacity)
(global-set-key (kbd "C-M-<triple-wheel-down>")   'decrease-opacity)

(global-set-key (kbd "C-M-S-<wheel-up>")          (lambda () (interactive) (increase-opacity 't)))
(global-set-key (kbd "C-M-S-<double-wheel-up>")   (lambda () (interactive) (increase-opacity 't)))
(global-set-key (kbd "C-M-S-<triple-wheel-up>")   (lambda () (interactive) (increase-opacity 't)))

(global-set-key (kbd "C-M-S-<wheel-down>")        (lambda () (interactive) (decrease-opacity 't)))
(global-set-key (kbd "C-M-S-<double-wheel-down>") (lambda () (interactive) (decrease-opacity 't)))
(global-set-key (kbd "C-M-S-<triple-wheel-down>") (lambda () (interactive) (decrease-opacity 't)))

(global-set-key (kbd "M-<f2>") 'increase-opacity)
(global-set-key (kbd "M-<f1>") 'decrease-opacity)

(global-set-key (kbd "H-M-<f2>") (lambda () (interactive) (increase-opacity 't))) ;; all-frames
(global-set-key (kbd "H-M-<f1>") (lambda () (interactive) (decrease-opacity 't))) ;; all-frames

(global-set-key (kbd "H-0")     'toggle-transparency)
(global-set-key (kbd "M-H-0")   (lambda () (interactive) (toggle-transparency t))) ;; all-frames

(global-set-key (kbd "<f5>")    'toggle-transparency-animation)

(global-set-key (kbd "M-[") 'ns-next-frame)
(global-set-key (kbd "M-]") 'ns-prev-frame)

(provide 'hieros-transparency)
