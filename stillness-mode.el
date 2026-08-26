;;; stillness-mode.el --- Prevent windows from jumping on minibuffer activation -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025 neeasade
;; SPDX-License-Identifier: MIT
;;
;; Version: 0.1
;; Author: neeasade
;; Keywords: convenience
;; URL: https://github.com/neeasade/stillness-mode.el
;; Package-Requires: ((emacs "26.1") (dash "2.18.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; stillness-mode is a minor mode that prevents Emacs from scrolling the main
;; editing window when a multi-line minibuffer appears. It automatically adjusts
;; point just enough so that Emacs doesn't force a jump in the visible buffer.
;;
;;; Code:

(require 'dash)

(defgroup stillness-mode nil
  "Make your windows jump around less by altering the point and window layout."
  :prefix "stillness-mode"
  :group 'stillness)

(defcustom stillness-mode-minibuffer-height nil
  "Expected height (in lines) of the minibuffer.

If set to nil, will infer from supported modes."
  :type 'integer
  :group 'stillness)

(defcustom stillness-mode-minibuffer-point-offset 3
  "The number of lines above the minibuffer the point should be."
  :type 'integer
  :group 'stillness)

(defun stillness-mode--minibuffer-height ()
  "Return the expected minibuffer height."
  (or stillness-mode-minibuffer-height
    (and (bound-and-true-p vertico-mode) (symbol-value 'vertico-count))
    (and (bound-and-true-p ivy-mode) (symbol-value 'ivy-height))
    10))

(defun stillness-mode--adjust-point (window minibuffer-count minibuffer-offset)
  "Return point's overlap with MINIBUFFER-COUNT screen lines in WINDOW."
  (let* ((point-height (count-screen-lines (window-start window) (point) nil window))
          (distance-from-bottom (- (window-body-height window) point-height))
          (overlap (- minibuffer-count distance-from-bottom))
          (moving? (> overlap 0))
          (buffer (window-buffer window))
          (restore (when (and moving? (eq 'ghostel-mode (buffer-local-value 'major-mode buffer)))
                     ;; coerce ghostel-mode
                     (with-current-buffer buffer
                       (let ((kind ghostel--input-mode))
                         (unless (eq 'copy kind)
                           (ghostel-copy-mode))
                         (lambda ()
                           (with-current-buffer buffer
                             (unless (eq 'copy kind)
                               (funcall (intern (format "ghostel-%s-mode" kind)))))))))))

    (when moving?
      (deactivate-mark)
      (vertical-motion (- (+ (+ 2 overlap) minibuffer-offset)) window)
      restore)))

(defun stillness-mode--handle-point (read-fn &rest args)
  "Move the point and windows for a still READ-FN invocation with ARGS."
  (let ((minibuffer-count (stillness-mode--minibuffer-height)) (minibuffer-offset stillness-mode-minibuffer-point-offset)
         (scroll-margin 0)
         (original-buffer (current-buffer))
         (state-restorations '()))
    (if (or (> (minibuffer-depth) 0)
          (> minibuffer-count (frame-height))) ; pebkac: should we message if this is the case?
      (apply read-fn args)
      (save-window-excursion
        (ignore-errors
          ;; delete any windows south of where the minibuffer will be:
          (->> (window-list)
            (--filter (-let (((_ top _ _) (window-edges it)))
                        (< (- (frame-height) (1+ (1+ top))) minibuffer-count)))
            (mapc #'delete-window)))

        ;; move the point in any affected windows:
        (save-mark-and-excursion
          (ignore-errors
            (setq state-restorations
              (--keep (with-selected-window it
                        (stillness-mode--adjust-point it minibuffer-count minibuffer-offset))
                (--remove (window-in-direction 'below it) (window-list)))))

          ;; tell windows to preserve themselves if they have a southern neighbor
          (-let* ((windows (--filter (window-in-direction 'below it) (window-list)))
                   (_ (--each windows (window-preserve-size it nil t)))
                   (did-quit nil)
                   (result (with-current-buffer original-buffer
                             (condition-case err
                               (apply read-fn args)
                               (quit ;; please hold.
                                 (setq did-quit t))))))
            ;; and then release those preservations
            (--each windows (window-preserve-size it nil nil))
            (mapc 'funcall state-restorations)
            (if did-quit
              (signal 'quit nil)
              result)))))))

;;;###autoload
(define-minor-mode stillness-mode
  "Global minor mode to prevent windows from jumping on minibuffer activation."
  :require 'stillness-mode
  :global t
  (let ((functions '(completing-read completing-read-multiple
                      ;; these c functions call completing read internally so they need advice too
                      read-command read-variable read-buffer
                      read-coding-system read-non-nil-coding-system)))
    (if stillness-mode
      (mapc (lambda (f) (advice-add f :around #'stillness-mode--handle-point '(depth 90))) functions)
      (mapc (lambda (f) (advice-remove f #'stillness-mode--handle-point)) functions))))

(provide 'stillness-mode)
;;; stillness-mode.el ends here
