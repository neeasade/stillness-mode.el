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

(defun stillness-mode--handle-point (read-fn &rest args)
  "Move the point and windows for a still READ-FN invocation with ARGS."
  (let ((minibuffer-count (stillness-mode--minibuffer-height))
         (minibuffer-offset stillness-mode-minibuffer-point-offset)
         (original-buffer (current-buffer)))
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

        (save-mark-and-excursion
          (ignore-errors
            ;; move the point in any affected windows:
            (-each (--remove (window-in-direction 'below it) (window-list))
              (lambda (window)
                (with-selected-window window
                  (-let* (((_ top _ bottom) (window-edges))
                           (local-height-ratio (/ (line-pixel-height) (float (frame-char-height))))
                           (ratio (lambda (n) (round (* n local-height-ratio))))
                           (bottom (funcall ratio bottom))
                           (distance-from-bottom (- bottom top (funcall ratio (count-screen-lines (window-start) (point))))))
                    (when (> (funcall ratio minibuffer-count) (- distance-from-bottom 2))
                      (deactivate-mark)
                      (line-move (- (+ (abs (- minibuffer-count distance-from-bottom))
                                      minibuffer-offset))
                        t nil nil)))))))

          ;; tell windows to preserve themselves if they have a southern neighbor
          (-let* ((windows (--filter (window-in-direction 'below it) (window-list)))
                   (_ (--each windows (window-preserve-size it nil t)))
                   (result (with-current-buffer original-buffer (apply read-fn args))))
            ;; and then release those preservations
            (--each windows (window-preserve-size it nil nil))
            result))))))

;;;###autoload
(define-minor-mode stillness-mode
  "Global minor mode to prevent windows from jumping on minibuffer activation."
  :require 'stillness-mode
  :global t
  (if stillness-mode
    (progn
      (advice-add 'completing-read :around #'stillness-mode--handle-point '(depth 90))
      (advice-add 'completing-read-multiple :around #'stillness-mode--handle-point '(depth 90)))
    (advice-remove 'completing-read #'stillness-mode--handle-point)
    (advice-remove 'completing-read-multiple #'stillness-mode--handle-point)))

(provide 'stillness-mode)
;;; stillness-mode.el ends here
