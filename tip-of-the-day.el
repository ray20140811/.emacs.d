(defvar tip-list
  '("When navigating through a document, you can use `M-<` or `M->` to move point to the beginning or end of the buffer."
    "You can use the `F3` key to start recording a macro in Emacs. After recording your macro, press `F4` to stop recording."
    "If you press `F3` key twice, it activates Emacs counter, which starts at 0 by default.When you execute the macro (by pressing `F4` twice), it will insert an incremented number each time"
    "You can use `C-x C-t` to swap the current line with the line above it."
    "Use `M-t` to transpose two words."
    "To comment or uncomment a region of code, you can use `C-@` to mark a region and then use `M-;` to comment it."
    ))

(defvar my-current-tip-index 0
    "Index of the current tip in the tip list.")

(defun tip-of-the-day ()
  "Display a random tip of the day."
  (interactive)
  (message (nth my-current-tip-index tip-list))
  (setq my-current-tip-index (mod (1+ my-current-tip-index) (length tip-list))))
