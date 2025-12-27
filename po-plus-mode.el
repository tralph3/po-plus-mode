;;; po-plus-mode.el --- Major mode for editing PO files  -*- lexical-binding: t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2025
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/tralph3/po-plus-mode
;; Keywords: convenience files i18n

;; This program is free software: you can redistribute it and/or modify
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

;; A complete editing environment for PO files


;;; Code:

(require 'cl-lib)

(defgroup po-plus nil
  "Enhanced editing and navigation for PO files."
  :group 'text
  :prefix "po-plus-")

(defvar po-plus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'po-plus-edit-open)
    (define-key map (kbd "n") #'po-plus-jump-to-next-editable-string)
    (define-key map (kbd "p") #'po-plus-jump-to-prev-editable-string)
    (define-key map (kbd "u") #'po-plus-jump-to-next-untranslated)
    (define-key map (kbd "U") #'po-plus-jump-to-prev-untranslated)
    (define-key map (kbd "f") #'po-plus-jump-to-next-fuzzy)
    (define-key map (kbd "F") #'po-plus-jump-to-prev-fuzzy)
    (define-key map (kbd "k") #'po-plus-kill-msgstr)
    (define-key map (kbd "y") #'po-plus-yank-msgstr)
    (define-key map (kbd "w") #'po-plus-save-msgstr)
    (define-key map (kbd "C-j") #'po-plus-msgid-to-msgstr)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "<delete>") #'po-plus-fuzzy-entry-at-point)
    (define-key map (kbd "<tab>") #'po-plus-unfuzzy-entry-at-point)
    map)
  "Keymap for `po-plus-mode'.")

(defvar po-plus-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'po-plus-edit-apply-and-close)
    (define-key map (kbd "C-c C-k") #'po-plus-edit-abort)
    (define-key map (kbd "M-n") #'po-plus-edit-apply-and-next-msgstr)
    (define-key map (kbd "M-p") #'po-plus-edit-apply-and-prev-msgstr)
    (define-key map (kbd "C-j") #'po-plus-edit-msgid-to-msgstr)
    map)
  "Keymap for `po-plus-edit-mode'.")

(defvar po-plus-reference-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") #'po-plus-follow-reference-at-point)
    (define-key map (kbd "RET") #'po-plus-follow-reference-at-point)
    map)
  "Keymap used to enable following references in PO+ buffers.")

(defvar po-plus-msgstr-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") #'po-plus-edit-open)
    (define-key map (kbd "RET") #'po-plus-edit-open)
    map)
  "Keymap used to enable editing strings in PO+ buffers.")

(defvar po-plus--yank-index 0
  "Current yank index for `po-plus-yank-msgstr'.")

(defcustom po-plus-empty-string-message "<Not yet translated>"
  "Message to be displayed when a string has not yet been translated."
  :type 'string
  :group 'po-plus)

(defcustom po-plus-edit-help-function-list
  (list
   #'po-plus-edit-apply-and-close
   #'po-plus-edit-abort
   #'po-plus-edit-apply-and-next-msgstr
   #'po-plus-edit-apply-and-prev-msgstr)
  "List of functions to display the keybinding and short description of
when in the edit buffer.

Setting it to nil removes the help section entirely."
  :type '(repeat function)
  :group 'po-plus)

(defcustom po-plus-highlight-on-jump t
  "Wether to highlight a section of text whenever the cursor jumps to a new
position."
  :type 'boolean
  :group 'po-plus)

(defface po-plus-translator-comments-face
  `((t ,(list
         :inherit 'font-lock-comment-face)))
  "Face to use for translator comments.")

(defface po-plus-extracted-comments-face
  `((t ,(list
         :slant 'italic
         :inherit 'font-lock-doc-face)))
  "Face to use for extracted comments.")

(defface po-plus-reference-face
  `((t ,(list
         :inherit 'link
         :height 0.7)))
  "Face to use for code references.")

(defface po-plus-msgctxt-face
  `((t ,(list
         :inherit 'homoglyph
         :height 0.9)))
  "Face to use for msgctxt.")

(defface po-plus-msgid-face
  `((t ,(list
         :foreground "burlywood"
         :height 0.9)))
  "Face to use for msgid.")

(defface po-plus-msgid-plural-face
  `((t ,(list
         :inherit 'po-plus-msgid-face)))
  "Face to use for the plural form of msgid.")

(defface po-plus-msgstr-face
  nil
  "Face to use for msgstr.")

(defface po-plus-empty-msgid-face
  `((t ,(list
         :inherit 'font-lock-keyword-face)))
  "Face to use for empty msgid.")

(defface po-plus-flag-face
  `((t ,(list
         :slant 'normal
         :box t
         :inherit 'font-lock-keyword-face)))
  "Face to use for flags.")

(defface po-plus-divider-face
  `((t ,(list
         :underline t
         :extend t
         :inherit 'shadow)))
  "Face to use for dividers.")

(cl-defstruct po-plus-entry
  translator-comments
  extracted-comments
  references
  flags
  previous-untranslated
  obsolete
  msgctxt
  msgid
  msgid-plural
  msgstr)

(cl-defstruct po-plus-edit-session
  entry
  plural-index
  source-buffer)

(cl-defstruct po-plus-stats
  total
  untranslated
  fuzzy
  obsolete)

(cl-defstruct po-plus-buffer-data
  source-file
  header
  entries
  stats)


;; --- Section: PUBLIC API ---

;;;###autoload
(defun po-plus-open (file &optional dont-validate)
  "Open a PO file for editing in `po-plus-mode'.

If FILE is nil, prompt for a file name.

Unless DONT-VALIDATE is non-nil, FILE must have a \"po\" extension; this
heuristic is used to reject non-PO files."
  (interactive
   (list (read-file-name "Open PO file: " nil nil t)
         current-prefix-arg))
  (setq file (expand-file-name file))
  (when (and
         (not dont-validate)
         (not (string= "po" (file-name-extension file))))
    (user-error "This is likely not a PO file (no .po extension). Aborting"))
  (switch-to-buffer (po-plus--generate-po-plus-buffer file)))

(defun po-plus-save ()
  (interactive)
  (unless (po-plus-buffer-data-entries po-plus--buffer-data)
    (user-error "Buffer has no PO entries"))
  (let ((file (or (po-plus-buffer-data-source-file po-plus--buffer-data)
                  (read-file-name "No source file set, choose where to save: ")))
        (entries (po-plus-buffer-data-entries po-plus--buffer-data))
        (header (po-plus-buffer-data-header po-plus--buffer-data)))
    (with-temp-buffer
      (po-plus--write-entries `(,header))
      (insert "\n")
      (po-plus--write-entries entries)
      (write-file file))
    (set-buffer-modified-p nil)))

(defun po-plus-follow-reference-at-point ()
  (interactive)
  (unless (get-text-property (point) 'po-plus-is-reference)
    (user-error "Point is not over a reference"))
  (let* ((start (previous-property-change (point)))
         (end (next-property-change (point)))
         (reference (buffer-substring start end))
         (match (string-match "^\\([^:]+\\)\\(?::\\([0-9,]*\\)\\)?\\(?::\\([0-9]+\\)\\)?" reference))
         (filename (match-string 1 reference))
         (line-str (match-string 2 reference))
         (column (match-string 3 reference))
         (lines (when (stringp line-str)
                  (string-split line-str "," nil "[[:space:]]+"))))
    (unless (file-exists-p filename)
      (user-error (format "File '%s' does not exist" filename)))
    (find-file-read-only-other-window filename)
    (let* ((start (string-to-number (or (car lines) "")))
           (end (if (string= (cadr lines) "")
                    (line-number-at-pos (point-max))
                  (max (string-to-number (or (cadr lines) "")) start))))
      (unless (and (= start 0) (= end 0))
        (goto-char (point-min))
        (forward-line (1- start))
        (when po-plus-highlight-on-jump
          (if (eq start end)
              (pulse-momentary-highlight-one-line)
            (pulse-momentary-highlight-region
             (save-excursion
               (goto-char (point-min))
               (forward-line (1- start))
               (move-to-column 0)
               (point))
             (save-excursion
               (goto-char (point-min))
               (forward-line (1- end))
               (move-to-column 0)
               (point))))))
      (when column
        (move-to-column (string-to-number column))))))

(defun po-plus-jump-to-next-editable-string (&optional index)
  "Moves point to the next editable string.

If optional argument INDEX is a number, jumps to the next string
with that plural index."
  (interactive)
  (let ((match (text-property-search-forward
                'po-plus-is-msgstr t
                (lambda (expected actual)
                  (and
                   (eq expected actual)
                   (eq (if (eq index nil)
                           nil
                         (get-text-property (point) 'po-plus-plural-index))
                       index)))
                t)))
    (unless match
      (user-error "No next entry"))
    (let ((start (prop-match-beginning match))
          (end   (prop-match-end match)))
      (goto-char start)
      (when po-plus-highlight-on-jump
        (pulse-momentary-highlight-region start end))
      (when (eq (selected-window) (get-buffer-window (current-buffer)))
        (recenter)))))

(defun po-plus-jump-to-prev-editable-string (&optional index)
  "Moves point to the previous editable string.

Behavior is otherwise the same as
`po-plus-jump-to-next-editable-string'."
  (interactive)
  (let ((match (text-property-search-backward
                'po-plus-is-msgstr t
                (lambda (expected actual)
                  (and
                   (eq expected actual)
                   (eq (if (eq index nil)
                           nil
                         (get-text-property (point) 'po-plus-plural-index))
                       index)))
                t)))
    (unless match
      (user-error "No previous entry"))
    (let ((start (prop-match-beginning match))
          (end   (prop-match-end match)))
      (goto-char start)
      (when po-plus-highlight-on-jump
        (pulse-momentary-highlight-region start end))
      (when (eq (selected-window) (get-buffer-window (current-buffer)))
        (recenter)))))

(defun po-plus-jump-to-next-untranslated ()
  (interactive)
  (let ((match (text-property-search-forward 'po-plus-is-untranslated t t t)))
    (unless match
      (user-error "No untranslated entries!"))
    (let ((start (prop-match-beginning match))
          (end (prop-match-end match)))
      (goto-char start)
      (when po-plus-highlight-on-jump
        (pulse-momentary-highlight-region start end))
      (when (eq (selected-window) (get-buffer-window (current-buffer)))
        (recenter)))))

(defun po-plus-jump-to-prev-untranslated ()
  (interactive)
  (let ((match (text-property-search-backward 'po-plus-is-untranslated t t t)))
    (unless match
      (user-error "No untranslated entries!"))
    (let ((start (prop-match-beginning match))
          (end (prop-match-end match)))
      (goto-char start)
      (when po-plus-highlight-on-jump
        (pulse-momentary-highlight-region start end))
      (when (eq (selected-window) (get-buffer-window (current-buffer)))
        (recenter)))))

(defun po-plus-jump-to-next-fuzzy ()
  (interactive)
  (let (found-pos match)
    (save-excursion
      (while (and (not found-pos)
                  (setq match (text-property-search-forward 'entry nil nil t)))
        (when (po-plus--is-entry-fuzzy
               (get-text-property (prop-match-beginning match) 'entry))
          (setq found-pos (prop-match-beginning match)))))
    (unless found-pos
      (user-error "No fuzzy entries!"))
    (goto-char found-pos)
    (po-plus-jump-to-next-editable-string)))

(defun po-plus-jump-to-prev-fuzzy ()
  (interactive)
  (let (found-pos match)
    (save-excursion
      (while (and (not found-pos)
                  (setq match (text-property-search-backward 'entry nil nil t)))
        (when (po-plus--is-entry-fuzzy
               (get-text-property (prop-match-beginning match) 'entry))
          (setq found-pos (prop-match-beginning match)))))
    (unless found-pos
      (user-error "No fuzzy entries!"))
    (goto-char found-pos)
    (po-plus-jump-to-next-editable-string)))

(defun po-plus-unfuzzy-entry-at-point ()
  (interactive)
  (let ((entry (get-text-property (point) 'entry))
        (line (line-number-at-pos))
        (column (current-column)))
    (unless entry
      (user-error "No entry to unfuzzy here!"))
    (unless (member "fuzzy" (po-plus-entry-flags entry))
      (user-error "Entry is not fuzzy!"))
    (setf (po-plus-entry-flags entry)
          (remove "fuzzy" (po-plus-entry-flags entry)))
    (po-plus--refresh-entry entry)
    (when (= (length (po-plus-entry-flags entry)) 0)
      (setq line (max (1- line) 0)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun po-plus-fuzzy-entry-at-point ()
  (interactive)
  (let ((entry (get-text-property (point) 'entry))
        (line (line-number-at-pos))
        (column (current-column)))
    (unless entry
      (user-error "No entry to fuzzy here!"))
    (when (member "fuzzy" (po-plus-entry-flags entry))
      (user-error "Entry is already fuzzy!"))
    (push "fuzzy" (po-plus-entry-flags entry))
    (po-plus--refresh-entry entry)
    (when (= (length (po-plus-entry-flags entry)) 1)
      (setq line (max (1+ line) 0)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun po-plus-kill-msgstr ()
  (interactive)
  (unless (get-text-property (point) 'po-plus-is-msgstr)
    (user-error "No editable string here"))
  (let ((entry (get-text-property (point) 'entry))
        (plural-index (get-text-property (point) 'po-plus-plural-index)))
    (kill-new (po-plus--entry-msgstr-with-index entry plural-index))
    (setf (po-plus--entry-msgstr-with-index entry plural-index) "")
    (po-plus--refresh-entry entry)
    (po-plus-jump-to-next-editable-string plural-index))
  (set-buffer-modified-p t))

(defun po-plus-yank-msgstr ()
  (interactive)
  (unless (get-text-property (point) 'po-plus-is-msgstr)
    (user-error "No editable string here"))
  (unless (eq last-command this-command)
    (setq po-plus--yank-index 0))

  (let ((entry (get-text-property (point) 'entry))
        (plural-index (get-text-property (point) 'po-plus-plural-index))
        (text (current-kill po-plus--yank-index t)))
    (unless text
      (user-error "Kill ring is empty"))
    (setf (po-plus--entry-msgstr-with-index entry plural-index) text)
    (po-plus--refresh-entry entry)
    (po-plus-jump-to-next-editable-string plural-index))
  (set-buffer-modified-p t)
  (setq po-plus--yank-index (1+ po-plus--yank-index)))

(defun po-plus-save-msgstr ()
  (interactive)
  (unless (get-text-property (point) 'po-plus-is-msgstr)
    (user-error "No editable string here"))
  (let ((entry (get-text-property (point) 'entry))
        (plural-index (get-text-property (point) 'po-plus-plural-index)))
    (kill-new (po-plus--entry-msgstr-with-index entry plural-index))
    (message "Saved on kill ring!")))

(defun po-plus-msgid-to-msgstr ()
  (interactive)
  (unless (get-text-property (point) 'po-plus-is-msgstr)
    (user-error "No editable string here"))
  (let ((entry (get-text-property (point) 'entry))
        (plural-index (get-text-property (point) 'po-plus-plural-index)))
    (setf (po-plus--entry-msgstr-with-index entry plural-index) (po-plus-entry-msgid entry))
    (po-plus--refresh-entry entry)
    (po-plus-jump-to-next-editable-string plural-index))
  (set-buffer-modified-p t))

(defun po-plus-edit-open ()
  "Open the msgstr at point for editing.

If point is not at a msgstr, signal a user error.

Only one editing buffer can exist at a time. If another is opened while
one already exists, it will be effectively replaced."
  (interactive)
  (unless (get-text-property (point) 'po-plus-is-msgstr)
    (user-error "No editable string here"))
  (let* ((entry (get-text-property (point) 'entry))
         (plural-index (get-text-property (point) 'po-plus-plural-index))
         (source-buffer (current-buffer))
         (buf (get-buffer-create "*PO+ Edit*"))
         (text (po-plus--entry-msgstr-with-index entry plural-index)))
    (with-current-buffer buf
      (remove-overlays)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (po-plus-edit-mode)
      (setq-local po-plus--edit-session
                  (make-po-plus-edit-session
                   :entry entry
                   :plural-index plural-index
                   :source-buffer source-buffer))
      (po-plus--edit-insert-help-overlays))
    (pop-to-buffer buf)))

(defun po-plus-edit-abort ()
  "Discard the current translation and close the edit buffer."
  (interactive)
  (kill-buffer-and-window))

(defun po-plus-edit-apply-and-close ()
  "Apply the current translation and close the edit buffer."
  (interactive)
  (unless po-plus--edit-session
    (user-error "Not in a PO+ edit buffer"))
  (po-plus-edit-apply)
  (if (one-window-p)
      (kill-buffer)
    (kill-buffer-and-window)))

(defun po-plus-edit-apply-and-next-msgstr ()
  "Apply the current translation and open the next string."
  (interactive)
  (unless po-plus--edit-session
    (user-error "Not in a PO+ edit buffer"))
  (po-plus-edit-apply)
  (po-plus--with-source-window #'po-plus-jump-to-next-editable-string)
  (po-plus--with-source-window #'po-plus-edit-open))

(defun po-plus-edit-apply-and-prev-msgstr ()
  "Apply the current translation and open the previous string."
  (interactive)
  (unless po-plus--edit-session
    (user-error "Not in a PO+ edit buffer")) ;
  (po-plus-edit-apply)
  (po-plus--with-source-window #'po-plus-jump-to-prev-editable-string)
  (po-plus--with-source-window #'po-plus-edit-open))

(defun po-plus-edit-msgid-to-msgstr ()
  (interactive)
  (unless po-plus--edit-session
    (user-error "Not in a PO+ edit buffer"))
  (let ((entry (po-plus-edit-session-entry po-plus--edit-session)))
    (erase-buffer)
    (insert (po-plus-entry-msgid entry))))

(defun po-plus-edit-apply ()
  (interactive)
  (let* ((session po-plus--edit-session)
         (entry (po-plus-edit-session-entry session))
         (source-buffer (po-plus-edit-session-source-buffer session))
         (idx (po-plus-edit-session-plural-index session)))
    (setf (po-plus--entry-msgstr-with-index entry idx) (buffer-string))
    (po-plus--with-source-window #'po-plus--refresh-entry entry)
    ;; this jump is for restoring point position after refresh
    (po-plus--with-source-window #'po-plus-jump-to-next-editable-string idx)
    (po-plus--with-source-window #'set-buffer-modified-p t)))


;; --- Section: INTERNAL HELPERS ---

(defun po-plus--entry-msgstr-with-index (entry &optional index)
  (if (null index)
      (po-plus-entry-msgstr entry)
    (aref (po-plus-entry-msgstr entry) index)))

(gv-define-setter po-plus--entry-msgstr-with-index (value entry &optional index)
  `(if (null ,index)
       (setf (po-plus-entry-msgstr ,entry) ,value)
     (aset (po-plus-entry-msgstr ,entry) ,index ,value)))

(defun po-plus--with-source-window (fn &rest args)
  (let* ((buf (po-plus-edit-session-source-buffer po-plus--edit-session))
         (win (or (get-buffer-window buf 'visible)
                  (display-buffer buf))))
    (with-selected-window win
      (with-current-buffer buf
        (apply fn args)))))

(defun po-plus--refresh-entry (entry)
  (let ((entries (po-plus-buffer-data-entries po-plus--buffer-data)))
    (save-excursion
      (if (not (eq entry (get-text-property (point) 'entry)))
          (goto-char (point-min))
        ;; search-forward doesnt give us the true start of the entry,
        ;; it only reads from POINT, so we need to search backward to
        ;; find it
        (forward-char 1) ;; safety net
        (let ((match (text-property-search-backward 'entry entry t)))
          (goto-char (prop-match-beginning match))))

      (let ((match (text-property-search-forward 'entry entry t)))
        (unless match
          (error "Couldn't find entry in buffer."))
        (let ((start (prop-match-beginning match))
              (end (prop-match-end match))
              (inhibit-read-only t)
              (inhibit-redisplay t)
              (inhibit-modification-hooks t))
          (goto-char start)
          (delete-region start end)
          (po-plus--insert-entry entry))))))

(defun po-plus--is-entry-fuzzy (entry)
  (let ((flags (po-plus-entry-flags entry)))
    (not (not (member "fuzzy" flags)))))

(defun po-plus--is-entry-untranslated (entry)
  (let ((msgstr (po-plus-entry-msgstr entry)))
    (cond
     ((stringp msgstr)
      (string= msgstr ""))
     ((vectorp msgstr)
      (let (untranslated)
        (dotimes (i (length msgstr))
          (when (string= (aref msgstr i) "")
            (setq untranslated t)))
        untranslated)))))

(defun po-plus--edit-insert-help-overlays ()
  (when po-plus-edit-help-function-list
    (let ((ov (make-overlay (point-min) (point-min)))
          (help-str ""))
      (dolist (func po-plus-edit-help-function-list)
        (setq help-str
              (concat
               help-str
               " " (propertize (key-description (or (car (where-is-internal func))
                                                    [?\M-x]))
                               'face 'help-key-binding)
               " " (propertize (car (split-string (documentation func) "\n"))
                               'face 'font-lock-comment-face) "\n")))
      (overlay-put ov 'after-string
                   (concat help-str
                           (propertize "\n"
                                       'face (list
                                              :strike-through t
                                              :extend t
                                              :inherit 'shadow)))))))

(defun po-plus--update-header-line ()
  (let* ((stats (po-plus-buffer-data-stats po-plus--buffer-data))
         (untranslated (po-plus-stats-untranslated stats))
         (total (po-plus-stats-total stats))
         (translated (- total untranslated))
         (fuzzy (po-plus-stats-fuzzy stats))
         (obsolete (po-plus-stats-obsolete stats))
         (percent (if (> total 0)
                      (/ (* translated 100) total)
                    0)))
    (setq header-line-format
          (format
           " [ Translated %d/%d (%d%%%%) ] Fuzzy: %d"
           translated
           total
           percent
           fuzzy))))

(defun po-plus--recalculate-stats ()
  (unless po-plus--buffer-data
    (user-error "This may not be a PO+ buffer"))
  (unless (po-plus-buffer-data-stats po-plus--buffer-data)
    (setf (po-plus-buffer-data-stats po-plus--buffer-data)
          (make-po-plus-stats
           :total 0
           :untranslated 0
           :fuzzy 0
           :obsolete 0)))
  (let ((stats (po-plus-buffer-data-stats po-plus--buffer-data))
        (entries (po-plus-buffer-data-entries po-plus--buffer-data)))
    (dolist (entry entries)
      (cl-incf (po-plus-stats-total stats))
      (cond
       ((po-plus--is-entry-fuzzy entry)
        (cl-incf (po-plus-stats-fuzzy stats)))
       ((po-plus--is-entry-untranslated entry)
        (cl-incf (po-plus-stats-untranslated stats)))
       ((po-plus-entry-obsolete entry)
        (cl-incf (po-plus-stats-obsolete stats)))))))

(defun po-plus--revert-buffer (ignore-auto noconfirm)
  (unless po-plus--buffer-data
    (user-error "This may not be a PO+ buffer"))
  (let* ((line (line-number-at-pos))
         (column (current-column))
         (buffer-data po-plus--buffer-data)
         (source-file (po-plus-buffer-data-source-file buffer-data))
         new-data)
    (po-plus--generate-po-plus-buffer source-file t)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (when (eq (selected-window) (get-buffer-window (current-buffer)))
      (recenter))))

(defun po-plus--unescape-string (s)
  "Convert C-style escapes (\\n, \\t, \\\", etc.) in S to real characters."
  (read (concat "\"" s "\"")))


;; --- Section: BUFFER PARSING ---

(defun po-plus--generate-po-plus-buffer (file &optional regenerate)
  "Generate the PO+ buffer.

Read PO entries from FILE and display them in a PO+ buffer. If the
buffer already exists, switch to it.

If the optional argument REGENERATE is non-nil, regenerate the buffer
even if it already exists.

Return the buffer."
  (let ((buf-name (format "PO+ %s" file))
        (inhibit-read-only t)
        (inhibit-redisplay t)
        (inhibit-modification-hooks t)
        (gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage 0.8)
        buffer-data)
    (when (or (not (get-buffer buf-name))
              regenerate)
      (with-current-buffer (get-buffer-create buf-name)
        (remove-overlays)
        (erase-buffer)
        (po-plus-mode)
        (garbage-collect)
        (with-temp-buffer
          (insert-file-contents file)
          (setq buffer-data (po-plus--parse-buffer)))
        (setf (po-plus-buffer-data-source-file buffer-data) file)
        (setq-local po-plus--buffer-data buffer-data)
        (po-plus--insert-buffer-data po-plus--buffer-data)
        (po-plus--recalculate-stats)
        (po-plus--update-header-line)))
    (get-buffer buf-name)))

(defun po-plus--parse-buffer (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-data (make-po-plus-buffer-data))
            current
            current-field
            current-is-obsolete
            string-accumulator
            current-msgstr-index)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (unless current
              (setq current (make-po-plus-entry)))
            (cond
             ((string-prefix-p "# " line)
              (push (substring-no-properties line 2)
                    (po-plus-entry-translator-comments current)))

             ((string-prefix-p "#. " line)
              (push (substring-no-properties line 3)
                    (po-plus-entry-extracted-comments current)))

             ((string-prefix-p "#: " line)
              (push (split-string (substring-no-properties line 3) " " t "[[:space:]]+")
                    (po-plus-entry-references current)))

             ((string-prefix-p "#, " line)
              (push (split-string (substring-no-properties line 3) "," t "[[:space:]]+")
                    (po-plus-entry-flags current)))

             ((string-prefix-p "#| " line)
              (push (substring-no-properties line 3)
                    (po-plus-entry-previous-untranslated current)))

             ((or (string-prefix-p "msgid_plural" line)
                  (setq current-is-obsolete (string-prefix-p "#~ msgid_plural" line)))
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (when current-is-obsolete
                (setf (po-plus-entry-obsolete current) t))
              (setq current-field :msgid-plural
                    string-accumulator (substring-no-properties line (1+ (string-search "\"" line)) -1)))

             ((or (string-prefix-p "msgid" line)
                  (setq current-is-obsolete (string-prefix-p "#~ msgid" line)))
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (when current-is-obsolete
                (setf (po-plus-entry-obsolete current) t))
              (setq current-field :msgid
                    string-accumulator (substring-no-properties line (1+ (string-search "\"" line)) -1)))

             ((or (string-prefix-p "msgstr[" line)
                  (setq current-is-obsolete (string-prefix-p "#~ msgstr[" line)))
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (when current-is-obsolete
                (setf (po-plus-entry-obsolete current) t))
              (let ((index-start (1+ (string-search "[" line)))
                    (index-end (string-search "]" line)))
                (setq current-field :msgstr-plural
                      current-msgstr-index (string-to-number (substring-no-properties line index-start index-end))
                      string-accumulator (substring-no-properties line (1+ (string-search "\"" line)) -1))))

             ((or (string-prefix-p "msgstr" line)
                  (setq current-is-obsolete (string-prefix-p "#~ msgstr" line)))
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (when current-is-obsolete
                (setf (po-plus-entry-obsolete current) t))
              (setq current-field :msgstr
                    string-accumulator (substring-no-properties line (1+ (string-search "\"" line)) -1)))

             ((or (string-prefix-p "msgctxt" line)
                  (setq current-is-obsolete (string-prefix-p "#~ msgctxt" line)))
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (when current-is-obsolete
                (setf (po-plus-entry-obsolete current) t))
              (setq current-field :msgctxt
                    string-accumulator (substring-no-properties line (1+ (string-search "\"" line)) -1)))

             ((and current-field
                   (string-prefix-p "\"" line))
              (setq string-accumulator
                    (concat string-accumulator (substring-no-properties line 1 -1))))

             ((string-blank-p line)
              (when current
                (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
                (setq current-field nil
                      string-accumulator nil)
                (when (po-plus-entry-msgid current)
                  (setf (po-plus-entry-references current)
                        (apply #'nconc (nreverse (po-plus-entry-references current))))
                  (setf (po-plus-entry-flags current)
                        (apply #'nconc (nreverse (po-plus-entry-flags current))))
                  (if (and (string= (po-plus-entry-msgid current) "")
                           (not (po-plus-buffer-data-header buffer-data)))
                      (setf (po-plus-buffer-data-header buffer-data) current)
                    (push current (po-plus-buffer-data-entries buffer-data)))
                  (setq current nil))))

             (t
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (setq current-field nil
                    string-accumulator nil
                    current-msgstr-index nil))))
          (forward-line 1))
        (when (and
               current
               (po-plus-entry-msgid current))
          (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
          (setf (po-plus-entry-references current)
                (apply #'nconc (nreverse (po-plus-entry-references current))))
          (setf (po-plus-entry-flags current)
                (apply #'nconc (nreverse (po-plus-entry-flags current))))
          (if (and (string= (po-plus-entry-msgid current) "")
                   (not (po-plus-buffer-data-header buffer-data)))
              (setf (po-plus-buffer-data-header buffer-data) current)
            (push current (po-plus-buffer-data-entries buffer-data)))
          (setf (po-plus-buffer-data-entries buffer-data)
                (nreverse (po-plus-buffer-data-entries buffer-data)))
          buffer-data)))))

(defun po-plus--parse-header (header)
  (let (header-plist)
    (dolist (elt (string-split (string-trim header) "\n" t "[[:space:]]+"))
      (let* ((split-pos (string-search ":" elt))
             (key (string-trim (format ":%s" (substring elt 0 split-pos))))
             (val (string-trim (substring elt (1+ split-pos)))))
        (setq header-plist (plist-put header-plist (intern key) val))))
    header-plist))

(defun po-plus--flush-field (current field acc &optional index)
  (when (and current field)
    (setq acc (po-plus--unescape-string acc))
    (pcase field
      (:msgid
       (setf (po-plus-entry-msgid current) acc))
      (:msgid-plural
       (setf (po-plus-entry-msgid-plural current) acc))
      (:msgctxt
       (setf (po-plus-entry-msgctxt current) acc))
      (:msgstr
       (setf (po-plus-entry-msgstr current) acc))
      (:msgstr-plural
       (let ((vec (or (po-plus-entry-msgstr current)
                      (make-vector (1+ index) nil))))
         ;; ensure vector is big enough
         (when (<= (length vec) index)
           (setq vec (vconcat vec (make-vector (- (1+ index) (length vec)) nil))))
         (aset vec index acc)
         (setf (po-plus-entry-msgstr current) vec))))))


;; --- Section: PO+ BUFFER GENERATION ---

(defun po-plus--insert-maybe-multiline-string (string)
  (let* ((ends-with-nl (string-suffix-p "\n" string))
         (str (string-replace "\t" "\\t" (substring (format "%S" string) 1 -1)))
         (lines (split-string str "\n")))
    (when ends-with-nl
      (setq lines (nbutlast lines 1)))
    (if (= (length lines) 1)
        (insert "\"" (car lines) "\"\n")
      (insert "\"\"\n")
      (dotimes (i (length lines))
        (insert "\""
                (nth i lines)
                (if (and
                     (>= i (1- (length lines)))
                     (not ends-with-nl))
                    ""
                  "\\n")
                "\"\n")))))

(defun po-plus--insert-buffer-data  (buffer-data)
  (when (po-plus-buffer-data-header buffer-data)
    (po-plus--insert-header (po-plus-buffer-data-header buffer-data)))
  (po-plus--insert-entries (po-plus-buffer-data-entries buffer-data))
  (goto-char (point-min))
  (set-buffer-modified-p nil))

(defun po-plus--insert-entries (entries)
  (let ((xs entries)
        next)
    (while xs
      (setq next (cdr xs))
      (po-plus--insert-entry (car xs))
      (when next
        (insert "\n\n"))
      (setq xs next))))

(defun po-plus--insert-translator-comments (comments)
  (dolist (comment (reverse comments))
    (insert (propertize comment
                        'rear-sticky nil
                        'front-sticky nil
                        'po-plus-is-translator-comment t
                        'face 'po-plus-translator-comments-face) "\n")))

(defun po-plus--insert-extracted-comments (comments)
  (dolist (comment (reverse comments))
    (insert (propertize comment
                        'rear-sticky nil
                        'front-sticky nil
                        'po-plus-is-extracted-comment t
                        'face 'po-plus-extracted-comments-face) "\n")))

(defun po-plus--insert-msgctxt (msgctxt)
  (insert (propertize msgctxt
                      'rear-sticky nil
                      'front-sticky nil
                      'po-plus-is-msgctxt t
                      'face 'po-plus-msgctxt-face) "\n"))

(defun po-plus--insert-msgid (msgid)
  (insert (propertize msgid
                      'line-prefix "• "
                      'rear-sticky nil
                      'front-sticky nil
                      'po-plus-is-msgid t
                      'face 'po-plus-msgid-face) "\n"))

(defun po-plus--insert-msgid-plural (msgid-plural)
  (insert (propertize msgid-plural
                      'line-prefix "→ "
                      'rear-sticky nil
                      'front-sticky nil
                      'po-plus-is-msgid-plural t
                      'face 'po-plus-msgid-plural-face) "\n"))

(defun po-plus--insert-msgstr-as-string (msgstr)
  (let ((is-empty (string= msgstr "")))
    (insert (propertize (if is-empty po-plus-empty-string-message msgstr)
                        'line-prefix "→ "
                        'rear-sticky nil
                        'front-sticky nil
                        'mouse-face 'highlight
                        'help-echo "Edit string"
                        'keymap po-plus-msgstr-map
                        'po-plus-is-msgstr t
                        'po-plus-is-untranslated is-empty
                        'face (if is-empty 'po-plus-empty-msgid-face 'po-plus-msgstr-face)) "\n")))

(defun po-plus--insert-msgstr-as-vector (msgstr)
  (dotimes (i (length msgstr))
    (let* ((str (aref msgstr i))
           (is-empty (string= str "")))
      (insert (propertize (if is-empty po-plus-empty-string-message str)
                          'line-prefix (format "[%d] → " i)
                          'rear-sticky nil
                          'front-sticky nil
                          'mouse-face 'highlight
                          'help-echo "Edit string"
                          'keymap po-plus-msgstr-map
                          'po-plus-is-msgstr t
                          'po-plus-plural-index i
                          'po-plus-is-untranslated is-empty
                          'face (if is-empty 'po-plus-empty-msgid-face 'po-plus-msgstr-face)) "\n"))))

(defun po-plus--insert-references (references)
  (setq references (reverse references))
  (dotimes (i (length references))
    (insert (propertize (nth i references)
                        'face 'po-plus-reference-face
                        'rear-sticky nil
                        'front-sticky nil
                        'mouse-face 'highlight
                        'po-plus-is-reference t
                        'keymap po-plus-reference-map
                        'help-echo "Visit reference")
            (if (< i (1- (length references)))
                (propertize "|" 'face (list
                                       :inherit '(shadow po-plus-reference-face)
                                       :underline nil))
              "")))
  (insert "\n"))

(defun po-plus--insert-flags (flags)
  (setq flags (reverse flags))
  (dotimes (i (length flags))
    (insert (propertize (nth i flags)
                        'rear-sticky nil
                        'front-sticky nil
                        'face 'po-plus-flag-face)
            (if (< i (1- (length flags)))
                " "
              "")))
  (insert "\n"))

(defun po-plus--insert-msgstr (msgstr)
  (cond
   ((stringp msgstr)
    (po-plus--insert-msgstr-as-string msgstr))
   ((vectorp msgstr)
    (po-plus--insert-msgstr-as-vector msgstr))))

(defun po-plus--insert-divider ()
  (insert (propertize "\n"
                      'rear-sticky nil
                      'front-sticky nil
                      'face 'po-plus-divider-face)))

(defun po-plus--insert-entry (entry)
  (let ((beg (point))
        (msgid (po-plus-entry-msgid entry)))
    (when (po-plus-entry-flags entry)
      (po-plus--insert-flags (po-plus-entry-flags entry)))
    (when (po-plus-entry-translator-comments entry)
      (po-plus--insert-translator-comments (po-plus-entry-translator-comments entry)))
    (when (po-plus-entry-extracted-comments entry)
      (po-plus--insert-extracted-comments (po-plus-entry-extracted-comments entry)))
    (when (po-plus-entry-msgctxt entry)
      (po-plus--insert-msgctxt (po-plus-entry-msgctxt entry)))
    (po-plus--insert-msgid msgid)
    (when (po-plus-entry-msgid-plural entry)
      (po-plus--insert-msgid-plural (po-plus-entry-msgid-plural entry)))
    (po-plus--insert-msgstr (po-plus-entry-msgstr entry))
    (when (po-plus-entry-references entry)
      (po-plus--insert-references (po-plus-entry-references entry)))
    (po-plus--insert-divider)
    (add-text-properties beg (point) (list
                                      'entry entry
                                      'rear-sticky nil
                                      'front-sticky nil))))

(defun po-plus--insert-header (header)
  (let ((beg (point))
        (parsed-header (po-plus--parse-header (po-plus-entry-msgstr header))))
    (dolist (i (number-sequence 0 (- (length parsed-header) 2) 2))
      (let* ((key (nth i parsed-header))
             (val (nth (1+ i) parsed-header)))
        (insert (format "%s %s\n"
                        (propertize (substring (symbol-name key) 1)
                                    'face '(:box t))
                        val))))
    (insert "\n\n\n")
    (add-text-properties beg (point) (list
                                      'header header
                                      'parsed-header parsed-header
                                      'rear-sticky nil
                                      'front-sticky nil))))


;; --- Section: BUFFER SAVING ---

(defun po-plus--write-entries (entries)
  (dolist (entry entries)
    (dolist (tr-comment (reverse (po-plus-entry-translator-comments entry)))
      (insert (format "# %s\n" (string-replace "\t" "\\t" tr-comment))))
    (dolist (ex-comment (reverse (po-plus-entry-extracted-comments entry)))
      (insert (format "#. %s\n" (string-replace "\t" "\\t" ex-comment))))
    (when (po-plus-entry-flags entry)
      (insert (format "#, %s\n" (string-replace "\t" "\\t" (string-join (po-plus-entry-flags entry) ", ")))))
    (dolist (previous-untranslated (reverse (po-plus-entry-previous-untranslated entry)))
      (insert (format "#| %s\n" (string-replace "\t" "\\t" previous-untranslated))))
    (dolist (reference (reverse (po-plus-entry-references entry)))
      (insert (format "#: %s\n" (string-replace "\t" "\\t" reference))))
    (when (po-plus-entry-msgctxt entry)
      (when (po-plus-entry-obsolete entry)
        (insert "#~ "))
      (insert "msgctxt ")
      (po-plus--insert-maybe-multiline-string (po-plus-entry-msgctxt entry)))
    (when (po-plus-entry-obsolete entry)
      (insert "#~ "))
    (insert "msgid ")
    (po-plus--insert-maybe-multiline-string (po-plus-entry-msgid entry))
    (when (po-plus-entry-msgid-plural entry)
      (when (po-plus-entry-obsolete entry)
        (insert "#~ "))
      (insert "msgid_plural ")
      (po-plus--insert-maybe-multiline-string (po-plus-entry-msgid-plural entry)))
    (cond
     ((stringp (po-plus-entry-msgstr entry))
      (when (po-plus-entry-obsolete entry)
        (insert "#~ "))
      (insert "msgstr ")
      (po-plus--insert-maybe-multiline-string (po-plus-entry-msgstr entry)))
     ((vectorp (po-plus-entry-msgstr entry))
      (dotimes (i (length (po-plus-entry-msgstr entry)))
        (when (po-plus-entry-obsolete entry)
          (insert "#~ "))
        (insert (format "msgstr[%d] " i))
        (po-plus--insert-maybe-multiline-string (aref (po-plus-entry-msgstr entry) i)))))
    (insert "\n"))
  (delete-trailing-whitespace))


;; --- Section: INTEGRATIONS ---

(defun po-plus--imenu-index ()
  (let (all fuzzy untranslated)
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward
                          'po-plus-is-msgid t t t)))
        (let* ((pos   (copy-marker (prop-match-beginning match)))
               (entry (get-text-property pos 'entry))
               (name  (buffer-substring-no-properties
                       (prop-match-beginning match)
                       (prop-match-end match))))
          (push (cons name pos) all)
          (when (po-plus--is-entry-fuzzy entry)
            (push (cons name pos) fuzzy))
          (when (po-plus--is-entry-untranslated entry)
            (push (cons name pos) untranslated)))))
    (delq nil
          (list
           (cons "All" (nreverse all))
           (and fuzzy
                (cons "Fuzzy" (nreverse fuzzy)))
           (and untranslated
                (cons "Untranslated" (nreverse untranslated)))))))

(with-eval-after-load 'consult-imenu
  (add-to-list
   'consult-imenu-config
   '(po-plus-mode
     :toplevel "All"
     :types
     ((?a "All" po-plus-mgstr-face)
      (?f "Fuzzy" po-plus-msgid-face)
      (?u "Untranslated" po-plus-empty-msgid-face)))))


(define-derived-mode po-plus-mode special-mode "PO+"
  "Major mode for editing PO files."
  (set-keymap-parent po-plus-mode-map nil)
  (setq-local revert-buffer-function #'po-plus--revert-buffer)
  (setq-local word-wrap t)
  (setq-local imenu-create-index-function #'po-plus--imenu-index))

(define-derived-mode po-plus-edit-mode text-mode "PO+ Edit"
  "Edit a single PO translation.")

(provide 'po-plus-mode)

;;; po-plus-mode.el ends here
