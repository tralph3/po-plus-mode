;;; po-plus-mode.el --- Major mode for editing PO files  -*- lexical-binding: t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2025
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Homepage: https://github.com/tralph3/po-plus-mode
;; Keywords: po tool

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

(defvar po-plus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'po-plus-edit-string)
    (define-key map (kbd "<tab>") #'po-plus-jump-to-next-string)
    (define-key map (kbd "<backtab>") #'po-plus-jump-to-prev-string)
    map)
  "Keymap for `po-plus-mode'.")

(defvar po-plus-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'po-plus-edit-commit)
    (define-key map (kbd "C-c C-k") #'po-plus-edit-abort)
    map)
  "Keymap for `po-plus-edit-mode'.")

(defcustom po-plus-empty-string-message "<Not yet translated>"
  "Message to be displayed when a string has not yet been translated."
  :type 'string)

(defcustom po-plus-empty-string-face
  '(:inherit font-lock-keyword-face)
  "Face to use for empty strings."
  :type 'face)

(defcustom po-plus-normal-string-face nil
  "Face to use for normal strings."
  :type 'face)

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
  msgstr
  overlays)

(cl-defstruct po-plus-edit-session
  entry
  plural-index
  source-buffer)

(cl-defstruct po-plus-buffer-data
  source-file
  entries)

(defun po-plus--set-empty-overlay-metadata (ov)
  (overlay-put ov 'empty t)
  (overlay-put ov 'face po-plus-empty-string-face))

(defun po-plus--remove-empty-overlay-metadata (ov)
  (overlay-put ov 'empty nil)
  (overlay-put ov 'face po-plus-normal-string-face))

(defun po-plus--editable-overlay-at-point ()
  (seq-find
   (lambda (ov) (overlay-get ov 'po-plus-entry))
   (overlays-at (point))))

(defun po-plus-edit-abort ()
  (interactive)
  (kill-buffer-and-window))

(defun po-plus-entry-msgstr-with-index (entry &optional index)
  (if (null index)
      (po-plus-entry-msgstr entry)
    (aref (po-plus-entry-msgstr entry) index)))

(gv-define-setter po-plus-entry-msgstr-with-index (value entry &optional index)
  `(if (null ,index)
       (setf (po-plus-entry-msgstr ,entry) ,value)
     (aset (po-plus-entry-msgstr ,entry) ,index ,value)))

(defun po-plus--refresh-entry-msgstr (entry plural-index)
  (let* ((idx (or plural-index 0))
         (ov (nth idx (po-plus-entry-overlays entry)))
         (beg (overlay-start ov))
         (end (overlay-end ov))
         (text (or (po-plus-entry-msgstr-with-index entry plural-index) "")))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (let ((new-beg (point)))
          (if (string-empty-p text)
              (progn
                (po-plus--insert-read-only po-plus-empty-string-message)
                (po-plus--set-empty-overlay-metadata ov))
            (progn
              (po-plus--insert-read-only text)
              (po-plus--remove-empty-overlay-metadata ov)))
          (move-overlay ov new-beg (point)))))))

(defun po-plus-edit-commit ()
  (interactive)
  (let* ((session po-plus--edit-session)
         (entry (po-plus-edit-session-entry session))
         (source-buffer (po-plus-edit-session-source-buffer session))
         (text (buffer-string)))

    (let* ((data (buffer-local-value 'po-plus--buffer-data source-buffer))
           (entries (po-plus-buffer-data-entries data))
           (idx (po-plus-edit-session-plural-index session)))
      (unless (memq entry entries)
        (error "Entry not found in entries list"))
      (setf (po-plus-entry-msgstr-with-index entry idx) text))

    (with-current-buffer source-buffer
      (po-plus--refresh-entry-msgstr
       entry
       (po-plus-edit-session-plural-index session)))
    (kill-buffer-and-window)))

(defun po-plus-edit-string ()
  (interactive)
  (let* ((ov (po-plus--editable-overlay-at-point))
         (entry (and ov (overlay-get ov 'po-plus-entry))))
    (unless entry
      (user-error "No editable string here"))

    (let* ((source-buffer (current-buffer))
           (buf (get-buffer-create "*PO+ Edit*"))
           (text (if (overlay-get ov 'index)
                     (aref (po-plus-entry-msgstr entry) (overlay-get ov 'index))
                   (po-plus-entry-msgstr entry))))
      (with-current-buffer buf
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (po-plus-edit-mode)

        (setq-local po-plus--edit-session
                    (make-po-plus-edit-session
                     :entry entry
                     :plural-index (overlay-get ov 'index)
                     :source-buffer source-buffer)))
      (pop-to-buffer buf))))

(defun po-plus-jump-to-prev-string ()
  (interactive)
  (let ((pos (point))
        prev
        found)
    (while (and (not found)
                (setq prev (previous-overlay-change pos))
                (< prev pos))
      (setq pos prev)
      (dolist (ov (overlays-at pos))
        (when (overlay-get ov 'po-plus-entry)
          (pulse-momentary-highlight-overlay ov)
          (setq found (overlay-start ov)))))
    (if found
        (goto-char found)
      (message "No previous entry"))))

(defun po-plus-jump-to-next-string ()
  (interactive)
  (let ((pos (point))
        next
        found)
    (while (and (not found)
                (setq next (next-overlay-change pos))
                (> next pos))
      (setq pos next)
      (dolist (ov (overlays-at pos))
        (when (overlay-get ov 'po-plus-entry)
          (pulse-momentary-highlight-overlay ov)
          (setq found (overlay-start ov)))))
    (if found
        (goto-char found)
      (message "No next entry"))))

(defun po-plus--flush-field (current field acc &optional index)
  (when (and current field)
    (setq acc (po-plus-unescape-string acc))
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

(defun po-plus-parse-header (header)
  (let (header-plist)
    (dolist (elt (string-split header "\n" t "[[:space:]]+"))
      (let* ((split-pos (string-search ":" elt))
             (key (string-trim (format ":%s" (substring elt 0 split-pos))))
             (val (string-trim (substring elt (1+ split-pos)))))
        (setq header-plist (plist-put header-plist (intern key) val))))
    header-plist))

(defun po-plus-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (po-plus-parse-buffer)))

(defun po-plus-parse-buffer (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; ensures blank lines match ""
      (delete-trailing-whitespace (point-min) (point-max))
      (let (entries
            current
            current-field
            string-accumulator
            current-msgstr-index)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (unless current
              (setq current (make-po-plus-entry)))
            (cond
             ((string-match "^# \\(.*\\)" line)
              (push (match-string 1 line)
                    (po-plus-entry-translator-comments current)))

             ((string-match "^#\\. \\(.*\\)" line)
              (push (match-string 1 line)
                    (po-plus-entry-extracted-comments current)))

             ((string-match "^#: \\(.*\\)" line)
              (push (match-string 1 line)
                    (po-plus-entry-references current)))

             ((string-match "^#, \\(.*\\)" line)
              (setf (po-plus-entry-flags current)
                    (append
                     (split-string (match-string 1 line) "," t "[[:space:]]+")
                     (po-plus-entry-flags current))))

             ((string-match "^#| \\(.*\\)" line)
              (push (match-string 1 line)
                    (po-plus-entry-previous-untranslated current)))

             ((string-match "^#~ \\(.*\\)" line)
              (push (match-string 1 line)
                    (po-plus-entry-obsolete current)))

             ((string-match "^msgid \"\\(.*\\)\"" line)
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (setq current-field :msgid
                    string-accumulator (match-string 1 line)))

             ((string-match "^msgid_plural \"\\(.*\\)\"" line)
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (setq current-field :msgid-plural
                    string-accumulator (match-string 1 line)))

             ((string-match "^msgstr \"\\(.*\\)\"" line)
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (setq current-field :msgstr
                    string-accumulator (match-string 1 line)))

             ((string-match "^msgstr\\[\\([0-9]+\\)\\] \"\\(.*\\)\"" line)
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (setq current-field :msgstr-plural
                    current-msgstr-index (string-to-number (match-string 1 line))
                    string-accumulator (match-string 2 line)))

             ((string-match "^msgctxt \"\\(.*\\)\"" line)
              (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
              (setq current-field :msgctxt
                    string-accumulator (match-string 1 line)))

             ((and current-field
                   (string-match "^\"\\(.*\\)\"" line))
              (setq string-accumulator
                    (concat string-accumulator (match-string 1 line))))

             ((string= line "")
              (when current
                (po-plus--flush-field current current-field string-accumulator current-msgstr-index)
                (setq current-field nil
                      string-accumulator nil)
                (when (po-plus-entry-msgid current)
                  (push current entries)
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
          (push current entries))
        (nreverse entries)))))

(defun po-plus-unescape-string (s)
  "Convert C-style escapes (\\n, \\t, \\\", etc.) in S to real characters."
  (read (concat "\"" s "\"")))

(defun po-plus--insert-maybe-multiline-string (string)
  (let* ((str (substring (format "%S" string) 1 -1))
         (split (split-string str "\n")))
    (if (eq (length split) 1)
        (insert "\"" (car split) "\"\n")
      (insert "\"\"\n")
      (dolist (part split)
        (insert "\"" part "\\n\"\n")))))

(defun po-plus-write-entries (entries)
  (dolist (entry entries)
    (dolist (tr-comment (reverse (po-plus-entry-translator-comments entry)))
      (insert (format "# %s\n" tr-comment)))
    (dolist (ex-comment (reverse (po-plus-entry-extracted-comments entry)))
      (insert (format "#. %s\n" ex-comment)))
    (when (po-plus-entry-flags entry)
      (insert (format "#, %s\n" (string-join (po-plus-entry-flags entry) ", "))))
    (dolist (previous-untranslated (reverse (po-plus-entry-previous-untranslated entry)))
      (insert (format "#| %s\n" previous-untranslated)))
    (dolist (reference (reverse (po-plus-entry-references entry)))
      (insert (format "#: %s\n" reference)))
    (dolist (obsolete (reverse (po-plus-entry-obsolete entry)))
      (insert (format "#~ %s\n" obsolete)))
    (when (po-plus-entry-msgctxt entry)
      (insert "msgctxt ")
      (po-plus--insert-maybe-multiline-string (po-plus-entry-msgctxt entry)))
    (insert "msgid ")
    (po-plus--insert-maybe-multiline-string (po-plus-entry-msgid entry))
    (when (po-plus-entry-msgid-plural entry)
      (insert "msgid_plural ")
      (po-plus--insert-maybe-multiline-string (po-plus-entry-msgid-plural entry)))
    (cond
     ((stringp (po-plus-entry-msgstr entry))
      (insert "msgstr ")
      (po-plus--insert-maybe-multiline-string (po-plus-entry-msgstr entry)))
     ((vectorp (po-plus-entry-msgstr entry))
      (dotimes (i (length (po-plus-entry-msgstr entry)))
        (insert (format "msgstr[%d] " i))
        (po-plus--insert-maybe-multiline-string (aref (po-plus-entry-msgstr entry) i)))))
    (insert "\n"))
  (delete-trailing-whitespace))

(defun po-plus-save ()
  (interactive)
  (unless (po-plus-buffer-data-entries po-plus--buffer-data)
    (user-error "Buffer has no PO entries"))
  (let ((file (or (po-plus-buffer-data-source-file po-plus--buffer-data)
                  (read-file-name "No source-file set, choose where to save: ")))
        (entries (po-plus-buffer-data-entries po-plus--buffer-data)))
    (with-temp-buffer
      (po-plus-write-entries entries)
      (write-file file))))

(defun po-plus-open ()
  (interactive)
  (when (not (string= "po"
              (file-name-extension (or buffer-file-name ""))))
    (user-error "This is likely not a PO file. Aborting"))
  (let ((buf-name (format "PO+ %s" (buffer-name)))
        (source-buffer (current-buffer))
        (source-file buffer-file-name))
    (if (get-buffer buf-name)
        (switch-to-buffer (get-buffer buf-name))
      (switch-to-buffer (get-buffer-create buf-name))
      (po-plus-mode)
      (setq-local po-plus--buffer-data
                  (make-po-plus-buffer-data
                   :entries (po-plus-parse-buffer source-buffer)
                   :source-file source-file))
      (dolist (entry (po-plus-buffer-data-entries po-plus--buffer-data))
        (po-plus--insert-entry entry))
      (goto-char (point-min)))))

(defun po-plus--insert-msgstr (msgstr entry)
  (let ((beg (point))
        ov)
    (po-plus--insert-read-only (if (string-empty-p msgstr)
                                   po-plus-empty-string-message
                                 msgstr))
    (setq ov (make-overlay beg (point)))
    (when (string-empty-p msgstr)
      (po-plus--set-empty-overlay-metadata ov))
    (overlay-put ov 'po-plus-entry entry)
    (overlay-put ov 'line-prefix "→ ")
    (setf (po-plus-entry-overlays entry) (append (po-plus-entry-overlays entry) `(,ov)))
    ov))

(defun po-plus--insert-entry (entry)
  (let ((msgid (po-plus-entry-msgid entry))
        (msgstr (po-plus-entry-msgstr entry)))
    (if (string= msgid "")
        (po-plus--insert-header (po-plus-parse-header msgstr))
      (dolist (flag (po-plus-entry-flags entry))
        (let ((beg (point)))
          (po-plus--insert-read-only flag)
          (let ((ov (make-overlay beg (point))))
            (overlay-put ov 'face '(:box
                                    (:line-size 1 :color "red")
                                    :inherit font-lock-comment-face)))
          (po-plus--insert-read-only " ")))
      (when (po-plus-entry-flags entry)
        (po-plus--insert-read-only "\n"))
      (let ((beg (point)))
        (po-plus--insert-read-only msgid "\n")
        (let ((ov (make-overlay beg (point))))
          (overlay-put ov 'face '(:slant italic
                                         :foreground "burlywood"))
          (overlay-put ov 'line-prefix "• "))
        (setq beg (point))
        (cond
         ((stringp msgstr)
          (po-plus--insert-msgstr msgstr entry))
         ((vectorp msgstr)
          (dotimes (i (length msgstr))
            (let ((str (aref msgstr i))
                  ov)
              (setq ov (po-plus--insert-msgstr str entry))
              (overlay-put ov 'line-prefix (format "%s → " i))
              (overlay-put ov 'index i)
              (when (< i (1- (length msgstr)))
                (po-plus--insert-read-only "\n")))))))))
  (po-plus--insert-read-only "\n\n"))

(defun po-plus--insert-read-only (&rest strings)
  "Insert into a read only buffer"
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (apply #'insert strings)))

(defun po-plus--insert-header (header)
  "Insert HEADER plist in pairs with overlays."
  (dolist (i (number-sequence 0 (- (length header) 2) 2))
    (let* ((key (nth i header))
           (val (nth (1+ i) header))
           (beg (point)))
      (po-plus--insert-read-only (format "%s %s\n" (symbol-name key) val))
      (let ((ov (make-overlay beg (+ (length (symbol-name key)) beg))))
        (overlay-put ov 'face '(:box (:line-width 1 :color "grey"))))
      (let ((ov (make-overlay beg (point))))
        (overlay-put ov 'face '(:inherit font-lock-comment-face))))))

(defun po-plus-imenu-index ()
  "Return an index alist of PO entries for `imenu`."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^ID: \\(.*\\)$" nil t)
        (let ((msgid (match-string 1))
              (pos (match-beginning 0)))
          (push (cons msgid pos) index))))
    (nreverse index)))

;;;###autoload
(define-derived-mode po-plus-mode special-mode "PO+"
  "Major mode for editing PO files."
  (set-keymap-parent po-plus-mode-map nil)
  (setq-local imenu-create-index-function #'po-plus-imenu-index))

(define-derived-mode po-plus-edit-mode text-mode "PO+ Edit"
  "Edit a single PO translation."
  (setq-local header-line-format
              "Edit translation — C-c C-c to save, C-c C-k to cancel"))

(provide 'po-plus-mode)

;;; po-plus-mode.el ends here
