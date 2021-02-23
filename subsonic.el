;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;;; Code:

(require 's)
(require 'dash)
(require 'json)
(require 'transient)
(require 'url)


(defvar mpv--process nil)

(defvar subsonic-auth (let ((auth (auth-source-search :port "subsonic")))
                        (if auth
                            (car auth)
                          (error "Failed to find subsonic auth in .authinfo"))))

(defun list->query (al)
    (seq-reduce
     (lambda (accu q)
       (if (string-empty-p accu)
           (concat "?" (car q) "=" (cadr q))
         (concat accu "&" (car q) "=" (cadr q))))
     al ""))

(defun get-json (url)
  (condition-case nil
      (let* ((json-array-type 'list)
             (json-key-type 'string))
        (json-read-from-string (with-temp-buffer (url-insert-file-contents url)
                                                 (prog1 (buffer-string)
                                                   (kill-buffer)))))
    (json-readtable-error (error "could not read \"%s\" as json" body))))

(defun subsonic-build-url (endpoint extra-query)
  (concat "https://"
          (plist-get subsonic-auth :host)
          "/rest" endpoint
          (list->query (append (list (list "u" (plist-get subsonic-auth :user))
                                     (list "p" (funcall (plist-get subsonic-auth :secret)))
                                     (list "c" "ElSonic")
                                     (list "v" "1.16.0")
                                     (list "f" "json"))
                               extra-query))))

(defun subsonic-artists-parse (data)
  (let* ((artists (assoc-default "index" (assoc-default "indexes" (assoc-default
                                                                   "subsonic-response"
                                                                   data))))
         (result (seq-reduce (lambda (accu artist-index)
                               (append accu (mapcar (lambda (artist)
                                                      (list (assoc-default "id"
                                                                           artist)
                                                            (vector
                                                             (assoc-default
                                                              "name" artist))))
                                                    (assoc-default "artist" artist-index))))
                             artists '()))) result))


(defun subsonic-albums-parse (data)
  (let* ((albums (assoc-default "child" (assoc-default "directory"
                                                       (assoc-default
                                                        "subsonic-response"
                                                        data))))
         (result (mapcar (lambda (album)
                           (list (assoc-default "id" album)
                                 (vector (assoc-default "title" album))))
                         albums)))
    result))


(defun subsonic-tracks-parse (data)
  (let* ((tracks (assoc-default "child" (assoc-default "directory"
                                                       (assoc-default
                                                        "subsonic-response"
                                                        data))))
         (result (mapcar (lambda (track)
                           (let* ((duration (assoc-default "duration" track)))
                             (list (assoc-default "id" track)
                                   (vector (assoc-default "title" track)
                                           (format-seconds "%m:%.2s" duration)
                                           (format "%d" (assoc-default "track" track))))))
                         tracks)))
    result))

(defun subsonic-artists-refresh ()
  (setq tabulated-list-entries
        (subsonic-artists-parse
         (get-json (subsonic-build-url "/getIndexes.view" '())))))

(defun subsonic-albums-refresh (id)
  (setq tabulated-list-entries
        (subsonic-albums-parse
         (get-json (subsonic-build-url "/getMusicDirectory.view" `(("id" ,id)))))))

(defun subsonic-tracks-refresh (id)
  (setq tabulated-list-entries
        (subsonic-tracks-parse
         (get-json (subsonic-build-url "/getMusicDirectory.view" `(("id" ,id)))))))


(defun subsonic-open-album ()
  (interactive)
  (subsonic-albums (tabulated-list-get-id)))

(defun subsonic-open-tracks ()
  (interactive)
  (subsonic-tracks (tabulated-list-get-id)))

(defun subsonic-play-tracks ()
  (interactive)
  (start-process "mpv" nil "mpv"
                 "--no-terminal"
                 (concat (subsonic-build-url "/stream.view" `(("id" ,(tabulated-list-get-id)))))))

(defvar subsonic-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'subsonic-open-album) map))


(defvar subsonic-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'subsonic-open-tracks) map))

(defvar subsonic-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'subsonic-play-tracks) map))

;;;###autoload
(defun subsonic-artists ()
  "List subsonic artists."
  (interactive)
  (let ((new-buff (get-buffer-create "*subsonic-artists*")))
    (set-buffer new-buff)
    (setq buffer-read-only t)
    (subsonic-artist-mode)
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun subsonic-albums (id)
  (let ((new-buff (get-buffer-create "*subsonic-albums*")))
    (set-buffer new-buff)
    (subsonic-album-mode)
    (subsonic-albums-refresh id)
    (tabulated-list-revert)
    (pop-to-buffer-same-window (current-buffer))))

;;;###autoload
(defun subsonic-tracks (id)
  (let ((new-buff (get-buffer-create "*subsonic-tracks*")))
    (set-buffer new-buff)
    (subsonic-tracks-mode)
    (subsonic-tracks-refresh id)
    (tabulated-list-revert)
    (pop-to-buffer-same-window (current-buffer))))

(define-derived-mode subsonic-tracks-mode tabulated-list-mode
  "Subsonic Albums"
  (setq tabulated-list-format [("Title" 30 t) ("Duration" 10 t) ("Track" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(define-derived-mode subsonic-album-mode tabulated-list-mode
  "Subsonic Albums"
  (setq tabulated-list-format [("Albums" 30 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))


(define-derived-mode subsonic-artist-mode tabulated-list-mode
  "Subsonic Artists"
  (setq tabulated-list-format [("Artist" 30 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'subsonic-artists-refresh nil t)
  (tabulated-list-init-header))


(provide 'subsonic-artists)

;;; subsonic-artists.el ends here
