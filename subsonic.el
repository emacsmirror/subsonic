;; -*- lexical-binding: t -*-

;;; Code:
(require 'json)
(require 'url)


;; Credit & thanks to the mpv.el and docker-mode projects for examples
;; and much of the code here :)

(defvar subsonic-mpv--process nil)
(defvar subsonic-mpv--queue nil)

(defun subsonic-mpv-kill ()
  "Kill the mpv process."
  (interactive)
  (when subsonic-mpv--queue
    (tq-close subsonic-mpv--queue))
  (when (subsonic-mpv-live-p)
    (kill-process subsonic-mpv--process))
  (with-timeout
      (0.5 (error "Failed to kill mpv"))
    (while (subsonic-mpv-live-p)
      (sleep-for 0.05)))
  (setq subsonic-mpv--process nil)
  (setq subsonic-mpv--queue nil))

(defun subsonic-mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and subsonic-mpv--process (eq (process-status subsonic-mpv--process) 'run)))

(defun subsonic-mpv-start (args)
  (subsonic-mpv-kill)
  (let ((socket (make-temp-name
                 (expand-file-name "subsonic-mpv-" temporary-file-directory))))
    (setq subsonic-mpv--process (apply #'start-process
                              (append
                               (list "mpv-player" nil "mpv"
                                     "--no-terminal"
                                     "--no-video"
                                     (concat "--input-ipc-server=" socket))
                               args)))
    (set-process-query-on-exit-flag subsonic-mpv--process nil)
    (set-process-sentinel subsonic-mpv--process
                          (lambda (process _event)
                            (when (memq (process-status process) '(exit signal))
                              (subsonic-mpv-kill)
                              (when (file-exists-p socket)
                                (with-demoted-errors (delete-file socket))))))
    (with-timeout (0.5 (subsonic-mpv-kill)
                       (error "Failed to connect to mpv"))
      (while (not (file-exists-p socket))
        (sleep-for 0.05)))
    (setq subsonic-mpv--queue (tq-create
                      (make-network-process :name "subsonic-mpv-socket"
                                            :family 'local
                                            :service socket)))
    (set-process-filter
     (tq-process subsonic-mpv--queue)
     (lambda (_proc string)
       nil))
    t))

(defvar subsonic-auth (let ((auth (auth-source-search :port "subsonic")))
                        (if auth
                            (car auth)
                          (error "Failed to find subsonic auth in .authinfo"))))

(defun alist->query (al)
  (seq-reduce
   (lambda (accu q)
     (if (string-empty-p accu)
         (concat "?" (car q) "=" (cdr q))
       (concat accu "&" (car q) "=" (cdr q))))
   al ""))

(defun get-json (url)
  (condition-case nil
      (let* ((json-array-type 'list)
             (json-key-type 'string))
        (json-read-from-string (with-temp-buffer (url-insert-file-contents url)
                                                 (prog1 (buffer-string)
                                                   (kill-buffer)))))
    (json-readtable-error (error "could not read \"%s\" as json" body))))


(defun recursive-assoc (data keys)
  (if keys
      (recursive-assoc (assoc-default (car keys) data)
                       (cdr keys))
    data))

(defun subsonic-build-url (endpoint extra-query)
  (concat "https://"
          (plist-get subsonic-auth :host)
          "/rest" endpoint
          (alist->query (append `(("u" . ,(plist-get subsonic-auth :user))
                                  ("p" . ,(funcall (plist-get subsonic-auth :secret)))
                                  ("c" . "ElSonic")
                                  ("v" . "1.16.0")
                                  ("f" . "json"))
                                extra-query))))

(defun subsonic-artists-parse (data)
  (let* ((artists (recursive-assoc data '("subsonic-response" "indexes" "index")))
         (result (seq-reduce (lambda (accu artist-index)
                               (append accu (mapcar (lambda (artist)
                                                      (list (assoc-default "id"artist)
                                                            (vector (assoc-default "name" artist))))
                                                    (assoc-default "artist" artist-index))))
                             artists '()))) result))


(defun subsonic-albums-parse (data)
  (let* ((albums (recursive-assoc data '("subsonic-response" "directory" "child")))
         (result (mapcar (lambda (album)
                           (list (assoc-default "id" album)
                                 (vector (assoc-default "title" album))))
                         albums)))
    result))


(defun subsonic-tracks-parse (data)
  (let* ((tracks (recursive-assoc data '("subsonic-response" "directory" "child")))
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
         (get-json (subsonic-build-url "/getMusicDirectory.view" `(("id" . ,id)))))))

(defun subsonic-tracks-refresh (id)
  (setq tabulated-list-entries
        (subsonic-tracks-parse
         (get-json (subsonic-build-url "/getMusicDirectory.view" `(("id" . ,id)))))))


(defun subsonic-open-album ()
  (interactive)
  (subsonic-albums (tabulated-list-get-id)))

(defun subsonic-open-tracks ()
  (interactive)
  (subsonic-tracks (tabulated-list-get-id)))

(defun get-tracklist-id (id)
  (reverse (seq-reduce (lambda (accu current)
                         (if (equal (car current) id)
                             (list (car current))
                           (if (null accu)
                               '()
                             (cons (car current) accu))))
                       tabulated-list-entries '())))

(defun subsonic-play-tracks ()
  (interactive)
  (subsonic-mpv-start (mapcar (lambda (id)
                       (subsonic-build-url "/stream.view" `(("id" . ,id))))
                     (get-tracklist-id (tabulated-list-get-id)))))

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
