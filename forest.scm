(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "helix/static.scm")
(require "helix/ext.scm")
(require (prefix-in helix. "helix/commands.scm"))

(define *forest-width* 32)
(define *forest-search-height* 3)

(define *forest-ignore-set*
  (hashset ".git" "target" ".direnv" "node_modules" "__pycache__" ".hg"))

(define *forest-active* #f)
(define *forest-focused* #f)
(define *forest-tree* '())
(define *forest-cursor* 0)
(define *forest-window-start* 0)
(define *forest-visible-height* 30)
(define *forest-directories* (hash))
(define *forest-query* "")
(define *forest-all-files* '())
(define *forest-search-results* '())

(provide forest-open)

(define (forest-take lst n)
  (if (or (null? lst) (<= n 0)) '() (cons (car lst) (forest-take (cdr lst) (- n 1)))))

(define (forest-drop lst n)
  (if (or (null? lst) (<= n 0)) lst (forest-drop (cdr lst) (- n 1))))

(define (forest-truncate s max-w)
  (if (<= (string-length s) max-w)
      s
      (string-append (substring s 0 (max 0 (- max-w 1))) "…")))

(define (forest-repeat-str s n)
  (if (<= n 0) "" (string-append s (forest-repeat-str s (- n 1)))))

(define (forest-searching?) (not (equal? *forest-query* "")))

;; dirs before files, alphabetic oder
(define (forest-sort-entries lst)
  (define dirs (sort (filter is-dir? lst) string<?))
  (define files (sort (filter (lambda (p) (not (is-dir? p))) lst) string<?))
  (append dirs files))

(define (forest-dir-marker path)
  (if (hash-contains? *forest-directories* path)
      (if (hash-try-get *forest-directories* path) "▶ " "▼ ")
      "▶ "))

(define (forest-build-tree!)
  (define result '())
  (define (walk path depth)
    (define name (file-name path))
    (unless (hashset-contains? *forest-ignore-set* name)
      (define indent (forest-repeat-str "  " depth))
      (define marker (if (is-dir? path) (forest-dir-marker path) "  "))
      (set! result (cons (cons path (string-append indent marker name)) result))
      (when (is-dir? path)
        (unless (hash-contains? *forest-directories* path)
          (set! *forest-directories* (hash-insert *forest-directories* path (> depth 0))))
        (unless (hash-try-get *forest-directories* path)
          (for-each (lambda (child) (walk child (+ depth 1)))
                    (forest-sort-entries (read-dir path)))))))
  (walk (helix-find-workspace) 0)
  (set! *forest-tree* (reverse result)))

;; flat recursive file list for search
;; researches files independent of the fold state
(define (forest-scan-files!)
  (define root (helix-find-workspace))
  (define root-prefix (string-append root (path-separator)))
  (define acc '())
  (define (walk dir)
    (for-each
     (lambda (p)
       (define name (file-name p))
       (unless (hashset-contains? *forest-ignore-set* name)
         (if (is-dir? p)
             (walk p)
             (set! acc (cons p acc)))))
     (with-handler (lambda (_) '()) (read-dir dir))))
  (walk root)
  (set! *forest-all-files*
        (sort (map (lambda (p) (substring p (string-length root-prefix) (string-length p))) acc)
              string<?)))

(define (forest-active-count)
  (if (forest-searching?) (length *forest-search-results*) (length *forest-tree*)))

(define (forest-cursor-down!)
  (define n (forest-active-count))
  (when (< *forest-cursor* (- n 1))
    (set! *forest-cursor* (+ *forest-cursor* 1))
    (when (> *forest-cursor* (+ *forest-window-start* (- *forest-visible-height* 1)))
      (set! *forest-window-start* (+ *forest-window-start* 1)))))

(define (forest-cursor-up!)
  (when (> *forest-cursor* 0)
    (set! *forest-cursor* (- *forest-cursor* 1))
    (when (< *forest-cursor* *forest-window-start*)
      (set! *forest-window-start* (- *forest-window-start* 1)))))

(define (forest-current-entry)
  (if (forest-searching?)
      (and (not (null? *forest-search-results*))
           (let ([rel (list-ref *forest-search-results* *forest-cursor*)])
             (cons (string-append (helix-find-workspace) (path-separator) rel) rel)))
      (and (not (null? *forest-tree*))
           (list-ref *forest-tree* *forest-cursor*))))

(define (forest-refresh-search!)
  (set! *forest-search-results*
        (if (forest-searching?) (fuzzy-match *forest-query* *forest-all-files*) '())))

(define (forest-type! ch)
  (set! *forest-query* (string-append *forest-query* (string ch)))
  (forest-refresh-search!)
  (set! *forest-cursor* 0)
  (set! *forest-window-start* 0))

(define (forest-backspace!)
  (define len (string-length *forest-query*))
  (when (> len 0)
    (set! *forest-query* (substring *forest-query* 0 (- len 1))))
  (forest-refresh-search!)
  (set! *forest-cursor* 0)
  (set! *forest-window-start* 0))

;; refreshes the view after an eaction like deletion
(define (forest-refresh-all!)
  (define old *forest-cursor*)
  (forest-build-tree!)
  (forest-scan-files!)
  (forest-refresh-search!)
  (set! *forest-cursor* (min old (max 0 (- (forest-active-count) 1)))))

(define (forest-toggle-dir! path)
  (set! *forest-directories*
        (hash-insert *forest-directories* path (not (hash-try-get *forest-directories* path))))
  (define old *forest-cursor*)
  (forest-build-tree!)
  (set! *forest-cursor* (min old (max 0 (- (length *forest-tree*) 1)))))

(define (forest-activate!)
  (define entry (forest-current-entry))
  (cond
    [(not entry) event-result/consume]
    [(is-file? (car entry))
     (define path (car entry))
     ;; hand focus to the buffer about to open
     (set! *forest-focused* #f)
     (enqueue-thread-local-callback (lambda () (helix.open path)))
     event-result/close]
    [(is-dir? (car entry))
     (forest-toggle-dir! (car entry))
     event-result/consume]))

(define (forest-unfocus!)
  (set! *forest-focused* #f))

;; leaves the tree focused but pops it off the stack, so the editor gets input again
;; this has to be reachable from inside forest-handle-event-fg directly: while focused,
;; the fg component owns every keypress, so a global leader keymap like space+e never
;; reaches Helix's keymap layer to re-invoke forest-open
(define (forest-switch-to-editor!)
  (pop-last-component-by-name! "forest-fg")
  (forest-unfocus!))

(define (forest-close!)
  (set! *forest-active* #f)
  (set! *forest-focused* #f)
  (pop-last-component-by-name! "forest-fg")
  (pop-last-component-by-name! "forest-bg")
  (enqueue-thread-local-callback (lambda () (set-editor-clip-left! 0))))

;; footer for rename
(define *forest-input-prompt* "")
(define *forest-input-buffer* "")
(define *forest-input-callback* #f)

(struct ForestInputState ())

(define (forest-input-render state rect frame)
  (define w (area-width rect))
  (define y (- (area-height rect) 1))
  (define text (string-append *forest-input-prompt* *forest-input-buffer*))
  (define st (theme-scope-ref "ui.text"))
  (frame-set-string! frame 0 y (make-string w #\space) st)
  (frame-set-string! frame 0 y (forest-truncate text (- w 1)) st))

(define (forest-input-cursor-fn state area)
  (position (- (area-height area) 1)
            (string-length (string-append *forest-input-prompt* *forest-input-buffer*))))

(define (forest-input-handle-event state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-enter? event)
     (define result *forest-input-buffer*)
     (define cb *forest-input-callback*)
     (set! *forest-input-callback* #f)
     (when cb (enqueue-thread-local-callback (lambda () (cb result))))
     event-result/close]
    [(key-event-escape? event)
     (set! *forest-input-callback* #f)
     event-result/close]
    [(key-event-backspace? event)
     (define len (string-length *forest-input-buffer*))
     (when (> len 0)
       (set! *forest-input-buffer* (substring *forest-input-buffer* 0 (- len 1))))
     event-result/consume]
    [(char? ch)
     (set! *forest-input-buffer* (string-append *forest-input-buffer* (string ch)))
     event-result/consume]
    [else event-result/consume]))

(define (forest-show-input! prompt-text initial-value callback)
  (set! *forest-input-prompt* prompt-text)
  (set! *forest-input-buffer* initial-value)
  (set! *forest-input-callback* callback)
  (push-component!
   (new-component! "forest-input"
                   (ForestInputState)
                   forest-input-render
                   (hash "handle_event" forest-input-handle-event
                         "cursor" forest-input-cursor-fn))))

;; footer for confirmation
(define *forest-confirm-prompt* "")
(define *forest-confirm-callback* #f)

(struct ForestConfirmState ())

(define (forest-confirm-render state rect frame)
  (define w (area-width rect))
  (define y (- (area-height rect) 1))
  (define st (theme-scope-ref "ui.text"))
  (frame-set-string! frame 0 y (make-string w #\space) st)
  (frame-set-string! frame 0 y (forest-truncate *forest-confirm-prompt* (- w 1)) st))

(define (forest-confirm-handle-event state event)
  (define ch (key-event-char event))
  (define cb *forest-confirm-callback*)
  (set! *forest-confirm-callback* #f)
  (when cb (enqueue-thread-local-callback (lambda () (cb (and (char? ch) (equal? ch #\y))))))
  event-result/close)

(define (forest-show-confirm! prompt-text callback)
  (set! *forest-confirm-prompt* prompt-text)
  (set! *forest-confirm-callback* callback)
  (push-component!
   (new-component! "forest-confirm"
                   (ForestConfirmState)
                   forest-confirm-render
                   (hash "handle_event" forest-confirm-handle-event))))

;; shells out to mv mkdir since steel has no rename builtin
(define (forest-run-mv! from-path to-path)
  (let ([proc (~> (command "mv" (list from-path to-path))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (if (Ok? proc)
        (let ([stderr (read-port-to-string (child-stderr (Ok->value proc)))])
          (when (not (string=? (trim stderr) ""))
            (error (trim stderr))))
        (error "mv: could not spawn process"))))

(define (forest-run-mkdir-p! path)
  (let ([proc (~> (command "mkdir" (list "-p" path))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (if (Ok? proc)
        (let ([stderr (read-port-to-string (child-stderr (Ok->value proc)))])
          (when (not (string=? (trim stderr) ""))
            (error (trim stderr))))
        (error "mkdir: could not spawn process"))))

(define (forest-prompt-create!)
  (define entry (forest-current-entry))
  (when entry
    (define path (car entry))
    (define base (if (is-dir? path)
                      (string-append path (path-separator))
                      (trim-end-matches path (file-name path))))
    (enqueue-thread-local-callback
     (lambda ()
       (push-component!
        (prompt (string-append "New (end with " (path-separator) " for dir): " base)
                (lambda (name)
                  (define full (string-append base name))
                  (if (ends-with? name (path-separator))
                      (forest-run-mkdir-p! full)
                      (begin
                        (helix.vsplit-new)
                        (helix.open full)
                        (helix.write full)
                        (helix.quit)))
                  (enqueue-thread-local-callback forest-refresh-all!))))))))

(define (forest-prompt-rename!)
  (define entry (forest-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define dir (trim-end-matches path (string-append (path-separator) name)))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-input!
        "Rename: "
        name
        (lambda (new-name)
          (when (and (not (equal? new-name "")) (not (equal? new-name name)))
            (forest-run-mv! path (string-append dir (path-separator) new-name))
            (enqueue-thread-local-callback forest-refresh-all!))))))))

(define (forest-prompt-delete!)
  (define entry (forest-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define kind (if (is-dir? path) "directory" "file"))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-confirm!
        (string-append "Delete " kind " '" name "'? (y/N) ")
        (lambda (confirmed?)
          (when confirmed?
            (if (is-dir? path)
                (delete-directory! path) ; only works if empty
                (delete-file! path))
            (enqueue-thread-local-callback forest-refresh-all!))))))))

(struct ForestBgState ())

(define (forest-render-bg state rect frame)
  (define w (min *forest-width* (area-width rect)))
  (define h (area-height rect))
  (set! *forest-visible-height* (max 1 (- h *forest-search-height*)))
  (set-editor-clip-left! w)

  ;; theme components
  (define bg-style (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define hl-style (theme-scope-ref "ui.menu.selected"))
  (define dir-style (theme-scope-ref "ui.text.info"))
  (define dim-style (style-with-dim (theme-scope-ref "ui.text")))

  ;; no border for cleaner look
  (define panel-area (area 0 0 w h))
  (buffer/clear-with frame panel-area bg-style)

  (define search-area (area 0 0 w *forest-search-height*))
  (block/render frame search-area (make-block bg-style bg-style "all" "rounded"))
  (frame-set-string! frame 1 1 (forest-truncate *forest-query* (- w 2)) text-style)

  (define list-y0 *forest-search-height*)
  (define max-text-w (- w 1))

  (if (forest-searching?)
      (if (null? *forest-search-results*)
          (frame-set-string! frame 1 list-y0 "(no matches)" dim-style)
          (let ([visible (forest-take (forest-drop *forest-search-results* *forest-window-start*)
                                       *forest-visible-height*)])
            (let loop ([items visible] [row 0])
              (unless (or (null? items) (>= row *forest-visible-height*))
                (define abs-idx (+ *forest-window-start* row))
                (define text (forest-truncate (car items) max-text-w))
                (define y (+ list-y0 row))
                (define hl? (= abs-idx *forest-cursor*))
                (when hl?
                  (frame-set-string! frame 0 y (make-string w #\space) hl-style))
                (frame-set-string! frame 0 y text (if hl? hl-style text-style))
                (loop (cdr items) (+ row 1))))))
      (let ([visible (forest-take (forest-drop *forest-tree* *forest-window-start*)
                                   *forest-visible-height*)])
        (let loop ([items visible] [row 0])
          (unless (or (null? items) (>= row *forest-visible-height*))
            (define entry (car items))
            (define abs-idx (+ *forest-window-start* row))
            (define path (car entry))
            (define text (forest-truncate (cdr entry) max-text-w))
            (define y (+ list-y0 row))
            (define hl? (= abs-idx *forest-cursor*))
            (when hl?
              (frame-set-string! frame 0 y (make-string w #\space) hl-style))
            (frame-set-string! frame 0 y text
                               (cond [hl? hl-style]
                                     [(is-dir? path) dir-style]
                                     [else text-style]))
            (loop (cdr items) (+ row 1)))))))

(define (forest-handle-event-bg state event)
  ;; makes the editor receive events while the panel is unfocused
  event-result/ignore)

(struct ForestFgState ())

(define (forest-render-fg state rect frame) void) ; bg handles all drawing

(define (forest-cursor-fn-fg state area)
  (position 1 (+ 1 (string-length *forest-query*))))

(define (forest-handle-event-fg state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-down? event) (forest-cursor-down!) event-result/consume]
    [(key-event-up? event) (forest-cursor-up!) event-result/consume]
    [(key-event-enter? event) (forest-activate!)]
    [(key-event-tab? event)
     (define entry (forest-current-entry))
     (when (and entry (is-dir? (car entry))) (forest-toggle-dir! (car entry)))
     event-result/consume]

    [(key-event-escape? event)
     (forest-switch-to-editor!)
     event-result/close] ; pops fg only; bg stays visible

    [(and (char? ch) (equal? ch #\q) (equal? (key-event-modifier event) key-modifier-ctrl))
     (forest-close!)
     event-result/close] ; pops fg; forest-close! already popped bg

    ;; Ctrl+<letter> cuz plain letters need stay free for search
    [(and (char? ch) (equal? ch #\n) (equal? (key-event-modifier event) key-modifier-ctrl))
     (forest-prompt-create!)
     event-result/consume]
    [(and (char? ch) (equal? ch #\r) (equal? (key-event-modifier event) key-modifier-ctrl))
     (forest-prompt-rename!)
     event-result/consume]
    [(and (char? ch) (equal? ch #\x) (equal? (key-event-modifier event) key-modifier-ctrl))
     (forest-prompt-delete!)
     event-result/consume]
    [(and (char? ch) (equal? ch #\e) (equal? (key-event-modifier event) key-modifier-ctrl))
     (forest-refresh-all!)
     event-result/consume]

    [(key-event-backspace? event)
     (forest-backspace!)
     event-result/consume]

    [(char? ch)
     (forest-type! ch)
     event-result/consume]

    [else event-result/consume])) ; block unknown keys from editor while focused

(define (forest-make-bg-component)
  (new-component! "forest-bg"
                  (ForestBgState)
                  forest-render-bg
                  (hash "handle_event" forest-handle-event-bg)))

(define (forest-make-fg-component)
  (new-component! "forest-fg"
                  (ForestFgState)
                  forest-render-fg
                  (hash "handle_event" forest-handle-event-fg
                        "cursor" forest-cursor-fn-fg)))

;;@doc
;; Open the file tree
(define (forest-open)
  (cond
    [(not *forest-active*)
     (set! *forest-active* #t)
     (set! *forest-focused* #t)
     (set! *forest-cursor* 0)
     (set! *forest-window-start* 0)
     (set! *forest-query* "")
     (set! *forest-search-results* '())
     (forest-build-tree!)
     (forest-scan-files!)
     (push-component! (forest-make-bg-component))
     (push-component! (forest-make-fg-component))]

    [*forest-focused*
     (forest-switch-to-editor!)]

    [else
     (set! *forest-focused* #t)
     (push-component! (forest-make-fg-component))]))
