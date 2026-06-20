(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))

(define *forest-width* 32)

(define *forest-ignore-set*
  (hashset ".git" "target" ".direnv" "node_modules" "__pycache__" ".hg"))

(define *forest-active* #f)
(define *forest-focused* #f)
(define *forest-tree* '())
(define *forest-cursor* 0)
(define *forest-window-start* 0)
(define *forest-visible-height* 30)
(define *forest-directories* (hash))

(provide forest-toggle)

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

(define (forest-cursor-down!)
  (define n (length *forest-tree*))
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
  (and (not (null? *forest-tree*))
       (list-ref *forest-tree* *forest-cursor*)))

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

(define (forest-close!)
  (set! *forest-active* #f)
  (set! *forest-focused* #f)
  (pop-last-component-by-name! "forest-fg")
  (pop-last-component-by-name! "forest-bg")
  (enqueue-thread-local-callback (lambda () (set-editor-clip-left! 0))))

(struct ForestBgState ())

(define (forest-render-bg state rect frame)
  (define w (min *forest-width* (area-width rect)))
  (define h (area-height rect))
  (set! *forest-visible-height* (max 1 (- h 1)))
  (set-editor-clip-left! w)

  ;; theme components 
  (define bg-style (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define hl-style (theme-scope-ref "ui.menu.selected"))
  (define dir-style (theme-scope-ref "ui.text.info"))
  (define title-style (theme-scope-ref "ui.statusline.normal"))

  ;; no border for cleaner look
  (define panel-area (area 0 0 w h))
  (buffer/clear-with frame panel-area bg-style)

  (define ws-name (file-name (helix-find-workspace)))
  (frame-set-string! frame 1 0 (forest-truncate ws-name (- w 2)) title-style)

  (define max-text-w (- w 1))
  (define visible (forest-take (forest-drop *forest-tree* *forest-window-start*) *forest-visible-height*))

  (let loop ([items visible] [row 0])
    (unless (or (null? items) (>= row *forest-visible-height*))
      (define entry (car items))
      (define abs-idx (+ *forest-window-start* row))
      (define path (car entry))
      (define text (forest-truncate (cdr entry) max-text-w))
      (define y (+ 1 row))
      (define hl? (= abs-idx *forest-cursor*))
      (when hl?
        (frame-set-string! frame 0 y (make-string w #\space) hl-style))
      (frame-set-string! frame 0 y text
                         (cond [hl? hl-style]
                               [(is-dir? path) dir-style]
                               [else text-style]))
      (loop (cdr items) (+ row 1)))))

(define (forest-handle-event-bg state event)
  ;; makes the editor receive events while the panel is unfocused
  event-result/ignore)

(struct ForestFgState ())

(define (forest-render-fg state rect frame) void) ; bg handles all drawing

(define (forest-cursor-fn-fg state area)
  (position (+ 1 (- *forest-cursor* *forest-window-start*)) 0))

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
     (forest-unfocus!)
     event-result/close] ; pops fg only; bg stays visible

    [(and (char? ch) (equal? ch #\q))
     (forest-close!)
     event-result/close] ; pops fg; forest-close! already popped bg

    [(and (char? ch) (equal? ch #\j)) (forest-cursor-down!) event-result/consume]
    [(and (char? ch) (equal? ch #\k)) (forest-cursor-up!) event-result/consume]
    [(and (char? ch) (equal? ch #\o)) (forest-activate!)]

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
(define (forest-toggle)
  (cond
    [(not *forest-active*)
     (set! *forest-active* #t)
     (set! *forest-focused* #t)
     (set! *forest-cursor* 0)
     (set! *forest-window-start* 0)
     (forest-build-tree!)
     (push-component! (forest-make-bg-component))
     (push-component! (forest-make-fg-component))]

    [*forest-focused*
     (pop-last-component-by-name! "forest-fg")
     (forest-unfocus!)]

    [else
     (set! *forest-focused* #t)
     (push-component! (forest-make-fg-component))]))
