(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "helix/static.scm")
(require "helix/ext.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "notify/notify.scm")
(require "glyph/glyph.scm")

(define (forest-info msg)
  (notify msg #:title "forest.hx"))

(define (forest-error msg)
  (notify msg #:severity 'error #:title "forest.hx"))

(define *forest-width* 32) ;
(define *forest-min-width* 16)
(define *forest-max-width* 60)
(define *forest-search-height* 3)
(define *forest-side* 'left) ; left or right set with forest-configure!
(define *forest-show-separator?* #t)
(define *forest-bg-focused* #f)
(define *forest-bg-unfocused* #f)
(define *forest-search-color-focused* #f)
(define *forest-search-color-unfocused* #f)
(define *forest-search-default-focused* "#ffa500")
(define *forest-search-default-unfocused* "#ffffff")
(define *forest-search-follow-focus?* #t)

(define *forest-ignore-set*
  (hashset ".git" "target" ".direnv" "node_modules" "__pycache__" ".hg"))

;; dotfiles and git-ignored entries are hidden by default
(define *forest-show-hidden* #f)
(define *forest-show-git-ignored* #f)
(define *forest-git-ignored-set* (hashset))

(define (forest-dotfile? name)
  (and (> (string-length name) 0) (char=? (string-ref name 0) #\.)))

(define (forest-git-repo? dir)
  (let ([proc (~> (command "git" (list "-C" dir "rev-parse" "--is-inside-work-tree"))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (and (Ok? proc)
         (string=? (trim (read-port-to-string (child-stdout (Ok->value proc)))) "true"))))

(define *forest-git-status-map* (hash))

;; classifies code it modifiles added deleted and renames
(define (forest-git-status-symbol code)
  (define x (string-ref code 0))
  (define y (string-ref code 1))
  (cond
    [(and (char=? x #\?) (char=? y #\?)) 'untracked]
    [(or (char=? x #\A) (char=? y #\A)) 'added]
    [(or (char=? x #\D) (char=? y #\D)) 'deleted]
    [(or (char=? x #\R) (char=? y #\R)) 'renamed]
    [(or (char=? x #\M) (char=? y #\M)) 'modified]
    [else #f]))

(define (forest-status-path rest)
  (define parts (split-many rest " -> "))
  (trim-end-matches (if (> (length parts) 1) (list-ref parts (- (length parts) 1)) rest)
                     (path-separator)))

(define (forest-parse-git-status-lines lines)
  (let loop ([ls lines] [ign (hashset)] [statuses (hash)])
    (if (null? ls)
        (cons ign statuses)
        (let ([line (car ls)])
          (if (< (string-length line) 3)
              (loop (cdr ls) ign statuses)
              (let* ([code (substring line 0 2)]
                     [path (forest-status-path (trim (substring line 3 (string-length line))))])
                (if (string=? code "!!")
                    (loop (cdr ls) (hashset-insert ign path) statuses)
                    (let ([sym (forest-git-status-symbol code)])
                      (loop (cdr ls) ign (if sym (hash-insert statuses path sym) statuses))))))))))

;; recomputes which workspace-relative paths git considers ignored
(define (forest-scan-git-ignored! root)
  (define parsed
    (with-handler
      (lambda (_) (cons (hashset) (hash)))
      (if (not (forest-git-repo? root))
          (cons (hashset) (hash))
          (let ([proc (~> (command "git" (list "-C" root "status" "--porcelain" "--ignored=matching"))
                          with-stdout-piped
                          with-stderr-piped
                          spawn-process)])
            (if (Ok? proc)
                (let* ([output (read-port-to-string (child-stdout (Ok->value proc)))]
                       [lines (filter (lambda (l) (> (string-length l) 0)) (split-many output "\n"))])
                  (forest-parse-git-status-lines lines))
                (cons (hashset) (hash)))))))
  (set! *forest-git-ignored-set* (car parsed))
  (set! *forest-git-status-map* (cdr parsed)))

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
(define *forest-typing?* #f)

(define *forest-default-keybinds*
  (hash 'down "j"
        'up "k"
        'enter "l" ; snacks: enter dir or open file; mini: cascade
        'back "h" ; snacks: leave folder; mini: parent column
        'search "/"
        'create "n"
        'rename "r"
        'delete "d"
        'refresh "R"
        'toggle-hidden "."
        'toggle-git-ignored "i"
        'wider "+"
        'narrower "-"
        'menu " " ; space key
        'quit "q"))

(define *forest-keybinds* *forest-default-keybinds*)

;; looks up which action (if any) a keypress is bound to
(define (forest-action-for-char ch)
  (define s (string ch))
  (let loop ([ks (hash-keys->list *forest-keybinds*)])
    (cond
      [(null? ks) #f]
      [(equal? (hash-try-get *forest-keybinds* (car ks)) s) (car ks)]
      [else (loop (cdr ks))])))

(provide forest-open)
(provide forest-close)
(provide forest-configure!)
(provide forest-set-style!)
(provide forest-set-keybinds!)
(provide forest-set-sidebar-bg!)
(provide forest-set-search-color!)
(provide forest-snacks-active?)
(provide forest-snacks-side)
(provide forest-snacks-width)

;;@doc
;; true while the snacks sidebar is open, so plugins wont go over sidebar
(define (forest-snacks-active?)
  (and *forest-active* (equal? *forest-style* 'snacks)))

;;@doc
;; Side the snacks sidebar sits on: 'left or 'right
(define (forest-snacks-side)
  *forest-side*)

;;@doc
;; Width in columns of the snacks sidebar, 0 while it is closed
(define (forest-snacks-width)
  (if (and *forest-active* (equal? *forest-style* 'snacks)) *forest-width* 0))

;;@doc
;; Override any subset of forest's keybindings from init.scm
;; (forest-set-keybinds! (hash 'rename "R" 'refresh "r"))
(define (forest-set-keybinds! overrides)
  (set! *forest-keybinds*
        (let loop ([ks (hash-keys->list overrides)] [acc *forest-keybinds*])
          (if (null? ks)
              acc
              (loop (cdr ks) (hash-insert acc (car ks) (hash-try-get overrides (car ks))))))))

;;@doc
;; Set which side the file tree renders, hidden entries, and whether a
;; vertical separator line is drawn between the tree and the text buffer
(define (forest-configure! side
                            #:ignore [ignore (list )]
                            #:separator? [separator? #t])
  (set! *forest-side* side)
  (set! *forest-ignore-set* (apply hashset ignore))
  (set! *forest-show-separator?* separator?))

(define *forest-style* 'snacks)

;;@doc
;; Pick which explorer UI forest-open uses: 'snacks or 'mini
(define (forest-set-style! style)
  (set! *forest-style* style))

;; #rrggbb string to a Color, or #f on anything unparseable
(define (forest-hex->color hex)
  (and (string? hex) (with-handler (lambda (_) #f) (glyph-hex->color hex))))

;;@doc
;; Give the snacks sidebar its own background per focus state
(define (forest-set-sidebar-bg! #:focused [focused #f] #:unfocused [unfocused #f])
  (set! *forest-bg-focused* (forest-hex->color focused))
  (set! *forest-bg-unfocused* (forest-hex->color unfocused)))

;; the sidebar background for the current focus state, or #f for the theme default
(define (forest-panel-bg-color)
  (if *forest-focused* *forest-bg-focused* *forest-bg-unfocused*))

;;@doc
;; Color the snacks search box per focus mode
(define (forest-set-search-color! #:focused [focused #f]
                                   #:unfocused [unfocused #f]
                                   #:always [always #f]
                                   #:follow-focus? [follow-focus? #t])
  (define both (forest-hex->color always))
  (set! *forest-search-color-focused* (or both (forest-hex->color focused)))
  (set! *forest-search-color-unfocused* (or both (forest-hex->color unfocused)))
  (set! *forest-search-follow-focus?* (if both #f follow-focus?)))

;; search box outline color for the current focus state
(define (forest-search-color)
  (if (or (not *forest-search-follow-focus?*) *forest-focused*)
      (or *forest-search-color-focused* (forest-hex->color *forest-search-default-focused*))
      (or *forest-search-color-unfocused* (forest-hex->color *forest-search-default-unfocused*))))

;; keep the panel off the rows moka and scopeline reserve
(define *forest-reserved-top-fn* 'unresolved)
(define *forest-reserved-bottom-fn* 'unresolved)

(define (forest-resolve-reserved!)
  (when (equal? *forest-reserved-top-fn* 'unresolved)
    (set! *forest-reserved-top-fn* (with-handler (lambda (_) #f) (eval 'moka-reserved-top)))
    (set! *forest-reserved-bottom-fn* (with-handler (lambda (_) #f) (eval 'moka-reserved-bottom)))))

(define (forest-reserved-top)
  (forest-resolve-reserved!)
  (+ (if *forest-reserved-top-fn* (with-handler (lambda (_) 0) (*forest-reserved-top-fn*)) 0)
     (with-handler (lambda (_) 0)
       (let ([v (eval-string "(scopeline-reserved-top)")])
         (if (number? v) v 0)))))

(define (forest-reserved-bottom)
  (forest-resolve-reserved!)
  (if *forest-reserved-bottom-fn* (with-handler (lambda (_) 0) (*forest-reserved-bottom-fn*)) 0))

(define *forest-moka-clip-fn* 'unresolved)
(define *forest-scopeline-clip-fn* 'unresolved)

(define (forest-resolve-clip-fns!)
  (when (equal? *forest-moka-clip-fn* 'unresolved)
    (set! *forest-moka-clip-fn* (with-handler (lambda (_) #f) (eval 'moka-set-forest-clip!)))
    (set! *forest-scopeline-clip-fn* (with-handler (lambda (_) #f) (eval 'scopeline-set-forest-clip!)))))

;; tell moka and scopeline where the snacks sidebar is, so their bars stop at the buffer
(define (forest-publish-clip! side w)
  (forest-resolve-clip-fns!)
  (when *forest-moka-clip-fn*
    (with-handler (lambda (_) #f) (*forest-moka-clip-fn* side w)))
  (when *forest-scopeline-clip-fn*
    (with-handler (lambda (_) #f) (*forest-scopeline-clip-fn* side w))))

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

(struct ForestHelpState ())
(define *forest-help-open?* #f)
(define *forest-help-style* 'snacks)
(define *forest-help-dispatch* #f)

;; the bound key for an action with fallback to ?
(define (forest-help-key action)
  (define k (hash-try-get *forest-keybinds* action))
  (cond
    [(equal? k " ") "space"]
    [(and (string? k) (not (equal? k ""))) k]
    [else "?"]))

;; snaks keybinds
(define (forest-snacks-help-rows)
  (list
    (list (string-append (forest-help-key 'down) " / " (forest-help-key 'up) " / ↑ / ↓") "Navigate in folder")
    (list (string-append (forest-help-key 'enter) " / →") "Enter dir or open file")
    (list (string-append (forest-help-key 'back) " / ←") "Leave folder")
    (list "Enter / Tab" "Toggle directory")
   (list (forest-help-key 'search) "Fuzzy search")
   (list (forest-help-key 'create) "Create file or dir")
   (list (forest-help-key 'rename) "Rename entry")
   (list (forest-help-key 'delete) "Delete entry")
   (list (forest-help-key 'refresh) "Refresh tree")
   (list (forest-help-key 'toggle-hidden) "Toggle dotfiles")
   (list (forest-help-key 'toggle-git-ignored) "Toggle git-ignored")
   (list (string-append (forest-help-key 'wider) " / " (forest-help-key 'narrower)) "Widen or narrow panel")
   (list "Esc" "Focus editor")
   (list (forest-help-key 'quit) "Close panel")))

;; mini keybinds
(define (forest-mini-help-rows)
  (list
   (list (string-append (forest-help-key 'down) " / " (forest-help-key 'up) " / ↑ / ↓") "Move")
   (list (string-append (forest-help-key 'enter) " / → / Enter") "Open file or enter dir")
   (list (string-append (forest-help-key 'back) " / ←") "Parent column")
   (list (forest-help-key 'search) "Fuzzy search")
   (list (forest-help-key 'create) "Create file or dir")
   (list (forest-help-key 'rename) "Rename entry")
   (list (forest-help-key 'delete) "Delete entry")
   (list (forest-help-key 'refresh) "Refresh column")
   (list (forest-help-key 'toggle-hidden) "Toggle dotfiles")
   (list (forest-help-key 'toggle-git-ignored) "Toggle git-ignored")
   (list (string-append (forest-help-key 'wider) " / " (forest-help-key 'narrower)) "Widen or narrow")
   (list (string-append "Esc / " (forest-help-key 'quit)) "Close")))

;; helix style box tucked into the bottom right corner
(define (forest-help-metrics rect rows)
  (define key-w (apply max (cons 1 (map (lambda (r) (string-length (car r))) rows))))
  (define desc-w (apply max (cons 1 (map (lambda (r) (string-length (cadr r))) rows))))
  (define content-w (+ key-w 2 desc-w))
  (define w (min (max 0 (- (area-width rect) 2)) (+ content-w 4)))
  ;; two borders around one row per binding
  (define h (min (max 0 (- (area-height rect) 2)) (+ (length rows) 2)))
  ;; placed above the reserved line e.g for moka statusline
  (define bottom (+ (forest-reserved-bottom) 1))
  (define x (max 0 (- (area-width rect) w)))
  (define y (max 0 (- (area-height rect) h bottom)))
  (list x y w h key-w))

(define (forest-help-render state rect frame)
  (define rows (if (equal? *forest-help-style* 'mini) (forest-mini-help-rows) (forest-snacks-help-rows)))

  (define bg-style (theme-scope-ref "ui.menu"))
  (define text-style (theme-scope-ref "ui.text"))
  (define key-style (style-with-bold (theme-scope-ref "ui.text.info")))
  (define border-style (style-with-dim text-style))

  (define m (forest-help-metrics rect rows))
  (define x (list-ref m 0))
  (define y (list-ref m 1))
  (define w (list-ref m 2))
  (define h (list-ref m 3))
  (define key-w (list-ref m 4))

  (define box (area x y w h))
  (define inner-x (+ x 1))

  (buffer/clear-with frame box bg-style)
  (block/render frame box (make-block bg-style border-style "all" "plain"))

  (let loop ([rs rows] [row 0])
    (when (and (pair? rs) (< row (- h 2)))
      (define r (car rs))
      (define ry (+ y 1 row))
      (define dx (+ inner-x 1 key-w 2))
      (define avail (max 0 (- (+ x w) 1 dx)))
      (frame-set-string! frame (+ inner-x 1) ry (car r) key-style)
      (frame-set-string! frame dx ry (forest-truncate (cadr r) avail) text-style)
      (loop (cdr rs) (+ row 1)))))

;; pressing a listed key runs that action and closes and esc or space
;; closes without running anything
(define (forest-help-handle-event state event)
  (define ch (key-event-char event))
  (cond
    [(mouse-event? event) event-result/consume]
    [(key-event-escape? event) (set! *forest-help-open?* #f) event-result/close]
    [(char? ch)
     (define action (forest-action-for-char ch))
     (define dispatch *forest-help-dispatch*)
     (set! *forest-help-open?* #f)
     (when (and action dispatch (not (equal? action 'menu)))
       ;; deferred so the menu is gone before the action pushes a modal of its own
       (enqueue-thread-local-callback (lambda () (dispatch action))))
     event-result/close]
    [else (set! *forest-help-open?* #f) event-result/close]))

;; pops the popup from outside its own handler, e.g. when the explorer closes
(define (forest-help-dismiss!)
  (when *forest-help-open?*
    (set! *forest-help-open?* #f)
    (pop-last-component-by-name! "forest-help")))

;; space opens the which-key menu
(define (forest-whichkey-open! style dispatch)
  (unless *forest-help-open?*
    (set! *forest-help-style* style)
    (set! *forest-help-dispatch* dispatch)
    (set! *forest-help-open?* #t)
    (push-component!
     (new-component! "forest-help"
                     (ForestHelpState)
                     forest-help-render
                     (hash "handle_event" forest-help-handle-event)))))

;; strips the workspace prefix so prompts show a short path instead of the full one
(define (forest-relpath path)
  (define prefix (string-append (helix-find-workspace) (path-separator)))
  (if (and (>= (string-length path) (string-length prefix))
           (equal? (substring path 0 (string-length prefix)) prefix))
      (substring path (string-length prefix) (string-length path))
      path))

(define (forest-git-ignored? path)
  (hashset-contains? *forest-git-ignored-set* (forest-relpath path)))

(define (forest-git-status path)
  (hash-try-get *forest-git-status-map* (forest-relpath path)))

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
    (unless (or (hashset-contains? *forest-ignore-set* name)
                (and (not *forest-show-hidden*) (forest-dotfile? name))
                (and (not *forest-show-git-ignored*) (forest-git-ignored? path)))
      (define indent (forest-repeat-str "  " depth))
      (define marker (if (is-dir? path) (forest-dir-marker path) "  "))
      (set! result (cons (list path indent marker name) result))
      (when (is-dir? path)
        (unless (hash-contains? *forest-directories* path)
          (set! *forest-directories* (hash-insert *forest-directories* path (> depth 0))))
        (unless (hash-try-get *forest-directories* path)
          (for-each (lambda (child) (walk child (+ depth 1)))
                    (forest-sort-entries (read-dir path)))))))
  (walk (helix-find-workspace) 0)
  (set! *forest-tree* (reverse result)))

(define (forest-parent-path path)
  (trim-end-matches path (string-append (path-separator) (file-name path))))

(define (forest-half-floor n)
  (let loop ([n n] [h 0])
    (if (< n 2) h (loop (- n 2) (+ h 1)))))

;; marks every old dir between the workspace root and path as open
(define (forest-open-ancestors-for-file! path)
  (define ws (helix-find-workspace))
  (define ws-prefix (string-append ws (path-separator)))
  (when (and (string? path)
             (>= (string-length path) (string-length ws-prefix))
             (equal? (substring path 0 (string-length ws-prefix)) ws-prefix))
    (define (open-up! p)
      (define parent (forest-parent-path p))
      (set! *forest-directories* (hash-insert *forest-directories* parent #f))
      (unless (equal? parent ws)
        (open-up! parent)))
    (open-up! path)))

;; moves the cursor to the file path
(define (forest-seek-file! path)
  (when (string? path)
    (define idx
      (let loop ([items *forest-tree*] [i 0])
        (cond [(null? items) #f]
              [(equal? (car (car items)) path) i]
              [else (loop (cdr items) (+ i 1))])))
    (when idx
      (set! *forest-cursor* idx)
      (set! *forest-window-start*
            (max 0 (- idx (forest-half-floor *forest-visible-height*)))))))

(define (forest-reveal-current-file!)
  (define path (editor-document->path (editor->doc-id (editor-focus))))
  (forest-open-ancestors-for-file! path)
  (forest-build-tree!)
  (unless (forest-searching?)
    (forest-seek-file! path)))

;; flat recursive file list for search
;; searches files indepedent of the fold state
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

(define (forest-index-of lst x)
  (let loop ([xs lst] [i 0])
    (cond
      [(null? xs) #f]
      [(equal? (car xs) x) i]
      [else (loop (cdr xs) (+ i 1))])))

(define (forest-wrap i n)
  (cond
    [(< i 0) (- n 1)]
    [(>= i n) 0]
    [else i]))

(define (forest-ensure-cursor-visible!)
  (define last-vis (+ *forest-window-start* (- *forest-visible-height* 1)))
  (when (> *forest-cursor* last-vis)
    (set! *forest-window-start* (- *forest-cursor* (- *forest-visible-height* 1))))
  (when (< *forest-cursor* *forest-window-start*)
    (set! *forest-window-start* *forest-cursor*)))

;; indices of entries that share the current entry's parent
(define (forest-sibling-indices)
  (define entry (forest-current-entry))
  (if (not entry)
      '()
      (let ([parent (forest-parent-path (car entry))])
        (let loop ([items *forest-tree*] [i 0] [acc '()])
          (cond
            [(null? items) (reverse acc)]
            [(equal? (forest-parent-path (car (car items))) parent)
             (loop (cdr items) (+ i 1) (cons i acc))]
            [else (loop (cdr items) (+ i 1) acc)])))))

(define (forest-cursor-sibling! delta)
  (define idxs (forest-sibling-indices))
  (define n (length idxs))
  (define pos (forest-index-of idxs *forest-cursor*))
  (when (and (> n 0) pos)
    (set! *forest-cursor* (list-ref idxs (forest-wrap (+ pos delta) n)))
    (forest-ensure-cursor-visible!)))

(define (forest-cursor-down!)
  (if (forest-searching?)
      (let ([n (forest-active-count)])
        (when (> n 0)
          (set! *forest-cursor* (forest-wrap (+ *forest-cursor* 1) n))
          (forest-ensure-cursor-visible!)))
      (forest-cursor-sibling! 1)))

(define (forest-cursor-up!)
  (if (forest-searching?)
      (let ([n (forest-active-count)])
        (when (> n 0)
          (set! *forest-cursor* (forest-wrap (- *forest-cursor* 1) n))
          (forest-ensure-cursor-visible!)))
      (forest-cursor-sibling! -1)))

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

;; / starts a new query  and letters stay free for single-key commands
(define (forest-enter-search!)
  (set! *forest-typing?* #t)
  (set! *forest-query* "")
  (forest-refresh-search!)
  (set! *forest-cursor* 0)
  (set! *forest-window-start* 0))

(define (forest-clear-search!)
  (set! *forest-query* "")
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

(define *forest-refresh-mini-fn* #f)

;; refreshes whichever style is active immediatelly
(define (forest-refresh-current-style!)
  (if (and (equal? *forest-style* 'mini) *forest-refresh-mini-fn*)
      (*forest-refresh-mini-fn*)
      (forest-refresh-all!)))

(define (forest-toggle-hidden!)
  (set! *forest-show-hidden* (not *forest-show-hidden*))
  (forest-info (if *forest-show-hidden* "forest: showing dotfiles" "forest: hiding dotfiles"))
  (forest-refresh-current-style!))

(define (forest-toggle-git-ignored!)
  (set! *forest-show-git-ignored* (not *forest-show-git-ignored*))
  (forest-info (if *forest-show-git-ignored* "forest: showing git-ignored" "forest: hiding git-ignored"))
  (forest-refresh-current-style!))

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

(define (forest-dir-expanded? path)
  (and (is-dir? path)
       (hash-contains? *forest-directories* path)
       (not (hash-try-get *forest-directories* path))))

(define (forest-path-in-workspace? path)
  (define ws (helix-find-workspace))
  (or (equal? path ws)
      (starts-with? path (string-append ws (path-separator)))))

(define (forest-first-child-index path)
  (let loop ([items *forest-tree*] [i 0])
    (cond
      [(null? items) #f]
      [(equal? (forest-parent-path (car (car items))) path) i]
      [else (loop (cdr items) (+ i 1))])))

;; expand if needed and move onto the first child
(define (forest-enter-dir! path)
  (unless (forest-dir-expanded? path)
    (forest-toggle-dir! path))
  (define idx (forest-first-child-index path))
  (when idx
    (set! *forest-cursor* idx)
    (forest-ensure-cursor-visible!)))

(define (forest-enter-or-open!)
  (define entry (forest-current-entry))
  (cond
    [(not entry) event-result/consume]
    [(is-file? (car entry)) (forest-activate!)]
    [(is-dir? (car entry))
     (forest-enter-dir! (car entry))
     event-result/consume]
    [else event-result/consume]))

;; leave the current folder collapses it and moves to parent
(define (forest-goto-parent!)
  (define entry (forest-current-entry))
  (when (and entry (not (forest-searching?)))
    (define path (car entry))
    (define ws (helix-find-workspace))
    (cond
      [(equal? path ws) void]
      [else
       (define parent (forest-parent-path path))
       (when (and (string? parent) (forest-path-in-workspace? parent))
         (when (and (forest-dir-expanded? parent) (not (equal? parent ws)))
           (forest-toggle-dir! parent))
         (forest-seek-file! parent))])))

(define (forest-unfocus!)
  (forest-reset-mouse!)
  (set! *forest-focused* #f))

;; leaves the tree focused but pops it off the stack, so the editor gets input again
;; this has to be reachable from inside forest-handle-event-fg directly: while focused,
;; the fg component owns every keypress, so a global leader keymap like space+e never
;; reaches Helix's keymap layer to re-invoke forest-open
(define (forest-switch-to-editor!)
  (pop-last-component-by-name! "forest-fg")
  (forest-unfocus!))

(define (forest-close!)
  (forest-reset-mouse!)
  (forest-help-dismiss!)
  (set! *forest-active* #f)
  (set! *forest-focused* #f)
  (pop-last-component-by-name! "forest-fg")
  (pop-last-component-by-name! "forest-bg")
  (forest-publish-clip! #f 0)
  (enqueue-thread-local-callback
   (lambda ()
     (if (equal? *forest-side* 'right)
         (set-editor-clip-right! 0)
         (set-editor-clip-left! 0)))))

(define (forest-wider!)
  (set! *forest-width* (min *forest-max-width* (+ *forest-width* 2)))
  (helix.redraw '()))

(define (forest-narrower!)
  (set! *forest-width* (max *forest-min-width* (- *forest-width* 2)))
  (helix.redraw '()))

(define *forest-modal-open?* #f)
(define *forest-modal-mode* 'input)
(define *forest-modal-label* "")
(define *forest-modal-buffer* "")
(define *forest-modal-callback* #f)

(struct ForestModalState ())

(define (forest-modal-width rect)
  (define content-len (+ (string-length *forest-modal-label*) (string-length *forest-modal-buffer*)))
  (min (- (area-width rect) 4) (max 40 (+ content-len 4))))

(define (forest-modal-origin rect)
  (define w (forest-modal-width rect))
  (define x (quotient (- (area-width rect) w) 2))
  (define y (quotient (- (area-height rect) 3) 2))
  (list x y w))

(define (forest-modal-render state rect frame)
  (define origin (forest-modal-origin rect))
  (define x (list-ref origin 0))
  (define y (list-ref origin 1))
  (define w (list-ref origin 2))
  (define bg-style (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define modal-area (area x y w 3))
  (buffer/clear-with frame modal-area bg-style)
  (block/render frame modal-area (make-block bg-style bg-style "all" "rounded"))
  (define text (string-append *forest-modal-label* *forest-modal-buffer*))
  (frame-set-string! frame (+ x 1) (+ y 1) (forest-truncate text (- w 2)) text-style))

(define (forest-modal-cursor-fn state rect)
  (if (equal? *forest-modal-mode* 'confirm)
      #f ; single keypress, no caret needed
      (let* ([origin (forest-modal-origin rect)]
             [x (list-ref origin 0)]
             [y (list-ref origin 1)])
        (position (+ y 1) (+ x 1 (string-length *forest-modal-label*) (string-length *forest-modal-buffer*))))))

(define (forest-modal-handle-event state event)
  (define ch (key-event-char event))
  (cond
    ;; the confirm branch below reads anything that isn't y as no, and a nudge of
    ;; the mouse shouldn't cancel the prompt it is sitting in front of
    [(mouse-event? event) event-result/consume]
    [(equal? *forest-modal-mode* 'confirm)
     (define cb *forest-modal-callback*)
     (set! *forest-modal-callback* #f)
     (set! *forest-modal-open?* #f)
     (when cb (enqueue-thread-local-callback (lambda () (cb (and (char? ch) (equal? ch #\y))))))
     event-result/close]
    [(key-event-enter? event)
     (define result *forest-modal-buffer*)
     (define cb *forest-modal-callback*)
     (set! *forest-modal-callback* #f)
     (set! *forest-modal-open?* #f)
     (when cb (enqueue-thread-local-callback (lambda () (cb result))))
     event-result/close]
    [(key-event-escape? event)
     (set! *forest-modal-callback* #f)
     (set! *forest-modal-open?* #f)
     event-result/close]
    [(key-event-backspace? event)
     (define len (string-length *forest-modal-buffer*))
     (when (> len 0)
       (set! *forest-modal-buffer* (substring *forest-modal-buffer* 0 (- len 1))))
     event-result/consume]
    [(char? ch)
     (set! *forest-modal-buffer* (string-append *forest-modal-buffer* (string ch)))
     event-result/consume]
    [else event-result/consume]))

(define (forest-show-modal! mode label initial-value callback)
  (set! *forest-modal-open?* #t)
  (set! *forest-modal-mode* mode)
  (set! *forest-modal-label* label)
  (set! *forest-modal-buffer* initial-value)
  (set! *forest-modal-callback* callback)
  (push-component!
   (new-component! "forest-modal"
                   (ForestModalState)
                   forest-modal-render
                   (hash "handle_event" forest-modal-handle-event
                         "cursor" forest-modal-cursor-fn))))

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

(define (forest-run-touch! path)
  (let ([proc (~> (command "touch" (list path))
                  with-stdout-piped
                  with-stderr-piped
                  spawn-process)])
    (if (Ok? proc)
        (let ([stderr (read-port-to-string (child-stderr (Ok->value proc)))])
          (when (not (string=? (trim stderr) ""))
            (error (trim stderr))))
        (error "touch: could not spawn process"))))

(define (forest-prompt-create!)
  (define entry (forest-current-entry))
  (when entry
    (define path (car entry))
    (define base (if (is-dir? path)
                      (string-append path (path-separator))
                      (trim-end-matches path (file-name path))))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-modal!
        'input
        (string-append "New (end with " (path-separator) " for dir): ")
        (forest-relpath base)
        (lambda (name)
          (define full (string-append (helix-find-workspace) (path-separator) name))
          (with-handler
            (lambda (err) (forest-error (string-append "create failed: " (error-object-message err))))
            (begin
              (if (ends-with? name (path-separator))
                  (forest-run-mkdir-p! full)
                  (begin
                    (forest-run-mkdir-p! (forest-parent-path full))
                    (forest-run-touch! full)
                    (helix.open full)))
              (forest-info (string-append "created " name))))
          (enqueue-thread-local-callback forest-refresh-all!)))))))

(define (forest-prompt-rename!)
  (define entry (forest-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define dir (trim-end-matches path (string-append (path-separator) name)))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-modal!
        'input
        "Rename: "
        name
        (lambda (new-name)
          (when (and (not (equal? new-name "")) (not (equal? new-name name)))
            (with-handler
              (lambda (err) (forest-error (string-append "rename failed: " (error-object-message err))))
              (begin
                (forest-run-mv! path (string-append dir (path-separator) new-name))
                (forest-info (string-append "renamed " name " -> " new-name))))
            (enqueue-thread-local-callback forest-refresh-all!))))))))

(define (forest-prompt-delete!)
  (define entry (forest-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define kind (if (is-dir? path) "directory" "file"))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-modal!
        'confirm
        (string-append "Delete " kind " '" name "'? (y/N) ")
        ""
        (lambda (confirmed?)
          (when confirmed?
            (with-handler
              (lambda (err) (forest-error (string-append "delete failed: " (error-object-message err))))
              (begin
                (if (is-dir? path)
                    (delete-directory! path) ; only works if empty
                    (delete-file! path))
                (forest-info (string-append "deleted " name))))
            (enqueue-thread-local-callback forest-refresh-all!))))))))

(struct ForestBgState ())

;; panel's left edge is 0 when left else put against the right edge
(define (forest-panel-x0 rect w)
  (if (equal? *forest-side* 'right) (- (area-width rect) w) 0))

(define (forest-snacks-y0) 1)

(define *forest-query-prefix* "> ")

(define (forest-match-positions name query)
  (let loop ([ns (string->list (string-downcase name))]
             [qs (string->list (string-downcase query))]
             [i 0]
             [acc '()])
    (cond
      [(or (null? qs) (null? ns)) (reverse acc)]
      [(char=? (car ns) (car qs)) (loop (cdr ns) (cdr qs) (+ i 1) (cons i acc))]
      [else (loop (cdr ns) qs (+ i 1) acc)])))

(define (forest-match-style base)
  (define c (style->fg (theme-scope-ref "special")))
  (style-with-bold (if c (style-fg base c) base)))

(define (forest-render-name-hl frame x y name avail base-style match-style positions)
  (define truncated (forest-truncate name avail))
  (define tlen (string-length truncated))
  (define pset (let loop ([ps positions] [acc (hashset)])
                 (if (null? ps) acc (loop (cdr ps) (hashset-insert acc (car ps))))))
  (let loop ([i 0])
    (when (< i tlen)
      (define on? (hashset-contains? pset i))
      (define j (let scan ([k (+ i 1)])
                  (if (and (< k tlen) (equal? (hashset-contains? pset k) on?)) (scan (+ k 1)) k)))
      (frame-set-string! frame (+ x i) y (substring truncated i j) (if on? match-style base-style))
      (loop j))))

;; inserts one matched relative path into the merged ancestor tree
(define (forest-search-tree-insert node segs)
  (define seg (car segs))
  (define rest (cdr segs))
  (if (null? rest)
      (hash-insert node seg #t)
      (let* ([existing (hash-try-get node seg)]
             [child (if (hash? existing) existing (hash))])
        (hash-insert node seg (forest-search-tree-insert child rest)))))

(define (forest-search-build-tree matches)
  (let loop ([ms matches] [root (hash)])
    (if (null? ms)
        root
        (loop (cdr ms) (forest-search-tree-insert root (split-many (car ms) (path-separator)))))))

;; depth 0 sits flush like the normal browsing view, nested levels are just indented
(define (forest-search-flatten node path depth)
  (define keys (hash-keys->list node))
  (define dirs (sort (filter (lambda (k) (hash? (hash-try-get node k))) keys) string<?))
  (define files (sort (filter (lambda (k) (not (hash? (hash-try-get node k)))) keys) string<?))
  (define ordered (append dirs files))
  (let loop ([items ordered])
    (if (null? items)
        '()
        (let* ([name (car items)]
               [val (hash-try-get node name)]
               [dir? (hash? val)]
               [own (forest-repeat-str "  " depth)]
               [rel (if (equal? path "") name (string-append path (path-separator) name))]
               [entry (list own dir? name rel)])
          (append (list entry)
                  (if dir? (forest-search-flatten val rel (+ depth 1)) '())
                  (loop (cdr items)))))))

;; mouse kinds are bare integers; helix/components.scm documents the full table
(define *forest-mouse-left-down* 0)
(define *forest-mouse-left-up* 3)
(define *forest-mouse-scroll-down* 10)
(define *forest-mouse-scroll-up* 11)

(define *forest-scroll-amount* 1) ; entries per wheel notch

(define (forest-mouse-kind? event kind)
  (and (mouse-event? event) (equal? (event-mouse-kind event) kind)))

(define (forest-mouse-left-down? event)
  (forest-mouse-kind? event *forest-mouse-left-down*))

(define (forest-mouse-left-up? event)
  (forest-mouse-kind? event *forest-mouse-left-up*))

;; acting on the press would let the drag and release through to the buffer, so
;; clicks act on the release and have to land where the press did
(define *forest-press* #f)

(define (forest-press! target)
  (set! *forest-press* target))

(define (forest-take-press!)
  (define pressed *forest-press*)
  (set! *forest-press* #f)
  pressed)

;; 'up, 'down, or #f when this isn't a wheel event
(define (forest-mouse-scroll-direction event)
  (cond
    [(forest-mouse-kind? event *forest-mouse-scroll-up*) 'up]
    [(forest-mouse-kind? event *forest-mouse-scroll-down*) 'down]
    [else #f]))

;; components.scm has mouse-event-within-area?, but it excludes the top row of an
;; area, which would make the first entry of every panel unclickable
(define (forest-mouse-in-area? event a)
  (and a
       (mouse-event? event)
       (let ([row (event-mouse-row event)]
             [col (event-mouse-col event)])
         (and (>= col (area-x a)) (< col (+ (area-x a) (area-width a)))
              (>= row (area-y a)) (< row (+ (area-y a) (area-height a)))))))

;; recorded while rendering: the geometry depends on state the handler can't see
;; (reserved bars, the flattened search tree, column fitting)
(define *forest-hit-panel* #f)   ; area of the whole sidebar
(define *forest-hit-search* #f)  ; area of the search box above the entries
(define *forest-hit-list* #f)    ; area of just its entry rows
;; one slot per visible row: an index into *forest-tree* while browsing, a
;; workspace-relative path while searching, or #f for an unselectable row
(define *forest-hit-rows* '())
(define *forest-hit-window* 0) ; first result row on screen, while searching

(define (forest-hit-row-at event)
  (and (forest-mouse-in-area? event *forest-hit-list*)
       (let ([row (- (event-mouse-row event) (area-y *forest-hit-list*))])
         (and (>= row 0)
              (< row (length *forest-hit-rows*))
              (list-ref *forest-hit-rows* row)))))

;; resolves a recorded slot to a cursor position, or #f if it can't be selected
(define (forest-hit-cursor-for slot)
  (cond
    [(int? slot) (and (< slot (length *forest-tree*)) slot)]
    [(string? slot)
     (let loop ([rs *forest-search-results*] [i 0])
       (cond
         [(null? rs) #f]
         [(equal? (car rs) slot) i]
         [else (loop (cdr rs) (+ i 1))]))]
    [else #f]))

;; what a press or release landed on, so the two can be compared
(define (forest-click-target event)
  (define slot (forest-hit-row-at event))
  (cond
    [slot (list 'row slot)]
    [(forest-mouse-in-area? event *forest-hit-search*) 'search]
    ;; the row padding and search headings are panel too, they just aren't rows
    [(forest-mouse-in-area? event *forest-hit-panel*) 'panel]
    [else 'buffer]))

;; the row the last click landed on, so a second one activates it without a timer
;; the cursor won't do: it can already be sitting on the row the first click hits
(define *forest-click-slot* #f)

;; results and mini columns centre on the selection as they render, sliding the
;; clicked row out from under the pointer, so the window is held while it stays armed
(define *forest-click-window* #f)      ; snacks search: first row of the window
(define *forest-mini-click-window* #f) ; mini: (column window-start)

(define (forest-forget-click!)
  (set! *forest-click-slot* #f)
  (set! *forest-click-window* #f)
  (set! *forest-mini-click-window* #f))

(define (forest-arm-click! slot)
  (set! *forest-click-slot* slot))

;; a keypress ends any gesture and stales whatever was armed
(define (forest-reset-mouse!)
  (forest-forget-click!)
  (set! *forest-press* #f))

;; a click in the tree leaves the query being typed, selectable row or not, but one
;; in the search box is aiming at the query
(define (forest-leave-typing-on-click! event)
  (when (forest-mouse-in-area? event *forest-hit-list*)
    (set! *forest-typing?* #f)))

;; no-op if the slot fell out of the tree since it was recorded
(define (forest-select-clicked! slot)
  (define cursor (forest-hit-cursor-for slot))
  (when cursor
    (set! *forest-cursor* cursor)
    (forest-arm-click! slot)
    ;; browse mode scrolls independently of the cursor and needs no pinning
    (when (forest-searching?) (set! *forest-click-window* *forest-hit-window*))))

(define (forest-mouse-select! slot)
  (cond
    [(equal? slot *forest-click-slot*)
     ;; toggling reshuffles the tree, so the armed row stops meaning the same entry
     (forest-forget-click!)
     (forest-activate!)]
    [else
     (forest-select-clicked! slot)
     event-result/consume]))

;; cursor-up!/down! handle the window, so a notch is just repeated steps
;; a short list doesn't shift under the pointer, so the arm goes with it
(define (forest-scroll-by! direction amount)
  (forest-forget-click!)
  (let loop ([n amount])
    (when (> n 0)
      (if (equal? direction 'up) (forest-cursor-up!) (forest-cursor-down!))
      (loop (- n 1)))))

(define *forest-scroll-pending* #f)

(define (forest-debounce-scroll! action-thunk)
  (unless *forest-scroll-pending*
    (set! *forest-scroll-pending* #t) ;; close event gate
    (action-thunk) ;; apply scroll
    (enqueue-thread-local-callback
     (lambda () (set! *forest-scroll-pending* #f))))) ;; reopen gate

(define (forest-render-bg state rect frame)
  (define w (min *forest-width* (area-width rect)))
  (define h (area-height rect))
  (define x0 (forest-panel-x0 rect w))
  ;; one blank row above the search box
  (define y0 (forest-snacks-y0))
  (define panel-h (max 1 (- h y0)))
  (set! *forest-visible-height* (max 1 (- panel-h *forest-search-height*)))
  (if (equal? *forest-side* 'right)
      (set-editor-clip-right! w)
      (set-editor-clip-left! w))
  (forest-publish-clip! *forest-side* w)

  ;; theme components a configured sidebar background tints only these panel
  ;; styles, so the buffer keeps the theme background
  (define panel-bg (forest-panel-bg-color))
  (define (forest-with-panel-bg s) (if panel-bg (style-bg s panel-bg) s))
  (define bg-style (forest-with-panel-bg (theme-scope-ref "ui.background")))
  (define text-style (forest-with-panel-bg (theme-scope-ref "ui.text")))
  ;; the selection and the title dim while the editor holds focus
  (define hl-base (theme-scope-ref "ui.menu.selected"))
  (define hl-style (if *forest-focused* hl-base (style-with-dim hl-base)))
  (define dir-style (forest-with-panel-bg (theme-scope-ref "ui.text.info")))
  (define dim-style (forest-with-panel-bg (style-with-dim (theme-scope-ref "ui.text"))))
  (define title-style (if *forest-focused* (style-with-bold dir-style) dim-style))

  ;; no border for cleaner look
  (define panel-area (area x0 y0 w panel-h))
  (buffer/clear-with frame panel-area bg-style)

  ;; border matches bg so it blends in instead of clashing across themes;
  (define border-style bg-style)

  ;; on the divider side reserve the last column for the line
  ;; entries stay flush left and fill up to one gap column before it
  (define left-divider? (and *forest-show-separator?* (not (equal? *forest-side* 'right))))
  (define list-w (if left-divider? (- w 2) w))
  ;; the search box is centred between the left edge and the divider
  (define box-x (if left-divider? (+ x0 1) x0))
  (define box-w (if left-divider? (- w 3) w))

  (define search-area (area box-x y0 box-w *forest-search-height*))
  ;; outline color marks focus
  (define search-line (forest-search-color))
  (define search-border-style
    (if search-line
        (style-fg bg-style search-line)
        (forest-with-panel-bg (theme-scope-ref "ui.text"))))
  (block/render frame search-area (make-block bg-style search-border-style "all" "rounded"))

  (define title "Explorer")
  (when (> box-w (+ (string-length title) 4))
    (frame-set-string! frame (+ box-x (quotient (- box-w (string-length title)) 2)) y0
                        title title-style))

  (define prompt (string-append *forest-query-prefix* *forest-query*))
  (define prompt-shown (forest-truncate prompt (- box-w 2)))
  (frame-set-string! frame (+ box-x 1) (+ y0 1) prompt-shown text-style)

  (when (forest-searching?)
    (define counter (string-append (number->string (length *forest-search-results*))
                                    "/" (number->string (length *forest-all-files*))))
    (define counter-x (- (+ box-x box-w) 1 (string-length counter)))
    (when (>= counter-x (+ box-x 2 (string-length prompt-shown)))
      (frame-set-string! frame counter-x (+ y0 1) counter dim-style)))

  (when *forest-show-separator?*
    (define sep-x (if (equal? *forest-side* 'right) (- x0 1) (- (+ x0 w) 1)))
    (define sep-top y0)
    (define sep-bottom (- (+ y0 panel-h) 1))
    (when (and (>= sep-x 0) (< sep-x (area-width rect)))
      (let loop ([y sep-top])
        (when (<= y sep-bottom)
          (frame-set-string! frame sep-x y "│" border-style)
          (loop (+ y 1))))))

  (define list-y0 (+ y0 *forest-search-height*))
  (define max-text-w (- list-w 1))

  (set! *forest-hit-panel* panel-area)
  (set! *forest-hit-search* search-area)
  (set! *forest-hit-list* (area x0 list-y0 w *forest-visible-height*))
  (set! *forest-hit-rows* '())

  (if (forest-searching?)
      (if (null? *forest-search-results*)
          (frame-set-string! frame (+ x0 1) list-y0 "(no matches)" dim-style)
          (let* ([tree (forest-search-build-tree *forest-search-results*)]
                 [rows (forest-search-flatten tree "" 0)]
                 [selected-rel (list-ref *forest-search-results* *forest-cursor*)]
                 [selected-row (let loop ([rs rows] [i 0])
                                 (cond [(null? rs) 0]
                                       [(and (not (list-ref (car rs) 1)) (equal? (list-ref (car rs) 3) selected-rel)) i]
                                       [else (loop (cdr rs) (+ i 1))]))]
                 [total-rows (length rows)]
                 [window-start (max 0 (min (max 0 (- total-rows *forest-visible-height*))
                                            (or *forest-click-window*
                                                (max 0 (- selected-row (forest-half-floor *forest-visible-height*))))))]
                 [visible (forest-take (forest-drop rows window-start) *forest-visible-height*)])
            ;; headings aren't selectable, so they record #f and a click does nothing
            (set! *forest-hit-window* window-start)
            (set! *forest-hit-rows*
                  (map (lambda (e) (if (list-ref e 1) #f (list-ref e 3))) visible))
            (let loop ([items visible] [row 0])
              (unless (or (null? items) (>= row *forest-visible-height*))
                (define entry (car items))
                (define own-prefix (list-ref entry 0))
                (define dir? (list-ref entry 1))
                (define name (list-ref entry 2))
                (define rel (list-ref entry 3))
                (define icon (if dir? (glyph-dir-icon name) (glyph-icon name)))
                (define icon-color (if dir? (glyph-dir-color name) (glyph-color name)))
                (define git-status (and (not dir?) (forest-git-status rel)))
                (define git-icon (if git-status (glyph-git-icon git-status) " "))
                (define git-color (if git-status (glyph-git-color git-status) #f))
                (define y (+ list-y0 row))
                (define hl? (and (not dir?) (equal? rel selected-rel)))
                (define row-style (cond [hl? hl-style] [dir? dir-style] [else text-style]))
                (define prefix-w (string-length own-prefix))
                (define icon-w (string-length icon))
                (define git-x (+ x0 prefix-w icon-w 1))
                (define git-w (if dir? 0 1))
                (define gap (if dir? 0 1))
                (define name-x (+ git-x git-w gap))
                (define avail (max 0 (- max-text-w prefix-w icon-w 1 git-w gap)))
                (define positions (and (not dir?) (forest-match-positions name *forest-query*)))
                (when hl?
                  (frame-set-string! frame x0 y (make-string list-w #\space) hl-style))
                (frame-set-string! frame x0 y own-prefix row-style)
                (frame-set-string! frame (+ x0 prefix-w) y icon (glyph-style icon-color #:base row-style))
                (unless dir?
                  (frame-set-string! frame git-x y git-icon
                                      (if git-color (glyph-style git-color #:base row-style) row-style)))
                (if (and positions (pair? positions))
                    (forest-render-name-hl frame name-x y name avail row-style (forest-match-style row-style) positions)
                    (frame-set-string! frame name-x y (forest-truncate name avail) row-style))
                (loop (cdr items) (+ row 1))))))
      (let ([visible (forest-take (forest-drop *forest-tree* *forest-window-start*)
                                   *forest-visible-height*)])
        (set! *forest-hit-rows*
              (let loop ([items visible] [i *forest-window-start*])
                (if (null? items) '() (cons i (loop (cdr items) (+ i 1))))))
        (let loop ([items visible] [row 0])
          (unless (or (null? items) (>= row *forest-visible-height*))
            (define entry (car items))
            (define abs-idx (+ *forest-window-start* row))
            (define path (list-ref entry 0))
            (define indent (list-ref entry 1))
            (define marker (list-ref entry 2))
            (define name (list-ref entry 3))
            (define prefix (string-append indent marker))
            (define dir? (is-dir? path))
            (define icon (if dir? (glyph-dir-icon name) (glyph-icon name)))
            (define icon-color (if dir? (glyph-dir-color name) (glyph-color name)))
            (define git-status (and (not dir?) (forest-git-status path)))
            (define git-icon (if git-status (glyph-git-icon git-status) " "))
            (define git-color (if git-status (glyph-git-color git-status) #f))
            (define y (+ list-y0 row))
            (define hl? (= abs-idx *forest-cursor*))
            (define row-style (cond [hl? hl-style] [dir? dir-style] [else text-style]))
            (define prefix-w (string-length prefix))
            (define icon-w (string-length icon))
            (define git-x (+ x0 prefix-w icon-w 1))
            (define git-w (if dir? 0 1))
            (define gap (if dir? 0 1))
            (define name-x (+ git-x git-w gap))
            (define avail (max 0 (- max-text-w prefix-w icon-w 1 git-w gap)))
            (when hl?
              (frame-set-string! frame x0 y (make-string list-w #\space) hl-style))
            (frame-set-string! frame x0 y prefix row-style)
            (frame-set-string! frame (+ x0 prefix-w) y icon (glyph-style icon-color #:base row-style))
            (unless dir?
              (frame-set-string! frame git-x y git-icon
                                  (if git-color (glyph-style git-color #:base row-style) row-style)))
            (frame-set-string! frame name-x y (forest-truncate name avail) row-style)
            (loop (cdr items) (+ row 1)))))))

;; forest-snacks-open! would reveal the current file and move off the clicked row
;; deferred since the compositor can't be restacked mid-dispatch
(define (forest-refocus-from-click!)
  (unless *forest-focused*
    (set! *forest-focused* #t)
    (enqueue-thread-local-callback
     ;; a second fg would never be popped
     (lambda () (when *forest-focused* (push-component! (forest-make-fg-component)))))))

(define (forest-handle-event-bg state event)
  ;; unfocused, so anything the mouse doesn't claim falls through to the editor
  (define dir (forest-mouse-scroll-direction event))
  (cond
    ;; a key ends any gesture, so a lost release can't leave the panel eating clicks
    [(not (mouse-event? event))
     (forest-reset-mouse!)
     event-result/ignore]
    ;; tested before the panel rect, since the gesture may have wandered off it
    [*forest-press*
     (when (forest-mouse-left-up? event)
       ;; select rather than activate, but arm the row so the next click opens it
       (define target (forest-click-target event))
       (when (equal? target (forest-take-press!))
         (forest-leave-typing-on-click! event)
         (when (pair? target) (forest-select-clicked! (cadr target)))
         (when (equal? target 'search) (set! *forest-typing?* #t))
         (forest-refocus-from-click!)))
     event-result/consume]
    [(not (forest-mouse-in-area? event *forest-hit-panel*)) event-result/ignore]
    [(forest-mouse-left-down? event)
     (forest-press! (forest-click-target event))
     event-result/consume]
    [dir
     ;; scrolling over the panel reads as inspecting it, not entering it
     (forest-debounce-scroll! (lambda () (forest-scroll-by! dir *forest-scroll-amount*)))
     (helix.redraw '()) ; unfocused, so consuming alone won't re-render
     event-result/consume]
    [else event-result/ignore]))

(struct ForestFgState ())

(define (forest-render-fg state rect frame) void) ; bg handles all drawing

;; cursor only needs to appear while actively typing a search query
(define (forest-cursor-fn-fg state area)
  (if *forest-typing?*
      (let* ([w (min *forest-width* (area-width area))]
             [x0 (forest-panel-x0 area w)]
             [box-x (if (and *forest-show-separator?* (not (equal? *forest-side* 'right)))
                        (+ x0 1) x0)])
        (position (+ (forest-snacks-y0) 1)
                  (+ box-x 1 (string-length *forest-query-prefix*) (string-length *forest-query*))))
      #f))

(define (forest-handle-event-typing state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-enter? event)
     ;; confirms the query without opening anything, so the matches can
     ;; still be browsed with j/k before committing to one with a second enter
     (set! *forest-typing?* #f)
     event-result/consume]
    [(key-event-escape? event)
     ;; leaves the filtered results in place
     ;; stops updating the query
     (set! *forest-typing?* #f)
     event-result/consume]
    [(key-event-backspace? event)
     (forest-backspace!)
     event-result/consume]
    [(char? ch)
     (forest-type! ch)
     event-result/consume]
    [else event-result/consume]))

(define (forest-command-action! action)
  (cond
    [(equal? action 'down) (forest-cursor-down!) event-result/consume]
    [(equal? action 'up) (forest-cursor-up!) event-result/consume]
    [(equal? action 'enter) (forest-enter-or-open!)]
    [(equal? action 'back) (forest-goto-parent!) event-result/consume]
    [(equal? action 'search) (forest-enter-search!) event-result/consume]
    [(equal? action 'create) (forest-prompt-create!) event-result/consume]
    [(equal? action 'rename) (forest-prompt-rename!) event-result/consume]
    [(equal? action 'delete) (forest-prompt-delete!) event-result/consume]
    [(equal? action 'refresh) (forest-refresh-all!) event-result/consume]
    [(equal? action 'toggle-hidden) (forest-toggle-hidden!) event-result/consume]
    [(equal? action 'toggle-git-ignored) (forest-toggle-git-ignored!) event-result/consume]
    [(equal? action 'wider) (forest-wider!) event-result/consume]
    [(equal? action 'narrower) (forest-narrower!) event-result/consume]
    [(equal? action 'menu) (forest-whichkey-open! 'snacks forest-command-action!) event-result/consume]
    [(equal? action 'quit) (forest-close!) event-result/close]
    [else event-result/consume]))

(define (forest-handle-event-command state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-down? event) (forest-cursor-down!) event-result/consume]
    [(key-event-up? event) (forest-cursor-up!) event-result/consume]
    [(key-event-right? event) (forest-enter-or-open!)]
    [(key-event-left? event) (forest-goto-parent!) event-result/consume]
    [(key-event-enter? event) (forest-activate!)]
    [(key-event-tab? event)
     (define entry (forest-current-entry))
     (when (and entry (is-dir? (car entry))) (forest-toggle-dir! (car entry)))
     event-result/consume]

    [(key-event-escape? event)
     (forest-switch-to-editor!)
     event-result/close] ; pops fg only; bg stays visible

    [(key-event-backspace? event)
     (forest-clear-search!)
     event-result/consume]

    [(and (char? ch) (equal? ch #\=)) (forest-wider!) event-result/consume] ; old alias not remappable

    [(char? ch)
     (define action (forest-action-for-char ch))
     (if action (forest-command-action! action) event-result/consume)]

    [else event-result/consume])) ; block unknown keys from editor while focused

(define (forest-handle-mouse-fg state event)
  (define dir (forest-mouse-scroll-direction event))
  (cond
    [(forest-mouse-left-down? event)
     (forest-press! (forest-click-target event))
     event-result/consume]
    [(forest-mouse-left-up? event)
     (define target (forest-click-target event))
     (cond
       [(not (equal? target (forest-take-press!))) event-result/consume]
       [(pair? target)
        (forest-leave-typing-on-click! event)
        (forest-mouse-select! (cadr target))]
       ;; picks up wherever the query left off rather than starting a new one
       [(equal? target 'search)
        (set! *forest-typing?* #t)
        event-result/consume]
       [(equal? target 'panel)
        (forest-leave-typing-on-click! event)
        event-result/consume]
       ;; helix has already spent this event, so it takes another to place the caret
       [else
        (forest-switch-to-editor!)
        event-result/close])]
    [dir
     (if (forest-mouse-in-area? event *forest-hit-panel*)
         (begin
           (forest-debounce-scroll! (lambda () (forest-scroll-by! dir *forest-scroll-amount*)))
           event-result/consume)
         ;; let the buffer scroll under the pointer without losing the panel
         event-result/ignore)]
    ;; drags and bare movement would otherwise fall through to the editor
    [else event-result/consume]))

(define (forest-handle-event-fg state event)
  (cond
    [*forest-modal-open?* event-result/ignore]
    [*forest-help-open?* event-result/ignore]
    ;; ahead of the typing branch so the tree stays clickable mid-query
    [(mouse-event? event) (forest-handle-mouse-fg state event)]
    [else
     ;; any keypress can move the cursor, so an armed row stops meaning anything
     (forest-reset-mouse!)
     (if *forest-typing?*
         (forest-handle-event-typing state event)
         (forest-handle-event-command state event))]))

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

(define (forest-snacks-open!)
  (cond
    [(not *forest-active*)
     (set! *forest-active* #t)
     (set! *forest-focused* #t)
     (set! *forest-cursor* 0)
     (set! *forest-window-start* 0)
     (set! *forest-query* "")
     (set! *forest-search-results* '())
     (set! *forest-typing?* #f)
     (forest-scan-git-ignored! (helix-find-workspace))
     (forest-reveal-current-file!)
     (forest-scan-files!)
     (push-component! (forest-make-bg-component))
     (push-component! (forest-make-fg-component))]

    [*forest-focused*
     (forest-switch-to-editor!)]

    [else
     (set! *forest-focused* #t)
     (forest-scan-git-ignored! (helix-find-workspace))
     (forest-reveal-current-file!)
     (push-component! (forest-make-fg-component))]))

(define *forest-mini-min-w* 14)
(define *forest-mini-max-w* 40)
(define *forest-mini-min-h* 3)
(define *forest-mini-max-h* 24)
(define *forest-mini-gap* 0)
(define *forest-mini-margin* 1)

(define *forest-mini-stack* '()) ; list of columns, oldest first and active last

(struct ForestMiniColumn (path entries cursor))

(define (forest-mini-list-dir path)
  (define children
    (filter (lambda (p)
              (define name (file-name p))
              (not (or (hashset-contains? *forest-ignore-set* name)
                       (and (not *forest-show-hidden*) (forest-dotfile? name))
                       (and (not *forest-show-git-ignored*) (forest-git-ignored? p)))))
            (with-handler (lambda (_) '()) (read-dir path))))
  (map (lambda (p) (cons p (file-name p))) (forest-sort-entries children)))

(define (forest-mini-cursor col) (unbox (ForestMiniColumn-cursor col)))
(define (forest-mini-set-cursor! col v) (set-box! (ForestMiniColumn-cursor col) v))

(define (forest-mini-last lst)
  (if (null? (cdr lst)) (car lst) (forest-mini-last (cdr lst))))

(define (forest-mini-drop-last lst)
  (if (null? (cdr lst)) '() (cons (car lst) (forest-mini-drop-last (cdr lst)))))

(define (forest-mini-active-column) (forest-mini-last *forest-mini-stack*))

(define (forest-mini-current-entry)
  (define col (forest-mini-active-column))
  (define entries (ForestMiniColumn-entries col))
  (and (not (null? entries)) (list-ref entries (forest-mini-cursor col))))

(define (forest-mini-move! delta)
  (define col (forest-mini-active-column))
  (define n (length (ForestMiniColumn-entries col)))
  (when (> n 0)
    (forest-mini-set-cursor! col (max 0 (min (- n 1) (+ (forest-mini-cursor col) delta))))))

(define (forest-mini-close!)
  (forest-reset-mouse!)
  (forest-help-dismiss!)
  (set! *forest-active* #f)
  (pop-last-component-by-name! "forest-mini"))

;; enters a directory cascades a new column to the side or opens a file
(define (forest-mini-enter!)
  (define entry (forest-mini-current-entry))
  (cond
    [(not entry) event-result/consume]
    [(is-dir? (car entry))
     (set! *forest-mini-stack*
           (append *forest-mini-stack*
                   (list (ForestMiniColumn (car entry) (forest-mini-list-dir (car entry)) (box 0)))))
     event-result/consume]
    [(is-file? (car entry))
     (define path (car entry))
     (forest-mini-close!)
     (enqueue-thread-local-callback (lambda () (helix.open path)))
     event-result/close]
    [else event-result/consume]))

;; steps back to the parent column
(define (forest-mini-back!)
  (when (> (length *forest-mini-stack*) 1)
    (set! *forest-mini-stack* (forest-mini-drop-last *forest-mini-stack*))))

;; rebuilds the active column in place after a create/rename/delete
;; keeping the cursor in bounds
(define (forest-mini-refresh-active!)
  (define col (forest-mini-active-column))
  (define new-entries (forest-mini-list-dir (ForestMiniColumn-path col)))
  (define new-cursor (max 0 (min (forest-mini-cursor col) (- (length new-entries) 1))))
  (set! *forest-mini-stack*
        (append (forest-mini-drop-last *forest-mini-stack*)
                (list (ForestMiniColumn (ForestMiniColumn-path col) new-entries (box new-cursor))))))

;; refesh after toggle
(define (forest-mini-refresh-all!)
  (set! *forest-mini-stack*
        (map (lambda (col)
               (define new-entries (forest-mini-list-dir (ForestMiniColumn-path col)))
               (define new-cursor (max 0 (min (forest-mini-cursor col) (- (length new-entries) 1))))
               (ForestMiniColumn (ForestMiniColumn-path col) new-entries (box new-cursor)))
             *forest-mini-stack*)))

(set! *forest-refresh-mini-fn* forest-mini-refresh-all!)

(define (forest-mini-index-of lst target)
  (let loop ([l lst] [i 0])
    (cond
      [(null? l) #f]
      [(equal? (car l) target) i]
      [else (loop (cdr l) (+ i 1))])))

;; path segments below root
(define (forest-mini-relative-components root path)
  (define prefix (string-append root (path-separator)))
  (if (and (>= (string-length path) (string-length prefix))
           (equal? (substring path 0 (string-length prefix)) prefix))
      (split-many (substring path (string-length prefix) (string-length path)) (path-separator))
      '()))

;; cascades a column per ancestor from root down to path
(define (forest-mini-build-stack-for root path)
  (let loop ([dir root] [comps (forest-mini-relative-components root path)] [acc '()])
    (define entries (forest-mini-list-dir dir))
    (cond
      [(null? comps) (reverse (cons (ForestMiniColumn dir entries (box 0)) acc))]
      [else
       (define idx (forest-mini-index-of (map cdr entries) (car comps)))
       (define col (ForestMiniColumn dir entries (box (if idx idx 0))))
       (if (and idx (pair? (cdr comps)))
           (loop (car (list-ref entries idx)) (cdr comps) (cons col acc))
           (reverse (cons col acc)))])))

;; expands ancestors down to whatever file the editor has focused
(define (forest-mini-reveal-current-file!)
  (define root (helix-find-workspace))
  (define path (editor-document->path (editor->doc-id (editor-focus))))
  (if (string? path)
      (forest-mini-build-stack-for root path)
      (list (ForestMiniColumn root (forest-mini-list-dir root) (box 0)))))

;; flat recursive file list for search, independent of the cascaded columns
(define (forest-mini-scan-files root)
  (define prefix (string-append root (path-separator)))
  (define acc '())
  (define (walk dir)
    (for-each
     (lambda (p)
       (unless (hashset-contains? *forest-ignore-set* (file-name p))
         (if (is-dir? p) (walk p) (set! acc (cons p acc)))))
     (with-handler (lambda (_) '()) (read-dir dir))))
  (walk root)
  (sort (map (lambda (p) (substring p (string-length prefix) (string-length p))) acc) string<?))

;; panels grow and shrink with their own content with safe bound clamping
(define (forest-mini-longest-name entries)
  (let loop ([lst entries] [best 0])
    (if (null? lst) best (loop (cdr lst) (max best (string-length (cdr (car lst))))))))

(define *forest-mini-width-boost* 0)

(define (forest-mini-col-width entries)
  (min *forest-mini-max-w* (max *forest-mini-min-w* (+ (forest-mini-longest-name entries) 4 *forest-mini-width-boost*))))

(define (forest-mini-col-height count max-h)
  (min max-h (max *forest-mini-min-h* count)))

;; no explicit redraw, consuming the event already re-renders
(define (forest-mini-wider!)
  (set! *forest-mini-width-boost* (min 40 (+ *forest-mini-width-boost* 4))))

(define (forest-mini-narrower!)
  (set! *forest-mini-width-boost* (max (- *forest-mini-min-w*) (- *forest-mini-width-boost* 4))))

(define (forest-mini-clamp-window start count height)
  (max 0 (min (max 0 (- count height)) start)))

;; centers the cursor within a column's visible window
(define (forest-mini-window-start cursor count height)
  (forest-mini-clamp-window (- cursor (quotient height 2)) count height))

;; a clicked column keeps the window it was clicked against
(define (forest-mini-pinned-window col cursor count height)
  (if (and *forest-mini-click-window*
           (equal? col (car *forest-mini-click-window*)))
      (forest-mini-clamp-window (cadr *forest-mini-click-window*) count height)
      (forest-mini-window-start cursor count height)))

(define *forest-mini-preview-max-lines* 200)
(define *forest-mini-preview-min-w* 15)
(define *forest-mini-preview-max-w* 70)

(define (forest-mini-preview-lines path max-lines)
  (with-handler
    (lambda (_) (list "(unable to preview)"))
    (let* ([p (open-input-file path)]
           [content (read-port-to-string p)])
      (close-input-port p)
      (forest-take (split-many content "\n") max-lines))))

(define (forest-mini-longest-line lines cap)
  (let loop ([lst lines] [best 0])
    (if (null? lst) best (loop (cdr lst) (max best (min cap (string-length (car lst))))))))

(define (forest-mini-preview)
  (define entry (forest-mini-current-entry))
  (cond
    [(not entry) (list 'empty #f)]
    [(is-dir? (car entry)) (list 'dir (forest-mini-list-dir (car entry)))]
    [(is-file? (car entry)) (list 'file (forest-mini-preview-lines (car entry) *forest-mini-preview-max-lines*))]
    [else (list 'empty #f)]))

(define (forest-mini-prompt-create!)
  (define col (forest-mini-active-column))
  (define base (string-append (ForestMiniColumn-path col) (path-separator)))
  (enqueue-thread-local-callback
   (lambda ()
     (forest-show-modal!
      'input
      (string-append "New (end with " (path-separator) " for dir): ")
      (forest-relpath base)
      (lambda (name)
        (define full (string-append (helix-find-workspace) (path-separator) name))
        (with-handler
          (lambda (err) (forest-error (string-append "create failed: " (error-object-message err))))
          (begin
            (if (ends-with? name (path-separator))
                (forest-run-mkdir-p! full)
                (begin
                  (forest-run-mkdir-p! (forest-parent-path full))
                  (forest-run-touch! full)
                  (helix.open full)))
            (forest-info (string-append "created " name))))
        (enqueue-thread-local-callback forest-mini-refresh-active!))))))

(define (forest-mini-prompt-rename!)
  (define entry (forest-mini-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define dir (trim-end-matches path (string-append (path-separator) name)))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-modal!
        'input
        "Rename: "
        name
        (lambda (new-name)
          (when (and (not (equal? new-name "")) (not (equal? new-name name)))
            (with-handler
              (lambda (err) (forest-error (string-append "rename failed: " (error-object-message err))))
              (begin
                (forest-run-mv! path (string-append dir (path-separator) new-name))
                (forest-info (string-append "renamed " name " -> " new-name))))
            (enqueue-thread-local-callback forest-mini-refresh-active!))))))))

(define (forest-mini-prompt-delete!)
  (define entry (forest-mini-current-entry))
  (when entry
    (define path (car entry))
    (define name (file-name path))
    (define kind (if (is-dir? path) "directory" "file"))
    (enqueue-thread-local-callback
     (lambda ()
       (forest-show-modal!
        'confirm
        (string-append "Delete " kind " '" name "'? (y/N) ")
        ""
        (lambda (confirmed?)
          (when confirmed?
            (with-handler
              (lambda (err) (forest-error (string-append "delete failed: " (error-object-message err))))
              (begin
                (if (is-dir? path)
                    (delete-directory! path) ; only works if empty
                    (delete-file! path))
                (forest-info (string-append "deleted " name))))
            (enqueue-thread-local-callback forest-mini-refresh-active!))))))))

;; searches the whole workspace and re-cascades the stack to the match
(define (forest-mini-prompt-search!)
  (define root (helix-find-workspace))
  (enqueue-thread-local-callback
   (lambda ()
     (forest-show-modal!
      'input
      "Search: "
      ""
      (lambda (query)
        (unless (equal? query "")
          (define matches (fuzzy-match query (forest-mini-scan-files root)))
          (if (null? matches)
              (forest-error (string-append "no matches for '" query "'"))
              (set! *forest-mini-stack*
                    (forest-mini-build-stack-for root (string-append root (path-separator) (car matches)))))))))))

(struct ForestMiniState ())

(define (forest-mini-render-entries frame x y0 w h entries ws cursor active?
                               text-style hl-style dir-style dim-style)
  (if (null? entries)
      (frame-set-string! frame x y0 (forest-truncate "(empty)" w) dim-style)
      (let iloop ([items (forest-take (forest-drop entries ws) h)] [row 0])
        (unless (or (null? items) (>= row h))
          (define e (car items))
          (define idx (+ ws row))
          (define dir? (is-dir? (car e)))
          (define hl? (and active? (= idx cursor)))
          (define icon (if dir? (glyph-dir-icon (cdr e)) (glyph-icon (cdr e))))
          (define icon-color (if dir? (glyph-dir-color (cdr e)) (glyph-color (cdr e))))
          (define git-status (and (not dir?) (forest-git-status (car e))))
          (define git-icon (if git-status (glyph-git-icon git-status) " "))
          (define git-color (if git-status (glyph-git-color git-status) #f))
          (define row-style (cond [hl? hl-style] [dir? dir-style] [else text-style]))
          (define name (string-append (cdr e) (if dir? (path-separator) "")))
          (define icon-w (string-length icon))
          (define git-x (+ x icon-w 1))
          (define git-w (if dir? 0 1))
          (define gap (if dir? 0 1))
          (define name-x (+ git-x git-w gap))
          (define avail (max 0 (- w icon-w 1 git-w gap)))
          (define y (+ y0 row))
          (when hl? (frame-set-string! frame x y (make-string w #\space) hl-style))
          (frame-set-string! frame x y icon (glyph-style icon-color #:base row-style))
          (unless dir?
            (frame-set-string! frame git-x y git-icon
                                (if git-color (glyph-style git-color #:base row-style) row-style)))
          (frame-set-string! frame name-x y (forest-truncate name avail) row-style)
          (iloop (cdr items) (+ row 1))))))

;; file-preview panel in plain text
(define (forest-mini-render-lines frame x y0 w h lines style)
  (let iloop ([items (forest-take lines h)] [row 0])
    (unless (or (null? items) (>= row h))
      (frame-set-string! frame x (+ y0 row) (forest-truncate (car items) w) style)
      (iloop (cdr items) (+ row 1)))))

;; recorded while rendering: which ancestors survived fit isn't visible to the handler
(define *forest-mini-hit-panels* '()) ; one area per panel drawn, borders included
(define *forest-mini-hit-cols* '())   ; (area column window-start) per column
(define *forest-mini-hit-preview* #f) ; (area entry-count) when the preview lists a directory

;; one box over all of them would swallow clicks beside a short column
(define (forest-mini-inside-panel? event)
  (let loop ([ps *forest-mini-hit-panels*])
    (cond
      [(null? ps) #f]
      [(forest-mouse-in-area? event (car ps)) #t]
      [else (loop (cdr ps))])))

;; (column entry-index window-start), or #f when no column is under the pointer
(define (forest-mini-hit event)
  (let loop ([cs *forest-mini-hit-cols*])
    (cond
      [(null? cs) #f]
      [else
       (define rect (list-ref (car cs) 0))
       (define col (list-ref (car cs) 1))
       (define ws (list-ref (car cs) 2))
       (if (forest-mouse-in-area? event rect)
           (list col (+ ws (- (event-mouse-row event) (area-y rect))) ws)
           (loop (cdr cs)))])))

(define (forest-mini-in-preview? event)
  (and *forest-mini-hit-preview*
       (forest-mouse-in-area? event (car *forest-mini-hit-preview*))))

;; as above, but a column hit carries the entry, since a panel holds many
(define (forest-mini-click-target event hit)
  (cond
    [hit (list 'entry (list-ref hit 0) (list-ref hit 1))]
    [(forest-mini-in-preview? event) 'preview]
    [(forest-mini-inside-panel? event) 'panel]
    [else 'buffer]))

(define (forest-mini-click! hit)
  (define col (list-ref hit 0))
  (define entry-idx (list-ref hit 1))
  (define ws (list-ref hit 2))
  (define entries (ForestMiniColumn-entries col))
  (cond
    [(or (null? entries) (>= entry-idx (length entries))) event-result/consume]
    [else
     (define idx (forest-mini-index-of *forest-mini-stack* col))
     (define active? (equal? col (forest-mini-active-column)))
     (define slot (list idx entry-idx))
     (cond
       ;; second click on the same entry cascades into it or opens it
       [(and active? (equal? slot *forest-click-slot*))
        ;; cascading rebuilds the stack, so the armed slot stops meaning anything
        (forest-forget-click!)
        (forest-mini-enter!)]
       [else
        ;; clicking an ancestor drops the cascade off it, as h repeatedly would
        (when (and idx (not active?))
          (set! *forest-mini-stack* (forest-take *forest-mini-stack* (+ idx 1))))
        (forest-mini-set-cursor! col entry-idx)
        (forest-arm-click! slot)
        (set! *forest-mini-click-window* (list col ws))
        event-result/consume])]))

;; the preview looks just like the column cascading would open, so a click descends
;; and lands on the entry clicked
(define (forest-mini-preview-click! event)
  (define row (- (event-mouse-row event) (area-y (car *forest-mini-hit-preview*))))
  (define entry (forest-mini-current-entry))
  (cond
    ;; a short directory is padded to the minimum height; that padding is inert
    [(or (< row 0) (>= row (cadr *forest-mini-hit-preview*))) event-result/consume]
    [(not (and entry (is-dir? (car entry)))) event-result/consume]
    [else
     (define result (forest-mini-enter!))
     (define col (forest-mini-active-column))
     ;; the directory can have shrunk since it was drawn
     (when (< row (length (ForestMiniColumn-entries col)))
       (forest-mini-set-cursor! col row)
       (forest-arm-click! (list (forest-mini-index-of *forest-mini-stack* col) row))
       ;; the preview is drawn from the top, so the cascaded column has to be too
       (set! *forest-mini-click-window* (list col 0)))
     result]))

(define (forest-mini-handle-mouse state event)
  (define dir (forest-mouse-scroll-direction event))
  (define hit (forest-mini-hit event))
  (cond
    [(forest-mouse-left-down? event)
     (forest-press! (forest-mini-click-target event hit))
     event-result/consume]
    [(forest-mouse-left-up? event)
     (define target (forest-mini-click-target event hit))
     (cond
       [(not (equal? target (forest-take-press!))) event-result/consume]
       [hit (forest-mini-click! hit)]
       [(equal? target 'preview) (forest-mini-preview-click! event)]
       [(equal? target 'panel) event-result/consume]
       ;; the columns float over the buffer, so clicking off them matches escape
       [else
        (forest-mini-close!)
        event-result/close])]
    ;; a column's window follows its cursor, so scrolling an ancestor could only
    ;; move the screen by moving its selection, dropping the cascade off it
    [(and dir (or (not hit) (equal? (list-ref hit 0) (forest-mini-active-column))))
     (if (forest-mini-inside-panel? event)
         ;; a short column doesn't scroll under the pointer, so the arm goes with it
         (begin
           (forest-forget-click!)
           (forest-debounce-scroll!
            (lambda ()
              (forest-mini-move! (if (equal? dir 'up)
                                     (- *forest-scroll-amount*)
                                     *forest-scroll-amount*))))
           event-result/consume)
         event-result/ignore)]
    [else event-result/consume]))

(define (forest-mini-render state rect frame)
  (define sw (area-width rect))
  (define sh (area-height rect))
  (define max-h (min *forest-mini-max-h* (max *forest-mini-min-h* (- sh 4))))

  (define bg-style (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define hl-style (theme-scope-ref "ui.menu.selected"))
  (define dir-style (theme-scope-ref "ui.text.info"))
  (define dim-style (style-with-dim (theme-scope-ref "ui.text")))
  ;; border matches bg so it blends in instead of clashing across themes
  (define border-style bg-style)

  (define active-col (forest-mini-active-column))

  (define col-specs
    (map (lambda (col)
           (define entries (ForestMiniColumn-entries col))
           (list 'col col (forest-mini-col-width entries) (forest-mini-col-height (length entries) max-h)))
         *forest-mini-stack*))

  (define preview (forest-mini-preview))
  (define preview-kind (car preview))
  (define preview-data (cadr preview))
  (define preview-spec
    (cond
      [(equal? preview-kind 'dir)
       (list 'preview-dir preview-data
             (forest-mini-col-width preview-data) (forest-mini-col-height (length preview-data) max-h))]
      [(equal? preview-kind 'file)
       (list 'preview-file preview-data
             (min *forest-mini-preview-max-w*
                  (max *forest-mini-preview-min-w*
                       (+ (forest-mini-longest-line preview-data *forest-mini-preview-max-w*) 4 *forest-mini-width-boost*)))
             (forest-mini-col-height (length preview-data) max-h))]
      [else (list 'preview-empty #f *forest-mini-min-w* *forest-mini-min-h*)]))

  (define all-specs (append col-specs (list preview-spec)))

  (define (total-width specs)
    (+ (apply + (map (lambda (s) (+ (list-ref s 2) 2)) specs))
       (* *forest-mini-gap* (max 0 (- (length specs) 1)))))

  ;; drop the oldest ancestor columns first if the stack is wider than the screen
  (define (fit specs)
    (if (or (<= (total-width specs) (- sw 2)) (<= (length specs) 2))
        specs
        (fit (cdr specs))))
  (define visible (fit all-specs))

  ;; anchor at a top corner
  (define x0 (if (equal? *forest-side* 'right)
                 (max 0 (- sw (total-width visible) *forest-mini-margin*))
                 *forest-mini-margin*))
  (define y0 *forest-mini-margin*)

  (set! *forest-mini-hit-cols* '())
  (set! *forest-mini-hit-panels* '())
  (set! *forest-mini-hit-preview* #f)

  (let loop ([lst visible] [x x0])
    (unless (null? lst)
      (define spec (car lst))
      (define kind (list-ref spec 0))
      (define w (list-ref spec 2))
      (define h (list-ref spec 3))
      (define pw (+ w 2))
      (define ph (+ h 2))
      (define panel-area (area x y0 pw ph))
      (define cx (+ x 1))
      (define cy (+ y0 1))

      (buffer/clear-with frame panel-area bg-style)
      (block/render frame panel-area (make-block bg-style border-style "all" "rounded"))
      (set! *forest-mini-hit-panels* (cons panel-area *forest-mini-hit-panels*))

      (cond
        [(equal? kind 'col)
         (define col (list-ref spec 1))
         (define entries (ForestMiniColumn-entries col))
         (define cursor (forest-mini-cursor col))
         (define active? (equal? col active-col))
         (define ws (forest-mini-pinned-window col cursor (length entries) h))
         (set! *forest-mini-hit-cols* (cons (list (area cx cy w h) col ws) *forest-mini-hit-cols*))
         (forest-mini-render-entries frame cx cy w h entries ws cursor active?
                                text-style hl-style dir-style dim-style)]
        [(equal? kind 'preview-dir)
         (set! *forest-mini-hit-preview* (list (area cx cy w h) (length (list-ref spec 1))))
         (forest-mini-render-entries frame cx cy w h (list-ref spec 1) 0 -1 #f
                                text-style hl-style dir-style dim-style)]
        [(equal? kind 'preview-file)
         (forest-mini-render-lines frame cx cy w h (list-ref spec 1) dim-style)]
        [else
         (frame-set-string! frame cx cy (forest-truncate "(empty)" w) dim-style)])

      (loop (cdr lst) (+ x pw *forest-mini-gap*)))))

(define (forest-mini-command-action! action)
  (cond
    [(equal? action 'down) (forest-mini-move! 1) event-result/consume]
    [(equal? action 'up) (forest-mini-move! -1) event-result/consume]
    [(equal? action 'enter) (forest-mini-enter!)]
    [(equal? action 'back) (forest-mini-back!) event-result/consume]
    [(equal? action 'quit) (forest-mini-close!) event-result/close]
    [(equal? action 'create) (forest-mini-prompt-create!) event-result/consume]
    [(equal? action 'rename) (forest-mini-prompt-rename!) event-result/consume]
    [(equal? action 'delete) (forest-mini-prompt-delete!) event-result/consume]
    [(equal? action 'refresh) (forest-mini-refresh-active!) event-result/consume]
    [(equal? action 'search) (forest-mini-prompt-search!) event-result/consume]
    [(equal? action 'toggle-hidden) (forest-toggle-hidden!) event-result/consume]
    [(equal? action 'toggle-git-ignored) (forest-toggle-git-ignored!) event-result/consume]
    [(equal? action 'wider) (forest-mini-wider!) event-result/consume]
    [(equal? action 'narrower) (forest-mini-narrower!) event-result/consume]
    [(equal? action 'menu) (forest-whichkey-open! 'mini forest-mini-command-action!) event-result/consume]
    [else event-result/consume]))

(define (forest-mini-handle-keys state event)
  (define ch (key-event-char event))
  (cond
    [(key-event-down? event) (forest-mini-move! 1) event-result/consume]
    [(key-event-up? event) (forest-mini-move! -1) event-result/consume]
    [(key-event-right? event) (forest-mini-enter!)]
    [(key-event-enter? event) (forest-mini-enter!)]
    [(key-event-left? event) (forest-mini-back!) event-result/consume]
    [(key-event-escape? event) (forest-mini-close!) event-result/close]

    [(and (char? ch) (equal? ch #\=)) (forest-mini-wider!) event-result/consume] ; legacy alias

    [(char? ch)
     (define action (forest-action-for-char ch))
     (if action (forest-mini-command-action! action) event-result/consume)]

    [else event-result/consume]))

(define (forest-mini-handle-event state event)
  (cond
    ;; do not register keys when doing new/rename
    [*forest-modal-open?* event-result/ignore]
    [*forest-help-open?* event-result/ignore]
    [(mouse-event? event) (forest-mini-handle-mouse state event)]
    [else
     ;; any keypress can move the cursor, so an armed entry stops meaning anything
     (forest-reset-mouse!)
     (forest-mini-handle-keys state event)]))

(define (forest-mini-make-component)
  (new-component! "forest-mini" (ForestMiniState) forest-mini-render (hash "handle_event" forest-mini-handle-event)))

(define (forest-mini-open!)
  (cond
    [(not *forest-active*)
     (forest-scan-git-ignored! (helix-find-workspace))
     (set! *forest-mini-stack* (forest-mini-reveal-current-file!))
     (set! *forest-active* #t)
     (push-component! (forest-mini-make-component))]
    [else (forest-mini-close!)]))

;;@doc
;; Open the file tree
(define (forest-open)
  (if (equal? *forest-style* 'mini)
      (forest-mini-open!)
      (forest-snacks-open!)))

;;@doc
;; Close the file tree
(define (forest-close)
  (when *forest-active*
    (if (equal? *forest-style* 'mini)
        (forest-mini-close!)
        (forest-close!))))
