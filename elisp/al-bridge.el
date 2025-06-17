;; goal: easily pipe text between different search/replace modes
;; equivalent of manually copy pasting string between modes
;; inspired by https://karthinks.com/software/bridging-islands-in-emacs-1/

;; todo: how do i pick a node when inside minibuffer already? cycle back/forward with speical keys?

;; todo: rename all to al-bridge/ or al-pipe/
(require 'dash)
(require 'map)

;; why do i call them nodes?
;; current head node 
(defvar br/node '(:data nil) "Current head node")
(defvar br/old-nodes nil "List with old nodes, might be capped at 100")

;; todo
;; watch br/node for change, if source is chaged, put the previous br/node into old-nodes


;; debug
(al-watch-add 'br/node)
(al-watch-add 'isearch-string)

(defun br/reb-to-node (start end length)
  "Pipe re-builder buffer content to the node/reb"
  (setq br/node (map-merge 'plist
                               br/node
                               (list :source 'reb-mode :data (ignore-errors (read (buffer-string)))))))

(defun br/minibuffer-to-node (start end length)
  "Pipe re-builder buffer content to the node/reb"
  (setq br/node (map-merge 'plist
                               br/node
                               (list :source 'minibuffer-mode :data (ignore-errors (symbol-name (read (minibuffer-contents))))))))
(defun br/isearch-to-node (symbol newval operation where)
  (setq br/node (map-merge 'plist
                               br/node
                               (list :source 'isearch-mode :data isearch-string))))

(defun br/setup-to-node/reb () (add-hook 'after-change-functions 'br/reb-to-node nil 'local))
(defun br/setup-to-node/minibuffer () (add-hook 'after-change-functions 'br/minibuffer-to-node nil 'local))

(defun br/consult-to-node (symbol newval operation where)
  (setq br/node (map-merge 'plist
                               br/node
                               (list :source 'consult-grep :data isearch-string))))

(add-hook 'reb-mode-hook 'br/setup-to-node/reb)
(add-hook 'minibuffer-mode-hook 'br/setup-to-node/minibuffer)
(add-variable-watcher 'isearch-string 'br/isearch-to-node)
;; todo add consult ---history watchers to capture grep and find
;; consult--path-history
;; consult--grep-history
;; consult--find-history
;; consult--man-history
;; consult--line-history
;; consult--line-multi-history
;; consult--theme-history
;; consult--minor-mode-menu-history
;; consult--buffer-history



(defun br/yank-node ()
  (interactive)
  (insert (or (plist-get br/node :data) "")))



;; todo
;; this dhould yank from the previous node, that is first item in old-nodes
;; again why do i call it nodes?
(global-set-key (kbd "M-SPC .") 'br/yank-node)


