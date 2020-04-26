(evil-define-key 'normal sql-mode-map
  ",eb" 'sql-send-buffer
  ",ee" 'sql-send-line-and-next)

(evil-define-key 'visual sql-mode-map
  ",ee" 'sql-send-region)
