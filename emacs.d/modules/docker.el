(require 'org)

(use-package docker-tramp
  :config
  (setq docker-tramp-use-names t))

(setq almacs/helm-docker-candidates nil)

(defun almacs/-helm-docker-action (cmd)
  (->> (helm-marked-candidates)
       (-map (-lambda ((&plist :name name)) name))
       (s-join " ")
       (spacecat "docker" cmd)
       (async-shell-command)))

(setq almacs/helm-docker-actions
      (helm-make-actions
       "Tramp open" (-lambda ((&plist :tramp-path path))
                      (find-file path))
       "Shell" (-lambda ((&plist :tramp-path path))
                 (let ((default-directory path))
                   (shell (s-concat "*docker tramp shell - " path))))
       "Stop" (lambda (_) (almacs/-helm-docker-action "stop"))
       "Start" (lambda (_)(almacs/-helm-docker-action "start"))
       "Restart" (lambda (_) (almacs/-helm-docker-action "restart"))))

(setq almacs/helm-docker-ps-source (helm-build-sync-source "docker ps and stats"
                                     :action 'almacs/helm-docker-actions
                                     :candidates 'almacs/helm-docker-candidates))

(defun almacs/helm-docker-ps-entries (output)
  (->> output
       (s-lines)
       (-remove 's-blank?)
       (-map (-partial 's-split "\t"))
       (-map (-lambda ((name ports status))
               (list :name name :ports ports :status status)))))

(defun almacs/helm-docker-stats-entries (output)
  (->> output
       (s-lines)
       (-remove 's-blank?)
       (-map (-partial 's-split "\t"))
       (-map (-lambda ((name cpu mem))
               (list :name name :cpu cpu :mem mem)))))


(defun almacs/helm-docker-make-candidates (ps stats)
  (->> (-concat
        (almacs/helm-docker-stats-entries stats)
        (almacs/helm-docker-ps-entries ps))
       (-group-by (-lambda ((&plist :name name)) name))
       (-map (lambda (a) (apply 'org-combine-plists (cdr a))))
       (-sort (-lambda ((&plist :name n1) (&plist :name n2)) (s-less? n1 n2)))
       (-map (-lambda
               ((e &as &plist
                   :name name :ports ports :status status :cpu cpu :mem mem))
               (let* ((display-name (spacecat
                                     (s-pad-right 32 " " name)
                                     (s-pad-right 32 " " (s-concat "(" status ")"))
                                     (s-pad-right 8 " " (s-concat cpu))
                                     (s-pad-right 8 " " (s-concat mem))
                                     (s-pad-right 32 " " ports)))
                      (tramp-path (s-concat "/docker:" name ":/"))
                      (res (org-combine-plists (list :tramp-path tramp-path) e)))
                 `(,display-name . ,res))))))

(defun almacs/helm-docker-ps ()
  (interactive)
  ;; Initially run only ps since stats is slow
  (setq almacs/helm-docker-candidates
        (almacs/helm-docker-make-candidates
         (shell-command-to-string "docker ps -a --format \"{{.Names}}\t{{.Ports}}\t{{.Status}}\"")
         ""))
  (let ((timer (run-at-time nil 3 (lambda ()
                                    (async-start
                                     (lambda ()
                                       (list
                                        (shell-command-to-string
                                         "docker ps -a --format \"{{.Names}}\t{{.Ports}}\t{{.Status}}\"")
                                        (shell-command-to-string
                                         "docker stats --no-stream --format \"{{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}\"")))
                                     (-lambda ((ps stats))
                                       (with-helm-buffer
                                         (setq almacs/helm-docker-candidates
                                               (almacs/helm-docker-make-candidates ps stats))
                                         (let ((n (helm-candidate-number-at-point)))
                                           (helm-force-update
                                            (lambda ()
                                              (goto-char (point-min))
                                              (forward-line n)))))))))))
    (ignore-errors
      (helm :sources '(almacs/helm-docker-ps-source) :buffer "*helm docker ps*"))
    (cancel-timer timer)))
