(use-package docker-tramp
  :config
  (require 'docker-tramp-compat)
  (setq docker-tramp-use-names t))

(setq almacs/helm-docker-actions
      (helm-make-actions
       "Tramp open" (-lambda ((&plist :tramp-path path))
                      (find-file path))
       "Shell" (-lambda ((&plist :tramp-path path))
                 (let ((default-directory path))
                   (shell (s-concat "*docker tramp shell - " path))))
       "Stop" (-lambda ((&plist :name name))
                (shell-command-to-string (spacecat "docker stop" name)))
       "Start" (-lambda ((&plist :name name))
                 (shell-command-to-string (spacecat "docker start" name)))
       "Restart" (-lambda ((&plist :name name))
                   (shell-command-to-string (spacecat "docker restart" name)))))

(defun almacs/helm-docker-ps-candidates ()
  (->> "docker ps -a --format \"{{.Names}}\t{{.Ports}}\t{{.Status}}\""
       (shell-command-to-string)
       (s-lines)
       (-remove 's-blank?)
       (-map (-partial 's-split "\t"))
       (-map (-lambda ((name port status))
               (let ((display-name (->> (spacecat
                                         (s-pad-right 32 " " name)
                                         (s-pad-right 32 " " (s-concat "(" status ")"))
                                         (s-pad-right 32 " " port))))
                     (tramp-path (s-concat "/docker:" name ":/")))
                 `(,display-name . (:tramp-path ,tramp-path
                                                :name ,name
                                                :port ,port
                                                :status ,status)))))))


(setq almacs/helm-docker-ps-source (helm-build-sync-source "docker ps -a"
                                     :action 'almacs/helm-docker-actions
                                     :candidates 'almacs/helm-docker-ps-candidates))

(defun almacs/helm-docker-ps ()
  (interactive)
  (helm :sources '(almacs/helm-docker-ps-source) :buffer "*helm docker ps*"))
