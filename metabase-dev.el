;;; metabase-dev.el --- Metabase REPL configuration and control -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 John Swanson
;;
;; Author: John Swanson <john.swanson@metabase.com>
;; Maintainer: John Swanson <john.swanson@metabase.com>
;; Created: October 24, 2025
;; Modified: October 24, 2025
;; Version: 0.1.0
;; Keywords: tools convenience languages
;; Homepage: https://github.com/johnswanson/metabase-dev
;; Package-Requires: ((emacs "25.1") (transient "0.3.0") (cider "1.0.0") (projectile "2.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides a convenient interface for starting and managing
;; Metabase REPL sessions with CIDER. It offers:
;;
;; - Configuration management for Metabase edition (EE/OSS), database type,
;;   and other runtime settings
;; - Interactive transient menu for easy configuration changes
;; - Automatic environment variable setup based on configuration
;; - Support for different database backends (PostgreSQL, MySQL, MariaDB, H2)
;; - Test mode toggle for including test aliases
;; - REPL restart with optional fresh database initialization
;;
;; Usage:
;;
;; Call `metabase-dev-jack-in-with-config' to start a Metabase REPL with your
;; current configuration, or use `metabase-dev-transient-menu' for an interactive
;; configuration interface.
;;
;; The package requires CIDER for Clojure REPL interaction and Projectile
;; for project management.
;;
;;; Code:

(require 'transient)
(require 'cider)
(require 'projectile)

;;; --- Stateful Metabase Configuration System ---------------------------------

(defcustom metabase-dev-config-is-ee t
  "Whether to start Metabase in Enterprise Edition mode."
  :type 'boolean
  :group 'metabase-dev)

(defcustom metabase-dev-config-ee-token 'all-features
  "The token to use for EE, if any."
  :type '(choice
          (const :tag "All Features" all-features)
          (const :tag "Starter Cloud" starter-cloud)
          (const :tag "Pro Cloud" pro-cloud)
          (const :tag "Pro Self Hosted" pro-self-hosted)
          (const :tag "None" none))
  :group 'metabase-dev)

(defcustom metabase-dev-config-db-type 'postgres
  "Database type for Metabase application database."
  :type '(choice (const :tag "PostgreSQL" postgres)
                 (const :tag "MySQL" mysql)
                 (const :tag "MariaDB" mariadb)
                 (const :tag "H2" h2))
  :group 'metabase-dev)

(defcustom metabase-dev-config-db-version "latest"
  "Database version. Can be `latest`, `oldest`, or a specific version string."
  :type 'string
  :group 'metabase-dev)

(defcustom metabase-dev-config-additional-aliases ""
  "Additional Clojure CLI aliases to include (e.g., ':db/postgres-15')."
  :type 'string
  :group 'metabase-dev)

(defcustom metabase-dev-config-config-file "dev/config.yml"
  "The config file to use for Metabase"
  :type 'string
  :group 'metabase-dev)

;;; --- Configuration Setter Functions -----------------------------------------

;;;###autoload
(defun metabase-dev-toggle-edition ()
  "Toggle between EE and OSS editions."
  (interactive)
  (setq metabase-dev-config-is-ee (not metabase-dev-config-is-ee))
  (message "Metabase edition: %s" (if metabase-dev-config-is-ee "EE" "OSS")))

;;;###autoload
(defun metabase-dev-set-token-type (token-type)
  "Set the token type for Metabase to TOKEN-TYPE."
  (interactive
   (list (intern (completing-read "Token type: "
                                  '("all-features"
                                    "starter-cloud"
                                    "pro-cloud"
                                    "pro-self-hosted"
                                    "none")))))
  (setq metabase-dev-config-ee-token token-type)
  (message "Token type set to: %s" token-type))

;;;###autoload
(defun metabase-dev-set-db-type (db-type)
  "Set the database type for Metabase to DB-TYPE."
  (interactive
   (list (intern (completing-read "Database type: "
                                  '("postgres" "mysql" "mariadb" "h2")
                                  nil t))))
  (setq metabase-dev-config-db-type db-type)
  (message "Database type set to: %s" db-type))

;;;###autoload
(defun metabase-dev-set-db-version (version)
  "Set the database version to VERSION.
Choose `latest`, `oldest`, or enter a specific version."
  (interactive
   (list (let ((choice (completing-read "Version: " '("latest" "oldest" "specific") nil t)))
           (if (string= choice "specific")
               (read-string "Enter specific version: ")
             choice))))
  (setq metabase-dev-config-db-version version)
  (message "Database version set to: %s" version))

;;;###autoload
(defun metabase-dev-set-additional-aliases (aliases)
  "Set additional Clojure CLI aliases to ALIASES."
  (interactive "sAdditional aliases: ")
  (setq metabase-dev-config-additional-aliases aliases)
  (message "Additional aliases set to: %s" aliases))

;;;###autoload
(defun metabase-dev-set-config-file (file)
  "Set the config file for metabase dev, e.g. `dev/config.yml'"
  (interactive "sFile: ")
  (setq metabase-dev-config-config-file file)
  (message "Config file set to: %s" file))

;;; --- Helper Functions -------------------------------------------------------

(defvar metabase-dev-port-file ".metabase-dev.el.ports"
  "File to store database container port mappings.")

(defun metabase-dev-port-file-path ()
  "Get the full path to the port file in the project root."
  (expand-file-name metabase-dev-port-file (projectile-project-root)))

(defun metabase-dev-write-port-file (data)
  "Write DATA (a plist) to the port file."
  (let ((file-path (metabase-dev-port-file-path)))
    (with-temp-file file-path
      (prin1 data (current-buffer)))))


(defun metabase-dev-container-name ()
  "Generate a container name for the current database configuration."
  (format "metabase-dev-%s-%s" metabase-dev-config-db-type metabase-dev-config-db-version))

(defun metabase-dev-container-info ()
  "Gets the current container info"
  (let* ((container-name (metabase-dev-container-name))
         (port (metabase-dev-get-container-port container-name))
         (image-name (metabase-dev-docker-image-name))
         (container-id (string-trim (shell-command-to-string
                                     (format "docker ps -aq --filter name=^%s$" container-name)))))
    (list :container-name container-name
          :container-id container-id
          :port port
          :image-digest (metabase-dev-get-image-digest image-name))))

(defun metabase-dev-docker-image-name ()
  "Get the Docker image name for the current database configuration."
  (pcase metabase-dev-config-db-type
    ('postgres (format "postgres:%s" metabase-dev-config-db-version))
    ('mysql (format "mysql:%s" metabase-dev-config-db-version))
    ('mariadb (format "mariadb:%s" metabase-dev-config-db-version))))

(defun metabase-dev-container-port ()
  "Get the internal container port for the current database type."
  (pcase metabase-dev-config-db-type
    ('postgres "5432")
    ('mysql "3306")
    ('mariadb "3306")))

(defun metabase-dev-check-container-exists (container-name)
  "Check if a Docker container with CONTAINER-NAME exists."
  (let ((output (shell-command-to-string
                 (format "docker ps -aq --filter name=^%s$" container-name))))
    (not (string-empty-p (string-trim output)))))

(defun metabase-dev-check-container-running (container-name)
  "Check if a Docker container with CONTAINER-NAME is running."
  (let ((output (shell-command-to-string
                 (format "docker ps -q --filter name=^%s$" container-name))))
    (not (string-empty-p (string-trim output)))))

(defun metabase-dev-get-container-port (container-name)
  "Get the exposed host port for CONTAINER-NAME."
  (let* ((internal-port (metabase-dev-container-port))
         (output (shell-command-to-string
                  (format "docker port %s %s" container-name internal-port))))
    (when (string-match ":\\([0-9]+\\)$" output)
      (match-string 1 output))))

(defun metabase-dev-get-image-digest (image-name)
  "Get the digest of the Docker image IMAGE-NAME."
  (let ((output (shell-command-to-string
                 (format "docker inspect --format='{{.Id}}' %s" image-name))))
    (string-trim output)))

(defun metabase-dev-start-container (container-name)
  "Start an existing Docker container."
  (shell-command-to-string (format "docker start %s" container-name)))

(defun metabase-dev-stop-container (container-name)
  "Stop a running Docker container."
  (shell-command-to-string (format "docker stop %s" container-name)))

(defun metabase-dev-remove-container (container-name)
  "Remove a Docker container."
  (shell-command-to-string (format "docker rm -f %s" container-name)))

(defun metabase-dev-create-container ()
  "Create and start a new Docker container for the current database configuration.
Returns a plist with :container-name, :container-id, :port, and :image-digest."
  (let* ((container-name (metabase-dev-container-name))
         (image-name (metabase-dev-docker-image-name))
         (internal-port (metabase-dev-container-port))
         (env-vars (pcase metabase-dev-config-db-type
                     ('postgres "-e POSTGRES_USER=metabase -e POSTGRES_PASSWORD=password -e POSTGRES_DB=metabase")
                     ('mysql "-e MYSQL_ROOT_PASSWORD=password -e MYSQL_DATABASE=metabase -e MYSQL_USER=metabase -e MYSQL_PASSWORD=password")
                     ('mariadb "-e MYSQL_ROOT_PASSWORD=password -e MYSQL_DATABASE=metabase -e MYSQL_USER=metabase -e MYSQL_PASSWORD=password")))
         (cmd (format "docker run -d --name %s %s -p 0:%s %s"
                      container-name env-vars internal-port image-name)))

    ;; Pull the image first to ensure we have the latest version
    (message "Pulling Docker image: %s" image-name)
    (shell-command-to-string (format "docker pull %s" image-name))

    ;; Create and start the container
    (message "Creating container: %s" container-name)
    (shell-command-to-string cmd)
    (sleep-for 2)
    (metabase-dev-container-info)))

(defun metabase-dev-ensure-container (fresh)
  "Ensure a database container is running.
If FRESH is non-nil, remove and recreate the container.
Returns container info plist."
  (let ((container-name (metabase-dev-container-name)))
    (cond
     ;; Fresh database requested: remove old container and create new
     (fresh
      (when (metabase-dev-check-container-exists container-name)
        (message "Removing existing container: %s" container-name)
        (metabase-dev-remove-container container-name))
      (metabase-dev-create-container))

     ;; Container exists and is running: reuse it
     ((metabase-dev-check-container-running container-name)
      (message "Reusing existing container: %s" container-name)
      (metabase-dev-container-info))

     ;; Container exists but is stopped: restart it
     ((metabase-dev-check-container-exists container-name)
      (message "Starting stopped container: %s" container-name)
      (metabase-dev-start-container container-name)
      (sleep-for 2)
      (metabase-dev-container-info))

     ;; No container exists: create new one
     (t
      (metabase-dev-create-container)))))

(defun metabase-dev-build-connection-uri (port)
  "Build a connection URI for the current database configuration using PORT."
  (pcase metabase-dev-config-db-type
    ('postgres (format "postgresql://metabase:password@localhost:%s/metabase" port))
    ('mysql (format "mysql://metabase:password@localhost:%s/metabase" port))
    ('mariadb (format "mysql://metabase:password@localhost:%s/metabase" port))))

(defun metabase-dev-build-connection-command (port)
  "Build a shell connection command for the current database configuration using PORT."
  (pcase metabase-dev-config-db-type
    ('postgres (format "PGPASSWORD=password psql -U metabase -h localhost -p %s metabase" port))
    ('mysql (format "mysql -u metabase -ppassword -h localhost -P %s metabase" port))
    ('mariadb (format "mysql -u metabase -ppassword -h localhost -P %s metabase" port))))

(defun metabase-dev-reset-env! ()
  "Reset all MB_* environment variables to start Metabase cleanly."
  (dolist (env process-environment)
    (when (string-match "^MB_" env)
      (let ((split-env (split-string env "=")))
        (setenv (car split-env) "")))))

(defun metabase-dev-send-dev ()
  "Send MB_* environment variables and (dev) to the CIDER REPL."
  (let* ((mb-env-vars (delq nil (mapcar (lambda (env)
                                          (when (string-match "^MB_" env)
                                            (let ((split-env (split-string env "=")))
                                              (cons (car split-env) (cadr split-env)))))
                                        process-environment)))
         (print-env-cmd (mapconcat (lambda (var)
                                     (format "(println \"%s=%s\")" (car var) (cdr var)))
                                   mb-env-vars "\n")))
    ;; Evaluate the generated command in CIDER
    (cider-interactive-eval (concat "(do\n" print-env-cmd "\n(dev))")))
  (remove-hook 'cider-connected-hook 'metabase-dev-send-dev))

;;; --- Core Jack-In Function --------------------------------------------------

;;;###autoload
(defun metabase-dev-jack-in-with-config ()
  "Start a Metabase REPL using the current configuration state."
  (interactive)
  (metabase-dev-reset-env!)

  ;; Set environment variables based on database type
  (if (eq metabase-dev-config-db-type 'h2)
      (let ((db-file (format-time-string "/tmp/metabase_%Y%m%d%H%M%S")))
        (setenv "MB_DB_TYPE" "h2")
        (setenv "MB_DB_FILE" db-file)
        (setenv "MB_DANGEROUS_UNSAFE_ENABLE_TESTING_H2_CONNECTIONS_DO_NOT_ENABLE" "true"))
    ;; For non-H2 databases, ensure container is running and set connection URI
    (let* ((container-info (metabase-dev-ensure-container nil))
           (port (plist-get container-info :port))
           (connection-uri (metabase-dev-build-connection-uri port)))
      (message "Container info: %s" container-info)
      (message "Using database at port %s" port)

      ;; Set Metabase environment variables
      (setenv "MB_DB_TYPE" (symbol-name metabase-dev-config-db-type))
      (setenv "MB_DB_CONNECTION_URI" connection-uri)))

  ;; Set common environment variables
  (setenv "MB_DANGEROUS_UNSAFE_ENABLE_TESTING_H2_CONNECTIONS_DO_NOT_ENABLE" "true")
  (setenv "MB_CONFIG_FILE_PATH" metabase-dev-config-config-file)

  ;; Set edition-specific env vars
  (if metabase-dev-config-is-ee
      (progn
        (setenv "MB_EDITION" "ee")
        (setenv "MB_PREMIUM_EMBEDDING_TOKEN" (pcase metabase-dev-config-ee-token
                                               ('all-features (getenv "MBDEV_ALL_FEATURES_TOKEN"))
                                               ('starter-cloud (getenv "MBDEV_STARTER_CLOUD_TOKEN"))
                                               ('pro-cloud (getenv "MBDEV_PRO_CLOUD_TOKEN"))
                                               ('pro-self-hosted (getenv "MBDEV_PRO_SELF_HOSTED_TOKEN"))
                                               ('none "")))
        (setenv "METASTORE_DEV_SERVER_URL" "https://token-check.staging.metabase.com"))
    (progn
      (setenv "MB_PREMIUM_EMBEDDING_TOKEN" "")
      (setenv "MB_EDITION" "oss")))

  ;; Build aliases string (no longer includes database-specific aliases)
  (let* ((base-aliases ":otel:dev:drivers:drivers-dev")

         (ee-aliases  (if metabase-dev-config-is-ee
                          ":ee:ee-dev"))

         (all-aliases (concat base-aliases
                              (or ee-aliases "")
                              metabase-dev-config-additional-aliases)))

    ;; Add hook and jack in
    (add-hook 'cider-connected-hook 'metabase-dev-send-dev)
    (let ((cider-clojure-cli-aliases all-aliases))
      (cider-jack-in-clj nil))))

;;; --- Database Connection Functions ------------------------------------------

;;;###autoload
(defun metabase-dev-copy-db-connection ()
  "Copy the database connection command to the clipboard/kill ring."
  (interactive)
  (if (eq metabase-dev-config-db-type 'h2)
      (message "H2 databases don't support external connections")
    (let ((container-info (metabase-dev-container-info)))
      (if container-info
          (let* ((port (plist-get container-info :port))
                 (command (metabase-dev-build-connection-command port)))
            (kill-new command)
            (message "Copied to clipboard: %s" command))
        (message "No database container found. Start the REPL first to create a database.")))))

;;; --- REPL Restart Functions -------------------------------------------------

;;;###autoload
(defun metabase-dev-restart-repl ()
  "Restart the Metabase REPL with current configuration (reuses existing database)."
  (interactive)
  (ignore-errors (cider-quit))
  (metabase-dev-jack-in-with-config))

;;;###autoload
(defun metabase-dev-restart-repl-fresh-db ()
  "Restart with a fresh database container, then restart the REPL with current configuration."
  (interactive)
  (ignore-errors (cider-quit))
  (unless (eq metabase-dev-config-db-type 'h2)
    (message "Starting fresh database container...")
    (let* ((container-info (metabase-dev-ensure-container t))
           (port (plist-get container-info :port)))
      (message "Fresh database started on port %s" port)))
  (metabase-dev-jack-in-with-config))

;;; --- Transient Menu ---------------------------------------------------------

;;;###autoload (autoload 'metabase-dev-transient-menu "metabase-dev" nil t)
(with-eval-after-load 'transient
  (transient-define-prefix metabase-dev-transient-menu ()
    "Metabase REPL configuration and control menu."
    [:description
     (lambda ()
       (concat
        "Metabase REPL Config\n"
        (format "\tEdition: %s (token %s)\n"
                (if metabase-dev-config-is-ee "EE" "OSS")
                metabase-dev-config-ee-token)
        (format "\tDB: %s (%s)\n" metabase-dev-config-db-type metabase-dev-config-db-version)
        (format "\tConfig file: %s\n" metabase-dev-config-config-file)
        (format "\tAdditional aliases: %s\n" metabase-dev-config-additional-aliases)))
     ["Configuration"
      ("e" "Toggle Edition (EE/OSS)" metabase-dev-toggle-edition :transient t)
      ("t" "Set Token Type" metabase-dev-set-token-type :transient t)
      ("d" "Set Database Type" metabase-dev-set-db-type :transient t)
      ("v" "Set DB Version" metabase-dev-set-db-version :transient t)
      ("f" "Set Config File" metabase-dev-set-config-file :transient t)
      ("a" "Set Additional Aliases" metabase-dev-set-additional-aliases :transient t)]
     ["Actions"
      ("r" "Restart REPL" metabase-dev-restart-repl)
      ("R" "Restart with Fresh DB" metabase-dev-restart-repl-fresh-db)
      ("c" "Copy DB Connection Command" metabase-dev-copy-db-connection)
      ("q" "Quit REPL" cider-quit)]]))

(provide 'metabase-dev)
;;; metabase-dev.el ends here
