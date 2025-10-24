;;; metabase-dev.el --- Metabase REPL configuration and control -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 John Swanson
;;
;; Author: John Swanson <jds@arc.local>
;; Maintainer: John Swanson <jds@arc.local>
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
;; Call `metabase-jack-in-with-config' to start a Metabase REPL with your
;; current configuration, or use `metabase-transient-menu' for an interactive
;; configuration interface.
;;
;; The package requires CIDER for Clojure REPL interaction and Projectile
;; for project management.
;;
;;; Code:

(require 'transient)
(require 'cider)
(require 'projectile)

;;; --- Metabase Environment Variables -----------------------------------------

(defvar metabase-env-vars '()
  "List of environment variables to set when starting Metabase.")

;;; --- Stateful Metabase Configuration System ---------------------------------

(defcustom metabase-config-is-ee t
  "Whether to start Metabase in Enterprise Edition mode."
  :type 'boolean
  :group 'metabase)

(defcustom metabase-config-ee-token 'all-features
  "The token to use for EE, if any."
  :type '(choice
          (const :tag "All Features" all-features)
          (const :tag "Starter Cloud" starter-cloud)
          (const :tag "Pro Cloud" pro-cloud)
          (const :tag "Pro Self Hosted" pro-self-hosted)
          (const :tag "None" none))
  :group 'metabase)

(defcustom metabase-config-db-type 'postgres
  "Database type for Metabase application database."
  :type '(choice (const :tag "PostgreSQL" postgres)
                 (const :tag "MySQL" mysql)
                 (const :tag "MariaDB" mariadb)
                 (const :tag "H2" h2))
  :group 'metabase)

(defcustom metabase-config-db-version "latest"
  "Database version. Can be 'latest', 'oldest', or a specific version string."
  :type 'string
  :group 'metabase)

(defcustom metabase-config-test-mode nil
  "Whether to include test aliases when starting the REPL."
  :type 'boolean
  :group 'metabase)

(defcustom metabase-config-additional-aliases ""
  "Additional Clojure CLI aliases to include (e.g., ':db/postgres-15')."
  :type 'string
  :group 'metabase)

;;; --- Configuration Setter Functions -----------------------------------------

;;;###autoload
(defun metabase-toggle-edition ()
  "Toggle between EE and OSS editions."
  (interactive)
  (setq metabase-config-is-ee (not metabase-config-is-ee))
  (message "Metabase edition: %s" (if metabase-config-is-ee "EE" "OSS")))

;;;###autoload
(defun metabase-set-token-type (token-type)
  "Set the token type for Metabase to TOKEN-TYPE."
  (interactive
   (list (intern (completing-read "Token type: "
                                  '("all-features"
                                    "starter-cloud"
                                    "pro-cloud"
                                    "pro-self-hosted"
                                    "none")))))
  (setq metabase-config-ee-token token-type)
  (message "Token type set to: %s" token-type))

;;;###autoload
(defun metabase-set-db-type (db-type)
  "Set the database type for Metabase to DB-TYPE."
  (interactive
   (list (intern (completing-read "Database type: "
                                  '("postgres" "mysql" "mariadb" "h2")
                                  nil t))))
  (setq metabase-config-db-type db-type)
  (message "Database type set to: %s" db-type))

;;;###autoload
(defun metabase-set-db-version (version)
  "Set the database version to VERSION.
Choose 'latest', 'oldest', or enter a specific version."
  (interactive
   (list (let ((choice (completing-read "Version: " '("latest" "oldest" "specific") nil t)))
           (if (string= choice "specific")
               (read-string "Enter specific version: ")
             choice))))
  (setq metabase-config-db-version version)
  (message "Database version set to: %s" version))

;;;###autoload
(defun metabase-toggle-test-mode ()
  "Toggle test mode on/off."
  (interactive)
  (setq metabase-config-test-mode (not metabase-config-test-mode))
  (message "Test mode: %s" (if metabase-config-test-mode "ON" "OFF")))

;;;###autoload
(defun metabase-set-additional-aliases (aliases)
  "Set additional Clojure CLI aliases to ALIASES."
  (interactive "sAdditional aliases: ")
  (setq metabase-config-additional-aliases aliases)
  (message "Additional aliases set to: %s" aliases))

;;; --- Helper Functions -------------------------------------------------------

(defun metabase-reset-env! ()
  "Reset all MB_* environment variables to start Metabase cleanly."
  (dolist (env process-environment)
    (when (string-match "^MB_" env)
      (let ((split-env (split-string env "=")))
        (setenv (car split-env) ""))))
  ;; Loop over metabase-env-vars and set them
  (dolist (env metabase-env-vars)
    (setenv (car env) (cdr env))))

(defun metabase-send-dev ()
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
  (remove-hook 'cider-connected-hook 'metabase-send-dev))

;;; --- Core Jack-In Function --------------------------------------------------

;;;###autoload
(defun metabase-jack-in-with-config ()
  "Start a Metabase REPL using the current configuration state."
  (interactive)
  (metabase-reset-env!)

  ;; Set environment variables based on database type
  (if (eq metabase-config-db-type 'h2)
      (let ((db-file (format-time-string "/tmp/metabase_%Y%m%d%H%M%S")))
        (setenv "MB_DB_TYPE" "h2")
        (setenv "MB_DB_FILE" db-file)
        (setenv "MB_DANGEROUS_UNSAFE_ENABLE_TESTING_H2_CONNECTIONS_DO_NOT_ENABLE" "true")))

  ;; Set common environment variables
  (setenv "MB_DANGEROUS_UNSAFE_ENABLE_TESTING_H2_CONNECTIONS_DO_NOT_ENABLE" "true")
  (setenv "MB_ENABLE_TEST_ENDPOINTS" (if metabase-config-test-mode "true" "false"))
  (setenv "MB_CONFIG_FILE_PATH" "dev/config.yml")

  ;; Set edition-specific env vars
  (if metabase-config-is-ee
      (progn
        (setenv "MB_EDITION" "ee")
        (setenv "MB_PREMIUM_EMBEDDING_TOKEN" (pcase metabase-config-ee-token
                                               ('all-features (getenv "MBDEV_ALL_FEATURES_TOKEN"))
                                               ('starter-cloud (getenv "MBDEV_STARTER_CLOUD_TOKEN"))
                                               ('pro-cloud (getenv "MBDEV_PRO_CLOUD_TOKEN"))
                                               ('pro-self-hosted (getenv "MBDEV_PRO_SELF_HOSTED_TOKEN"))
                                               ('none "")))
        (setenv "METASTORE_DEV_SERVER_URL" "https://token-check.staging.metabase.com"))
    (progn
      (setenv "MB_PREMIUM_EMBEDDING_TOKEN" "")
      (setenv "MB_EDITION" "oss")))

  ;; Build aliases string
  (let* ((base-aliases ":otel:dev:drivers:drivers-dev")

         (ee-aliases  (if metabase-config-is-ee
                          ":ee:ee-dev"))

         (test-aliases (if metabase-config-test-mode ":test" ""))

         (db-alias (unless (eq metabase-config-db-type 'h2)
                     (format ":db/%s-%s"
                             metabase-config-db-type
                             metabase-config-db-version)))

         (all-aliases (concat base-aliases
                              (or ee-aliases "")
                              test-aliases
                              (or db-alias "")
                              metabase-config-additional-aliases)))

    ;; Add hook and jack in
    (add-hook 'cider-connected-hook 'metabase-send-dev)
    (let ((cider-clojure-cli-aliases all-aliases))
      (cider-jack-in-clj nil))))

;;; --- REPL Restart Functions -------------------------------------------------

;;;###autoload
(defun metabase-restart-repl ()
  "Restart the Metabase REPL with current configuration (no DB restart)."
  (interactive)
  (ignore-errors (cider-quit))
  (metabase-jack-in-with-config))

;;;###autoload
(defun metabase-restart-repl-fresh-db ()
  "Restart the database using bin/mage, then restart the REPL with current configuration."
  (interactive)
  (ignore-errors (cider-quit))
  (unless (eq metabase-config-db-type 'h2)
    (let ((db-spec (format "%s %s" metabase-config-db-type metabase-config-db-version)))
      (message "Starting fresh database: %s" db-spec)
      (projectile-run-shell-command-in-root
       (format "bin/mage start-db %s" db-spec))))
  (metabase-jack-in-with-config))

;;; --- Transient Menu ---------------------------------------------------------

;;;###autoload (autoload 'metabase-transient-menu "metabase-dev" nil t)
(with-eval-after-load 'transient
  (transient-define-prefix metabase-transient-menu ()
    "Metabase REPL configuration and control menu."
    [:description
     (lambda ()
       (format "Metabase REPL Config\n\tEdition: %s (token: %s)\n\tDB: %s (%s)\n\tTest Mode: %s"
               (if metabase-config-is-ee "EE" "OSS")
               metabase-config-ee-token
               metabase-config-db-type metabase-config-db-version
               (if metabase-config-test-mode "ON" "OFF")))
     ["Configuration"
      ("e" "Toggle Edition (EE/OSS)" metabase-toggle-edition :transient t)
      ("d" "Set Database Type" metabase-set-db-type :transient t)
      ("v" "Set DB Version" metabase-set-db-version :transient t)
      ("t" "Set Token Type" metabase-set-token-type :transient t)
      ("z" "Toggle Test Mode" metabase-toggle-test-mode :transient t)
      ("a" "Set Additional Aliases" metabase-set-additional-aliases :transient t)]
     ["Actions"
      ("r" "Restart REPL" metabase-restart-repl)
      ("R" "Restart with Fresh DB" metabase-restart-repl-fresh-db)
      ("q" "Quit REPL" cider-quit)]]))

(provide 'metabase-dev)
;;; metabase-dev.el ends here
