# metabase-dev.el

An Emacs package for managing Metabase REPL sessions with CIDER. This package provides a convenient interface for configuring and starting Metabase development environments with different editions, database backends and runtime settings.

## Features

- **Interactive Configuration Menu**: Transient-based menu for easy configuration management
- **Edition Support**: Toggle between Enterprise Edition (EE) and Open Source (OSS)
- **Multiple Database Backends**: Support for PostgreSQL, MySQL, MariaDB, and H2
- **Token Management**: Configure different EE token types (all-features, starter-cloud, pro-cloud, pro-self-hosted)
- **Test Mode**: Easily toggle test aliases on/off
- **REPL Management**: Start, restart, and manage REPL sessions with proper environment setup
- **Fresh Database**: Option to restart with a fresh database using `bin/mage`

## Requirements

- Emacs 25.1 or later
- [CIDER](https://github.com/clojure-emacs/cider)
- [Transient](https://github.com/magit/transient)
- [Projectile](https://github.com/bbatsov/projectile)

## Installation

### Using straight.el

```elisp
(straight-use-package
 '(metabase-dev :type git :host github :repo "johnswanson/metabase-dev.el"))
```

### Using use-package with straight.el

```elisp
(use-package metabase-dev
  :straight (metabase-dev :type git :host github :repo "johnswanson/metabase-dev.el")
  :commands (metabase-jack-in-with-config metabase-transient-menu)
  :config
  ;; Optional: Set default configuration
  (setq metabase-config-is-ee t
        metabase-config-db-type 'postgres
        metabase-config-db-version "latest"))
```

### Manual Installation

1. Clone this repository:
```bash
git clone https://github.com/johnswanson/metabase-dev.el.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/metabase-dev.el")
(require 'metabase-dev)
```

## Usage

### Interactive Transient Menu

The easiest way to use metabase-dev is through the interactive transient menu:

```elisp
M-x metabase-transient-menu
```

This opens a menu where you can:
- **e**: Toggle Edition (EE/OSS)
- **d**: Set Database Type (postgres, mysql, mariadb, h2)
- **v**: Set Database Version (latest, oldest, or specific version)
- **t**: Set Token Type (all-features, starter-cloud, pro-cloud, pro-self-hosted, none)
- **z**: Toggle Test Mode
- **a**: Set Additional Clojure CLI Aliases
- **r**: Restart REPL with current configuration
- **R**: Restart with Fresh Database
- **q**: Quit REPL

### Direct Functions

You can also call functions directly:

```elisp
;; Start a REPL with current configuration
M-x metabase-jack-in-with-config

;; Restart REPL (keeps existing database)
M-x metabase-restart-repl

;; Restart REPL with fresh database
M-x metabase-restart-repl-fresh-db

;; Configuration functions
M-x metabase-toggle-edition
M-x metabase-set-db-type
M-x metabase-set-db-version
M-x metabase-toggle-test-mode
```

### Configuration Variables

You can set these variables in your Emacs configuration:

```elisp
;; Edition configuration
(setq metabase-config-is-ee t)  ; t for EE, nil for OSS

;; Token type (for EE only)
(setq metabase-config-ee-token 'all-features)
;; Options: 'all-features, 'starter-cloud, 'pro-cloud, 'pro-self-hosted, 'none

;; Database configuration
(setq metabase-config-db-type 'postgres)
;; Options: 'postgres, 'mysql, 'mariadb, 'h2

(setq metabase-config-db-version "latest")
;; Options: "latest", "oldest", or specific version like "15"

;; Test mode
(setq metabase-config-test-mode nil)  ; t to enable test aliases

;; Additional Clojure CLI aliases
(setq metabase-config-additional-aliases ":my-custom-alias")
```

### Environment Variables

The package automatically sets the following environment variables based on your configuration:

- `MB_EDITION`: Set to "ee" or "oss"
- `MB_PREMIUM_EMBEDDING_TOKEN`: Set based on token type and environment
- `MB_DB_TYPE`: Database type (for H2)
- `MB_DB_FILE`: Database file path (for H2)
- `MB_ENABLE_TEST_ENDPOINTS`: Enabled when test mode is on
- `MB_CONFIG_FILE_PATH`: Points to dev/config.yml
- `METASTORE_DEV_SERVER_URL`: Set for EE mode

For EE tokens, the package reads from these environment variables:
- `MB_ALL_FEATURES_TOKEN`
- `MB_STARTER_CLOUD_TOKEN`
- `MB_PRO_CLOUD_TOKEN`
- `MB_PRO_SELF_HOSTED_TOKEN`

Make sure to set these in your shell environment before starting Emacs if you're using EE mode.

## Example Workflows

### Starting a Fresh EE Development Environment

1. Open the transient menu: `M-x metabase-transient-menu`
2. Press `e` to ensure EE mode is enabled
3. Press `d` and select your desired database (e.g., "postgres")
4. Press `v` and select "latest"
5. Press `R` to start with a fresh database

### Switching from OSS to EE

1. Open the transient menu: `M-x metabase-transient-menu`
2. Press `e` to toggle to EE
3. Press `t` to set your token type
4. Press `r` to restart the REPL

### Testing with H2

1. Open the transient menu: `M-x metabase-transient-menu`
2. Press `d` and select "h2"
3. Press `z` to enable test mode if needed
4. Press `r` to restart

## How It Works

When you start a REPL with `metabase-jack-in-with-config`:

1. The package resets all `MB_*` environment variables
2. Sets up environment variables based on your configuration
3. Builds a Clojure CLI aliases string combining:
   - Base aliases: `:otel:dev:drivers:drivers-dev`
   - EE aliases: `:ee:ee-dev` (if EE mode)
   - Test aliases: `:test` (if test mode enabled)
   - Database alias: `:db/{type}-{version}` (unless H2)
   - Any additional aliases you've configured
4. Calls `cider-jack-in-clj` with the constructed aliases
5. Automatically evaluates `(dev)` in the REPL after connection

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

This package is provided as-is. See the LICENSE file for details.

## Author

John Swanson <john.swanson@metabase.com>
