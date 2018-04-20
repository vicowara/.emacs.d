(require 'package)
;; package.elが管理していないelisp置き場
(add-to-list 'load-path "~/.emacs.d/elisp/")
;; MELPAを追加(package.el)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Marmaladeを追加(package.el)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; package.elの初期化
(package-initialize)

;; package-selected-packagesが必要なので先にcustom.elを読み込む
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; package-selected-packagesで指定されたパッケージをインストール(初回時を想定)
(setq package-pinned-packages
      '((web-mode . "melpa-stable")
        (auto-complete . "melpa-stable")))
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; 以降はinit-loaderに任せる
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits/")
