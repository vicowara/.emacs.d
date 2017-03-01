(require 'package)
;; package.elが管理していないelisp置き場
(add-to-list 'load-path "~/.emacs.d/elisp/")
;; MELPAを追加(package.el)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Marmaladeを追加(package.el)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; package.elの初期化
(package-initialize)
;; 以降はinit-loaderに任せる
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits/")
