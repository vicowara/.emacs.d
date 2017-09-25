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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(irony-cdb-compilation-databases
   (quote
    (irony-cdb-libclang irony-cdb-clang-complete irony-cdb-json)))
 '(package-selected-packages
   (quote
    (yasnippet-snippets yatex pcap-mode flycheck-clang-analyzer flycheck-tip company-irony-c-headers flycheck-irony company company-c-headers company-irony company-jedi irony volatile-highlights undohist twittering-mode tabbar sticky sr-speedbar smartrep smartparens shell-pop semi scala-mode python-mode pyflakes popwin php-mode php-eldoc pcre2el pandoc-mode pandoc multi-term mode-compile migemo markdown-mode magit jedi init-loader helm-gtags helm-flymake helm-flycheck helm-cscope helm-ag haskell-mode gtags graphviz-dot-mode google-translate go-mode ggtags foreign-regexp flycheck-pyflakes flycheck-pos-tip expand-region exec-path-from-shell evil django-mode ddskk codic bison-mode auto-install auto-complete-clang anzu ace-link ac-helm yasnippet)))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
