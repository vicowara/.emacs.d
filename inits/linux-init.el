;; エンコーディング
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-to-list 'exec-path "/usr/local/bin/")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; フォント設定
;; 英語
(set-face-attribute 'default nil
                    :family "Inconsolata" ;; font
                    :height 130)    ;; font size
;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Source Han Sans JP")) ;; font

;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
      '((".*Source_Han_Sans_JP.*" . 1.2)));; Linux用フォント設定
