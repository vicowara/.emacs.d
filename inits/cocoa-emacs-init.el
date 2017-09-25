;; エンコーディング
(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; システムに装飾キー渡さない
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)

(add-to-list 'exec-path "/usr/local/bin/")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; 英語
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 130)    ;; font size

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 ;; (font-spec :family "Hiragino Mincho Pro")) ;; font
 (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
      ;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定


;; コマンドキーをmetaキー
(setq ns-command-modifier (quote meta))

;; 円マークをバックスラッシュに
(define-key global-map [?\¥] [?\\])
(define-key global-map [?\C-¥] [?\C-\\])
(define-key global-map [?\M-¥] [?\M-\\])
(define-key global-map [?\C-\M-¥] [?\C-\M-\\])
