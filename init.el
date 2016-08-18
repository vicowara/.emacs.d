(set-language-environment "Japanese")
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (prefer-coding-system 'utf-8-unix)
         (set-default-coding-systems 'utf-8-unix)
         (setq file-name-coding-system 'sjis)
         (setq locale-coding-system 'utf-8))
        ((eq ws 'ns)
         (require 'ucs-normalize)
         (prefer-coding-system 'utf-8)
         (setq file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs)
         ;; システムに装飾キー渡さない
         (setq mac-pass-control-to-system nil)
         (setq mac-pass-command-to-system nil)
         (setq mac-pass-option-to-system nil)
         )
        ((eq ws 'x)
         (set-default-coding-systems 'utf-8)
         (set-keyboard-coding-system 'utf-8)
         (set-terminal-coding-system 'utf-8)
         (set-buffer-file-coding-system 'utf-8)
         (prefer-coding-system 'utf-8)
         )))

(tool-bar-mode 0)

(add-to-list 'load-path "/Users/vicco/.emacs.d/elisp/")
(add-to-list 'exec-path "/usr/local/bin/")

(require 'server)
(unless (server-running-p)
  (server-start))

(when (require 'package nil t)
  ;; MELPAを追加
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;; Marmaladeを追加
  (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; 初期化
  (package-initialize)
  (put 'set-goal-column 'disabled nil))

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  ;; 英語
  (set-face-attribute 'default nil
                      :family "Menlo" ;; font
                      :height 130)    ;; font size
  
  ;; 日本語
  (set-fontset-font
   nil 'japanese-jisx0208
   ;; (font-spec :family "Hiragino Mincho Pro")) ;; font
   (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
  
  ;; 半角と全角の比を1:2にしたければ
  (setq face-font-rescale-alist
        ;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
        '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定
  )

;; スタートアップ非表示
(setq inhibit-startup-message t)

;; 対応する括弧をハイライトする
(show-paren-mode 1)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 対応する括弧の色の設定
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

;; 行番号表示
(global-linum-mode t)
(column-number-mode t)
(line-number-mode t)

;; C-h を Backspace として使い，
;; C-c h を HELP に割当てる
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
(global-set-key "\C-ch" 'help-command)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; コマンドキーをmetaキー
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; C-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; センテンスの終了である ';' を入力したら，自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; RET キーで自動改行+インデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)
             (c-set-style "bsd")
             
             (setq c-basic-offset 4)
             ;; 演算式が複数行にまたがるときのオフセット
             (c-set-offset 'statement-cont 'c-lineup-math)
             ))

;;;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)


;; 全てバッファを自動的にセーブする
(setq mode-compile-always-save-buffer-p t)
;; コマンドをいちいち確認しない
(setq mode-compile-never-edit-command-p t)
;; メッセージ出力を抑制
(setq mode-compile-expert-p t)
;; メッセージを読み終わるまで待つ時間
(setq mode-compile-reading-time 0)

;; コンパイルが完了したらウィンドウを閉じる
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 0.3 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose)

(require 'auto-complete-config)
(ac-config-default)


;; 全角スペース
(require 'whitespace)
(set-face-foreground 'whitespace-space "DarkGoldenrod1")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
(set-face-background 'whitespace-tab nil)
(set-face-underline  'whitespace-tab t)

(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))

(global-whitespace-mode 1) ; 全角スペースを常に表示
(global-set-key (kbd "C-x w") 'global-whitespace-mode) ; 全角スペース表示の切替

(require 'epc)
(require 'python)
(when (eq system-type 'darwin)
  (setenv "PYTHONPATH" "/Library/Python/2.7/site-packages/"))
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

     ;;;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; 全てバッファを自動的にセーブする
(setq mode-compile-always-save-buffer-p t)
;; コマンドをいちいち確認しない
(setq mode-compile-never-edit-command-p t)
;; メッセージ出力を抑制
(setq mode-compile-expert-p t)
;; メッセージを読み終わるまで待つ時間
(setq mode-compile-reading-time 0)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)
(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(defun has-check-syntax ()
  (if (file-exists-p "Makefile")
    (progn (with-temp-buffer
       (insert-file-contents "Makefile")
       (let ((buffer (buffer-string)))
         (if (string-match "check-syntax" buffer)
             t
           nil)
         )))
    nil))

(defun switch-flycheck-or-flymake ()
  (if (has-check-syntax)
      (progn
        (flymake-mode t)
        (flycheck-mode -1)
        )
    (flymake-mode -1)
    (flycheck-mode t))
  )

(add-hook 'c-mode-hook '(lambda ()
                          (switch-flycheck-or-flymake)
                          ))

;; python
(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map "\C-m" 'newline-and-indent)
                               (define-key (current-local-map) "\C-h" 'python-backspace)
                               (setq indent-tabs-mode nil)
                               (setq python-indent-offset 4)
                               ))

(defun my-short-buffer-file-coding-system (&optional default-coding)
  (let ((coding-str (format "%S" buffer-file-coding-system)))
    (cond ((string-match "shift-jis" coding-str) 'shift_jis)
          ((string-match "euc-jp" coding-str) 'euc-jp)
          ((string-match "utf-8" coding-str) 'utf-8)
          (t (or default-coding 'utf-8)))))

(defun my-insert-file-local-coding ()
  "ファイルの先頭に `coding:' を自動挿入する"
  (interactive)
  (save-excursion
    (goto-line 2) (end-of-line) ; ２行目の行末の移動
    (let ((limit (point)))
      (goto-char (point-min))
      (unless (search-forward "coding:" limit t) ; 2行目以内に `coding:'がない
        (goto-char (point-min))
        ;; #!で始まる場合２行目に記述
        (when (and (< (+ 2 (point-min)) (point-max))
                   (string= (buffer-substring (point-min) (+ 2 (point-min))) "#!"))
          (unless (search-forward "\n" nil t) ; `#!'で始まり末尾に改行が無い場合
            (insert "\n")))                   ; 改行を挿入
        (let ((st (point)))
          (insert (format "-*- coding: %S -*-\n" (my-short-buffer-file-coding-system)))
          (comment-region st (point)))))))

(add-hook 'python-mode-hook 'my-insert-file-local-coding)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(require 'magit)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

(require 'undohist)
(undohist-initialize)

(require 'anzu)
(global-anzu-mode +1)

(require 'haskell-mode)
(require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(require 'twittering-mode)
(setq twittering-use-master-password t)

;; google-translate.el
(require 'google-translate)
(require 'google-translate-default-ui)
;; キーバインドの設定（お好みで）
(global-set-key "\C-xt" 'google-translate-at-point)
(global-set-key "\C-xT" 'google-translate-query-translate)

;; 翻訳のデフォルト値を設定（en -> ja）
(custom-set-variables
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja"))

;; popwin.el
(require 'popwin)
;; おまじない（よく分かってない，，）
(setq display-buffer-function 'popwin:display-buffer)
;; ポップアップを画面下に表示
(setq popwin:popup-window-position 'bottom)

;; google-translate.elの翻訳バッファをポップアップで表示させる
(push '("*Google Translate*") popwin:special-display-config)

(smartparens-global-mode t)


;; (require 'cedet)
;; ;; Semantic
;; (global-ede-mode 1)
;; (semantic-mode t)
;; (global-semantic-idle-scheduler-mode)
;; (global-semantic-idle-completions-mode)
;; (global-semantic-decoration-mode)
;; (global-semantic-highlight-func-mode)
;; (global-semantic-show-unmatched-syntax-mode)
;; (global-semanticdb-minor-mode)

;; ;; CC-mode
;; (add-hook 'c-mode-hook '(lambda ()
;;         (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;         (local-set-key (kbd "RET") 'newline-and-indent)
;;         (linum-mode t)
;;         (semantic-mode t)))

;; (add-hook 'c-mode-common-hook '(lambda ()
;;         (setq ac-sources (append '(ac-source-semantic) ac-sources))
;; ))

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region) ;; リージョンを狭める

;; transient-mark-modeが nilでは動作しませんので注意
(transient-mark-mode t)

(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (multi-term)))
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)
(global-set-key (kbd "C-c C-t") 'shell-pop)

(setq vr/engine 'pcre2el)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)

(defun replace-dot-comma ()
  "s/。/．/g; s/、/，/g;する"
  (interactive)
  (let ((curpos (point)))
    (goto-char (point-min))
    (while (search-forward "。" nil t) (replace-match "．"))
    
    (goto-char (point-min))
    (while (search-forward "、" nil t) (replace-match "，"))
    (goto-char curpos)
    ))

(add-hook 'tex-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'replace-dot-comma nil 'make-it-local)
             (flyspell-mode t)
             ))
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(when (require 'skk nil t)
  (setq skk-server-host "localhost") ; AquaSKK のサーバー機能を利用
  (setq skk-server-portnum 1178)	   ; ポートは標準
  (setq skk-share-private-jisyo t)   ; 複数 skk 辞書を共有
  (setq skk-tut-file "~/Dropbox/emacs/SKK/SKK.tut")
;  (setq skk-tut-file "~/.emacs.d/SKK.tut")
;  (setq skk-show-tooltip t)
;  (setq skk-inline-show t)
  (setq skk-show-annotation t)
  (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
  (setq skk-henkan-show-candidates-rows 2) ; 候補表示件数を2列に
  (setq skk-search-katakana t)
  
  ;; 動的候補表示
  (setq skk-dcomp-activate t)			 ; 動的補完
  (setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
  (setq skk-dcomp-multiple-rows 10)	 ; 動的補完の候補表示件数
  ;; 動的補完の複数表示群のフェイス
  (set-face-foreground 'skk-dcomp-multiple-face "Black")
  (set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
  (set-face-bold-p 'skk-dcomp-multiple-face nil)
  ;; 動的補完の複数表示郡の補完部分のフェイス
  (set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
  (set-face-bold-p 'skk-dcomp-multiple-trailing-face nil)
  ;; 動的補完の複数表示郡の選択対象のフェイス
  (set-face-foreground 'skk-dcomp-multiple-selected-face "White")
  (set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
  (set-face-bold-p 'skk-dcomp-multiple-selected-face nil)
  (require 'skk-hint)							; ヒント
  (setq skk-hint-start-char 58); hintを:に
  (add-hook 'skk-load-hook ; 自動的に入力モードを切り替え
            (lambda ()
              (require 'context-skk)))

  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
  (setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
  (require 'skk-study)                              ;;変換学習機能の追加
  (setq skk-sticky-key ";")
  (setq skk-jisyo-code 'utf-8)
  (setq skk-user-directory "~/Dropbox/emacs/SKK")
  (setq skk-large-jisyo "~/Dropbox/emacs/SKK/SKK-JISYO.L.utf8")
  (setq skk-record-file "~/Dropbox/emacs/SKK/skk-record.utf8")
  (setq skk-emacs-id-file "~/Dropbox/emacs/SKK/skk-emacs-id.utf8")
  (setq skk-jisyo "~/Dropbox/emacs/SKK/skk-jisyo.utf8")
  (setq skk-backup-jisyo "~/Dropbox/emacs/SKK/skk-jisyo.utf8.bak")
  (setq skk-study-file "~/Dropbox/emacs/SKK/skk-study.utf8")
  (setq skk-study-backup-file "~/Dropbox/emacs/SKK/skk-study.utf8.bak")
  )

(require 'multiple-cursors)
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
(global-unset-key "\C-t")
(smartrep-define-key global-map "C-t"
                     '(("C-t"      . 'mc/mark-next-like-this)
                       ("n"        . 'mc/mark-next-like-this)
                       ("p"        . 'mc/mark-previous-like-this)
                       ("m"        . 'mc/mark-more-like-this-extended)
                       ("u"        . 'mc/unmark-next-like-this)
                       ("U"        . 'mc/unmark-previous-like-this)
                       ("s"        . 'mc/skip-to-next-like-this)
                       ("S"        . 'mc/skip-to-previous-like-this)
                       ("*"        . 'mc/mark-all-like-this)
                       ("d"        . 'mc/mark-all-like-this-dwim)
                       ("i"        . 'mc/insert-numbers)
                       ("o"        . 'mc/sort-regions)
                       ("O"        . 'mc/reverse-regions)))

(when (require 'bison-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
  (add-to-list 'auto-mode-alist '("\\.l$" . bison-mode)))
;;
