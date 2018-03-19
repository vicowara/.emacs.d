;;;; シンタックスチェック
(require 'flycheck)
(require 'flymake)
(require 'helm-flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(defadvice flymake-post-syntax-check (before flymake-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
;(with-eval-after-load 'flycheck
;  (flycheck-pos-tip-mode))
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))
(with-eval-after-load 'flycheck
  (require 'flycheck-pycheckers)
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
(with-eval-after-load 'flycheck
   (require 'flycheck-clang-analyzer)
   (flycheck-clang-analyzer-setup))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

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



;;;; magit
(require 'magit)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")



;;;; semantic
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-mru-bookmark-mode 1)



;;;; srefactor
(require 'srefactor)
(require 'srefactor-lisp)
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)



;;;; hs-minor-mode
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'ruby-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)



;;;; yasnippest
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode 1)
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


;;;; C-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; センテンスの終了である ';' を入力したら，自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; RET キーで自動改行+インデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)
             (c-set-style "linux")

             (setq c-basic-offset 4)
             ;; 演算式が複数行にまたがるときのオフセット
             (c-set-offset 'statement-cont 'c-lineup-math)
             (switch-flycheck-or-flymake)
             ))
;; gtags
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; cscope
(add-hook 'c-mode-hook 'helm-cscope-mode)
(add-hook 'c++-mode-hook 'helm-cscope-mode)
(add-hook 'asm-mode-hook 'helm-cscope-mode)

;; irony
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;(require 'company-c-headers)
;;(add-to-list 'company-backends 'company-c-headers)
;;(require 'company-irony)
;;(add-to-list 'company-backends 'company-irony)
;;(require 'company-yasnippet)
;;(add-to-list 'company-backends 'company-yasnippet)
(require 'company-irony-c-headers)
(add-to-list 'company-backends 'company-irony-c-headers)

(delete 'company-semantic company-backends)

(define-key c-mode-map  [(backtab)] 'company-complete)
(define-key c++-mode-map  [(backtab)] 'company-complete)

;; 連想リストの中身を文字列のリストに変更せず、変数そのままの構造を利用。
;; 複数のコンパイルオプションはスペースで区切る
(setq irony-lang-compile-option-alist
      (quote ((c++-mode . "c++ -std=c++11 -lstdc++")
              (c-mode . "c")
              (objc-mode . "objective-c"))))
;; アドバイスによって関数を再定義。
;; split-string によって文字列をスペース区切りでリストに変換
;; (24.4以降 新アドバイス使用)
(defun ad-irony--lang-compile-option ()
  (defvar irony-lang-compile-option-alist)
  (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
    (when it (append '("-x") (split-string it "\s")))))
(advice-add 'irony--lang-compile-option :override #'ad-irony--lang-compile-option)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; コマンドをいちいち確認しない
(setq mode-compile-never-edit-command-p t)
;; メッセージ出力を抑制
(setq mode-compile-expert-p t)
;; メッセージを読み終わるまで待つ時間
(setq mode-compile-reading-time 0)

;; コンパイルが完了したらバッファを閉じる
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 0.3 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose)



;;;; python-mode
(require 'epc)
(require 'python)
(require 'flycheck-mypy)
(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map "\C-m" 'newline-and-indent)
                               (define-key (current-local-map) "\C-h" 'python-backspace)
                               (setq indent-tabs-mode nil)
                               (setq python-indent-offset 4)
                               ))
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'python-mode-hook (lambda () (setq auto-complete-mode nil)))
(require 'jedi-core)
(require 'company-jedi)
(require 'virtualenvwrapper)
(require 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
;(setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-to-list 'company-backends 'company-jedi) ; backendに追加

;;;; haskell-mode
(require 'haskell-mode)
(require 'haskell-cabal)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)



;;;; tex-mode
(defun replace-dot-comma ()
  "s/。/．/g; s/、/，/g;する"
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
(add-hook 'yatex-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'replace-dot-comma nil 'make-it-local)
             (flyspell-mode t)
             ))

(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; 拡張子が .tex なら yatex-mode に
(setq auto-mode-alist
  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX が利用する内部コマンドを定義する
(cond
  ((eq system-type 'gnu/linux) ;; GNU/Linux なら
    (setq dvi2-command "evince")) ;; evince で PDF を閲覧
  ((eq system-type 'darwin) ;; Mac なら
    (setq dvi2-command "open -a Preview"))) ;; プレビューで
(add-hook 'yatex-mode-hook '(lambda () (setq auto-fill-function nil)))


;;;; bison-mode
(require 'bison-mode)
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l$" . bison-mode))



;;;; markdown(orgtbl)
(require 'org-table)
(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                               org-table-last-alignment ""))
         (params2
          (list
           :splice t
           :hline (concat alignment "|")
           :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))
