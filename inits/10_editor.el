;; 対応する括弧をハイライトする
(show-paren-mode 1)

;; 対応する括弧の色の設定
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

;; smartparensの有効化
(require 'smartparens)
(smartparens-global-mode t)

;; 行番号表示
(global-linum-mode t)
(column-number-mode t)
(line-number-mode t)

;; C-h を Backspace として使い，
;; C-c h を HELP に割当てる
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
(global-set-key "\C-ch" 'help-command)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; 全てバッファを自動的にセーブする
(setq mode-compile-always-save-buffer-p t)

;; オートコンプリートの有効化
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

;; 空白文字の設定
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
;; 全角スペースの表示設定
(set-face-foreground 'whitespace-space "DarkGoldenrod1")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
;; タブの表示設定
(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
(set-face-background 'whitespace-tab nil)
(set-face-underline  'whitespace-tab t)
;; 全角スペースのみ表示する
(setq whitespace-space-regexp "\\(\x3000+\\)")
;; 全角スペースやタブを表現する文字
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(global-whitespace-mode 1) ; 全角スペースを常に表示

;; undo-treeの有効化
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; undohistの有効化
(require 'undohist)
(undohist-initialize)

;; anzuの有効化
(require 'anzu)
(global-anzu-mode +1)

;; expand regionの設定
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region) ;; リージョンを狭める
(transient-mark-mode t)

;; visual-regexpの設定
(require 'visual-regexp)
(setq vr/engine 'pcre2el)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)

;; google-translateの設定
;; キーバインドの設定
(require 'google-translate)
(global-set-key "\C-xt" 'google-translate-at-point)
(global-set-key "\C-xT" 'google-translate-query-translate)

;; 翻訳のデフォルト値を設定（en -> ja）
(custom-set-variables
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja"))

(require 'popwin)
;; おまじない（よく分かってない）
(setq display-buffer-function 'popwin:display-buffer)
;; ポップアップを画面下に表示
(setq popwin:popup-window-position 'bottom)

;; google-translate.elの翻訳バッファをポップアップで表示させる
(push '("*Google Translate*") popwin:special-display-config)

;; smartrepとmultiple-cursesの設定
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


;; dired
;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)
