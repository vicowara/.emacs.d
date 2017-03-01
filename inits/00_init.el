;; 日本語化
(set-language-environment "Japanese")

;; emacsサーバ起動
(require 'server)
(unless (server-running-p)
  (server-start))

;; ツールバーを無効化
(tool-bar-mode 0)

;; スタートアップメッセージ非表示
(setq inhibit-startup-message t)

;; ゴールカラムの無効化
(put 'set-goal-column 'disabled nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; twittering-modeの設定
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; multi-termの設定
(require 'multi-term)
(require 'shell-pop)
(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (multi-term)))
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)
(global-set-key (kbd "C-c C-t") 'shell-pop)

;; trampでssh先のdir-locals.elを有効にする
(setq enable-remote-dir-locals 1)
