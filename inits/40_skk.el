(require 'skk)
(setq skk-server-host "localhost") ; AquaSKK のサーバー機能を利用
(setq skk-server-portnum 1178)	   ; ポートは標準
(setq skk-share-private-jisyo t)   ; 複数 skk 辞書を共有
(setq skk-tut-file "~/Dropbox/emacs/SKK/SKK.tut")
;; (setq skk-tut-file "~/.emacs.d/SKK.tut")
;; (setq skk-show-tooltip t)
;; (setq skk-inline-show t)
(setq skk-show-annotation t)
(setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
(setq skk-henkan-show-candidates-rows 2) ; 候補表示件数を2列に
(setq skk-search-katakana t)

;; 動的候補表示
(setq skk-dcomp-activate t)			 ; 動的補完
(setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
(setq skk-dcomp-multiple-rows 10)	 ; 動的補完の候補表示件数
;; 動的補完の複数表示群のフェイス
(set-face-foreground 'skk-dcomp-multiple-face "Black")
(set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
(set-face-bold-p 'skk-dcomp-multiple-face nil)
;; 動的補完の複数表示郡の補完部分のフェイス
(set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
(set-face-bold-p 'skk-dcomp-multiple-trailing-face nil)
;; 動的補完の複数表示郡の選択対象のフェイス
(set-face-foreground 'skk-dcomp-multiple-selected-face "White")
(set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
(set-face-bold-p 'skk-dcomp-multiple-selected-face nil)
(require 'skk-hint)							; ヒント
(setq skk-hint-start-char 58); hintを:に
(add-hook 'skk-load-hook ; 自動的に入力モードを切り替え
          (lambda ()
            (require 'context-skk)))

(global-set-key (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
(setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
(require 'skk-study)                              ;;変換学習機能の追加
(setq skk-sticky-key ";")
(setq skk-jisyo-code 'utf-8)
(setq skk-user-directory "~/Dropbox/emacs/SKK")
(setq skk-large-jisyo "~/Dropbox/emacs/SKK/SKK-JISYO.L.utf8")
(setq skk-record-file "~/Dropbox/emacs/SKK/skk-record.utf8")
(setq skk-emacs-id-file "~/Dropbox/emacs/SKK/skk-emacs-id.utf8")
(setq skk-jisyo "~/Dropbox/emacs/SKK/skk-jisyo.utf8")
(setq skk-backup-jisyo "~/Dropbox/emacs/SKK/skk-jisyo.utf8.bak")
(setq skk-study-file "~/Dropbox/emacs/SKK/skk-study.utf8")
(setq skk-study-backup-file "~/Dropbox/emacs/SKK/skk-study.utf8.bak")

(setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '(
                ("wha" nil ("うぁ" . "ウァ"))
                ("whi" nil ("うぃ" . "ウィ"))
                ("whu" nil ("う" . "ウ"))
                ("whe" nil ("うぇ" . "ウェ"))
                ("who" nil ("うぉ" . "ウォ"))
                )))

(setq skk-kutouten-type 'en)
