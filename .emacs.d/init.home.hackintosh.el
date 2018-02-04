(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; 初期化
(package-initialize)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ howm                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;howmメモ保存の場所
(setq howm-directory(concat user-emacs-directory "/../Dropbox/howm"))
;;howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
(setq howm-file-name-format "memo/%Y-%m-%d-%H%M%S.txt")
;;howm-modeを読み込む
;(when (require 'howm-mode nil t)
   ;; C-c,,でhowm-menuを起動
   ;(define-key global-map (kbd "C-c ,,") 'howm-menu))
(global-set-key "\C-c,," 'howm-menu)
(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
     '(
             howm-menu 
             howm-list-all 
             howm-list-recent 
             howm-list-grep 
             howm-create 
             howm-keyword-to-kill-ring))
;;M-x calendar で好きな日付けを選び，RETとします
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map
       "\C-m" 'my-insert-day)
     (defun my-insert-day ()
       (interactive)
       (let ((day nil)
             (calendar-date-display-form
         '("[" year "-" (format "%02d" (string-to-int month))
           "-" (format "%02d" (string-to-int day)) "]")))
         (setq day (calendar-date-string
                    (calendar-cursor-to-date t)))
         (exit-calendar)
         (insert day)))))
;;今日の日付けの簡易入力
(defun my-get-date-gen (form)(insert (format-time-string form)))
(defun my-get-date ()(interactive)(my-get-date-gen "[%Y-%m-%d]"))
(defun my-get-time ()(interactive)(my-get-date-gen "[%H:%M]"))
(defun my-get-dtime ()(interactive)(my-get-date-gen "[%Y-%m-%d %H:%M]"))
(global-set-key "\C-c\C-d" 'my-get-date)
(global-set-key "\C-c\C-t" 'my-get-time)
;Ctrl-c ESC d
(global-set-key "\C-c\ed" 'my-get-dtime)
;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)
;; いちいち消すのも面倒なので
;; 内容が 0 ならファイルごと削除する
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         ;(string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
;;C-c C-c → 現バッファの内容を保存してバッファを消す
(defun my-save-and-kill-buffer ()
  (interactive)
  (save-buffer)
  (kill-buffer nil))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; ヘッドホンを付けて集中したい時など、ビープ音が邪魔な時ってありますよね。
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 警告音の代わりに画面フラッシュ
(setq visible-bell t)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
;(setq ring-bell-function 'ignore)

;;; テキスト折り返しを無効化
(setq-default truncate-lines t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-whitespace-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; メニューバーにファイルパスを表示する
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; Ctrl-H をバックスペースに割り当てる
(global-set-key "\C-h" 'backward-delete-char)
;; [Home] [End] キーでこれまではバッファの先頭、末尾に移動
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(create-fontset-from-ascii-font "Inconsolata-12:weight=normal:slant=normal" nil "myfavoritefontset")
(set-fontset-font "fontset-myfavoritefontset"
		  'japanese-jisx0208
		  (font-spec :family "TakaoExGothic" :size 12)
		  nil
		  'append)
(add-to-list 'default-frame-alist '(font . "fontset-myfavoritefontset"))
(setq face-font-rescale-alist
	'(("^-apple-hiragino.*" . 1.2)
	  (".*osaka-bold.*" . 1.2)
	  (".*osaka-medium.*" . 1.2)
	  (".*courier-bold-.*-mac-roman" . 1.0)
	  (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	  (".*monaco-bold-.*-mac-roman" . 0.9)
	  ("-cdac$" . 1.3)
	  (".*Inconsolata.*" . 1.0)))
