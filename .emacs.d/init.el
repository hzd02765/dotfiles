; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ load-path

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;; ↑　参考　http://d.hatena.ne.jp/sandai/20120304/p2

;;; 言語を日本語にする
(set-language-environment 'Japanese)
;;; 極力UTF-8とする
(prefer-coding-system 'utf-8)
;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)
;;; 行番号を表示
;(global-linum-mode t)
;(setq linum-format "%4d ")

;;; EmacsでC-hでmini buffer内でもbackspaceする
(keyboard-translate ?\C-h ?\C-?)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ howm                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;howmメモ保存の場所
(setq howm-directory(concat user-emacs-directory "/../Dropbox/howm"))
;;howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.txt")
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

