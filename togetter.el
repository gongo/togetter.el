;;; cui-about-me.el -- control for cui-about.me
;;
;; MAHALO License (based on MIT License)
;;
;; Copyright (c) 2012 Wataru MIYAGUNI (gonngo _at_ gmail.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; 1. The above copyright notice and this permission notice shall be included in
;;    all copies or substantial portions of the Software.
;; 2. Shall be grateful for something (including, but not limited this software).
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Usage:
;;
;;


(eval-when-compile (require 'cl))
(require 'xml)
(require 'helm)

(defvar togetter->uri "http://togetter.com/li/")
(defvar togetter->buffer-name "*Togetter*")
(defvar togetter->hot-source-buffer-name "*Togetter Hot Source*")
(defvar togetter->rec-source-buffer-name "*Togetter Recent Source*")
(defvar togetter->tmp-buffer-name "*Togetter Temporary*")
(defvar togetter->show-template "\

%s

by [[%s][%s]] at [[%s][%s]]
-----------------------------------------------------------------
")

(defun togetter:lrtrim (text)
  (replace-regexp-in-string "\\`\\(?:\\s-\\|\n\\)+\\|\\(?:\\s-\\|\n\\)+\\'" "" text))

(defun togetter:get-href-from (anode)
  (cdr (assq 'href (cadr anode))))

(defun togetter:find-tag-by-class (tree class &optional tag)
  (if (and
       (string-equal class (xml-get-attribute tree 'class))
       (if (null tag) t (string-equal tag (xml-node-name tree))))
      tree
    (loop for child in (xml-node-children tree)
          when (listp child)
          when (togetter:find-tag-by-class child class tag)
          collect (if (= (length it) 1) (car it) it))))

(defun togetter:tweet-readjust (tweet-node)
  (mapconcat
   (lambda (node)
     (if (listp node)
         (cond
          ((eq 'a (xml-node-name node))
           (format "[[%s][%s]]" (togetter:get-href-from node) (caddr node)))
          (t
           (togetter:tweet-readjust (xml-node-children node))))
       (togetter:lrtrim node)))
   tweet-node " "))

(defun togetter:get-tweet-block (item)
  (let* ((tweet (cddar (togetter:find-tag-by-class item "tweet")))
         (info  (xml-get-children
                 (car (togetter:find-tag-by-class item "status_right")) 'a))
         (name-node   (pop info))
         (name-label  (caddr name-node))
         (name-link   (togetter:get-href-from name-node))
         (status-node (pop info))
         (status-date (caddr status-node))
         (status-link (togetter:get-href-from status-node)))
    (if tweet
        (format togetter->show-template
                (togetter:tweet-readjust tweet)
                name-link name-label status-link status-date)
      "")))

(defun togetter:parse-tweet-box (&optional buffer)
  "Get \"li.list_item\" nodes"
  (if (null buffer) (setq buffer togetter->tmp-buffer-name))
  (with-current-buffer (get-buffer buffer)
    (let (begin end xml)
      (goto-char (point-max))
      (setq end (search-backward "<div class=\"social_box"))
      (setq begin (search-backward "<div class=\"tweet_box"))
      (setq xml (xml-parse-region begin end))
      (xml-get-children (car (xml-get-children (car xml) 'ul)) 'li))))

(defun togetter:parse-title (&optional buffer)
  "Get title.

全体を xml-parse-region しようかと思ったが、
xml error になるので自力で
"
  (if (null buffer) (setq buffer togetter->tmp-buffer-name))
  (with-current-buffer (get-buffer buffer)
    (let (begin end xml)
      (goto-char (point-min))
      (setq begin (search-forward "<title>"))
      (search-forward "</head>")
      (setq end (search-backward "</title>"))
      (buffer-substring-no-properties begin end))))

(defun togetter:get-page (uri &optional buffer)
  (if (null buffer) (setq buffer togetter->tmp-buffer-name))
  (ignore-errors (kill-buffer buffer))
  (call-process "curl" nil `(,buffer nil) nil
                "-f"
                "-X" "GET" uri))

(defun togetter:show (uri title items)
  (with-current-buffer (get-buffer-create togetter->buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "[[%s][%s]]\n\n" uri title))
      (dolist (i items)
        (insert (togetter:get-tweet-block i)))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (org-mode)
      (view-mode))
  (switch-to-buffer togetter->buffer-name))

(defun togetter:run (uri)
  (togetter:get-page uri)
  (togetter:show uri (togetter:parse-title) (togetter:parse-tweet-box)))

(defun togetter (id)
  (interactive "sTogetter id: ")
  (togetter:run (concat togetter->uri id)))

(defun togetter:make-rss-list (uri)
  (let (rss channel items)
    (togetter:get-page uri togetter->tmp-buffer-name)
    (setq rss
          (with-current-buffer (get-buffer togetter->tmp-buffer-name)
            (car (xml-parse-region (point-min) (point-max)))))
    (setq channel (car (xml-get-children rss 'channel)))
    (loop for item in (xml-get-children channel 'item)
      for link  = (caddar (xml-get-children item 'link))
      for title = (caddar (xml-get-children item 'title))
      collect (cons title link))))

(defun togetter:hot-list ()
  (togetter:make-rss-list "http://togetter.com/rss/index"))

(defun togetter:recent-list ()
  (togetter:make-rss-list "http://togetter.com/rss/recent"))

(defun helm-togetter-hot ()
  (interactive)
  (helm :sources `((name . "Togetter Hot Entry")
                   (candidates . ,(togetter:hot-list))
                   (action
                    ("View togetter" . (lambda (x) (togetter:run x)))
                    ("View togetter on browser"  . (lambda (x) (browse-url x)))
                    ))
        :buffer togetter->hot-source-buffer-name
        ))

(defun helm-togetter-recent ()
  (interactive)
  (helm :sources `((name . "Togetter Recent Entry")
                   (candidates . ,(togetter:recent-list))
                   (action
                    ("View togetter" . (lambda (x) (togetter:run x)))
                    ("View togetter on browser"  . (lambda (x) (browse-url x)))
                    ))
        :buffer togetter->rec-source-buffer-name
        ))

(provide 'togetter-el)
