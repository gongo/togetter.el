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

(defvar togetter->more-loading-str "[残りを読み込み中...]")
(defvar togetter->timeout 10
  "Timeout time in seconds that `togetter:get-page'")

(defconst togetter->base-uri "http://togetter.com"
  "The base URI on Togetter.")
(defconst togetter->api-uri (concat togetter->base-uri "/api/moreTweets")
  "API URI that get more tweet") ;212550?page=
(defconst togetter->buffer-name "*Togetter*"
  "Buffer name to display tweets.")
(defconst togetter->source-buffer-name "*Togetter Source*"
  "`helm' source buffer name")
(defconst togetter->tmp-buffer-name "*Togetter Temporary*"
  "Temporary buffer name")
(defconst togetter->show-template "\

%s

by [[%s][%s]] at [[%s][%s]]
-----------------------------------------------------------------
" "\
Template view of tweet.

1. tweet.
2. link of author
3. name of author
4. link of tweet
5. date of tweet
")

;; Initialize
;;
;; TODO 実は変数宣言時に属性つけれたりする?
(put-text-property 0 (length togetter->more-loading-str)
                   'togetter-loading-str t
                   togetter->more-loading-str)

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

(defun togetter:tweet-item-to-string (item)
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

(defun togetter:tweet-box-to-string (list)
  (mapconcat 'togetter:tweet-item-to-string list "\n"))

(defun togetter:parse-tweet-box ()
  "Parse \"li.list_item\" tags in current buffer."
  (let (begin end xml)
    (goto-char (point-max))
    (setq end (search-backward "<div class=\"social_box"))
    (setq begin (search-backward "<div class=\"tweet_box"))
    (setq xml (xml-parse-region begin end))
    (xml-get-children (car (xml-get-children (car xml) 'ul)) 'li)))

(defun togetter:parse-title (uri)
  "\
Parse <title> tag in current buffer,
and convert to link string (org-mode format).

  example:

  (let ((uri \"http://togetter.com/li/248076\"))
    (with-temp-buffer
      (togetter:get-page uri (current-buffer))
      (togetter:parse-title uri)))

  ;; => \"[[http://togetter.com/li/248076][#地獄の美夢沢 - Togetter]]\""
  (let (begin end title)
    (goto-char (point-min))
    (setq begin (search-forward "<title>"))
    (search-forward "</head>")
    (setq end (search-backward "</title>"))
    (setq title (buffer-substring-no-properties begin end))
    (format "[[%s][%s]]" uri title)))

(defun togetter:search-text-with-property-in-current-buffer (prop)
  (let (beg end)
    (save-excursion
      (goto-char (point-max))
      (setq end (previous-single-property-change (point) prop))
      (goto-char end)
      (setq beg (previous-single-property-change (point) prop))
      (unless beg
        (setq beg end
              end (point-max)))
      (cons beg end))))

(defun togetter:start-more-tweet-load (id page)
  (let ((current-point (point))
        (inhibit-read-only t)
        (region (togetter:search-text-with-property-in-current-buffer
                 'togetter-more-button))
        (uri (concat togetter->api-uri "/" id "?page=" page))
        xml tweet-items more)

    ;; print loading message
    (delete-region (car region) (cdr region))
    (insert togetter->more-loading-str)

    ;; loading start
    (with-current-buffer togetter->tmp-buffer-name
      (togetter:get-page uri togetter->tmp-buffer-name)
      (setq xml (car (xml-parse-region (point-min) (point-max))))
      (setq tweet-items (xml-get-children xml 'li))
      (setq more (togetter:parse-more-tweet-box)))

    ;; insert more tweets
    (setq region (togetter:search-text-with-property-in-current-buffer
                  'togetter-loading-str))
    (delete-region (car region) (cdr region))
    (insert (togetter:tweet-box-to-string tweet-items))
    (when more (insert more))

    (goto-char current-point)))

(defun togetter:add-togetter-more-text-property (str _id _page)
  (lexical-let ((id _id) (page _page))
    (let ((map (make-sparse-keymap))
          (func (lambda () (interactive) (togetter:start-more-tweet-load id page))))
      (define-key map [return] func)
      (define-key map [mouse-1] func)
      (define-key map [down-mouse-1] 'mouse-set-point)
      (add-text-properties 0 (length str)
                           (list 'local-map map
                                 'togetter-more-button t)
                           str)
      str)))

(defun togetter:parse-more-tweet-box ()
  "\
Search div#more_tweet_box_xxxxx (xxxxx = id) in current buffer,
and return text that load more tweet box in current buffer if click.

If not found, return nil."
  (let (begin end atag onclick loadstr page-id page-num)
    (goto-char (point-min))
    (when (search-forward "div id=\"more_tweet_box_" nil t)
      (setq end (search-forward "</a>"))
      (setq begin (search-backward "<a "))
      (setq atag (car (xml-parse-region begin end)))

      (setq loadstr (concat "[" (nth 2 atag) "]"))
      (setq onclick (xml-get-attribute atag 'onclick))
      (string-match "tgtr.moreTweets(\\([0-9]+\\),\\([0-9]+\\)" onclick)
      (setq page-id  (match-string 1 onclick))
      (setq page-num (match-string 2 onclick))

      (togetter:add-togetter-more-text-property loadstr page-id page-num))))

(defun togetter:get-page (uri &optional buffer)
  (if (null buffer) (setq buffer togetter->tmp-buffer-name))
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (call-process "curl" nil `(,buffer nil) nil
                "-f"
                "-X" "GET" "-m" (number-to-string togetter->timeout) uri))

(defun togetter:parse (uri &optional buffer)
  (if (null buffer) (setq buffer togetter->tmp-buffer-name))
  (with-current-buffer buffer
    (togetter:get-page uri buffer)
    (list (togetter:parse-title uri)
          (togetter:parse-tweet-box)
          (togetter:parse-more-tweet-box))))

(defun togetter:show (title items more)
  (with-current-buffer (get-buffer-create togetter->buffer-name)
    (erase-buffer)
    (insert title "\n\n" (togetter:tweet-box-to-string items))
    (org-mode)
    (view-mode)
    (when more
      (let ((inhibit-read-only t))
        (insert more)))
    (goto-char (point-min)))
  (switch-to-buffer togetter->buffer-name))

(defun togetter:run (uri)
  (let* ((result (togetter:parse uri))
         (title  (nth 0 result))
         (items  (nth 1 result))
         (more   (nth 2 result)))
    (togetter:show title items more)))

(defun togetter (id)
  (interactive "sTogetter id: ")
  (togetter:run (concat togetter->base-uri "/li/" id)))

(defun togetter:make-rss-list (uri)
  (let (rss channel items)
    (togetter:get-page uri)
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
        :buffer togetter->source-buffer-name
        ))

(defun helm-togetter-recent ()
  (interactive)
  (helm :sources `((name . "Togetter Recent Entry")
                   (candidates . ,(togetter:recent-list))
                   (action
                    ("View togetter" . (lambda (x) (togetter:run x)))
                    ("View togetter on browser"  . (lambda (x) (browse-url x)))
                    ))
        :buffer togetter->source-buffer-name
        ))

(provide 'togetter-el)
