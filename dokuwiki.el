;;; dokuwiki.el --- Edit Remote DokuWiki Pages Using XML-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Juan Karlo Licudine
;; additional edits
;;   2021 vincowl
;;   2018, 2023 WillForan
;;   2023-2024 Alexis <flexibeast@gmail.com>
;;   2025 esuarezsantana

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; URL: http://www.github.com/accidentalrebel/emacs-dokuwiki
;; Version: 1.2.2
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3") (xml-rpc "1.6.8"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a way to edit a remote Dokuwiki wiki on Emacs.
;; Uses Dokuwiki's XML-RPC API.

;; Usage:
;; (require 'dokuwiki) ;; unless installed as a package

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'xml-rpc)
(require 'ffap)
(require 'auth-source)
(require 'cl-lib)

(defgroup dokuwiki nil
  "Edit remote Dokuwiki pages using XML-RPC."
  :group 'dokuwiki)

(defcustom dokuwiki-xml-rpc-url ""
  "The url pointing to the \"xmlrpc.php\" file in the wiki to be accessed."
  :group 'dokuwiki
  :type 'string)

(defcustom dokuwiki-local-directory
  (cond
   ((eq system-type 'windows-nt)
    (substitute-in-file-name
     "%APPDATA%\dokuwiki\\"))
   ((getenv "XDG_DATA_HOME")
    (substitute-in-file-name
     "$XDG_DATA_HOME/dokuwiki/"))
   ((getenv "HOME")
    (substitute-in-file-name
     "$HOME/dokuwiki/"))
   (t
    "./dokuwiki/"))
  "Directory in which to save local copies of DokuWiki pages."
  :group 'dokuwiki
  :type 'directory)

(defcustom dokuwiki-login-user-name ""
  "The user name to use when logging in to the wiki."
  :group 'dokuwiki
  :type 'string)

(defcustom dokuwiki-page-opened-hook ()
  "Functions to run upon opening a wiki page."
  :group 'dokuwiki
  :type '(repeat function))

(defcustom dokuwiki-preferred-mode-alist nil
  "Alist of major modes to use when a predicate is true, together with
the relevant extension.

Each entry in the alist must be of the form:

  (MODE (PREDICATE . EXT))

where MODE is a major mode, PREDICATE is a function that returns t if
a page should be opened in that mode, and EXT is a string containing
the file extension to use with that mode. For example:

  (gemini-mode (dokuwiki-gemini-p . \"gmi\")))"
  :type '(alist
          :key-type function
          :value-type (list function string)))

(defcustom dokuwiki-save-local-copy nil
  "Whether to save a copy of the page into `dokuwiki-local-directory'
when saving."
  :group 'dokuwiki
  :type 'boolean)

(defcustom dokuwiki-use-dokuwiki-mode t
  "Whether to enable `dokuwiki-mode' upon opening a wiki page."
  :group 'dokuwiki
  :type 'boolean)

(defcustom dokuwiki-use-preferred-modes nil
  "Whether to use `dokuwiki-preferred-mode-alist' when editing wiki pages."
  :group 'dokuwiki
  :type 'boolean)

(defconst dokuwiki--dwiki-suffix-regexp "\\.dwiki\\'"
  "Regexp matching the `.dwiki' suffix on wiki edit buffers.")

(defvar-local dokuwiki--buffer-setup nil
  "Non-nil when `dokuwiki-setup' has been run in the current buffer.")

(defun dokuwiki--dwiki-buffer-p (&optional buffer)
  "Return t if BUFFER (defaults to current) is a `.dwiki' wiki edit buffer."
  (string-match-p dokuwiki--dwiki-suffix-regexp (buffer-name buffer)))

(defun dokuwiki--page-id-from-buffer-name (&optional buffer)
  "Return the wiki page id implied by BUFFER's name (defaults to current)."
  (replace-regexp-in-string dokuwiki--dwiki-suffix-regexp ""
                            (buffer-name buffer)))

(defun dokuwiki--xmlrpc-authenticate ()
  "Establish an XML-RPC session with the wiki.  Returns non-nil on success."
  (let* ((credentials (dokuwiki--credentials))
         (login-user-name (plist-get credentials :user))
         (login-password (plist-get credentials :password)))
    (dokuwiki--xmlrpc-login login-user-name login-password)))

(defun dokuwiki--maybe-enable-dokuwiki-mode ()
  "Register `dokuwiki-mode' on `dokuwiki-page-opened-hook' when configured."
  (when dokuwiki-use-dokuwiki-mode
    (if (featurep 'dokuwiki-mode)
        (unless (member #'dokuwiki-mode dokuwiki-page-opened-hook)
          (add-hook #'dokuwiki-page-opened-hook #'dokuwiki-mode))
      (warn "dokuwiki-mode is not installed; install it or set `dokuwiki-use-dokuwiki-mode' to nil"))))

;;;###autoload
(defun dokuwiki-login ()
  "Connect to the wiki and optionally enable `dokuwiki-mode' on open."
  (interactive)
  (dokuwiki--get-xml-rpc-url)
  (if (not (dokuwiki--xmlrpc-authenticate))
      (error "Login unsuccessful! Check if your dokuwiki-xml-rpc-url or login credentials are correct!")
    (message "Login successful!")
    (dokuwiki--maybe-enable-dokuwiki-mode)))

(defun dokuwiki--insert-top-heading (page-name)
  "When there's no content, insert a top level heading matching PAGE-NAME."
  (insert "====== " (replace-regexp-in-string ".*:" "" page-name) " ======"))

;;;###autoload
(defun dokuwiki-open-page (page-name-or-url)
  "Opens a page from the wiki.

PAGE-NAME-OR-URL: The page id or url to open.

To open a page in a particular namespace add the namespace name before
the page-name.  For example, \"namespace:wiki-page\" to open the
\"wiki-page\" page inside the \"namespace\" namespace.

If the specified page does not exist, it creates a new page once the
buffer is saved."
  (interactive "sEnter page name: ")
  (let* ((page-name (car (last (split-string page-name-or-url "/"))))
         (page-content (dokuwiki--xmlrpc-call 'wiki.getPage page-name)))
    (message "Page name is \"%s\"" page-name)
    (if (not page-content)
      (message "Page not found in wiki. Creating a new buffer with page name \"%s\"" page-name)
      (message "Page exists. Creating buffer for existing page \"%s\"" page-name))
    (get-buffer-create (concat page-name ".dwiki"))
    (switch-to-buffer (concat page-name ".dwiki"))
    (erase-buffer)
    (if page-content (insert page-content) (dokuwiki--insert-top-heading page-name))
    (goto-char (point-min))
    (if dokuwiki-use-preferred-modes
      (progn
        (dokuwiki--create-preferred-mode-maps)
        (dokuwiki--enable-preferred-mode)))
    (run-hooks #'dokuwiki-page-opened-hook)))

(defun dokuwiki-save ()
  "Wrapper for `dokuwiki-save-page', saving local copy of page if
  `dokuwiki-save-local-copy' is set to t."
  (interactive)
  (if (not dokuwiki-save-local-copy)
    (dokuwiki-save-page)
    (let* ((mode major-mode)
           (conspath (dokuwiki-path))
           (namespace (car conspath))
           (page-name (cdr conspath))
           (dir (if namespace
                    (apply #'file-name-concat dokuwiki-local-directory namespace)
                  dokuwiki-local-directory))
           (ext (or (let (value)
                      (dolist (entry dokuwiki-preferred-mode-alist)
                        (if (eq (car entry) mode)
                          (setq value (cadadr entry))))
                      value)
                    "dwiki"))
           (file (concat page-name "." ext))
           (local-file (file-name-concat dir file)))
      (make-directory dir t)
      (write-file local-file)
      (set-visited-file-name local-file)
      (funcall mode)
      (dokuwiki-save-page))))

(defun dokuwiki-save-page ()
  "Save the current buffer as a page in the wiki.

Uses the buffer name as the page name.  A buffer of \"wiki-page.dwiki\"
is saved as \"wikiurl.com/wiki-page\".  On the other hand, a buffer of
\"namespace:wiki-page.dwiki\" is saved as \"wikiurl.com/namespace:wiki-page\""
  (interactive)
  (unless (dokuwiki--dwiki-buffer-p)
    (error "The current buffer is not a .dwiki buffer"))
  (let ((page-name (dokuwiki--page-id-from-buffer-name)))
    (unless (y-or-n-p (concat "Do you want to save the page \"" page-name "\"?"))
      (user-error "Cancelled saving of the page"))
    (let* ((summary (read-string "Summary: "))
           (minor (y-or-n-p "Is this a minor change? "))
           (save-success
            (dokuwiki--xmlrpc-call 'wiki.putPage page-name (buffer-string)
                                   `(("sum" . ,summary) ("minor" . ,minor)))))
      (if save-success
          (message "Saving successful with summary %s and minor of %s." summary minor)
        (error "Saving unsuccessful!")))))

(defun dokuwiki-get-wiki-title ()
  "Gets the title of the current wiki."
  (interactive)
  (let ((dokuwiki-title (dokuwiki--xmlrpc-call 'dokuwiki.getTitle)))
    (message "The title of the wiki is \"%s\"" dokuwiki-title)))

(defun dokuwiki--fetch-page-ids ()
  "Return a list of page id strings from the remote wiki."
  (mapcar (lambda (page-detail) (cdr (assoc "id" page-detail)))
          (dokuwiki--xmlrpc-call 'wiki.getAllPages)))

(defun dokuwiki-get-page-list ()
  "Return page ids from the remote wiki."
  (dokuwiki--fetch-page-ids))

(defun dokuwiki-list-pages ()
  "Show a selectable list containing pages from the current wiki.  Not cached."
  (interactive)
  (dokuwiki-open-page (completing-read "Select a page to open: " (dokuwiki-get-page-list))))

(defun dokuwiki-path ()
  "Get the path of the current page as (NAMESPACE . PAGE-NAME).
NAMESPACE is nil for top-level pages (e.g. buffer \"start.dwiki\")."
  (let ((bfr-name (buffer-name)))
    (cond
     ((string-match "^\\(.+\\):\\(.+\\)\\.dwiki$" bfr-name)
      (cons (match-string 1 bfr-name) (match-string 2 bfr-name)))
     ((string-match "^\\(.+\\)\\.dwiki$" bfr-name)
      (cons nil (match-string 1 bfr-name)))
     (t
      (error "Not a dokuwiki buffer: %s" bfr-name)))))

(defun dokuwiki-insert-link-from-list ()
  "Insert link from wiki page list.  Not cached."
  (interactive)
  (insert (concat "[[" (completing-read "Select a page to link: " (dokuwiki-get-page-list)) "]]")))

;; Helpers
(defun dokuwiki--xmlrpc-login (username password)
  "Login to the Dokuwiki XML-RPC API.
USERNAME is the username to use for logging in.
PASSWORD is the password to use for logging in.
Returns the result of the XML-RPC call, or nil if the login failed."
  (xml-rpc-method-call (dokuwiki--get-xml-rpc-url)
                       'dokuwiki.login
                       username
                       password))

(defun dokuwiki--xmlrpc-call (method &rest params)
  "Call XML-RPC method with optional authentication disabling.

METHOD is the XML-RPC method to call (e.g. 'wiki.getPage).
PARAMS are the optional parameters to pass to the method as multiple arguments.

Returns the result of xml-rpc-method-call."
  (let ((rpc-url (dokuwiki--get-xml-rpc-url))
        (call-func nil))
    (setq call-func
          (lambda ()
            (apply #'xml-rpc-method-call (cons rpc-url (cons method params)))))
    ;; nullify 'url-get-authentication' to avoid authentication prompts
    (cl-letf (((symbol-function 'url-get-authentication)
               (lambda (&rest _args) nil)))
             (condition-case err
                             ;; First try
                             (funcall call-func)
                             (error
                               (if (string-match "401\\|403\\|Unauthorized\\|Forbidden"
                                                 (error-message-string err))
                                 (progn
                                   (message "Authentication required. Logging in...")
                                   (unless (dokuwiki--xmlrpc-authenticate)
                                     (error "Login unsuccessful! Check dokuwiki-xml-rpc-url and credentials"))
                                   (funcall call-func))
                                 (signal (car err) (cdr err))))))))

(defun dokuwiki--create-preferred-mode-maps ()
  "Create keymaps for editing a page with a particular mode by
inheriting the usual keymap for that mode, but binding \\`C-x C-s' to
the `dokuwiki-save' wrapper function."
  (if dokuwiki-preferred-mode-alist
      (dolist (entry dokuwiki-preferred-mode-alist)
        (let* ((mode (car entry))
               (mode-name (symbol-name mode))
               (root (substring mode-name 0 -5))
               (sym (intern (concat "dokuwiki-" root "-map"))))
          (set sym (make-sparse-keymap))
          (set-keymap-parent (symbol-value sym)
                             (symbol-value (intern (concat mode-name "-map"))))
          (keymap-set (symbol-value sym)
                      "C-x C-s" #'dokuwiki-save)))))

(defun dokuwiki--credentials ()
  "Read dokuwiki credentials either from auth source or from the user input."
  (let* ((parsed-uri (url-generic-parse-url (dokuwiki--get-xml-rpc-url)))
         (auth-source-credentials
          (nth
           0
           (auth-source-search
            :max 1
            :host (url-host parsed-uri)
            :port (url-port parsed-uri)
            :require '(:user :secret)))))
    (if auth-source-credentials
        (let* ((user (plist-get auth-source-credentials :user))
               (password-raw (plist-get auth-source-credentials :secret))
               (password (if (functionp password-raw) (funcall password-raw) password-raw)))
          (list :user user :password password))
      (let ((user (dokuwiki--get-login-user-name))
            (password (read-passwd "Enter password: ")))
        (list :user user :password password)))))

(defun dokuwiki--enable-preferred-mode ()
  "Enable the first major mode in `dokuwiki-preferred-mode-alist' whose predicate passes."
  (cl-loop for entry in dokuwiki-preferred-mode-alist
           for mode = (car entry)
           for fun = (caadr entry)
           when (funcall fun)
           do (progn
                (funcall mode)
                (let* ((mode-name (symbol-name mode))
                       (root (substring mode-name 0 -5)))
                  (use-local-map
                   (symbol-value
                    (intern (concat "dokuwiki-" root "-map")))))
           and return t)))

(defun dokuwiki--get-xml-rpc-url ()
  "Gets the xml-rpc to be used for logging in."
  (if (string= dokuwiki-xml-rpc-url "")
    (error "Please set the 'dokuwiki-xml-rpc-url' variable before proceeding.")
      dokuwiki-xml-rpc-url))

(defun dokuwiki--get-login-user-name ()
  "Gets the login user name to be used for logging in."
  (if (not (string= dokuwiki-login-user-name ""))
      dokuwiki-login-user-name
    (let ((login-name (read-string "Enter login user name: ")))
      (message "The entered login user name is \"%s\"." login-name)
      login-name)))

;; caching
(defvar dokuwiki-cached-page-list nil
  "List of wiki page id strings cached for listing and link completion.")

;;;###autoload
(defun dokuwiki-pages-get-list-cache (&optional refresh)
  "Get list of page; if cache is unset or REFRESH, fetch."
  (when (or (not dokuwiki-cached-page-list) refresh)
    (setq dokuwiki-cached-page-list (dokuwiki--fetch-page-ids))
    (message "Cached page list updated.")
    (sit-for 1))
  dokuwiki-cached-page-list)

(defun dokuwiki--page-link-candidates ()
  "Return page ids prefixed with \":\" for link completion and insertion."
  (mapcar (lambda (id) (concat ":" id)) dokuwiki-cached-page-list))

(defun dokuwiki-insert-link-from-cache ()
  "Show a selectable list containing pages from the current wiki.
Refresh when universal arg."
  (interactive)
  (dokuwiki-pages-get-list-cache current-prefix-arg)
  (if-let (page (completing-read "Select a page to link: "
                                  (dokuwiki--page-link-candidates)))
      (insert (concat "[[" page "]] "))))

(defun dokuwiki-list-pages-cached (arg)
  "Show a selectable list containing pages from the current wiki.
Refresh when universal arg."
  (interactive "P")
  (dokuwiki-open-page (completing-read "Select a page to open: " (dokuwiki-pages-get-list-cache arg))))

;;; completion
;; add completion canidates based on cached page names
;; useful with company mode
(defun dokuwiki-link-wrap ()
  "Wrap current word as link."
  (interactive)
  (let*
      ((bounds (bounds-of-thing-at-point 'filename))
      (x (car bounds))
      (y (cdr bounds))
      (s (buffer-substring-no-properties x y)))
    (progn
      (delete-region x y)
      (goto-char x)
      (insert (concat "[[" s "]]")))))

;; completion-at-point-functions
(defun dokuwiki--back-to-space-or-line (pt)
  "Get point of the closest space, or beginning of line if first.
Start search at PT."
  (let ((this-line (line-beginning-position)))
    (save-excursion
      (goto-char pt)
      (skip-syntax-backward "^ ")
      (max (point) this-line))))

(defun dokuwiki--capf-link-wrap (_comp-string status)
  "When capf STATUS is finished, wrap the completed text as a wiki link."
  (when (eq status 'finished)
    (dokuwiki-link-wrap)))

(defun dokuwiki--capf ()
  "Use cached page ids for \":\"-prefixed link completion; wrap when finished."
  (when dokuwiki-cached-page-list
    (let ((limit (- (point) (line-beginning-position))))
      (when (looking-back ":[-a-zA-Z0-9_:.]+" limit)
        (list (dokuwiki--back-to-space-or-line (match-beginning 0))
              (match-end 0)
              (dokuwiki--page-link-candidates)
              :exit-function #'dokuwiki--capf-link-wrap)))))

(defun dokuwiki--full-path (any-path)
  "Return full path for dokuwiki PATH."
  ;; - if the path starts with ':' then:
  ;;   - if the path starts with '::', then strip one colon and return the rest
  ;;   - otherwise return the path as is
  ;; - else if the path has any other ':' then:
  ;;   - if the path starts with a [a-z0-9], then return the path as is
  ;;   - if the path starts with a '.:', then strip that prefix and append the rest to the namespace
  ;;   - otherwise print an error
  ;; - else append the path to the current namespace
  (if (string-prefix-p ":" any-path)
    (if (string-prefix-p "::" any-path)
      (substring any-path 1)
      any-path)
    (let ((namespace (car (dokuwiki-path))))
      (if (string-match ":" any-path)
          (cond
           ((string-match "^\\.:\\(.*\\)" any-path)
            (if namespace
                (concat namespace ":" (match-string 1 any-path))
              (match-string 1 any-path)))
           ((string-match "^[a-z0-9]" any-path)
            any-path)
           (t
            (error "Invalid path: %s" any-path)))
        (if namespace
            (concat namespace ":" any-path)
          any-path)))))

;;; links
(defun dokuwiki--fwpap (&optional at-point)
  "Find wiki path around point using 'find file around point'.
Start at AT-POINT if given.
Move past any '[' and behind ']' before looking under point for a path.
NB text is :a:b not /a/b but same file pattern rules apply."
  (save-excursion
    (when at-point (goto-char at-point))
    ;; skip ahead of [[ if looking at first part of link
    (skip-chars-forward "[")
    (skip-chars-backward "]")
    ;; requires ffap
    (dokuwiki--full-path
     (ffap-string-at-point 'file))))

(defun dokuwiki-ffap ()
  "Open wiki path under cursor."
  (interactive)
  (dokuwiki-open-page (dokuwiki--fwpap)))

(defun dokuwiki--ffap-path (_event)
  "Open the wiki link at the mouse click location."
  (interactive "e")
  (dokuwiki-ffap))

;;; clickable links
;; generated with:
;; (if (featurep 'button-lock) (button-lock-set-button "[[.*?]]" 'dokuwiki--ffap-path))
(defvar dokuwiki-font-lock-link
  '("\\[\\[.*?\\]\\]"
    (0 '(face button-lock-face keymap (keymap (mouse-1 . dokuwiki--ffap-path))
            button-lock t mouse-face button-lock-mouse-face rear-nonsticky t)
       append))
  "Font-lock keywords to make `[[wiki links]]' clickable.")

;; This is an alias for 'dokuwiki-list-pages-cached'
;;;###autoload
(defun dokuwiki-launch ()
  "Simple entry point for dokuwiki."
  (interactive)
  (dokuwiki-list-pages-cached current-prefix-arg))

(defun dokuwiki-in-browser ()
  "Open current page in the browser.  Assumes fixed xmlrpc URL suffix."
  (interactive)
  (let* ((base-url (replace-regexp-in-string "/lib/exe/xmlrpc.php" "/doku.php"
                                             (dokuwiki--get-xml-rpc-url)))
         (page-name (dokuwiki--page-id-from-buffer-name)))
    (browse-url (concat base-url "?id=" page-name))))

;;; default bindings and hooks
(defun dokuwiki-setup ()
  "Provide suggested default key bindings for the current wiki buffer.
Note: dokuwiki-mode is a separate package, modifying its map."
  (interactive)
  (unless dokuwiki--buffer-setup
    (setq dokuwiki--buffer-setup t)
    (add-hook 'completion-at-point-functions #'dokuwiki--capf nil t)
    ;; not sure how to get new font locking (links as buttons) to work
    ;; (font-lock-add-keywords nil dokuwiki-font-lock-link)
    (local-set-key (kbd "C-c g") #'dokuwiki-list-pages-cached)
    (local-set-key (kbd "C-c s") #'dokuwiki-save-page)
    (local-set-key (kbd "C-c o") #'dokuwiki-ffap)
    (local-set-key (kbd "C-c b") #'dokuwiki-in-browser)
    (local-set-key (kbd "C-c l") #'dokuwiki-insert-link-from-cache)))


(provide 'dokuwiki)
;;; dokuwiki.el ends here
