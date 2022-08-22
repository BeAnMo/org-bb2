;; For syncing org files using backblaze b2.
(require 'dash)
(require 'fatch)

;; Globals
;; - key-id: id of api key
;; - key-value: value of api key
;; - bucket-id: selected bucket
(defvar bb2-api-key-id ""
  "Key for the backblaze API.")
(defvar bb2-api-key-value ""
  "Value for the backblaze API.")
(defvar bb2-bucket-id ""
  "The current bucket.")
(defvar bb2-response nil
  "The current API response data.")
(defvar bb2-ctx nil
  "The current context for basic operations.")
(defvar bb2-down-ctx nil
  "The current context for download operations.")
(defvar bb2-up-ctx nil
  "The current context for upload operations.")

;; Auth
;; - url: hard coded, unique to Auth
;; - authenticate(): set the Basic ctx
;;   - sets global Basic ctx
(defun bb2--res-get (key)
  (alist-get key bb2-response))

(defun bb2--pre-auth-headers ()
  `((Authorization . ,(format
		       "Basic %s"
		       (base64url-encode-string
			(format "%s:%s" bb2-api-key-id bb2-api-key-value))))))

(defclass bb2--ctx-interface ()
  ((url :initarg :url :interface-slot
	:documentation "The base URL for the given context.")
   (token :initarg :token :interface-slot
	  :documentation "The auth token for the given context."))
  "The interface for all contexts."
  :abstract t)

(cl-defmethod auth-headers ((ctx bb2--ctx-interface) &optional others)
  `((Authorization . ,(oref ctx token))
    ,@others))

(cl-defmethod make-url ((ctx bb2--ctx-interface) endpoint)
  (format "%s/%s"
	  (oref ctx url)
	  (bb2--format-endpoint endpoint)))

(cl-defmethod ctx-fetch ((ctx bb2--ctx-interface) endpoint &rest args)
  "Makes a request at ENDPOINT of the current CTX's url."
  (fatch
   (make-url ctx endpoint)
   (plist-get args :complete)
   :method (if-let ((meth (plist-get args :method)))
	       (upcase (if (symbolp meth)
			   (symbol-name meth)
			 meth))
	     "GET")
   :data (plist-get args :data)
   :params (plist-get args :params)
   :headers (auth-headers ctx (plist-get args :headers))
   :ctx (plist-get args :ctx)))

(defun bb2--format-endpoint (endpt)
  (let ((val (if (symbolp endpt)
		 (symbol-name endpt)
	       endpt)))
    (format "b2api/v2/b2_%s"
	    (replace-regexp-in-string "-" "_" val))))

;; Basic
;; - url: for normal operations
;; - token: for norm operations
;; - list-file-names(): retrieves bucket's file names/ids
(defclass bb2--ctx-basic (bb2--ctx-interface)
  ((files :initarg :files
	  :initform nil
	  :type list
	  :documentation "File listing for the current bucket.")
   (account-id :initarg :account-id
	       :initform ""
	       :type string
	       :documentation "Account ID for the current bucket."))
  "API context for basic operations.")

(cl-defmethod full-file-options ((ctx bb2--ctx-basic))
  (--> (oref bb2-ctx files)
       (alist-get 'files it)
       (-map (lambda (obj)
	       (cons (alist-get 'fileName obj)
		     (alist-get 'fileId obj)))
	     it)))

(cl-defmethod ask-for-id-by-name ((ctx bb2--ctx-basic))
  "Interactively gets a file-id based on the selected file-name."
  (let* ((full-opts (full-file-options ctx))
	 (name (ido-completing-read
		"Which org file? "
		(-map 'car full-opts))))
    (or (alist-get name full-opts) "")))

(cl-defmethod find-file-name-by-id ((ctx bb2--ctx-basic) file-id)
  "Retrieve the file name of FILE-ID from existing files."
  (let ((files (--> (oref bb2-ctx files)
		    (alist-get 'files it)
		    (seq-into it 'list))))
    (--> files
	 (-find (lambda (item)
		  (string= file-id (alist-get 'fileId item)))
		it)
	 (alist-get 'fileName it))))
	   

(cl-defmethod set-file-names ((ctx bb2--ctx-basic))
  (ctx-fetch
   ctx
   'list-file-names
   :method 'post
   :data (json-encode `((bucketId . ,bb2-bucket-id)))
   :complete (lambda ()
	       (let ((res (fatch-read-json)))
		 (message "[bb2] %s %s"
			  (fatch-args :method)
			  (fatch-args :url))
		 (oset (plist-get (fatch-args :ctx) :self)
		       files
		       res)))
   :ctx (list :self ctx)))

(cl-defmethod set-upload-config ((ctx bb2--ctx-basic))
  (ctx-fetch
   ctx
   'get-upload-url
   :method 'post
   :data (json-encode `((bucketId . ,bb2-bucket-id)))
   :headers (auth-headers ctx)
   :ctx (list :self ctx :upload-ctx bb2-up-ctx)
   :complete (lambda ()
	       (let ((res (fatch-read-json))
		     (ctx (plist-get (fatch-args :ctx) :upload-ctx)))
		 (message "[bb2] %s %s"
			  (fatch-args :method)
			  (fatch-args :url))
		 (oset ctx url (alist-get 'uploadUrl res))
		 (oset ctx token (alist-get 'authorizationToken res))))))

;; Downloading
;; - token: shared w/ normal reqs
;; - url: unique for downloading
(defclass bb2--ctx-download (bb2--ctx-interface)
  nil
  "API context for download operations.")

(cl-defmethod get-org-file-by-id ((ctx bb2--ctx-download) file-id)
  "Fetches the org file based on FILE-ID and loads it into its own buffer."
  (fatch
   (make-url ctx 'download-file-by-id)
   (lambda ()
     (let* ((val (fatch-read-text))
	    (id (alist-get 'fileId (fatch-args :params)))
	    (file-name (find-file-name-by-id bb2-ctx id)))
        (message "[bb2] %s %s ? %s"
      		(fatch-args :method)
      		(fatch-args :url)
      		(fatch-args :params))
        (with-current-buffer
	    (get-buffer-create (or file-name "*bb2-recent-file*"))
	  (erase-buffer)
	  (goto-char (point-min))
     	  (org-mode)
      	  (insert val))))
   :method "GET"
   :params `((fileId . ,file-id))
   :headers (auth-headers ctx)
   :ctx (list :self ctx)))

;; Uploading
;; - token: unique for uploading
;; - url: unique for uploading
;; - part-size: only needed for files > 5GB
(defclass bb2--ctx-upload (bb2--ctx-interface)
  ((part-size :initarg :part-size
	      :initform 100000000
	      :type number
	      :documentation "Recommended upload chunk size (bytes)."))
  "API context for upload operations.")

(cl-defmethod upload-org-file ((ctx bb2--ctx-upload) file-name contents)
  (fatch
   (oref ctx url)
   (lambda ()
     (let ((res (fatch-read-json)))
       (message "[bb2] %s %s"
		(fatch-args :method)
		(fatch-args :url))
       (pp res)))
   :method "POST"
   :headers (auth-headers
	     ctx
	     `((Content-Type . text/plain)
	       (X-Bz-Content-Sha1 . ,(secure-hash 'sha1 contents))
	       (X-Bz-File-Name . ,file-name)
	       (X-Bz-Info-Author . ,(user-login-name))
	       (X-Bz-Server-Side-Encryption . AES256)))
   :data contents
   :ctx (list :self ctx)))

;;;; Interactions
(defun bb2-authenticate ()
  (interactive)
  (fatch
   "https://api.backblazeb2.com/b2api/v2/b2_authorize_account"
   (lambda ()
     (let ((bb2-response (fatch-read-json)))
       (setq bb2-ctx (bb2--ctx-basic
		      :url (bb2--res-get 'apiUrl)
		      :token (bb2--res-get 'authorizationToken)))
       (setq bb2-down-ctx (bb2--ctx-download
			   :url (bb2--res-get 'downloadUrl)
			   :token (bb2--res-get 'authorizationToken)))
       (setq bb2-up-ctx (bb2--ctx-upload
			 :part-size (bb2--res-get 'recommendedPartSize)))
       (message "[bb2] Initial authentication complete.")))
  :type "GET"
  :headers (bb2--pre-auth-headers)))

(defun bb2-ls ()
  "Retrieves files from the current BB2-BUCKET-ID."
  (interactive)
  (set-file-names bb2-ctx))

(defun bb2-open-remote (file-id)
  "Download an org file from backblaze into a new buffer."
  (interactive
   (let ((file-id (ask-for-id-by-name bb2-ctx)))
     (list file-id)))
  (get-org-file-by-id bb2-down-ctx file-id))

(defun bb2-config-uploads ()
  "Fetches the configuration for uploading."
  (interactive)
  (set-upload-config bb2-ctx))

(defun bb2-save-remote (buf)
  "Upload the current org buffer BUF to backblaze. The buffer's name becomes
the new filename, overwriting any existing backblaze objects of the same name."
  (interactive
   (let ((buf (current-buffer)))
     (list buf)))
  (upload-org-file bb2-up-ctx
		   (buffer-name buf)
		   (buffer-substring-no-properties (point-min)
						   (point-max))))

(provide 'org-bb2)
