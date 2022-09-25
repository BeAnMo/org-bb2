;; For syncing org files using backblaze b2.
(require 'dash)
(require 'request)
(require 'assoak)

(defvar bb2-api-key-id ""
  "Key for the backblaze API.")
(defvar bb2-api-key-value ""
  "Value for the backblaze API.")
(defvar bb2-bucket-id ""
  "The current bucket.")

(defun bb2--with-current-ctx (ctx &rest args)
  (if (= (length args) 1)
      (plist-get ctx (car args))
    (dolist (pair (assoak-plist-to-alist args))
      (setq ctx (plist-put ctx (car pair) (cdr pair))))
    ctx))

(defvar bb2--response
  (list :data nil
	:meta nil
	:url ""
	:error nil
	:method "GET")
  "Current response for the bb2 API.")

(defmacro bb2--with-response (&rest body)
  `(cl-function
    (lambda (&key error-thrown &key data &allow-other-keys)
      (bb2-response :data data
		    :error error-thrown)
      (if error-thrown
	  (message "[bb2:error] %s %s %s"
		   (bb2-response :method)
		   (bb2-response :url)
		   (bb2-response :error))
	(message "[bb2:ok] %s %s"
		 (bb2-response :method)
		 (bb2-response :url)))
      ,@body)))

(defun bb2-response (&rest args)
  (apply 'bb2--with-current-ctx
	 (cons bb2--response args)))

(defvar bb2--ctxs
  (list :main nil
	:downloads nil
	:uploads nil)
  "Current context instances.")

(defun bb2-ctxs (&rest args)
  (apply 'bb2--with-current-ctx
	 (cons bb2--ctxs args)))

(defvar bb2-instance nil
  "Currently bound instance from `bb2--ctxs'.")

(defmacro bb2-with-instance (ctx-prop &rest body)
  (declare (indent defun))
  `(let ((bb2-instance (bb2-ctxs ,ctx-prop)))
     ,@body))

;; Auth
;; - url: hard coded, unique to Auth
;; - authenticate(): set the Basic ctx
;;   - sets global Basic ctx
(defun bb2--data-get (key)
  (alist-get key (bb2-response :data)))

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
  (let ((-url (make-url ctx endpoint)))
    (bb2-response :url -url
		  :meta (plist-get args :meta))
    (request
      -url
      :type (or (plist-get args :type)
		"GET")
      :parser (plist-get args :parser)
      :headers (auth-headers ctx (plist-get args :headers))
      :params (plist-get args :params)
      :data (plist-get args :data)
      :complete (plist-get args :complete))))
  
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
  (with-slots (files) ctx
    (--> files
	 (alist-get 'files it)
	 (-map (lambda (obj)
		 (cons (alist-get 'fileName obj)
		       (alist-get 'fileId obj)))
	       it))))

(cl-defmethod ask-for-id-by-name ((ctx bb2--ctx-basic))
  "Interactively gets a file-id based on the selected file-name."
  (let* ((full-opts (full-file-options ctx))
	 (name (ido-completing-read
		"Which org file? "
		(-map 'car full-opts))))
    (or (alist-get name full-opts) "")))

(cl-defmethod find-file-name-by-id ((ctx bb2--ctx-basic) file-id)
  "Retrieve the file name of FILE-ID from existing files."
  (with-slots (files) ctx
    (--> files
	 (alist-get 'files it)
	 (seq-into it 'list)
	 (-find (lambda (item)
		  (string= file-id (alist-get 'fileId item)))
		it)
	 (alist-get 'fileName it))))	   

(cl-defmethod set-file-names ((ctx bb2--ctx-basic))
  (ctx-fetch
   ctx
   'list-file-names
   :meta ctx
   :type "POST"
   :data (json-encode `((bucketId . ,bb2-bucket-id)))
   :complete (bb2--with-response
	      (oset (bb2-response :meta)
		    files
		    (bb2-response :data)))
   :parser 'json-read))

(cl-defmethod set-upload-config ((ctx bb2--ctx-basic))
  (ctx-fetch
   ctx
   'get-upload-url
   :meta ctx
   :type "POST"
   :data (json-encode `((bucketId . ,bb2-bucket-id)))
   :headers (auth-headers ctx)
   :ctx (list :self ctx :upload-ctx bb2-up-ctx)
   :complete (bb2--with-response
	      (bb2-with-instance :uploads
		(oset bb2-instance url (bb2--data-get 'uploadUrl))
		(oset bb2-instance token (bb2--data-get 'authorizationToken))))
   :parser 'json-read))

;; Downloading
;; - token: shared w/ normal reqs
;; - url: unique for downloading
(defclass bb2--ctx-download (bb2--ctx-interface)
  nil
  "API context for download operations.")

(cl-defmethod get-org-file-by-id ((ctx bb2--ctx-download) file-id)
  "Fetches the org file based on FILE-ID and loads it into its own buffer."
  (let ((params `((fileId . ,file-id)))
	(-url (make-url ctx 'download-file-by-id)))
    (bb2-response :meta params :url -url :method "GET")
    (request
     -url
     :type "GET"
     :params params
     :headers (auth-headers ctx)
     :complete (bb2--with-response
		(let* ((val (bb2-response :data))
		       (id (alist-get 'fileId (bb2-response :meta)))
		       (file-name (find-file-name-by-id (bb2-ctxs :main) id)))
		  (with-current-buffer
		      (get-buffer-create (or file-name "*bb2-recent-file*"))
		    (erase-buffer)
		    (goto-char (point-min))
     		    (org-mode)
      		    (insert val)))))))
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
  (let ((-url (oref ctx url)))
    (bb2-response :method "POST" :url -url)
    (request
      -url
      :meta ctx
      :type "POST"
      :headers (auth-headers
		ctx
		`((Content-Type . text/plain)
		  (X-Bz-Content-Sha1 . ,(secure-hash 'sha1 contents))
		  (X-Bz-File-Name . ,file-name)
		  (X-Bz-Info-Author . ,(user-login-name))
		  (X-Bz-Server-Side-Encryption . AES256)))
      :data contents
      :complete (bb2--with-response
		 (pp (bb2-response :data)))
      :parser 'json-read)))

;;;; Interactions
(defun bb2-authenticate ()
  (interactive)
  (let ((-url "https://api.backblazeb2.com/b2api/v2/b2_authorize_account"))
    (bb2-response :method "GET"
		  :url -url)
    (request
      -url 
      :type "GET"
      :headers (bb2--pre-auth-headers)
      :complete (bb2--with-response
		 (bb2-ctxs :main (bb2--ctx-basic
				  :url (bb2--data-get 'apiUrl)
				  :token (bb2--data-get 'authorizationToken))
			   :downloads (bb2--ctx-download
				       :url (bb2--data-get 'downloadUrl)
				       :token (bb2--data-get 'authorizationToken))
			   :uploads (bb2--ctx-upload
				     :part-size (bb2--data-get 'recommendedPartSize))))
      :parser 'json-read)))

(defun bb2-ls ()
  "Retrieves files from the current BB2-BUCKET-ID."
  (interactive)
  (bb2-with-instance :main
    (set-file-names bb2-instance)))

(defun bb2-open-remote (file-id)
  "Download an org file from backblaze into a new buffer."
  (interactive
   (let ((file-id (ask-for-id-by-name bb2-ctx)))
     (list file-id)))
  (bb2-with-instance :downloads
    (get-org-file-by-id bb2-instance file-id)))

(defun bb2-config-uploads ()
  "Fetches the configuration for uploading."
  (interactive)
  (bb2-with-instance :main
    (set-upload-config bb2-instance)))

(defun bb2-save-remote (buf)
  "Upload the current org buffer BUF to backblaze. The buffer's name becomes
the new filename, overwriting any existing backblaze objects of the same name."
  (interactive
   (let ((buf (current-buffer)))
     (list buf)))
  (bb2-with-instance :uploads
    (upload-org-file bb2-instance
		     (buffer-name buf)
		     (buffer-substring-no-properties (point-min)
						     (point-max)))))

(provide 'org-bb2)
