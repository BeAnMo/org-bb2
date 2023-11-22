;;; org-bb2.el --- Package for syncing with Backblaze's BB2 storage.
;; For syncing org files using backblaze b2.

;;; Commentary:
;; 

(require 'dash)
(require 'request)
(require 'promise)
(require 'assoak)

;; To set up:
;; 1. bb2-choose-bucket
;; 2. bb2-authenticate
;; 3. bb2-ls

;; To download:
;; 1. bb2-open-remote

;; To upload:
;; 1. bb2-config-uploads
;; 2. bb2-save-remote

;;; Code:

(defvar bb2-api-key-id ""
  "Key for the backblaze API.")
(defvar bb2-api-key-value ""
  "Value for the backblaze API.")
(defvar bb2-buckets nil
  "Account's bucket ids.")
(defvar bb2-bucket-id ""
  "The current bucket.")
(defvar bb2--upload-details nil
  "Details for uploading. (cons BUFFER CONTENT-TYPE")

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
  (declare (indent defun))
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
  "Eval BODY within the context of the selected `bb2-ctxs'.
CTX-PROP is one of `:main', `:downloads', or `:uploads'."
  (declare (indent defun))
  `(let ((bb2-instance (bb2-ctxs ,ctx-prop)))
     ,@body))

(defmacro bb2-lambda (instance-name argslist &rest body)
  "Create a lambda bound to the INSTANCE-NAME.
Argument ARGSLIST is the list or arguments passed to the lambda.
Optional argument BODY are the form(s) evaluated."
  (declare (indent defun))
  `(lambda ,argslist
     (bb2-with-instance ,instance-name
       ,@body)))

(defun bb2--args-to-params (args)
  "Converts the plist ARGS to an alist of ((<string> . <string>)),
stripping the leading ":" from each property name."
  (--> (assoak-plist-to-alist args)
       (-map (lambda (pair)
	       (cons (substring (symbol-name (car pair)) 1)
		     (cdr pair)))
	     it)))
  
;; Auth
;; - url: hard coded, unique to Auth
;; - authenticate(): set the Basic ctx
;;   - sets global Basic ctx
(defun bb2--pre-auth-headers ()
  `(("Authorization" . ,(format
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
  `(("Authorization" . ,(oref ctx token))
    ,@others))

(cl-defmethod make-url ((ctx bb2--ctx-interface) endpoint)
  (format "%s/%s"
	  (oref ctx url)
	  (bb2--format-endpoint endpoint)))

(cl-defmethod ctx-fetch ((ctx bb2--ctx-interface) endpoint &rest args)
  "Make a request at ENDPOINT of the current CTX's url."
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

(cl-defmethod ctx-fetch-P ((ctx bb2--ctx-interface) endpoint &rest args)
  (let ((-url (make-url ctx endpoint)))
    (bb2-response :url -url
		  :meta (plist-get args :meta))
    (promise:request-with-args
	-url
      (list :type (plist-get args :method)
	    :headers (--> args
			  (plist-get it :headers)
			  (auth-headers ctx it))
	    :data (plist-get args :data)
	    :params (plist-get args :params)
	    ;; parser doesn't work with promise?
	    :parser (plist-get args :parser)))))
  
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
  "From the `:main' instance, return a list of (cons FILE-NAME FILE-ID).
Argument CTX is the current `bb2-instance'."
  (with-slots (files) ctx
    (--> files
	 (alist-get 'files it)
	 (-map (lambda (obj)
		 (cons (alist-get 'fileName obj)
		       (alist-get 'fileId obj)))
	       it))))

(cl-defmethod ask-for-id-by-name ((ctx bb2--ctx-basic))
  "Interactively gets a file-id based on the selected file-name.
Argument CTX is the current `bb2-instance'."
  (let* ((full-opts (full-file-options ctx))
	 (name (ivy-completing-read
		"Which org file? "
		(-map 'car full-opts))))
    ;; alist-get comes up nil when using ivy-completing-read.
    ;; Why?
    (or (assoc-default name full-opts) "")))

(cl-defmethod find-file-name-by-id ((ctx bb2--ctx-basic) file-id)
  "Retrieve the file name of FILE-ID from existing files.
Argument CTX is the current `bb2-instance'."
  (with-slots (files) ctx
    (--> files
	 (alist-get 'files it)
	 (seq-into it 'list)
	 (-find (lambda (item)
		  (string= file-id (alist-get 'fileId item)))
		it)
	 (alist-get 'fileName it))))

(cl-defmethod set-file-names-P ((ctx bb2--ctx-basic))
  (ctx-fetch-P
   ctx
   'list-file-names
   :meta ctx
   :type "POST"
   :data (json-encode `((bucketId . ,bb2-bucket-id)))
   :parser 'json-read))

(cl-defmethod set-upload-config-P ((ctx bb2--ctx-basic))
  (condition-case no-token
      (promise-resolve
       (oref (bb2-ctxs :uploads) token))
    (unbound-slot
     (message "[bb2] No upload token found, retrieving.")
     (ctx-fetch-P
      ctx
      'get-upload-url
      :meta ctx
      :type "POST"
      :data (json-encode `((bucketId . ,bb2-bucket-id)))
      :headers (auth-headers ctx)
      :parser 'json-read))))

(cl-defmethod list-file-versions-P ((ctx bb2--ctx-basic) &rest args)
  "Fetches all file verions within a bucket.
Argument CTX is the current `bb2-instance'.
Optional ARGS:
  :startFileId string
  :startFileName string
  :maxFileCount integer (defaults to 100, limit is 1000)
  :prefix string (limit returned versions to given prefix)
  :delimiter string (used to split file names into folders)"
  (ctx-fetch-P
   ctx
   'list-file-versions
   :type "GET"
   :parser 'json-read
   :headers (auth-headers ctx)
   :params (cons (cons "bucketId" bb2-bucket-id)
		 (bb2--args-to-params args))))

(cl-defmethod delete-file-version-P ((ctx bb2--ctx-basic)
				     fileId
				     fileName
				     &optional bypassGovernance)
  "Deletes a file version with the given FILEID and FILENAME in the current
CTX. BYPASSGOVERNANCE overrides the Object Lock."
  (ctx-fetch-P
   ctx
   'delete-file-version
   :type "POST"
   :parser 'json-read
   :headers (auth-headers ctx)
   :data (json-encode `((fileName . ,fileName)
			(fileId . ,fileId)
			(bypassGovernance . ,bypassGovernance)))))


;; Downloading
;; - token: shared w/ normal reqs
;; - url: unique for downloading
(defclass bb2--ctx-download (bb2--ctx-interface)
  nil
  "API context for download operations.")

(cl-defmethod get-file-by-id ((ctx bb2--ctx-download) file-id)
  "Fetches the file based on FILE-ID and load it into its own buffer.
Argument CTX is the current `bb2-instance'."
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
     		    ;; (org-mode)
      		    (insert val)
		    (switch-to-buffer (current-buffer))))))))
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

(cl-defmethod upload-org-file-P ((ctx bb2--ctx-upload)
				 file-name
				 contents
				 &optional content-type)
  (promise:request-with-args
      (oref ctx url)
    (list
     :type "POST"
     :headers (auth-headers
	       ctx
	       `((Content-Type . ,(or content-type "text/plain"))
		 (X-Bz-Content-Sha1 . ,(secure-hash 'sha1 contents))
		 (X-Bz-File-Name . ,file-name)
		 (X-Bz-Info-Author . ,(user-login-name))
		 (X-Bz-Server-Side-Encryption . AES256)))
     :data contents
     :parser 'json-read)))

(defun bb2--auth-init (auth-res)
  (let-alist auth-res
    (bb2-ctxs
     :main (bb2--ctx-basic
	    :url .apiUrl
	    :token .authorizationToken)
     :downloads (bb2--ctx-download
		 :url .downloadUrl
		 :token .authorizationToken)
     :uploads (bb2--ctx-upload
	       :part-size .recommendedPartSize))))
  
(defun bb2-authenticate-P ()
  (let ((-url "https://api.backblazeb2.com/b2api/v2/b2_authorize_account"))
    (bb2-response :method "GET"
		  :url -url)
    (promise:request-with-args
	-url
      (list :headers (bb2--pre-auth-headers)
	    :type "GET"
	    :parser 'json-read))))

;;;; Interactions
(defun bb2-login (bucket-config)
  "Login with the current BUCKET-CONFIG."
   (interactive
    (let ((bucket-name (ivy-completing-read "Choose a bucket: "
					 (seq-map 'car bb2-buckets))))
      (list (assoc-default bucket-name bb2-buckets))))
   (let-alist (car bucket-config)
    (setq bb2-bucket-id .id
	  bb2-api-key-id .key-id
	  bb2-api-key-value .key-val)
    (message "[bb2] Set bucket."))
   (promise-chain
       (bb2-authenticate-P)
     (then (lambda (res)
	     (bb2--auth-init res)
	     (message "[bb2] Retrieved auth response.")))
     (then (bb2-lambda :main (auth-res)
	     (set-file-names-P bb2-instance)))
     (then (bb2-lambda :main (file-name-res)
	     (oset bb2-instance files file-name-res)
	     (message "[bb2] Retrieved bucket file information.")))
     (catch (lambda (reason)
	      (message "[bb2:err] %s" reason)))))

(defun bb2-open-remote (file-id)
  "Download an org file from backblaze into a new buffer.
Argument FILE-ID is the unique id for a bb2 file."
  (interactive
   (let ((file-id (bb2-with-instance :main
		    (ask-for-id-by-name bb2-instance))))
     (list file-id)))
  (bb2-with-instance :downloads
    (get-file-by-id bb2-instance file-id)))

(defun bb2-save-remote (buf &optional content-type)
  "Upload the current org buffer BUF to backblaze.
The buffer's name becomes
the new filename, overwriting any existing backblaze objects of the same name.
Optional argument CONTENT-TYPE is the HTTP content-type header value."
  (interactive
   (let ((buf (current-buffer))
	 (content-type (read-string "Content-type? (defaults to text/plain) ")))
     (list buf content-type)))
  (setq bb2--upload-details (cons buf content-type))
  (promise-chain
      (bb2-with-instance :main
	(set-upload-config-P bb2-instance))
    (then (bb2-lambda :uploads (upload-res-or-token)
	    (if (stringp upload-res-or-token)
		(message "[bb2] Found file upload configuration.")
	      (message "[bb2] Retrieved file upload configuration.")
	      (let-alist upload-res-or-token
		(oset bb2-instance url .uploadUrl)
		(oset bb2-instance token .authorizationToken)))))
    (then (bb2-lambda :uploads (_)
	    (with-current-buffer (car bb2--upload-details)
	      (message "[bb2] Prepping to upload '%s'."
		       (buffer-name (current-buffer)))
	      (upload-org-file-P bb2-instance
				 (buffer-name (current-buffer))
				 (buffer-substring-no-properties (point-min)
								 (point-max))
				 (if (string= "" (cdr bb2--upload-details))
				     nil
				   (cdr bb2--upload-details))))))
    (then (lambda (file-upload-res)
	    (let-alist file-upload-res
	      (message "[bb2] File '%s' uploaded at %s."
		       .fileName
		       .uploadTimestamp))))
    (catch (lambda (reason)
	     (message "[bb2:err] File uploaded error - %s" reason)))))

(provide 'org-bb2)

;;; org-bb2.el ends here
