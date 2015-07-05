
(define-module zipload
  (use binary.pack)
  (use gauche.record)
  (use rfc.zlib)
  (use srfi-26)
  (export add-load-zip))
(select-module zipload)

(define-record-type zip-header #t #t
  (signature)
  (version)
  (option)
  (method)
  (time)
  (crc32)
  (compressed-size)
  (uncompressed-size)
  (filename-size)
  (ext-field-len)
  (filename)
  )

(define (read-header port)
  (let1 header (apply make-zip-header (unpack "VvvvVVVVvv" :input port))
    (if (= (zip-header-signature header) #x04034b50)
        (let* ((filename-size (zip-header-filename-size header))
               (filename
                (car (unpack #`"a,(x->string filename-size)" :input port))))
          (zip-header-filename-set! header filename)
          (unpack-skip #`"a,(x->string (zip-header-ext-field-len header))"
                       :input port)
          header)
        #f)))

(define (search-in-zip port relpath suffixes)
  (do ((files '())
       (header (read-header port) (read-header port)))
      ((or (not header)
           (string=? relpath (zip-header-filename header))
           (find (lambda(x)(string=? (string-append relpath x)
                                     (zip-header-filename header)))
                 suffixes))
       header)
    (unpack-skip #`"a,(x->string (zip-header-compressed-size header))"
                 :input port)))

(define *load-zip* '())

(define (add-load-zip filename :optional (afterp #f))
  (push! *load-zip* filename)
  ((with-module gauche.internal %add-load-path) filename afterp))

(define (zip-load-path-hook archive relpath suffixes)
  (and (member archive *load-zip*)
       (call-with-input-file archive
         (^[port]
           (if-let1 header (search-in-zip port relpath suffixes)
             (let* ((body
                     (read-block (zip-header-compressed-size header) port))
                    (uncompressed
                     ((if (zero? (logand 8 (zip-header-method header)))
                          values
                          (cut inflate-string <> :window-bits -15))
                      body)))
               (cons relpath (^_ (open-input-string uncompressed))))
             #f)))))

((with-module gauche.internal %add-load-path-hook!)
 zip-load-path-hook)

