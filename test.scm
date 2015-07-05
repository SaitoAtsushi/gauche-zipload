;;;
;;; Test zipload
;;;

(use gauche.test)
(use srfi-1)
(use srfi-19)
(use rfc.zlib)
(use binary.pack)

(test-start "zipload")
(use zipload)
(test-module 'zipload)

(define (current-time/dos-format)
  (let* ((date (current-date))
         (year (date-year date))
         (month (date-month date))
         (day (date-day date))
         (hour (date-hour date))
         (minute (date-minute date))
         (second (date-second date)))
    (+ (ash (- year 1980) 25) (ash month 21) (ash day 16)
       (ash hour 11) (ash minute 5) (quotient second 2))))

(define (pk0304 port name body compressed timestamp checksum flag)
  (rlet1 lfh-position (port-tell port)
    (pack "VvvvVVVVvva*"
      (list #x04034b50 20  0 (if flag 8 0) timestamp checksum
            (string-size compressed) (string-size body) (string-size name)
            0 name)
      :output port)))

(define (pk0102 port name body compressed timestamp position checksum flag)
  (pack "VvvvvVVVVvvvvvVVa*"
    (list #x02014b50 20 20 0 (if flag 8 0) timestamp checksum
          (string-size compressed) (string-size body) (string-size name)
          0 0 0 0 0 position name)
        :output port))

(define (pk0506 port num eoc cd)
  (pack "VvvvvVVv" (list #x06054b50 0 0 num num (- eoc cd) cd 0) :output port))

(define (zip-encode output-filename lst)
  (receive (names bodies flags) (unzip3 lst)
    (let ((timestamp (current-time/dos-format))
          (compressed-bodies
           (map
            (^[body flag]
              (if flag
                  (deflate-string body :window-bits -15 :compression-level 9)
                  body))
                bodies flags))
          (checksums (map crc32 bodies)))
      (call-with-output-file output-filename
        (^p
         (let ((lfh-pos ;; local file headers
                (map (^[n b c checksum f]
                       (rlet1 x (pk0304 p n b c timestamp checksum f)
                         (display c p)))
                     names bodies compressed-bodies checksums flags))
               (cd-position (port-tell p))) ;;central directory structure
           (for-each
            (^[n b c pos checksum f]
              (pk0102 p n b c timestamp pos checksum f))
            names bodies compressed-bodies lfh-pos checksums flags)
           (let1 eoc-position (port-tell p) ;;end of central directory record
             (pk0506 p (length lst) eoc-position cd-position)
             )))))))

(test* "test-zipload" '(a b c)
       (begin
       (zip-encode "hoge.zip"
                   '(("T1.scm" "(load \"T2\")(load \"T3\")(load \"T4\")
                                (define (test-zipload) (list a b c))" #t)
                     ("T2.scm" "(define a 'a)" #f)
                     ("T3.sld" "(define b 'b)" #t)
                     ("T4.scm" "(define c 'c)" #f)))
       (add-load-zip "hoge.zip")
       (load "T1")
       (test-zipload)))

(test-end)
