#!/usr/bin/scheme --script

(define (ensure-ending-backslash dir)
  (let ((dir-as-char-list (string->list dir)))
    (if (equal? (last-pair dir-as-char-list) '(#\/))
      dir
      (string-append dir "/"))))

(define (first-occurrence-of-char-in-char-list char-list char pos)
  (cond ((null? char-list) -1)
    ((char=? char (car char-list)) pos)
    (else (first-occurrence-of-char-in-char-list (cdr char-list) char (add1 pos)))))

(define (get-file-path-without-extension file-path)
  (let ((dot-pos (first-occurrence-of-char-in-char-list (string->list file-path) #\. 0)))
    (if (eq? dot-pos -1)
      (raise "currupt file name")
      (substring file-path 0 dot-pos))))


(define (change-file-extension file-path ext)
  (let ((file-name (get-file-path-without-extension file-path)))
    (string-append file-name "." ext)))

(define (get-filename-from-path file-path)
  (let ((reversed-path-as-char-list (reverse (string->list file-path))) (len (string-length file-path)))
    (let ((bs-pos (first-occurrence-of-char-in-char-list reversed-path-as-char-list #\/ 0)))
      (if (eq? bs-pos -1)
        file-path
        (substring file-path (- len bs-pos) len)))))

(define pass-file-through-scheme-read
  (lambda (file-path-in out-dir)
    (let ((input-port (open-input-file file-path-in))
           (output-file-path (string-append out-dir (get-filename-from-path (change-file-extension file-path-in "out")))))
      (delete-file output-file-path)
      (let ((output-port (open-output-file output-file-path)))
        (let ((read-result (read input-port)))
          (write read-result output-port)
          (close-output-port output-port))
        (close-input-port input-port)))))

(define read-list-of-files
  (lambda (files-list out-dir)
    (if (null? files-list)
      '()
      (let ((current-file (car files-list)))
        (pass-file-through-scheme-read current-file out-dir)
        (read-list-of-files (cdr files-list) out-dir)))))

(define (files_to_scheme_read in-dir out-dir)
  (let ((files (directory-list in-dir)))
    (if (null? files)
      '()
      (read-list-of-files (filter (lambda (file) (not (file-directory? file)))
                            (map (lambda (file)
                                 (string-append (ensure-ending-backslash in-dir) file))
                              files))
        (ensure-ending-backslash out-dir)))))

(files_to_scheme_read (car (command-line-arguments)) (car (cdr (command-line-arguments))))
