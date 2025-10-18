;;;
;;; Copyright 2009 Yuichiro Moriguchi
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(library (file)
  (export
    directory-fold)

  (define (directory-fold2 path proc seed wild)
    (if (directory? path)
        (let loop ((l (directory-list path `(or directory ,wild)))
                   (s seed))
          (if (null? l) s
              (let ((r (directory-fold2
                         (make-path-name path (car l))
                         proc s wild)))
                (loop (cdr l) r))))
        (proc path seed)))

  (define directory-fold
    (case-lambda
      ((path proc seed)      (directory-fold2 path proc seed "*"))
      ((path proc seed wild) (directory-fold2 path proc seed wild))))

  (define (for-each-file0 proc l)
    (guard (e (else (close-line-cursor l)))
      (let loop ((l l))
        (cond ((line-buffer-null? l) (if #f #f))
              (else
                (proc (line-buffer-car l))
                (loop (line-buffer-cdr)))))))

  (define (map-file0 proc l w f)
    (guard (e (else (close-line-cursor l) (close-line-buffer w)))
      (let loop ((l l))
        (cond ((line-buffer-null? l)
                (if f (if #f #f) (get-list-from-buffer w)))
              (else
                (put-line-buffer! w (proc (line-buffer-car l)))
                (loop (line-buffer-cdr)))))))

  (define for-each-file
    (case-lambda
      ((proc fn)
         (for-each-file0 proc (open-line-cursor fn)))
      ((proc fn enc)
         (for-each-file0 proc (open-line-cursor fn enc)))))

  (define map-file
    (case-lambda
      ((proc fn)
         (map-file0
           proc
           (open-line-cursor fn)
           (open-list-buffer)
           #t))
      ((proc fn enc)
         (map-file0
           proc
           (open-line-cursor fn enc)
           (open-list-buffer)
           #t))
      ((proc fn enc fw)
         (map-file0
           proc
           (open-line-cursor fn)
           (open-file-buffer fw)
           #f))
      ((proc fn enc fw enc2)
         (map-file0
           proc
           (open-line-cursor fn enc)
           (open-file-buffer fw enc2)
           #f))))

  (define grep
    (case-lambda
      ((proc pat fn)
         (for-each-file0 proc (open-grep-cursor pat fn)))
      ((proc pat fn enc)
         (for-each-file0 proc (open-grep-cursor pat fn enc)))))

  (define map-grep
    (case-lambda
      ((proc pat fn)
         (map-file0
           proc
           (open-grep-cursor pat fn)
           (open-list-buffer)
           #t))
      ((proc pat fn enc)
         (map-file0
           proc
           (open-grep-cursor pat fn enc)
           (open-list-buffer)
           #t))
      ((proc pat fn enc fw)
         (map-file0
           proc
           (open-grep-cursor pat fn)
           (open-file-buffer fw)
           #f))
      ((proc pat fn enc fw enc2)
         (map-file0
           proc
           (open-grep-cursor pat fn enc)
           (open-file-buffer fw enc2)
           #f))))

)

(import (file))
