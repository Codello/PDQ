\version "2.18.2"

#(define (expand-repetitions arg)
;; 4*5 --> 4 4 4 4 4
;; (at any level of nesting)
  (fold-right
    (lambda (elem prev)
            (cond ((pair? elem)
                   (cons (expand-repetitions elem) prev))
                  ((symbol? elem)
                   (let* ((str (symbol->string elem))
                          (split (string-split str #\*))
                          (split (map (lambda (elem) (string->number elem)) split)))
                     (append (make-list (cadr split) (car split))
                             prev)))
                  (else (cons elem prev))))
    '()
    arg))

#(define ((bars-per-line-systems-per-page-engraver lst) ctx)
  (let* ((bars-and-systems? (any pair? lst))
         (working-copy (expand-repetitions lst))
         (systems-per-page
           (if bars-and-systems?
               (car working-copy)
               #f))
         (last-measure-seen (ly:context-property ctx 'currentBarNumber))
         (last-measure-seen (if (null? last-measure-seen)
                                0
                                (1- last-measure-seen)))
         (total
           (if systems-per-page
               (+ (car systems-per-page) last-measure-seen 1)
               (+ (car working-copy) last-measure-seen 1))))
  `((stop-translation-timestep
      . ,(lambda (trans)
          (let ((internal-bar (ly:context-property ctx 'internalBarNumber))
                (current-col (ly:context-property ctx 'currentCommandColumn)))
            ;; we are only interested in the first NonMusicalPaperColumn of
            ;; each measure
            (if (and (> internal-bar last-measure-seen)
                     (= (remainder internal-bar total) 0)
                     (pair? working-copy))
                (begin
                  (set! (ly:grob-property current-col 'line-break-permission) 'force)
                  (set! last-measure-seen internal-bar)
                  (if bars-and-systems?
                      (begin
                        (if (null? (cdr systems-per-page))
                            (begin
                              (set! (ly:grob-property current-col 'page-break-permission) 'force)
                              (if (pair? (cdr working-copy))
                                  (begin
                                    (set! working-copy (cdr working-copy))
                                    (set! systems-per-page (car working-copy)))
                                  (set! working-copy '())))
                            (set! systems-per-page (cdr systems-per-page)))
                        (set! total (+ total (car systems-per-page))))
                      (begin
                        (if (null? (cdr working-copy))
                            (set! working-copy lst)
                            (begin
                              (set! working-copy (cdr working-copy))))
                              (set! total (+ total (car working-copy)))))))))))))
