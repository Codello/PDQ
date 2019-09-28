\version "2.19.65"

dolce = \markup { \italic "dolce" }

ellipsis = {
  \once \override MultiMeasureRest.minimum-length = #0
  \once \override MultiMeasureRest.expand-limit = #1
  \once \override MultiMeasureRest.transparent = ##t
  \once \override MultiMeasureRestNumber.layer = #-1
  \once \override MultiMeasureRestNumber.Y-offset = #0
  \once \override MultiMeasureRestNumber.stencil = #(lambda (grob) (grob-interpret-markup grob
      (markup #:vcenter #:pad-x 1 (ly:grob-property grob 'text))))
  \once \override MultiMeasureRestNumber.text = \markup { \vspace #1 \draw-line #'(6 . 3) \hspace #-5.9 \draw-line #'(6 . 3) }
}

noSignature = {
  \once \override Staff.TimeSignature.break-visibility = #end-of-line-invisible
  \once \override Staff.KeySignature.break-visibility = #end-of-line-invisible
  \once \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
  \once \set Staff.printKeyCancellation = ##f
}

forceBarNumber = \once \override Score.BarNumber.break-visibility = #'#(#t #t #t)

% TODO: Remove Code Duplication
staccsOn = #(define-music-function (parser location dots) (number?)
#{
    \override Script #'stencil = #ly:text-interface::print
    \override Script #'text = #(lambda (grob)
      (define (build-lst count)
         (let ((lst (list #:musicglyph "scripts.staccato")))
            (if (> count 1) (append lst '(#:hspace 0.1) (build-lst (- count 1))) lst)))
      (eval (list markup #:concat (build-lst dots)) (interaction-environment)))
    \override Script #'X-offset =
    #(lambda (grob)
      (let* ((parent (ly:grob-parent grob X))
                (parent-extent (ly:grob-property parent 'X-extent '(0 . 0)))
                (parent-start (car parent-extent))
                (parent-end (cdr parent-extent))
                (parent-center (/ (+ parent-start parent-end) 2.0))
                (extent (ly:grob-property grob 'X-extent '(0 . 0)))
                (start (car extent))
                (end (cdr extent))
                (val (- parent-center (/ (- end start) 2.0))))
         val))
#})

% TODO: Calculate Staccatissimo Direction automatically (by duplicating existing expressive marks)
staccsUpOn = #(define-music-function (parser location dots) (number?)
#{
    \override Script #'stencil = #ly:text-interface::print
    \override Script #'text = #(lambda (grob)
      (define (build-lst count)
         (let ((lst (list #:musicglyph "scripts.dstaccatissimo")))
            (if (> count 1) (append lst '(#:hspace 0.1) (build-lst (- count 1))) lst)))
      (eval (list markup #:concat (build-lst dots)) (interaction-environment)))
    \override Script #'X-offset =
    #(lambda (grob)
      (let* ((parent (ly:grob-parent grob X))
                (parent-extent (ly:grob-property parent 'X-extent '(0 . 0)))
                (parent-start (car parent-extent))
                (parent-end (cdr parent-extent))
                (parent-center (/ (+ parent-start parent-end) 2.0))
                (extent (ly:grob-property grob 'X-extent '(0 . 0)))
                (start (car extent))
                (end (cdr extent))
                (val (- parent-center (/ (- end start) 2.0))))
         val))
#})

staccsDownOn = #(define-music-function (parser location dots) (number?)
#{
    \override Script #'stencil = #ly:text-interface::print
    \override Script #'text = #(lambda (grob)
      (define (build-lst count)
         (let ((lst (list #:musicglyph "scripts.ustaccatissimo")))
            (if (> count 1) (append lst '(#:hspace 0.1) (build-lst (- count 1))) lst)))
      (eval (list markup #:concat (build-lst dots)) (interaction-environment)))
    \override Script #'X-offset =
    #(lambda (grob)
      (let* ((parent (ly:grob-parent grob X))
                (parent-extent (ly:grob-property parent 'X-extent '(0 . 0)))
                (parent-start (car parent-extent))
                (parent-end (cdr parent-extent))
                (parent-center (/ (+ parent-start parent-end) 2.0))
                (extent (ly:grob-property grob 'X-extent '(0 . 0)))
                (start (car extent))
                (end (cdr extent))
                (val (- parent-center (/ (- end start) 2.0))))
         val))
#})

staccsOff =
{
  \revert Script #'stencil
  \revert Script #'text
  \revert Script #'X-offset
}
