%! File: elements.ily
%! This file contains definition of typical musical elements that are not contained in
%! the standard library. The PDQ stylesheet automatically includes this file so all of
%! this is automatically avaliable when using PDQ.
\version "2.19.65"

%!--------------------------------------------------------------------------------------
%! Group: Musical Functions
%!--------------------------------------------------------------------------------------

%! Function: ellipsis
%! --- Prototype
%! \ellipsis duration
%! ---
%! Marks a multi measure rest as an ellipsis. Unlike multi measure rests an ellipsis
%! does not show its length but is represented as two diagonal lines. This can be used
%! in exerpts where you might only want to include certain parts of a piece. This only
%! works with multi measure rests.
%!
%! Example:
%! --- LilyPond
%! \ellipsis R1*100
%! ---
%! This ellipsis would span 100 bars. As the ellipsis uses a MMR internally bar numbers
%! will resume correclty after the ellipsis.
%!
%! Parameters:
%!   duration - The duration of the ellipsis. Must be a multi measure rest.
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

%! Function: noSignature
%! --- Prototype
%! \noSignature
%! ---
%! Suppresses the time and key signature at the point where this function is used.
noSignature = {
  \once \override Staff.TimeSignature.break-visibility = #end-of-line-invisible
  \once \override Staff.KeySignature.break-visibility = #end-of-line-invisible
  \once \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
  \once \set Staff.printKeyCancellation = ##f
}

%! Function: forceBarNumber
%! --- Prototype
%! \forceBarNumber
%! ---
%! Forces LilyPond to output a bar number in the current bar. This can be very useful in
%! combination with <ellipsis> to inform the reader at which bar an exerpt continues
%! after an ellipsis.
forceBarNumber = \once \override Score.BarNumber.break-visibility = #end-of-line-invisible

%! Function: staccsOn
%! --- Prototype
%! \staccsOn number
%! ---
%! Begins using multiple staccato marks on every following note. The staccatos will
%! continue until <staccsOff> is used. This is usually used in combination with tremolo
%! repeats.
%!
%! Parameters:
%!   number - The number of staccatos to use on each note.
%!
%! See Also:
%!   <staccsUpOn>, <staccsDownOn>, <staccsOff>
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

%! Function: staccsUpOn
%! --- Prototype
%! \staccsUpOn number
%! ---
%! Begins using multiple staccatissimo marks on every following note. The marks will
%! continue until <staccsOff> is used. This is usually used in combination with tremolo
%! repeats.
%!
%! This function uses staccatissimo pointing upward. To use the downward direction use
%! <staccsDownOn> instead.
%!
%! Parameters:
%!   number - The number of staccatissimo marks to use on each note.
%!
%! See Also:
%!   <staccsOn>, <staccsDownOn>, <staccsOff>
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

%! Function: staccsDownOn
%! --- Prototype
%! \staccsDownOn number
%! ---
%! Begins using multiple staccatissimo marks on every following note. The marks will
%! continue until <staccsOff> is used. This is usually used in combination with tremolo
%! repeats.
%!
%! This function uses staccatissimo pointing downward. To use the upward direction use
%! <staccsUpOn> instead.
%!
%! Parameters:
%!   number - The number of staccatissimo marks to use on each note.
%!
%! See Also:
%!   <staccsOn>, <staccsUpOn>, <staccsOff>
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

%! Function: staccsOff
%! --- Prototype
%! \staccsOff
%! ---
%! Stops using staccato or staccatissimo marks on each note. Use this after using
%! <staccsOn>, <staccsUpOn>, or <staccsDownOn>.
%!
%! See Also:
%!   <staccsOn>, <staccsUpOn>, <staccsDownOn>
staccsOff =
{
  \revert Script #'stencil
  \revert Script #'text
  \revert Script #'X-offset
}
