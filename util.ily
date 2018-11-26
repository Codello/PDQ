\version "2.19.65"

% A type predicate that is always true.
#(define (any? object) #t)

#(define (andmap f xs)
  (cond ((null? xs) #t)
        ((f (car xs)) (andmap f (cdr xs)))
        (else #f)))

#(define (ormap f xs)
  (cond ((null? xs) #f)
        ((f (car xs)) #t)
        (else (ormap f (cdr xs)))))


% The \if-true markup command conditionally outputs markup based on a boolean
% value.
#(define-markup-command (if-true layout props predicate markp) (any? markup?)
  (if predicate (interpret-markup layout props markp) empty-stencil))

% The \if-false command conditionally outputs the second parameter markup if the
% first parameter evaluates to #f.
#(define-markup-command (if-false layout props predicate markp) (any? markup?)
  (if predicate empty-stencil (interpret-markup layout props markp)))

% Conditionally outputs one of two markups depending on a boolean value.
#(define-markup-command (if-else layout props predicate tmarkp fmarkp) (any? markup? markup?)
  (if predicate
      (interpret-markup layout props tmarkp)
      (interpret-markup layout props fmarkp)))

% The \when-property markup command allows you to conditionally output markup.
% If symbol exists markup is returned. Otherwise an empty markup block is
% returned.
#(define-markup-command (when-property layout props symbol markp) (any? markup?)
  (if (chain-assoc-get symbol props)
      (interpret-markup layout props markp)
      empty-stencil))

% Behaves like \when-property but returns the markup block if symbol does not
% exist. If the symbol exists an empty markup block is returned.
#(define-markup-command (when-not-property layout props symbol markp) (symbol? markup?)
  (if (chain-assoc-get symbol props)
      empty-stencil
      (interpret-markup layout props markp)))

% Behaves like \when-property but accepts a list of properties instead of a
% single property.
#(define-markup-command (when-some-properties layout props symbols markp) (list? markup?)
  (if (ormap (lambda (symbol) (chain-assoc-get symbol props)) symbols)
      (interpret-markup layout props markp)
      empty-stencil))

% Enables custom tweaks for single grobs.
#(define ((custom-script-tweaks ls) grob)
  (let* ((type (ly:prob-property (assoc-ref (ly:grob-properties grob)
'cause) 'articulation-type))
         (tweaks (assoc-ref ls type)))
    (if tweaks
        (for-each (lambda (x) (ly:grob-set-property! grob (car x)
(cdr x))) tweaks)
        '())))


% The defined? predicate tells you wether a specific symbol was previously
% defined. This may not work for standard functions but it works for custom
% definitions.
#(define (defined? symbol) (not (null? (ly:parser-lookup symbol))))

% The test-option predicate tests wether the specified symbol was defined and
% set to a value that evaluates to #t (a value other that #f). This can be used
% to test whether package options have been specified. If a (package) option is
% set to a non-boolean value this predicate will return that value.
#(define (get-option symbol default) (if (defined? symbol)
                                         (eval symbol (current-module))
                                         default))

% TODO: Commad startText / stopText http://lilypond.1069038.n5.nabble.com/Ritardando-and-accelerando-tp171061p171095.html
