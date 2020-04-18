\version "2.19.65"

%! File: util.ily
%! This file contains utility functions in Scheme that can be used to simplify some
%! advanced tasks in LilyPond.

%!--------------------------------------------------------------------------------------
%! Group: LilyPond Functions
%! LilyPond functions can be used directly within your LilyPond source code.
%!--------------------------------------------------------------------------------------

%! Function: escalate-warnings
%! --- Prototype ---
%! \escalate-warnings
%! -----------------
%! Makes LilyPond treat all (user-issued) warnings as errors.
%> \escalate-warnings
escalate-warnings = #(define-void-function (parser location) ()
                                            (set! ly:warning ly:error))

%!--------------------------------------------------------------------------------------
%! Group: Scheme Functions
%! Scheme functions must be called in a Scheme environment (and aren't really all that
%! useful elsewhere).
%!--------------------------------------------------------------------------------------

%! Function: defined?
%! The defined? predicate tells you wether a specific symbol was previously
%! defined. This may not work for standard functions but it works for custom
%! definitions.
%!
%! Parameters:
%!   symbol - A quoted symbol.
%# (define (defined? symbol) ...)
#(define (defined? symbol) (not (null? (ly:parser-lookup symbol))))

%! Function: get-option
%! The test-option predicate tests wether the specified symbol was defined and
%! set to a value that evaluates to #t (a value other that #f). This can be used
%! to test whether package options have been specified. If a (package) option is
%! set to a non-boolean value this predicate will return that value.
%!
%! Parameters:
%!   symbol  - A quoted symbol (the name of the option).
%!   default - A default value that is returned if no definition for symbol is found.
%# (define (get-option symbol default) ...)
#(define (get-option symbol default) (if (defined? symbol)
                     (eval symbol (current-module))
                     default))

%! Function: any?
%! A type predicate that is always true.
%!
%! Parameters:
%!   object - The object to test. This parameter is ignored.
%# (define (any? object) ...)
#(define (any? object) #t)

%! Function: andmap
%! A predicate that returns true iff f returns true for all elements in xs. If xs is
%! empty this predicates evaluates to #t.
%!
%! Parameters:
%!   f  - A predicate function to apply to all xs.
%!   xs - A list of values.
%!
%! See Also:
%!   <ormap>
%# (define (andmap f xs) ...)
#(define (andmap f xs)
  (cond ((null? xs) #t)
        ((f (car xs)) (andmap f (cdr xs)))
        (else #f)))

%! Function: ormap
%! A predicate that returns true iff f returns true for any of the elements in xs. If 
%! xs is empty #f is returned.
%!
%! Parameters:
%!   f  - A predicate function to apply to all xs.
%!   xs - A list of values.
%!
%! See Also:
%!   <andmap>
%# (define (ormap f xs) ...)
#(define (ormap f xs)
  (cond ((null? xs) #f)
        ((f (car xs)) #t)
        (else (ormap f (cdr xs)))))
        
%! Function: custom-script-tweaks
%! Enables custom tweaks for single grobs.
%!
%! Usage:
%! > \override Script.before-line-breaking = #(custom-script-tweaks alist)
%!
%! Parameters:
%!   ls - An alist of tweaks. Keys are strings and values are lists of pairs.
%# (define ((custom-script-tweaks ls) ...) ...)
#(define ((custom-script-tweaks ls) grob)
  (let* ((type (ly:prob-property (assoc-ref (ly:grob-properties grob) 'cause)
                                  'articulation-type))
         (tweaks (assoc-ref ls type)))
    (if tweaks
        (for-each (lambda (x) (ly:grob-set-property! grob (car x) (cdr x))) tweaks)
        '())))

%!--------------------------------------------------------------------------------------
%! Group: Markup Commands
%! Markup commands can be used inside of a \markup { ... } block. They provide
%! additional features such as styling options.
%!--------------------------------------------------------------------------------------

%! Function: warn
%! A markup command that can be used to emit a warning. This may for example be useful
%! if you want to mark a function as deprecated and emit a warning.
%!
%! Parameters:
%!   text - The warning text that should be emitted.
%!
%! Returns:
%!   Empty markup.
%> \warn text
#(define-markup-command (warn layout props text) (string?)
  (ly:warning text)
  empty-markup)

%! Function: if-true
%! This markup command conditionally outputs markup based on a boolean value.
%!
%! Parameters:
%!   predicate - A boolean value.
%!   markp     - Any markup.
%!
%! Returns:
%!   If predicate is #t markp is returned, otherwise an empty stencil.
%!
%! See Also:
%!   <if-false>, <if-else>
%> \if-true predicate markp
#(define-markup-command (if-true layout props predicate markp) (any? markup?)
  (if predicate (interpret-markup layout props markp) empty-stencil))

%! Function: if-false
%! This markup command conditionally outputs markup based on a boolean value.
%!
%! Parameters:
%!   predicate - A boolean value.
%!   markp     - Any markup.
%!
%! Returns:
%!   If predicate is #f markp is returned, otherwise an empty stencil.
%!
%! See Also:
%! <if-true>, <if-else>
%> \if-false predicate markp
#(define-markup-command (if-false layout props predicate markp) (any? markup?)
  (if predicate empty-stencil (interpret-markup layout props markp)))

%! Function: if-else
%! Conditionally outputs one of two markups depending on a boolean value.
%!
%! Parameters:
%!   predicate - A boolean value.
%!   tmarkp    - The markup returned if predicate is #t.
%!   fmarkp    - The markup returned if predicate is #f.
%!
%! See Also:
%! <if-true>, <if-false>
%> \if-else predicate tmarkp fmarkp
#(define-markup-command (if-else layout props predicate tmarkp fmarkp) (any? markup? markup?)
  (if predicate
      (interpret-markup layout props tmarkp)
      (interpret-markup layout props fmarkp)))

%! Function: when-property
%! The \when-property markup command allows you to conditionally output markup.
%! If symbol exists markp is returned. Otherwise an empty markup block is
%! returned.
%!
%! Parameters:
%!   symbol - A symbol.
%!   marp   - Some markup.
%!
%! Returns:
%!   markp if symbol exists in the current context, an empty stencil otherwise.
%!
%! See Also:
%! <when-not-property>, <when-some-properties>
%> \when-property symbol markp
#(define-markup-command (when-property layout props symbol markp) (any? markup?)
  (if (chain-assoc-get symbol props)
      (interpret-markup layout props markp)
      empty-stencil))

%! Function: when-not-property
%! Behaves like \when-property but returns the markup block if symbol does not
%! exist. If the symbol exists an empty markup block is returned.
%!
%! Parameters:
%!   symbol - A symbol.
%!   marp   - Some markup.
%!
%! Returns:
%!   markp if symbol does not exist in the current context, an empty stencil otherwise.
%!
%! See Also:
%! <when-property>, <when-some-properties>
%> \when-not-property symbol markp
#(define-markup-command (when-not-property layout props symbol markp) (symbol? markup?)
  (if (chain-assoc-get symbol props)
      empty-stencil
      (interpret-markup layout props markp)))

%! Behaves like \when-property but accepts a list of properties instead of a
%! single property.
%!
%! Parameters:
%!   symbols - A list of symbol.
%!   marp    - Some markup.
%!
%! Returns:
%!   markp if any of the symbols exist in the current context, an empty stencil
%!   otherwise.
%!
%! See Also:
%! <when-property>, <when-not-properties>
%> \when-some-properties symbols markp
#(define-markup-command (when-some-properties layout props symbols markp) (list? markup?)
  (if (ormap (lambda (symbol) (chain-assoc-get symbol props)) symbols)
      (interpret-markup layout props markp)
      empty-stencil))
