%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Partify -- A LilyPond Plugin for Automatic Part Generation          %
%                                                                              %
%                          Created by Kim Wittenburg                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\version "2.19.65"


%%%%%%%%%%%%%%%%%%
% Default Values %
%%%%%%%%%%%%%%%%%%

% When no part name is specified via the command line options this part will be
% typeset. Typically it is a good idea to set either all parts or just the score
% in that case.
defaultPartName = #'(Score)

% If you want to simplify using partify even more you can set this option to
% ##t. In that case partify will search for a variable with the same name as the
% part and typeset it. This works for various types of content, e.g. music,
% scores and books.
%
% By default this option is ##f, because it heavily depends on you usage of
% variables whether using this simplification makes sense or not. It is normally
% better to write a few more characters for improved readability of the source
% code.
lookupPartsByVariableName = ##f

% The name of the of the option that partify will query for the part names. You
% can redefine this option if you feel the need to do so.
#(define partifyOption 'part)



%%%%%%%%%%%%%%%%%%%%
% Public Interface %
%%%%%%%%%%%%%%%%%%%%

% A type predicate that accepts books, scores and music.
#(define (ly:book-or-score-or-music? x)
   (or (ly:book? x) (ly:score? x) (ly:music? x)))

% declaring and declare are the two functions that tell partify which output
% belongs to which part. The only difference between the two functions is that
% declare is a void function that does nothing but declare a part to partify
% whereas declaring can be used if you also want to bind the music to a variable
% at the same time. For example you could use:
%       CelloVoice = \declaring \new \Voice = "Cello" { ... }
% This line creates a new voice named "Cello" and assigns it to the variable
% CelloVoice. As a side effect the Voice is also declared as a part "Cello" to
% partify.
% As you can see from the example you can use context names as part names.
% Alternatively you can specify the name of the part separately:
%       CelloVoice = \declaring "Cello" \new \Voice { ... }
% If you specify both an explicit part name and a context name the explicit part
% name will take precedence for partify.
#(define declaring (define-music-function
                    (name declared)
                    ((string-or-symbol? #f) ly:book-or-score-or-music?)
                    (set! pfy:declarations (cons (cons (pfy:declaration-name name declared)
                                                       declared)
                                                 pfy:declarations))
                    declared))

#(define declare (define-void-function
                   (name declared)
                   ((string-or-symbol? #f) ly:book-or-score-or-music?)
                   (set! pfy:declarations (cons (cons (pfy:declaration-name name declared)
                                                      declared)
                                                pfy:declarations))))

% The partify function tells partify to evaluate the command line options and
% actually typeset the parts.
#(define (partify)
   (map pfy:process-declaration (pfy:partnames)))

% Generates the filename for a part to be saved. The partname parameter is the
% name of the part (as a string).
#(define (partify-filename partname)
   ; (string-append (ly:parser-output-name) " - " partname)
   partname)



%%%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%%%

% Checks if the lst parameter is a list of strings.
#(define (string-list? lst)
   (and (list? lst)
        (map string? lst)))

%%%%%%%%%%%%%
% Internals %
%%%%%%%%%%%%%

%%%%%%%%%% Internal Variables %%%%%%%%%%

% The accumulator for the declarations available to partify.
#(define pfy:declarations '())

#(define (pfy:default-paper) (ly:modules-lookup (list (current-module)) '$defaultpaper))
#(define (pfy:default-header) (ly:modules-lookup (list (current-module)) '$defaultheader))
#(define (pfy:default-layout) (ly:modules-lookup (list (current-module)) '$defaultlayout))


%%%%%%%%%% Command Line Options %%%%%%%%%%

% Returns a list of symbols identifying the parts that should be compiled.
#(define (pfy:partnames)
   (let ((partnames-literal (or (ly:get-option partifyOption)
                                defaultPartName)))
        (pfy:parse-partnames partnames-literal)))

% Converts the partnames specified by the user into a list of strings.
#(define (pfy:parse-partnames partnames)
   (cond ((symbol? partnames) (pfy:parse-partnames (symbol->string partnames)))
         ((symbol-list? partnames) (map symbol->string partnames))
         ((string? partnames) (string-split partnames #\,))
         ((string-list? partnames) partnames)
         (else (ly:error "The defaultPartName must be one or more symbols or a string."))))


%%%%%%%%%% Declaring Parts %%%%%%%%%%

% This function returns the name partify should associate a declaration with.
% - The parameter name is an explicit part name or #f if none was specified.
% - The declared parameter is the LilyPond content that should be made available
%   to partify.
% If no part name can be inferred this function raises an error.
#(define (pfy:declaration-name name declared)
   (or name
       (and (ly:context? declared) (ly:context-id declared))
       (and (ly:prob? declared) (ly:prob-property declared 'context-id))
       (ly:error "Must specify a name for declaration")))


%%%%%%%%%% Processing Declarations %%%%%%%%%%

% Takes the name of a partify declaration and typesets a book containing that
% declaration.
#(define (pfy:process-declaration name)
   (let ((filename (partify-filename name))
         (book (pfy:make-book name)))
     (ly:book-process book
                      (pfy:default-paper)
                      (pfy:default-layout)
                      filename)))

% Takes the name of a partify declaration and creates a corresponding book
% containing that declaration.
#(define (pfy:make-book name)
   (let ((declaration (pfy:lookup-declaration name)))
     (if declaration
         (pfy:any->book declaration)
         (ly:error (string-append "Part \"" name "\" has not been declared yet. Did you forget to set the defaultPartName?")))))

% Finds a partify declaration for the specified name. If
% lookupPartsByVariableName is true this function may search vor a variable with
% the specified name.
#(define (pfy:lookup-declaration name)
   (or (and (assoc name pfy:declarations) (cdr (assoc name pfy:declarations)))
       (and lookupPartsByVariableName
            (ly:modules-lookup (list (current-module)) (string->symbol name)))))

% Converts books, scores and music into books. If any is already a book it is
% simply returned. Otherwise it is wrapped in a book and then returned.
% This function is used to convert a declaration for a part into an exportable
% book that can be processed by typesetting.
#(define (pfy:any->book any)
   (cond ((ly:book? any) any)
         ((ly:score? any) (pfy:score->book any))
         ((ly:music? any) (pfy:music->book any))
         (else (ly:error "Could not process partify declaration. \\declare and \\declaring can only process books, scores and musical expressions."))))

% Converts a score into a book containing that score.
#(define (pfy:score->book score)
   (ly:make-book (pfy:default-paper)
                 (pfy:default-header) ; TODO: Add Part name.
                 score))

% Converts music into a book containing that music.
#(define (pfy:music->book music)
   (pfy:score->book (ly:make-score music)))
