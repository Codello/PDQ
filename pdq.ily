%!======================================================================================
%! Section: The PDQ Stylesheet
%! This file is the main entry point to the PDQ style sheet. This file should be
%! included in your own projects in order to use the PDQ styles. This file does provide
%! some functions however in most cases you don't need to use them directly as the
%! stylesheet can be customized via options.
%!======================================================================================
\version "2.19.65"

#(ly:set-option 'relative-includes #t)
\include "util.ily"
\include "pdq-header.ily"
\include "elements.ily"
#(ly:set-option 'relative-includes #f)

%! -------------------------------------------------------------------------------------
%! Group: Options
%! The PDQ stylesheet can be customized using options. Options must be specified
%! *before* the pdq.ily file is included. For example:
%! --- LilyPond
%! dinPaper = ##t
%! \include "pdq.ily"
%! ---
%!--------------------------------------------------------------------------------------

%! Option: dinPaper
%! --- Prototype
%! dinPaper = ##f
%! ---
%! By default PDQ uses a custom paper format. However this may prove difficult when
%! printing. This option overrides the default paper format and makes PDQ use the A4
%! paper format instead.
opt-din-paper = #(get-option 'dinPaper #f)

%! Option: debug
%! --- Prototype
%! debug = ##f
%! ---
%! Set this option to #t to enable the debug mode. The debug mode is intended to help
%! debug layout issue. PDQ will disable some spacing optimizations and will show
%! annotations for flexible spacing variables. Also this will treat all warnings as
%! errors. Debug mode should never be used when publishing scores.
opt-debug = #(get-option 'debug #f)

%! Option: strict
%! --- Prototype
%! strict = ##f
%! ---
%! Enables strict layout by enabling things ragged bottom pages. This should only be
%! used for debugging purposes. If you want to enable ragged bottoms do so by setting it
%! in a \paper block. In the future this option may include additional modifications
%! that are not suitable for publishing.
opt-strict = #(get-option 'strict #f)

%! Option: repeatTitle
%! --- Prototype
%! repeatTitle = ##t
%! ---
%! By default the title is printed in the header of each of each page. If you don't want
%! this behavior set the option to #f to suppress the title on subsequent pages.
opt-repeat-title = #(get-option 'repeatTitle #t)

%! Option: twoside
%! --- Prototype
%! twoside = ##t
%! ---
%! By default PDQ assumes that you will be creating a two sided score (that is two pages
%! will be visible next to each other with the exception of the first and possibly the
%! last page). To turn off this behavior set twoside to #f.
opt-twoside = #(get-option 'twoside #t)

%! Option: repeatFooter
%! --- Prototype
%! repeatFooter = ##f
%! ---
%! Print the footer not only on the first but on all pages.
opt-repeat-footer = #(get-option 'repeatFooter #f)

%! Option: defaultTagline
%! --- Prototype
%! defaultTagline = #f
%! ---
%! By default PDQ customizes the LilyPond tageline. To turn off these modifications set
%! defaultTageline to #t.
opt-default-tagline = #(get-option 'defaultTagline #f)


% Apply Option Values
#(if opt-debug (escalate-warnings))

\header {
  #(if (not opt-default-tagline) (define tagline "Created with LilyPond"))
}


%!--------------------------------------------------------------------------------------
%! Group: Custom Definitions
%! The following definitions are used in the PDQ stylesheet automatically.
%!--------------------------------------------------------------------------------------

%! Variable: Henle-scripts
%! These script tweaks are applied to the Script context.
#(define Henle-scripts '(
   ("staccato" . ((padding . 0.4)))
   ("fermata" . ((padding . 0.75)))))

%! Function: format-mark-pdq
%! Formats rehearsal marks for the PDQ stylesheet. The formatter puts every mark (as a
%! letter) in a square box. The marks are printed quite large for better readability.
#(define (format-mark-pdq mark context)
  (markup
    (#:box (#:pad-to-box '(-2 . 2) '(-2 . 2) (#:hcenter-in 4 (#:vcenter
      (#:bold (#:fontsize 3 (make-markalphabet-markup (1- mark))))))))))

%! Function: format-mark-pdq-numeric
%! Formats rehearsal marks with numbers instead of letters. The formatter puts every
%! mark in a square box. The marks are printed quite large for better readability.
#(define (format-mark-pdq-numeric mark context)
  (markup
    (#:box (#:pad-to-box '(-2 . 2) '(-2 . 2) (#:hcenter-in 4 (#:vcenter
      (#:bold (#:fontsize 3 (number->string (1- mark))))))))))

%!======================================================================================
%! Section: PDQ Paper
%! The PDQ stylesheet heavily modifies the LilyPond \paper. This section documents some
%! of the modifications.
%!======================================================================================

%! Topic: The PDQ Paper Format
%! The PDQ stylesheet adds its own "pdq" paper format to LilyPond's paper-alist. The PDQ
%! paper size is 23,3 cm x 30,9 cm.
#(set! paper-alist (cons '("pdq" . (cons (* 233 mm) (* 309 mm))) paper-alist))

\paper {
  %! Topic: Styling Options
  %! The PDQ layout configures the base layout a little:
  %!
  %!   - Page numbers are printed.
  %!   - A default indent is set.
  %!   - Options for two sided layout, debugging and strict spacing are set.

  annotate-spacing = #opt-debug
  check-consistency = #opt-debug

  indent = 15\mm

  print-page-number = ##t
  print-first-page-number = ##f

  two-sided = #opt-twoside

  ragged-right = #opt-strict
  ragged-bottom = #opt-strict
  ragged-last-bottom = #opt-strict

  %! Topic: Spacing
  %! The PDQ stylesheet heavily adjusts the default spacing variables. The main
  %! intention is to provide enough space for hand written annotations in scores. This
  %! is also the reason for PDQ's default paper format.

  % Default Paper Size
  #(set-paper-size "pdq")

  % DIN A4 Paper Support
  paper-height = #(if opt-din-paper 297 309)
  paper-width = #(if opt-din-paper 210 233)

  top-margin = 8\mm
  bottom-margin = 8\mm
  left-margin = 12\mm
  right-margin = 12\mm

  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 5))
  top-system-spacing =
    #'((basic-distance . 12)
       (minimum-distance . 10)
       (padding . 4)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 5)
       (minimum-distance . 3)
       (padding . 1.5)
       (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 12)
       (minimum-distance . 6)
       (padding . 0)
       (stretchability . 30)) % 60
  markup-markup-spacing =
    #'((basic-distance . 10)
       (padding . 3))
  % TODO: Add Option to reduce space between systems
  system-system-spacing =
    #'((basic-distance . 13)
       (minimum-distance . 10)
       (padding . 3)
       (stretchability . 30)) % 10
  % TODO: Tweak the following values. Possibly only for dinPaper
  page-breaking-system-system-spacing =
    #'((basic-distance . 13)
        (minimum-distance . 10)
        (padding . 1)
        (stretchability . 0))

  %! Topic: Fonts
  %! The PDQ stylesheet uses non-standard fonts. For musical notation it uses the
  %! beethoven font and for sans-serif texts it uses the Yorkten font.

  fonts = #(set-global-fonts
    #:music "beethoven"
    #:brace "beethoven"
    #:roman "Times New Roman"
    #:sans "Yorkten Thin"
    #:typewriter "monospace"
    #:factor (/ staff-height pt 20)
  )

  %!------------------------------------------------------------------------------------
  %! Group: Page Content
  %! The PDQ stylehseet defines its own page headers and footers. The individual
  %! elements are described next. For a more high-level overview of the possible
  %! configuration regarding headers see the <The PDQ Layout>.
  %!------------------------------------------------------------------------------------

  %! Event: headerCenter
  %! At the top of each page the instrument name and book title is displayed.
  headerCenter = \markup {
    \sans \on-the-fly #not-first-page \concat {
      \if-true #opt-repeat-title {
        \fromproperty #'header:title
        \when-property #'header:instrument " - "
      }
      \fromproperty #'header:instrument
    }
  }

  %! Event: evenHeaderMarkup
  %! The header displays page numbers on the left (or on the outer side for twoside
  %! layouts).
  evenHeaderMarkup = \markup
  \fill-line {
    \larger \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
    \headerCenter
    \null
  }

  %! Event: oddHeaderMarkup
  %! The header displays page numbers on the left (or on the outer side for twoside
  %! layouts).
  oddHeaderMarkup = \markup \if-else #opt-twoside \fill-line {
    \null
    \headerCenter
    \larger \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
  } \evenHeaderMarkup

  %! Event: firstPageFooter
  %! The footer on the first page displays information about the publisher, version,
  %! copyright, and a tagline. If the <repeatFooter> option is #t the footer is printed
  %! on every page.
  firstPageFooter = \markup \fill-line {
    \left-column {
      \tiny \fromproperty #'header:publisher
      \when-property #'header:version {
        \vspace #-0.25
        \tiny \fromproperty #'header:version
      }
    }

    \right-column {
      \tiny \fromproperty #'header:copyright
      \when-property #'header:tagline {
        \vspace #-0.25
        \tiny \fromproperty #'header:tagline
      }
    }
  }

  % Repeat the footer on every page if repeatFooter is #t.
  firstPageFooter = \markup \if-else #opt-repeat-footer \firstPageFooter { \on-the-fly \first-page \firstPageFooter }

  %! Event: oddFooterMarkup
  %! On odd pages the footer is displayed on the right.
  oddFooterMarkup = \markup \sans \fill-line {
    \null
    \firstPageFooter
  }

  %! Event: oddFooterMarkup
  %! On odd pages the footer is displayed on the left (if a twoside layout is used).
  evenFooterMarkup = \markup \if-else #opt-twoside \sans \fill-line {
    \firstPageFooter
    \null
  } \oddFooterMarkup

  %! Event: bookTitleMarkup
  %! Prints the header of a \book. See <The PDQ Layout> for details.
  bookTitleMarkup = \markup \column {
    \vspace #-2.1
    \sans {
      \pdqBookHeadlineMarkup
      \vspace #1
      % TODO: Add Option to include here
      %\pdqComposerMarkup
      %\vspace #-1
    }
  }

  %! Event: scoreTitleMarkup
  %! Prints the header of a \score. See <The PDQ Layout> for details.
  scoreTitleMarkup = \markup \column {
    % \vspace #1
    \sans {
      \when-some-properties #'(header:piece header:subpiece header:subsubpiece) {
        \pdqScoreHeadlineMarkup
        \vspace #1
      }
      \when-not-property #'header:movement {
        \vspace #-1.25
      }
      \when-some-properties #'(header:exerpt header:movement header:composer header:opus header:arranger) {
        \pdqComposerMarkup
        % \vspace #-0.5
      }
    }
  }
}


%!======================================================================================
%! Section: PDQ Layout
%! The PDQ stylesheet heavily customizes the default LilyPond layout. This documentation
%! provides an overview over the modifications.
%!
%! In addition to the options the PDQ stylesheet can be customized using layouts. A
%! layout is basically just a \layout block with some options applied to it. You can use
%! layouts just as you would use any other layout:
%! --- LilyPond
%! \layout {
%!   \partLayout
%!   % More customizations
%! }
%! ---
%!======================================================================================
\layout {
  %!------------------------------------------------------------------------------------
  %! Group: Context Tweaks
  %! The default PDQ layout tweaks multiple contexts to achieve its layout.
  %!------------------------------------------------------------------------------------

  %! Context: Score
  %! The main parts of PDQ's modifications apply to the Score context. Modifications
  %! include:
  %!
  %! - Slightly adjusted note thicknesses.
  %! - Tweaked accidentals and slurs to better match the beethoven font.
  %! - A custom rehearsal mark formatter using the <format-mark-pdq> function.
  %! - Adjusted bar number and instrument name fonts.
  %! - Adjusted spacing for handwritten annotations.
  \context {
    \Score
    % Notes
    \override SpacingSpanner.shortest-duration-space = #2.4
    \override Beam.beam-thickness = #0.5
    quotedCueEventTypes = #'(note-event rest-event tie-event
                             beam-event tuplet-span-event
                             dynamic-event slur-event) % Includes Dynamics

    % Accidentals and Slurs
    \override Script.before-line-breaking = #(custom-script-tweaks Henle-scripts)
    \override Script.outside-staff-priority = ##f
    \override Slur.thickness = #2
    \override Slur.height-limit = #3
    \override Slur.ratio = #0.2

    % Marks
    markFormatter = #format-mark-pdq
    \override MetronomeMark.font-size = #1
    \override MetronomeMark.padding = #1.25
    \override RehearsalMark.padding = #2.8

    \override BarLine.space-alist.next-note = #'(semi-fixed-space . 1.75)

    % Bar Numbers
    \override BarNumber.font-shape = #'italic
    \override BarNumber.font-size = #0
    \override BarNumber.self-alignment-X = #LEFT
    \override BarNumber.Y-offset = #4

    % Instrument Names
    \override InstrumentName.padding = #0.5
    \override InstrumentName.font-size = #1

    % Spacing
    \override VerticalAxisGroup.staff-staff-spacing =
      #'((basic-distance . 11.5)
         (minimum-distance . 9)
         (padding . 1.2)
         (stretchability . 2))
  }

  %! Context: Staff
  %! PDQ modifies the Staff context to use modern accidentals as well as matching fonts.
  \context {
    \Staff
    \override NoteHead.font-size = #-0.5
    \override Script.padding = #0.6
    \accidentalStyle modern
    \override MeasureCounter.direction = #UP
    \override MeasureCounter.font-series = #'bold
    \override MeasureCounter.font-shape = #'italic
  }

  %! Context: PianoStaff
  %! Stems spanning multiple systems are connected.
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
  }

  %! Context: Dynamics
  %! Adjusted fonts and better readability.
  \context {
    \Dynamics
    \override DynamicTextSpanner.whiteout = ##f
    \override DynamicTextSpanner.layer = #2
    \override DynamicTextSpanner.font-size = #0
    \override TextScript.font-shape = #'italic
  }

  %! Context: Voice
  %! Adjusted spacing and fonts.
  \context {
    \Voice {
      \override DynamicTextSpanner.whiteout = ##t
      \override DynamicTextSpanner.layer = #2
      \override DynamicTextSpanner.font-size = #0
      \override DynamicTextSpanner.style = #'none
      \override DynamicLineSpanner.staff-padding = #1.5
      \override TextScript.font-shape = #'italic
    }
  }

  %! Context: CueVoice
  %! Does not display dynamic spanners.
  \context {
    \CueVoice {
      \override DynamicTextSpanner.style = #'none
      % \override TextScript.font-shape = #'italic
    }
  }
}

%!--------------------------------------------------------------------------------------
%! Group: Selectable Layouts
%! You can easily apply specific layout tweaks using one of the predefined layouts. The
%! layouts are specialized for specifiec use cases but feel free to use any layout
%! however you may see fit. Some of the layouts also provide an associated paper
%! definition that applies some modifications to PDQ's default paper.
%!
%! When using a predefined layout you must make sure that you include your own layout
%! tweaks *after* using the predefined layout. Otherwise your tweaks will be overridden
%! even if the predefined layout does not define another value for some property. This
%! is due to the fact that layout blocks capture the layout at definition time.
%!
%! For the same reason you unfortunately cannot mix different predefined layouts. The
%! last layout in a \layout block will take precedence over all others.
%!
%! Example:
%! --- LilyPond
%! \layout {
%!   \partLayout
%!   % After using the \partLayout we can add our own customizations.
%!   indent = 5\mm
%! }
%!
%! % When we don't want to apply further customizations one may omit the outer \layout
%! % or \paper block.
%! \partPaper
%! ---
%!--------------------------------------------------------------------------------------


%! Function: partLayout
%! --- Prototype
%! partLayout = \layout {...}
%! ---
%! The part layout can be used to set single voice parts (but not neccesarily for
%! scores). It is not intended to be used for piano parts. This includes the skipBars
%! option, removes instrument names etc.
%!
%! For partLayouts it is recommended to also use the <partPaper>.
partLayout = \layout {
  indent = 0\mm
  \context {
    \Score
    skipBars = ##t
  }
  \context {
    \Staff
    \remove "Instrument_name_engraver"
    \consists "Page_turn_engraver"
    \consists "Measure_counter_engraver"
  }
}

%! Function: transposedPartLayout
%! --- Prototype
%! transposedPartLayout = \layout {...}
%! ---
%! A variant of the \partLayout that is optimized for transposed instruments (e.g.
%! french horn or clarinet). The layout keeps a small indent in order to enable
%! placement of a transposition info in the instrumentName of a staff.
%!
%! %! For transposedPartLayouts it is recommended to also use the <partPaper>.
transposedPartLayout = \layout {
  indent = 10\mm
  \context {
    \Score
    skipBars = ##t
  }
  \context {
    \Staff
    \consists "Page_turn_engraver"
    \consists "Measure_counter_engraver"
    \override InstrumentName.font-name = "Yorkten Regular,"
    \override InstrumentName.font-size = #2.5
  }
}

%! Function: partPaper
%! --- Prototype
%! partPaper = \paper {...}
%! ---
%! This paper is optimized for printing parts. It enables turn based page breaking and
%! adds page numbers.
partPaper = \paper {
  page-breaking = #ly:page-turn-breaking
  auto-first-page-number = ##t
}

%! Function: exerptLayout
%! --- Prototype
%! exerptLayout = \layout {...}
%! ---
%! This layout should be used for small exerpts of music. It should be combined with
%! <exerptPaper>.
exerptLayout = \partLayout

%! Function: exerptPaper
%! --- Prototype
%! exerptPaper = \paper {...}
%! ---
%! This paper should be used for small exerpts of music. It enables a ragged bottom for
%! the last page.
exerptPaper = \paper {
  ragged-last-bottom = ##t
}

%! Function: scoreLayout
%! --- Prototype
%! \scoreLayout = \layout {...}
%! ---
%! The score layout can be used to set full scores with many voices and staffs.
scoreLayout = \layout {
  indent = 20\mm
  short-indent = 7.5\mm
  \context {
    \Score
    skipBars = ##f
  }
}

%! Function: smallScoreLayout
%! --- Prototype
%! smallScoreLayout = \layout {...}
%! ---
%! This layout (like the score layout) can be used to set small scores with multiple but
%! not too many voices and staffs. It is up to you when a score is "small".
smallScoreLayout = \layout {
  indent = 20\mm
  short-indent = 0\mm
  \context {
    \Score
    skipBars = ##f
  }
}
