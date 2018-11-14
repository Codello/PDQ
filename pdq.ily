\version "2.19.65"

#(ly:set-option 'relative-includes #t)
\include "util.ily"
#(ly:set-option 'relative-includes #f)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Documentation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          The PDQ LilyPond Stylesheet                         %
%                 A LilyPond stylesheet dedicated to PDQ Bach.                 %
%                          Created by Kim Wittenburg                           %
%                              Version 1.0-alpha                               %
%                                                                              %
% Usage: \include "pdq.ily". That's it.                                        %
%                                                                              %
% Options                                                                      %
% =======                                                                      %
% Options need to be declared before including this file. Options are declared %
% as ordinary (boolean) variables.                                             %
% - dinPaper: Sets the paper size to DIN A4 instead of the custom PDQ format.  %
%             This option is set to #f by default.                             %
% - repeatTitle: By default title is printed in the header of each of each     %
%                page. Set the option to ##f to suppress this behavior. This   %
%                option is ##t by default.                                     %
% - debug: Enables extra debugging output. This should never be set to #t when %
%          setting the final output. The default is #f.                        %
% - twoside: Specifies whether to use a double sided layout. The default is    %
%            #t.                                                               %
% - defaultTagline: Tells PDQ to use default tagline instead of a modified     %
%                   one.                                                       %
%                                                                              %
% Modes                                                                        %
% =====                                                                        %
% Modes are special commands that heavily modify the appearance of the score.  %
% Please note that modes modify paper variables and may modify layout          %
% variables as well. Because of this you should use modes before any other     %
% custom tweaks, otherwise your changes will be overridden. For the same       %
% reason only one mode can be active at a time. Only the latest mode will be   %
% used.                                                                        %
% The following modes are available:                                           %
% - \strictMode: Enables strict layout by enabling things ragged bottom pages. %
%                This mode should only be used for debugging purposes.         %
%                                                                              %
% Layouts                                                                      %
% =======                                                                      %
% Layouts are commands/variables that enable layout changes. Layouts can be    %
% used to easily modify your layout to fit a specific purpose. If you use      %
% layouts you must include them before any custom layout changes. Otherwise    %
% your changes would be overridden by the layout command.                      %
%                                                                              %
% For example you could include the \partlayout addition by writing            %
%       \layout {                                                              %
%         \partlayout                                                          %
%         ... Your layout changes ...                                          %
%       }                                                                      %
% As with modes layouts can not be mixed. Only one layout can be active at a   %
% time.                                                                        %
%                                                                              %
% The following layouts are available:                                         %
% - \partLayout: Enables a layout that is suitable for single instrument parts %
%                (but not neccesarily for scores). This includes the skipBars  %
%                option, removes instrument names etc.                         %
% - \scoreLayout: Enables a layout that is suitable for typical scores with    %
%                 many voices and staffs. This includes for example instrument %
%                 names and disables skipBars etc.                             %
% - \smallScoreLayout: Enables a layout that is suitable for small scores      %
%                      (with few voices and staffs), e.g. chamber music        %
%                      scores. Most notably this layout expects that there are %
%                      no shortInstrumentNames.                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
opt-debug = #(get-option 'debug #f)
opt-repeat-title = #(get-option 'repeatTitle #t)
opt-din-paper = #(get-option 'dinPaper #f)
opt-twoside = #(get-option 'twoside #t)
opt-default-tagline = #(get-option 'defaultTagline #f)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Custom Definitions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define Henle-scripts '(
   ("staccato" . ((padding . 0.4)))
   ("fermata" . ((padding . 0.75)))))

% format-mark-pdq is a markFormatter for rehearsal marks. The formatter puts
% every mark (as a letter) in a square box. The marks are printed quite large.
#(define (format-mark-pdq mark context)
  (markup
    (#:box (#:pad-to-box '(-2 . 2) '(-2 . 2) (#:hcenter-in 4 (#:vcenter
      (#:bold (#:fontsize 3 (make-markalphabet-markup (1- mark))))))))))

% PDQ Paper Size: 23,3 cm x 30,9 cm
% The PDQ template uses the pdq paper size by default. The PDQ paper size is
% 23,3 cm x 30,9 cm. This paper size is set by default unless you define
%       dinPaper = ##t
% in which case the paper size is set to DIN A4.
#(set! paper-alist (cons '("pdq" . (cons (* 233 mm) (* 309 mm))) paper-alist))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Defaults %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\header {
  #(if (not opt-default-tagline) (define tagline "Created with LilyPond"))
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PDQ Paper %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\paper {
  %%%%%%%%%%%%%%%%%%%
  % Styling Options %
  %%%%%%%%%%%%%%%%%%%

  annotate-spacing = #opt-debug
  check-consistency = #opt-debug

  indent = 15\mm
  ragged-bottom = ##f
  ragged-last-bottom = ##f

  print-page-number = ##t
  print-first-page-number = ##f
  
  two-sided = #opt-twoside

  %%%%%%%%%%%%%%%%%%%%
  % Breaking Options %
  %%%%%%%%%%%%%%%%%%%%
  % TODO: Move to \partLayout
  page-breaking = #ly:page-turn-breaking
  auto-first-page-number = ##t

  %%%%%%%%%%%
  % Spacing %
  %%%%%%%%%%%

  % Default Paper Size
  #(set-paper-size "pdq")

  % DIN A4 Paper Support
  paper-height = #(if opt-din-paper 297 309)
  paper-width = #(if opt-din-paper 210 233)

  % TODO: Use Scaling Units with Paper Size
  top-margin = 8\mm
  bottom-margin = 8\mm
  left-margin = 12\mm
  right-margin = 12\mm

  top-system-spacing =
    #'((basic-distance . 12)
       (minimum-distance . 10)
       (padding . 4)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 3)
       (minimum-distance . 3)
       (padding . 3)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 13)
       (minimum-distance . 10)
       (padding . 2)
       (stretchability . 12))

  %%%%%%%%%
  % Fonts %
  %%%%%%%%%

  fonts = #(set-global-fonts
    #:music "beethoven"
    #:brace "beethoven"
    #:roman "Times New Roman"
    #:sans "Yorkten Thin"
    #:typewriter "monospace"
    #:factor (/ staff-height pt 20)
  )

  %%%%%%%%%%%%%%%%
  % Page Headers %
  %%%%%%%%%%%%%%%%
  headerCenter = \markup {
    \sans \on-the-fly #not-first-page \concat {
      \if-true #opt-repeat-title {
        \fromproperty #'header:title
        \when-property #'header:instrument " - "
      }
      \fromproperty #'header:instrument
    }
  }

  evenHeaderMarkup = \markup
  \fill-line {
    \larger \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
    \headerCenter
    \null
  }

  oddHeaderMarkup = \markup \if-else #opt-twoside \fill-line {
    \null
    \headerCenter
    \larger \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
  } \evenHeaderMarkup

  %%%%%%%%%%%%%%%%
  % Page Footers %
  %%%%%%%%%%%%%%%%


  footerCenter = \markup \center-column {
    % First Page Footer
    \on-the-fly \first-page {
      \override #'(font-name . "Yorkten Thin, Italic")
      \tiny \fromproperty #'header:copyright
    }
    % Last Page Footer
    \on-the-fly \last-page {
      \override #'(font-name . "Yorkten Thin,")
      \pad-to-box #'(0 . 0) #'(0 . 3) \tiny \fromproperty #'header:tagline
    }
  }

  versionMarkup = \markup \column {
    \concat {
      \when-property #'header:version {
        "Version "
        \fromproperty #'header:version
      }
    }
  }

  oddFooterMarkup = \markup \sans \fill-line {
    \null
    \footerCenter
    \versionMarkup
  }

  evenFooterMarkup = \markup \if-else #opt-twoside \sans \fill-line {
    \versionMarkup
    \footerCenter
    \null
  } \oddFooterMarkup

  %%%%%%%%%%
  % Titles %
  %%%%%%%%%%
  bookTitleMarkup = \markup \column {
    \vspace #-2.1
    \sans {
      \fill-line {
        \column {
          \vspace #3
          \when-property #'header:instrument \box \pad-markup #1 {
            \abs-fontsize #14
            \fromproperty #'header:instrument
          }
          \when-not-property #'header:instrument \null
        }

        \override #'(baseline-skip . 4.5)
        \center-column {
          \vspace #0.2
          \when-property #'header:dedication {
            \override #'(font-name . "Yorkten Light Italic,")
            \abs-fontsize #10
            \fromproperty #'header:dedication
            \vspace #0.7
          }
          \when-not-property #'header:dedication {
            \vspace #2.2
          }

          \override #'(font-name . "Yorkten Light,")
          \abs-fontsize #24
          \fromproperty #'header:title

          \abs-fontsize #17
          \fromproperty #'header:subtitle

          \vspace #-0.25
          \abs-fontsize #14
          \fromproperty #'header:subsubtitle
        }

        \null
      }

      \vspace #1

      \fill-line {
        \null

        \abs-fontsize #11
        \concat {
          \fromproperty #'header:composer
          \when-property #'header:opus {
            "  " \fromproperty #'header:opus
          }
        }
      }
      \vspace #-0.3
      \fill-line {
        \null
        \abs-fontsize #8
        \fromproperty #'header:arranger
      }
      \vspace #-1
    }
  }

  scoreTitleMarkup = \markup \column {
    \fill-line {
      \abs-fontsize #28
      \fromproperty #'header:piece
    }
  }
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PDQ Layout %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\layout {
  \context {
    \Score

    %%%%%%%%%
    % Notes %
    %%%%%%%%%
    \override SpacingSpanner.shortest-duration-space = #2.4
    \override Beam.beam-thickness = #0.5

    %%%%%%%%%%%%%%%%%%%%%%%%%
    % Accidentals and Slurs %
    %%%%%%%%%%%%%%%%%%%%%%%%%
    \override Script.before-line-breaking = #(custom-script-tweaks Henle-scripts)
    \override Script.outside-staff-priority = ##f
    \override Slur.thickness = #2
    \override Slur.height-limit = #3
    \override Slur.ratio = #0.2

    %%%%%%%%%
    % Marks %
    %%%%%%%%%
    markFormatter = #format-mark-pdq
    \override MetronomeMark.font-size = #1
    \override MetronomeMark.padding = #1.25

    \override BarLine.space-alist.next-note = #'(semi-fixed-space . 1.75)

    %%%%%%%%%%%%%%%
    % Bar Numbers %
    %%%%%%%%%%%%%%%
    \override BarNumber.font-shape = #'italic
    \override BarNumber.font-size = #0
    \override BarNumber.self-alignment-X = #LEFT
    \override BarNumber.Y-offset = #4

    %%%%%%%%%%%%%%%%%%%%
    % Instrument Names %
    %%%%%%%%%%%%%%%%%%%%
    \override InstrumentName.padding = #0.5
    \override InstrumentName.font-size = #1

    %%%%%%%%%%%
    % Spacing %
    %%%%%%%%%%%
    \override VerticalAxisGroup.staff-staff-spacing =
      #'((basic-distance . 11.5)
         (minimum-distance . 9)
         (padding . 1.2)
         (stretchability . 2))
  }

  \context {
    \Staff
    \override NoteHead.font-size = #-0.5
    \override Script.padding = #0.6
  }
  \context {
    \PianoStaff
    \consists #Span_stem_engraver
  }
  \context {
    \Dynamics
    \override DynamicTextSpanner.whiteout = ##t
    \override DynamicTextSpanner.layer = #2
    \override DynamicTextSpanner.font-size = #0
  }
  \context {
    \Voice {
      \override DynamicTextSpanner.whiteout = ##t
      \override DynamicTextSpanner.layer = #2
      \override DynamicTextSpanner.font-size = #0
      \override DynamicTextSpanner.style = #'none
      \override DynamicLineSpanner.staff-padding = #1.5
    }
  }
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Layouts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The part layout can be used to set single voice parts. It should not be used
% for piano parts.
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
  }
}

% The score layout can be used to set full scores with many voices and staffs.
scoreLayout = \layout {
  indent = 15\mm
  short-indent = 7.5\mm
  \context {
    \Score
    skipBars = ##f
  }
}

% This layout (like the score layout) can be used to set small scores with
% multiple but not too many voices and staffs. It is up to you when a score is
% "small".
smallScoreLayout = \layout {
  indent = 15\mm
  short-indent = 0\mm
  \context {
    \Score
    skipBars = ##f
  }
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strictMode = \paper {
  ragged-right = ##t
  ragged-bottom = ##t
  ragged-last-bottom = ##t
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
