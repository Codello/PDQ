%! Section: The PDQ Layout
%! The PDQ stylesheet implements a custom layout for books and scores that can be
%! customized by providing additional information in a respective \header block. For
%! simple cases where you only have one score in one book it is also possible to specify
%! the book and score headers in the same header block.
%!
%! Group: Book Headers
%! The book header supports the following fields. Although technically none of the
%! fields are required for successful compilation the stylesheet makes some assumptions
%! about which fields will be present.
%!
%! Fields:
%!   title       - The title of the book (required).
%!   subtitle    - The subtitle of the book (optional, but recommended).
%!   subsubtitle - A subsubtitle of the book.
%!   dedication  - A string containing a dedication of the work. This is displayed above
%!                the (sub)titles.
%!   instrument  - The name of the instrument the music is for.
%!   publisher   - The publisher of a work.
%!   version     - A string identifying the current version of a work.
%!   copyright   - A copyright string (possibly including the © symbol).
%!   tagline     - A string displayed at the bottom of a page.
%!
%! Group: Score Headers
%! The score header supports additional fields that identify a piece of music. In simple
%! cases these can be included in the same \header block as the book fields.
%!
%! Fields:
%!   piece       - The title of the piece of music (e.g. "Symphony No. 9"). This should
%!                 not be used for the movement.
%!   subpiece    - Additional information about the piece such as a title (e.g. "New
%!                 World Symphony").
%!   subsubpiece - Even more information about a piece such as its key (e.g. "in e
%!                 minor")
%!   movement    - The movement for this score. This works best if the movement is just
%!                 a roman numeral (e.g. "IV" for the fourth movement).
%!   composer    - The composer of the piece.
%!   opus        - The opus of the piece (including the "Op." prefix).
%!   arranger    - An optional arranger of a piece (including a possible prefix such as
%!                 "arr. by")
%!   exerpt      - If the score is just an exerpt you may identify the exerpt here by
%!                 for example specifiying its bar numbers.


%! Section: pdq-header.ily
%! This file contains the definitions for PDQ header elements. Usually it is not
%! neccessary to use any of these definitions directly as they are used by the PDQ
%! stylesheet automatically.
\version "2.24.0"

%!--------------------------------------------------------------------------------------
%! Group: Variables
%! This section contains definitions for variables that will be used in the default PDQ
%! markup. Users may override these values to provide some customization to the theme.
%!--------------------------------------------------------------------------------------

%! Variable: copyrightFont
%! This font is used for the copyright string.
copyrightFont = "Yorkten Thin Italic,"
% TODO: Footer Font Size

%! Variable: taglineFont
%! This font is used for the tagline.
taglineFont = "Yorkten Thin,"
% TODO: Font Size

%! Variable: titleFontSize
%! The font size for the main title.
titleFontSize = #24

%!--------------------------------------------------------------------------------------
%! Group: Page Markup
%! This section contains markup command for the various parts of a page that are used by
%! the PDQ stylesheet. You normally should have no need to use this manually.
%!--------------------------------------------------------------------------------------

%! Functions: pdqInstrumentMarkup
%! --- Prototype
%! \pdqInstrumentMarkup
%! ---
%! Prints the instrument name in a box.
pdqInstrumentMarkup = \markup \box \pad-markup #1 {
  \abs-fontsize #14
  \fromproperty #'header:instrument
}

%! Function: pdqBookHeadlineMarkup
%! --- Prototype
%! \pdqBookHeadlineMarkup
%! ---
%! Prints the headline of a \book.
pdqBookHeadlineMarkup = \markup \fill-line {
  \column {
    \vspace #3
    \when-property #'header:instrument \pdqInstrumentMarkup
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
    \abs-fontsize #titleFontSize
    \fromproperty #'header:title

    \abs-fontsize #17
    \fromproperty #'header:subtitle

    \vspace #-0.25
    \abs-fontsize #14
    \fromproperty #'header:subsubtitle
  }

  \null
}

%! Function: pdqScoreHeadlineMarkup
%! --- Prototype
%! \pdqScoreHeadlineMarkup
%! ---
%! Prints the headline of a \score.
pdqScoreHeadlineMarkup = \markup \fill-line {
  \null

  \center-column {
    \when-property #'header:piece {
      \override #'(font-name . "Yorkten Light,")
      \abs-fontsize #20
      \fromproperty #'header:piece
    }

    \when-property #'header:subpiece {
      \vspace #0.15
      \abs-fontsize #14
      \fromproperty #'header:subpiece
    }

    \when-property #'header:subsubpiece {
      \vspace #-0.25
      \abs-fontsize #10
      \fromproperty #'header:subsubpiece
    }
  }

  \null
}

%! Function: pdqComposerMarkup
%! --- Prototype
%! \pdqComposerMarkup
%! ---
%! Prints the composer section in a score.
pdqComposerMarkup = \markup \fill-line {
  \when-property #'header:exerpt {
    \abs-fontsize #12
    \replace #'(("-" . " — ")) \fromproperty #'header:exerpt
  }

  \when-property #'header:movement {
    \roman
    \abs-fontsize #22
    \fromproperty #'header:movement
  }

  \sans
  \right-column {
    \abs-fontsize #11
    \concat {
      \fromproperty #'header:composer
      \when-property #'header:opus {
        "  " \replace #'((". " . ".")) \fromproperty #'header:opus
      }
    }
    \when-property #'header:arranger {
      \vspace #-0.4
      \abs-fontsize #8
      \fromproperty #'header:arranger
    }
  }
}