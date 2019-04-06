\version "2.19.65"

%%%%%%%%%%%%%%%%%%%%%%%%%
% Font Families & Sizes %
%%%%%%%%%%%%%%%%%%%%%%%%%
copyrightFont = "Yorkten Thin Italic,"
% TODO: Footer Font Size

taglineFont = "Yorkten Thin,"
% TODO: Font Size

titleFontSize = #24

pdqInstrumentMarkup = \markup \box \pad-markup #1 {
  \abs-fontsize #14
  \fromproperty #'header:instrument
}

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

pdqComposerMarkup = \markup \fill-line {
  \when-property #'header:exerpt {
    \abs-fontsize #12
    \fromproperty #'header:exerpt
  }

  \when-property #'header:movement {
    \roman
    \abs-fontsize #24
    \fromproperty #'header:movement
  }

  \sans
  \right-column {
    \abs-fontsize #11
    \concat {
      \fromproperty #'header:composer
      \when-property #'header:opus {
        "  " \fromproperty #'header:opus
      }
    }
    \when-property #'header:arranger {
      \vspace #-0.4
      \abs-fontsize #8
      \fromproperty #'header:arranger
    }
  }
}