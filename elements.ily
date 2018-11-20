\version "2.19.65"

dolce = \markup { \italic "dolce" }

longRest = {
  \once \override MultiMeasureRestNumber.stencil = ##f
  R1*50
}
forceBarNumber = \once \override Score.BarNumber.break-visibility = #'#(#t #t #t)
