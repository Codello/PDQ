# LilyLib – LilyPond Library

This folder does not contain scores but a set of *libraries* for LilyPond. These
libraries range from small utility functions to full blown stylesheets.

## Partify

The script `partify.ily` provides some functions that make it possible to write
multiple parts or scores in a single document and choose the one you want when
compiling.

*Further documentation can be foud in the source of the script.*

## PDQ

`pdq.ily` is a stylesheet for LilyPond. Just include the file and watch your
scores become beautiful.

Note that PDQ by default does not use A4 paper format. See the PDQ documentation
for details.

## Util

`util.ily` is a collection of small utility functions. Usually you do not need
to include this file for your scores but it may be useful when developing a
library for LilyPond.

## Engraving Extras

This file contains additional engravers.
