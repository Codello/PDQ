# The PDQ LilyPond Stylesheet

This is the PDQ LilyPond Stylesheet, a simple to use modern layout for scores and parts.

## Installation

1. Install the OpenLilyPondFont *Beethoven* from [here](https://github.com/OpenLilyPondFonts/beethoven). Make sure that your LilyPond installation can find the font files.
2. Install the [Yorkten](https://www.fontspring.com/fonts/insigne/yorkten) font on your system.
3. Clone the PDQ git repository.

**Note**: The PDQ stylesheet does not use a standard paper format by default.

## Usage

In the most basic case you just include the `pdq.ily` file in your score:

```lilypond
\include "path/to/pdq.ily"
```

The PDQ stylesheet can be customized through some options. Options need to be set before the `pdq.ily` file is loaded. For example to use the A4 paper size:

```lilypond
dinPaper = ##t
\include "path/to/pdq.ily"
```

## Package Contents

Besides the PDQ layout this package also contains some utilities that might be useful when working with bigger projects.