# The PDQ LilyPond Stylesheet

![Documentation](https://github.com/Codello/PDQ/workflows/Documentation/badge.svg)

This is the PDQ LilyPond Stylesheet, a simple to use modern layout for scores and parts. The code is on [GitHub](https://github.com/Codello/PDQ), the documentation is available [here](https://codello.github.io/PDQ).

## Installation

The stylesheet uses non-standard fonts that need to be installed before PDQ can be used. In order to use the stylesheet do the following:

- Install the OpenLilyPondFont *Beethoven* from [here](https://github.com/OpenLilyPondFonts/beethoven). Make sure that your LilyPond installation can find the font files.
- Install the [Yorkten](https://www.fontspring.com/fonts/insigne/yorkten) font on your system.
- Clone the PDQ git repository.

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

For a description of available options see the [documentation](https://codello.github.io/PDQ/#File:pdq.ily).

### Headers

The PDQ stylesheet uses custom page layouts and headers. It also adds support for additional fields that can be used in headers. See the [documentation](https://codello.github.io/PDQ/#File:pdq-header.ily) for a list of available fields.

### Layouts

The resulting PDQ can be further customized using *Layouts*. See the [documentation](https://codello.github.io/PDQ/#File:pdq.ily) for details.

## Package Contents

Besides the PDQ layout this package also contains some utilities that might be useful when working with bigger projects. See the [documentation](https://codello.github.io/PDQ) for details.

