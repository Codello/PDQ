# The PDQ Stylesheet

The PDQ stylesheet is a modern stylesheet for typesetting scores and parts that implements most of the common notational features. PDQ uses quite a lot of different notation fonts and works best if everything is installed correctly (see Installation).

## Installation

###### LilyPond
For the PDQ template to produce the best results you should make sure that you are using LilyPond 2.19.x. PDQ was tested using LilyPond 2.19.65.

###### Text Fonts
The PDQ template uses non-standard text fonts. The roman font is Times New Roman but the Sans font is Yorkten. It is recommended that you install the entire Yorkten font family for the best results.

###### Notation Fonts
Since version 2.19 LilyPond supports alternative music notation fonts. PDQ uses the Beethoven font as its default notation fonts. You can download the Beethoven font from [here](https://github.com/OpenLilyPondFonts/beethoven). Other notation fonts are available on [GitHub](https://github.com/OpenLilyPondFonts). To install the fonts they need to be found by FontConfig. The straightforward way is to copy the contents of the `svg` and `otf` folders to `/share/lilypond/current/fonts/svg` and `/share/lilypond/current/fonts/otf` respectively. Instead of `current` the folder may also have the name of your LilyPond version.

*Note: On macOS you should copy the files to `/Applications/LilyPond/Contents/Resources/share/...` and not to `/share/...`.*

## Usage

The usage of the PDQ stylesheet is straightforward. Just include the pdq.ily file using
```lilypond
\include "pdq.ily"
```
That's it. The PDQ stylesheet is not active. The stylehseet should however be included somewhere at the top of your `.ly` file to avoid conflicts with your own definitions.

When compiling you should of course include the `lib` directory in your search paths using the `--include` option of LilyPond or specify the full path of the `pdq.ily` file.

### Options
The stylesheet supports a number of options. Those options are documented in the `pdq.ily` file. You use the options by setting flags and defining variables **before** including the `pdq.ily` file.

In the following example the `dinpaper` option is specified. The `dinpaper` option tells PDQ to use the DIN A4 paper format (instead of its own format).
```lilypond
dinpaper = ##t
\include "pdq.ily"
```
