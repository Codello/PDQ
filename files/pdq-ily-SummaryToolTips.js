NDSummary.OnToolTipsLoaded("File:pdq.ily",{14:"<div class=\"NDToolTip TSection LLilyPond\"><div class=\"TTSummary\">This file is the main entry point to the PDQ style sheet. This file should be included in your own projects in order to use the PDQ styles. This file does provide some functions however in most cases you don\'t need to use them directly as the stylesheet can be customized via options.</div></div>",15:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">The PDQ stylesheet can be customized using options. Options must be specified <b>before</b> the pdq.ily file is included. For example:</div></div>",16:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype16\" class=\"NDPrototype NoParameterForm\">dinPaper = ##f</div><div class=\"TTSummary\">By default PDQ uses a custom paper format. However this may prove difficult when printing. This option overrides the default paper format and makes PDQ use the A4 paper format instead.</div></div>",17:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype17\" class=\"NDPrototype NoParameterForm\">debug = ##f</div><div class=\"TTSummary\">Set this option to #t to enable the debug mode. The debug mode is intended to help debug layout issue. PDQ will disable some spacing optimizations and will show annotations for flexible spacing variables. Also this will treat all warnings as errors. Debug mode should never be used when publishing scores.</div></div>",18:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype18\" class=\"NDPrototype NoParameterForm\">strict = ##f</div><div class=\"TTSummary\">Enables strict layout by enabling things ragged bottom pages. This should only be used for debugging purposes. If you want to enable ragged bottoms do so by setting it in a \\paper block. In the future this option may include additional modifications that are not suitable for publishing.</div></div>",19:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype19\" class=\"NDPrototype NoParameterForm\">repeatTitle = ##t</div><div class=\"TTSummary\">By default the title is printed in the header of each of each page. If you don\'t want this behavior set the option to #f to suppress the title on subsequent pages.</div></div>",20:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype20\" class=\"NDPrototype NoParameterForm\">bookComposer = ##f</div><div class=\"TTSummary\">Print the composer in the book markup next to the title. For multi-movemement scores you usually want to set this to #t and set scoreComposer to #f to avoid printing the composer for each new movement.</div></div>",21:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype21\" class=\"NDPrototype NoParameterForm\">scoreComposer = ##t</div><div class=\"TTSummary\">Print the composer in the score markup. This is the default. This prints the composer before every score. While this is preferred for single scores or for multi-composer books you might want to disable this option if you are engraving multiple scores from the same piece.</div></div>",22:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype22\" class=\"NDPrototype NoParameterForm\">twoside = ##t</div><div class=\"TTSummary\">By default PDQ assumes that you will be creating a two sided score (that is two pages will be visible next to each other with the exception of the first and possibly the last page). To turn off this behavior set twoside to #f.</div></div>",23:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype23\" class=\"NDPrototype NoParameterForm\">repeatFooter = ##f</div><div class=\"TTSummary\">Print the footer not only on the first but on all pages.</div></div>",24:"<div class=\"NDToolTip TVariable LLilyPond\"><div id=\"NDPrototype24\" class=\"NDPrototype NoParameterForm\">defaultTagline = #f</div><div class=\"TTSummary\">By default PDQ customizes the LilyPond tageline. To turn off these modifications set defaultTageline to #t.</div></div>",25:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">The following definitions are used in the PDQ stylesheet automatically.</div></div>",26:"<div class=\"NDToolTip TVariable LLilyPond\"><div class=\"TTSummary\">These script tweaks are applied to the Script context.</div></div>",27:"<div class=\"NDToolTip TFunction LLilyPond\"><div class=\"TTSummary\">Formats rehearsal marks for the PDQ stylesheet. The formatter puts every mark (as a letter) in a square box. The marks are printed quite large for better readability.</div></div>",28:"<div class=\"NDToolTip TFunction LLilyPond\"><div class=\"TTSummary\">Formats rehearsal marks with numbers instead of letters. The formatter puts every mark in a square box. The marks are printed quite large for better readability.</div></div>",29:"<div class=\"NDToolTip TSection LLilyPond\"><div class=\"TTSummary\">The PDQ stylesheet heavily modifies the LilyPond \\paper. This section documents some of the modifications.</div></div>",30:"<div class=\"NDToolTip TInformation LLilyPond\"><div class=\"TTSummary\">The PDQ stylesheet adds its own &quot;pdq&quot; paper format to LilyPond\'s paper-alist. The PDQ paper size is 23,3 cm x 30,9 cm.</div></div>",31:"<div class=\"NDToolTip TInformation LLilyPond\"><div class=\"TTSummary\">The PDQ layout configures the base layout a little:</div></div>",32:"<div class=\"NDToolTip TInformation LLilyPond\"><div class=\"TTSummary\">The PDQ stylesheet heavily adjusts the default spacing variables. The main intention is to provide enough space for hand written annotations in scores. This is also the reason for PDQ\'s default paper format.</div></div>",33:"<div class=\"NDToolTip TInformation LLilyPond\"><div class=\"TTSummary\">The PDQ stylesheet uses non-standard fonts. For musical notation it uses the beethoven font and for sans-serif texts it uses the Yorkten font.</div></div>",34:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">The PDQ stylehseet defines its own page headers and footers. The individual elements are described next. For a more high-level overview of the possible configuration regarding headers see the The PDQ Layout.</div></div>",35:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">At the top of each page the instrument name and book title is displayed.</div></div>",36:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">The header displays page numbers on the left (or on the outer side for twoside layouts).</div></div>",37:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">The header displays page numbers on the left (or on the outer side for twoside layouts).</div></div>",38:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">The footer on the first page displays information about the publisher, version, copyright, and a tagline. If the repeatFooter option is #t the footer is printed on every page.</div></div>",39:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">On odd pages the footer is displayed on the right.</div></div>",40:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">On odd pages the footer is displayed on the left (if a twoside layout is used).</div></div>",41:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">Prints the header of a \\book. See The PDQ Layout for details.</div></div>",42:"<div class=\"NDToolTip TEvent LLilyPond\"><div class=\"TTSummary\">Prints the header of a \\score. See The PDQ Layout for details.</div></div>",43:"<div class=\"NDToolTip TSection LLilyPond\"><div class=\"TTSummary\">The PDQ stylesheet heavily customizes the default LilyPond layout. This documentation provides an overview over the modifications.</div></div>",44:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">The default PDQ layout tweaks multiple contexts to achieve its layout.</div></div>",45:"<div class=\"NDToolTip TContext LLilyPond\"><div class=\"TTSummary\">The main parts of PDQ\'s modifications apply to the Score context. Modifications include:</div></div>",46:"<div class=\"NDToolTip TContext LLilyPond\"><div class=\"TTSummary\">PDQ modifies the Staff context to use modern accidentals as well as matching fonts.</div></div>",47:"<div class=\"NDToolTip TContext LLilyPond\"><div class=\"TTSummary\">Stems spanning multiple systems are connected.</div></div>",48:"<div class=\"NDToolTip TContext LLilyPond\"><div class=\"TTSummary\">Adjusted fonts and better readability.</div></div>",49:"<div class=\"NDToolTip TContext LLilyPond\"><div class=\"TTSummary\">Adjusted spacing and fonts.</div></div>",50:"<div class=\"NDToolTip TContext LLilyPond\"><div class=\"TTSummary\">Does not display dynamic spanners.</div></div>",51:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">You can easily apply specific layout tweaks using one of the predefined layouts. The layouts are specialized for specifiec use cases but feel free to use any layout however you may see fit. Some of the layouts also provide an associated paper definition that applies some modifications to PDQ\'s default paper.</div></div>",52:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype52\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">partLayout = \\layout {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">The part layout can be used to set single voice parts (but not neccesarily for scores). It is not intended to be used for piano parts. This includes the skipBars option, removes instrument names etc.</div></div>",53:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype53\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">transposedPartLayout = \\layout {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">A variant of the \\partLayout that is optimized for transposed instruments (e.g.&nbsp; french horn or clarinet). The layout keeps a small indent in order to enable placement of a transposition info in the instrumentName of a staff.</div></div>",54:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype54\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">partPaper = \\paper {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">This paper is optimized for printing parts. It enables turn based page breaking and adds page numbers.</div></div>",55:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype55\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">exerptLayout = \\layout {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">This layout should be used for small exerpts of music. It should be combined with exerptPaper.</div></div>",56:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype56\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">exerptPaper = \\paper {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">This paper should be used for small exerpts of music. It enables a ragged bottom for the last page.</div></div>",57:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype57\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">\\scoreLayout = \\layout {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">The score layout can be used to set full scores with many voices and staffs.</div></div>",58:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype58\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">smallScoreLayout = \\layout {</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PModifierQualifier first last\">...</td></tr></table></td><td class=\"PAfterParameters\">}</td></tr></table></div><div class=\"TTSummary\">This layout (like the score layout) can be used to set small scores with multiple but not too many voices and staffs. It is up to you when a score is &quot;small&quot;.</div></div>"});