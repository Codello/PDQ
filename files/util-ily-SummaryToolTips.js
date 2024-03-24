NDSummary.OnToolTipsLoaded("File:util.ily",{59:"<div class=\"NDToolTip TFile LLilyPond\"><div class=\"TTSummary\">This file contains utility functions in Scheme that can be used to simplify some advanced tasks in LilyPond.</div></div>",60:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">LilyPond functions can be used directly within your LilyPond source code.</div></div>",61:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype61\" class=\"NDPrototype NoParameterForm\">\\escalate-warnings</div><div class=\"TTSummary\">Makes LilyPond treat all (user-issued) warnings as errors.</div></div>",62:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">Scheme functions must be called in a Scheme environment (and aren\'t really all that useful elsewhere).</div></div>",63:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype63\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">(</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PType first\">define&nbsp;</td><td class=\"PNamePrefix\">(</td><td class=\"PName last\">defined? symbol) ...</td></tr></table></td><td class=\"PAfterParameters\">)</td></tr></table></div><div class=\"TTSummary\">The defined? predicate tells you wether a specific symbol was previously defined. This may not work for standard functions but it works for custom definitions.</div></div>",64:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype64\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">(</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PType first\">define&nbsp;</td><td class=\"PNamePrefix\">(</td><td class=\"PName last\">get-option symbol default) ...</td></tr></table></td><td class=\"PAfterParameters\">)</td></tr></table></div><div class=\"TTSummary\">The test-option predicate tests wether the specified symbol was defined and set to a value that evaluates to #t (a value other that #f). This can be used to test whether package options have been specified. If a (package) option is set to a non-boolean value this predicate will return that value.</div></div>",65:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype65\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">(</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PType first\">define&nbsp;</td><td class=\"PNamePrefix\">(</td><td class=\"PName last\">any? object) ...</td></tr></table></td><td class=\"PAfterParameters\">)</td></tr></table></div><div class=\"TTSummary\">A type predicate that is always true.</div></div>",66:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype66\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">(</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PType first\">define&nbsp;</td><td class=\"PNamePrefix\">(</td><td class=\"PName last\">andmap f xs) ...</td></tr></table></td><td class=\"PAfterParameters\">)</td></tr></table></div><div class=\"TTSummary\">A predicate that returns true iff f returns true for all elements in xs. If xs is empty this predicates evaluates to #t.</div></div>",67:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype67\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">(</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PType first\">define&nbsp;</td><td class=\"PNamePrefix\">(</td><td class=\"PName last\">ormap f xs) ...</td></tr></table></td><td class=\"PAfterParameters\">)</td></tr></table></div><div class=\"TTSummary\">A predicate that returns true iff f returns true for any of the elements in xs. If xs is empty #f is returned.</div></div>",68:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype68\" class=\"NDPrototype WideForm CStyle\"><table><tr><td class=\"PBeforeParameters\">(</td><td class=\"PParametersParentCell\"><table class=\"PParameters\"><tr><td class=\"PType first\">define&nbsp;</td><td class=\"PNamePrefix\">((</td><td class=\"PName last\">custom-script-tweaks ls) ...) ...</td></tr></table></td><td class=\"PAfterParameters\">)</td></tr></table></div><div class=\"TTSummary\">Enables custom tweaks for single grobs.</div></div>",69:"<div class=\"NDToolTip TGroup LLilyPond\"><div class=\"TTSummary\">Markup commands can be used inside of a \\markup { ... } block. They provide additional features such as styling options.</div></div>",70:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype70\" class=\"NDPrototype NoParameterForm\">\\warn &lt;text&gt;</div><div class=\"TTSummary\">A markup command that can be used to emit a warning. This may for example be useful if you want to mark a function as deprecated and emit a warning.</div></div>",71:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype71\" class=\"NDPrototype NoParameterForm\">\\<span class=\"SHKeyword\">if</span>-<span class=\"SHKeyword\">true</span> &lt;predicate&gt; &lt;markp&gt;</div><div class=\"TTSummary\">This markup command conditionally outputs markup based on a boolean value.</div></div>",72:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype72\" class=\"NDPrototype NoParameterForm\">\\<span class=\"SHKeyword\">if</span>-<span class=\"SHKeyword\">false</span> &lt;predicate&gt; &lt;markp&gt;</div><div class=\"TTSummary\">This markup command conditionally outputs markup based on a boolean value.</div></div>",73:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype73\" class=\"NDPrototype NoParameterForm\">\\<span class=\"SHKeyword\">if</span>-<span class=\"SHKeyword\">else</span> &lt;predicate&gt; &lt;tmarkp&gt; &lt;fmarkp&gt;</div><div class=\"TTSummary\">Conditionally outputs one of two markups depending on a boolean value.</div></div>",74:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype74\" class=\"NDPrototype NoParameterForm\">\\when-property &lt;symbol&gt; &lt;markp&gt;</div><div class=\"TTSummary\">The \\when-property markup command allows you to conditionally output markup.&nbsp; If symbol exists markp is returned. Otherwise an empty markup block is returned.</div></div>",75:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype75\" class=\"NDPrototype NoParameterForm\">\\when-not-property &lt;symbol&gt; &lt;markp&gt;</div><div class=\"TTSummary\">Behaves like \\when-property but returns the markup block if symbol does not exist. If the symbol exists an empty markup block is returned.</div></div>",76:"<div class=\"NDToolTip TFunction LLilyPond\"><div id=\"NDPrototype76\" class=\"NDPrototype NoParameterForm\">\\when-some-properties &lt;symbols&gt; &lt;markp&gt;</div><div class=\"TTSummary\">Behaves like \\when-property but accepts a list of properties instead of a single property.</div></div>"});