;GIL-1
;	@(#)gcc.G	1.15 90/04/19 Copyright 1989 Sun Microsystems
(
(
	:type                   :popup-window
	:name                   win
	:owner                  nil
	:width                  287
	:height                 344
	:background-color       ""
	:foreground-color       ""
	:label                  ""
	:label-type             :string
	:mapped                 nil
	:show-footer            t
	:resizable              nil
	:pinned                 t
	:done-handler           nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :control-area
	:name                   controls
	:owner                  win
	:help                   ""
	:x                      0
	:y                      0
	:width                  287
	:height                 344
	:background-color       ""
	:foreground-color       ""
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :scrolling-list
	:name                   color_list
	:owner                  controls
	:help                   "Colors scrolling list:

The colors scrolling list shows all the
colors available to you in XView.  Click
SELECT on the name of the color you want.
The Color text field below changes to show the
color you chose."
	:x                      24
	:y                      8
	:width                  200
	:height                 236
	:foreground-color       ""
	:label                  ""
	:label-type             :string
	:rows                   12
	:read-only              t
	:multiple-selections    nil
	:selection-required     t
	:menu                   nil
	:notify-handler         gcc_list_proc
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :message
	:name                   color_blot
	:owner                  controls
	:help                   ""
	:x                      236
	:y                      249
	:width                  32
	:height                 32
	:foreground-color       ""
	:label                  "black.pr"
	:label-type             :glyph
	:label-bold             t
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :text-field
	:name                   color_name
	:owner                  controls
	:help                   "Color text field:

The Color text field shows the selected color
in the scrolling list above.  To enter a
color name without choosing it in the
scrolling list, click SELECT on the text
field and enter a proper XView color name in
the text field."
	:x                      12
	:y                      265
	:width                  208
	:height                 15
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Color:"
	:label-type             :string
	:value-x                59
	:value-y                265
	:layout-type            :horizontal
	:value-length           20
	:stored-length          80
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   apply
	:owner                  controls
	:help                   "The Apply button:

The Apply button applies any color changes
you've made when you click SELECT on it."
	:x                      88
	:y                      305
	:constant-width         nil
	:width                  53
	:height                 20
	:foreground-color       ""
	:label                  "Apply"
	:label-type             :string
	:menu                   nil
	:notify-handler         gcc_apply
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   reset
	:owner                  controls
	:help                   "The Reset button:

The Reset button returns any changed color to
its original state as it was when you last
used the Apply button or when you selected a
new color in the colors scrolling list (which
has the same effect as choosing the Apply
button.)  Click SELECT on the Reset button to
reset a color."
	:x                      152
	:y                      305
	:constant-width         nil
	:width                  51
	:height                 20
	:foreground-color       ""
	:label                  "Reset"
	:label-type             :string
	:menu                   nil
	:notify-handler         gcc_reset
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
)
