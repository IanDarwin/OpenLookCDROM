;GIL-3
(
(
	:type                   :base-window
	:name                   window1
	:owner                  nil
	:width                  400
	:height                 372
	:background-color       ""
	:foreground-color       ""
	:label                  "Base Window"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              ""
	:icon-label             ""
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  window1
	:help                   ""
	:x                      0
	:y                      0
	:width                  400
	:height                 211
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   fileMButton
	:owner                  controls1
	:help                   ""
	:x                      12
	:y                      22
	:width                  48
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "File"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   upMButton
	:owner                  controls1
	:help                   ""
	:x                      79
	:y                      22
	:width                  39
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Up"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   newMButton
	:owner                  controls1
	:help                   ""
	:x                      149
	:y                      22
	:width                  52
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "New"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   trashMButton
	:owner                  controls1
	:help                   ""
	:x                      221
	:y                      22
	:width                  69
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Delete"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   listMButton
	:owner                  controls1
	:help                   ""
	:x                      309
	:y                      22
	:width                  88
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Show List"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   downMButton
	:owner                  controls1
	:help                   ""
	:x                      69
	:y                      67
	:width                  61
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Down"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   phoneMButton
	:owner                  controls1
	:help                   ""
	:x                      153
	:y                      70
	:width                  52
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Dial"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   printMButton
	:owner                  controls1
	:help                   ""
	:x                      229
	:y                      70
	:width                  58
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Print"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   finMButton
	:owner                  controls1
	:help                   ""
	:x                      309
	:y                      70
	:width                  53
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Quit"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   findMButton
	:owner                  controls1
	:help                   ""
	:x                      310
	:y                      120
	:width                  51
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Find"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   findField
	:owner                  controls1
	:help                   ""
	:x                      8
	:y                      128
	:width                  294
	:height                 17
	:value-x                142
	:value-y                128
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Find Expression:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   slider1
	:owner                  controls1
	:help                   ""
	:x                      16
	:y                      176
	:width                  319
	:height                 24
	:value-x                133
	:value-y                176
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Card Number:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             t
	:show-value             t
	:min-value              0
	:max-value              100
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          0
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-pane
	:name                   textpane1
	:owner                  window1
	:help                   ""
	:x                      0
	:y                      220
	:width                  400
	:height                 152
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:read-only              nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
)
