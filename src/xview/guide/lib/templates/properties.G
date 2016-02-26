;GIL-3
; @(#)properties.G	2.4 91/10/15 Copyright 1990 Sun Microsystems
(
(
	:type                   :popup-window
	:name                   properties
	:owner                  nil
	:width                  400
	:height                 250
	:background-color       ""
	:foreground-color       ""
	:label                  "Sample: Properties"
	:label-type             :string
	:mapped                 nil
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  properties
	:help                   ""
	:x                      0
	:y                      0
	:width                  400
	:height                 250
	:background-color       ""
	:foreground-color       ""
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   apply
	:owner                  controls1
	:help                   ""
	:x                      142
	:y                      216
	:constant-width         nil
	:button-type            :normal
	:width                  53
	:height                 19
	:foreground-color       ""
	:label                  "Apply"
	:label-type             :string
	:menu                   nil
	:notify-handler         apply
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   reset
	:owner                  controls1
	:help                   ""
	:x                      207
	:y                      216
	:constant-width         nil
	:button-type            :normal
	:width                  51
	:height                 19
	:foreground-color       ""
	:label                  "Reset"
	:label-type             :string
	:menu                   nil
	:notify-handler         reset
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
)
