;GIL-3
(
(
	:type                   :popup-window
	:name                   mainPopup1
	:owner                  window1
	:width                  408
	:height                 234
	:background-color       ""
	:foreground-color       ""
	:label                  "SVR4 Facility Manager"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:pinned                 t
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls2
	:owner                  mainPopup1
	:help                   ""
	:x                      0
	:y                      0
	:width                  408
	:height                 234
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
	:type                   :scrolling-list
	:name                   list1
	:owner                  controls2
	:help                   ""
	:x                      16
	:y                      8
	:width                  200
	:height                 74
	:value-x                74
	:value-y                8
	:rows                   3
	:foreground-color       ""
	:label                  "Facility:"
	:title                  ""
	:label-type             :string
	:layout-type            :horizontal
	:read-only              nil
	:multiple-selections    nil
	:selection-required     nil
	:initial-state          :active
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   bootMessage
	:owner                  controls2
	:help                   ""
	:x                      85
	:y                      104
	:width                  69
	:height                 15
	:foreground-color       ""
	:label                  "Boot Time:"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   enableButton
	:owner                  controls2
	:help                   ""
	:x                      173
	:y                      104
	:width                  59
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Enable"
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
	:name                   disableButton
	:owner                  controls2
	:help                   ""
	:x                      261
	:y                      104
	:width                  63
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Disable"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   nowMessage
	:owner                  controls2
	:help                   ""
	:x                      85
	:y                      152
	:width                  70
	:height                 15
	:foreground-color       ""
	:label                  "Immediate:"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   startButton
	:owner                  controls2
	:help                   ""
	:x                      181
	:y                      152
	:width                  47
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Start"
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
	:name                   stopButton
	:owner                  controls2
	:help                   ""
	:x                      261
	:y                      152
	:width                  45
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Stop"
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
	:name                   quitButton
	:owner                  controls2
	:help                   ""
	:x                      176
	:y                      200
	:width                  45
	:height                 19
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
	:actions                (
		(
		:from                   (mainPopup1 quitButton)
		:when                   (Notify )
		:to                     (mainPopup1 quitButton)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("exit(0);	/* requested by Quit button */")
		)
	)
)
(
	:type                   :group
	:name                   group1
	:x                      85
	:y                      104
	:group-type             nil
	:members                (bootMessage enableButton disableButton nowMessage startButton stopButton quitButton )
	:rows                   1
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :left-edges
	:horizontal-spacing     10
	:vertical-spacing       10
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
)
