;GIL-3
(
(
	:type                   :base-window
	:name                   baseWindow
	:owner                  nil
	:width                  520
	:height                 300
	:background-color       ""
	:foreground-color       ""
	:label                  "Yet Another Mail Tool"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              ""
	:icon-label             "yamailtool"
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  baseWindow
	:help                   ""
	:x                      0
	:y                      0
	:width                  520
	:height                 300
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
	:name                   fileButton
	:owner                  controls1
	:help                   ""
	:x                      8
	:y                      8
	:width                  40
	:height                 19
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
	:name                   viewButton
	:owner                  controls1
	:help                   ""
	:x                      80
	:y                      8
	:width                  48
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "View"
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
	:name                   editButton
	:owner                  controls1
	:help                   ""
	:x                      152
	:y                      8
	:width                  42
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Edit"
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
	:name                   composeButton
	:owner                  controls1
	:help                   ""
	:x                      216
	:y                      8
	:width                  74
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Compose"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   setting1
	:owner                  controls1
	:help                   ""
	:x                      304
	:y                      8
	:width                  133
	:height                 23
	:value-x                360
	:value-y                8
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Setting:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Choice" "Choice" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:choice-defaults        (nil nil )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   prevButton
	:owner                  controls1
	:help                   ""
	:x                      8
	:y                      32
	:width                  70
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Previous"
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
	:name                   nextButton
	:owner                  controls1
	:help                   ""
	:x                      80
	:y                      32
	:width                  46
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Next"
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
	:name                   deleteButton
	:owner                  controls1
	:help                   ""
	:x                      152
	:y                      32
	:width                  57
	:height                 19
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
	:name                   replyButton
	:owner                  controls1
	:help                   ""
	:x                      232
	:y                      32
	:width                  52
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Reply"
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
	:name                   button9
	:owner                  controls1
	:help                   ""
	:x                      304
	:y                      32
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button10
	:owner                  controls1
	:help                   ""
	:x                      368
	:y                      32
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button11
	:owner                  controls1
	:help                   ""
	:x                      432
	:y                      32
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :scrolling-list
	:name                   list1
	:owner                  controls1
	:help                   ""
	:x                      8
	:y                      56
	:width                  500
	:height                 218
	:value-x                8
	:value-y                56
	:rows                   11
	:foreground-color       ""
	:label                  ""
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
	:type                   :base-window
	:name                   compWindow
	:owner                  nil
	:width                  532
	:height                 477
	:background-color       ""
	:foreground-color       ""
	:label                  "Yet Another Mail Tool - Compose/Reply"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              ""
	:icon-label             "yamt:Compose"
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls2
	:owner                  compWindow
	:help                   ""
	:x                      0
	:y                      0
	:width                  532
	:height                 477
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
	:name                   button12
	:owner                  controls2
	:help                   ""
	:x                      8
	:y                      8
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button13
	:owner                  controls2
	:help                   ""
	:x                      80
	:y                      8
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button14
	:owner                  controls2
	:help                   ""
	:x                      152
	:y                      8
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button15
	:owner                  controls2
	:help                   ""
	:x                      224
	:y                      8
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button16
	:owner                  controls2
	:help                   ""
	:x                      296
	:y                      8
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   toTextfield
	:owner                  controls2
	:help                   ""
	:x                      16
	:y                      32
	:width                  93
	:height                 15
	:value-x                45
	:value-y                32
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "To:"
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
	:type                   :text-field
	:name                   subjectTextfield
	:owner                  controls2
	:help                   ""
	:x                      16
	:y                      56
	:width                  122
	:height                 15
	:value-x                74
	:value-y                56
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Subject:"
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
	:type                   :text-field
	:name                   ccTextfield
	:owner                  controls2
	:help                   ""
	:x                      16
	:y                      80
	:width                  92
	:height                 15
	:value-x                44
	:value-y                80
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Cc:"
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
	:type                   :group
	:name                   group1
	:x                      16
	:y                      32
	:group-type             nil
	:members                (toTextfield subjectTextfield ccTextfield )
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
(
	:type                   :message
	:name                   message1
	:owner                  controls2
	:help                   ""
	:x                      24
	:y                      104
	:width                  54
	:height                 15
	:foreground-color       ""
	:label                  "Message"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   button17
	:owner                  controls2
	:help                   ""
	:x                      112
	:y                      392
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
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
	:name                   button18
	:owner                  controls2
	:help                   ""
	:x                      192
	:y                      392
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Button"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   sendPopup
	:owner                  compWindow
	:width                  590
	:height                 144
	:background-color       ""
	:foreground-color       ""
	:label                  "Send Options"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            nil
	:resizable              nil
	:pinned                 t
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls3
	:owner                  sendPopup
	:help                   ""
	:x                      0
	:y                      0
	:width                  590
	:height                 144
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
	:type                   :setting
	:name                   stdSetting
	:owner                  controls3
	:help                   ""
	:x                      16
	:y                      16
	:width                  475
	:height                 23
	:value-x                103
	:value-y                16
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Mail Format:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("RFC-822 (UNIX)" "Sun Mailtool (OWV3)" "MIME (New format)" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   whichKeySetting
	:owner                  controls3
	:help                   ""
	:x                      260
	:y                      60
	:width                  230
	:height                 23
	:value-x                335
	:value-y                60
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Using key:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("My Public Key" "My Private Key" "Sender's Public Key" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:choice-defaults        (nil nil nil )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   encSetting
	:owner                  controls3
	:help                   ""
	:x                      16
	:y                      64
	:width                  198
	:height                 23
	:value-x                101
	:value-y                64
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Encryption?"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Cleartext" "PKC" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
)
