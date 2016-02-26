;GIL-3
(
(
	:type                   :popup-window
	:name                   popup
	:owner                  nil
	:width                  427
	:height                 288
	:background-color       ""
	:foreground-color       ""
	:label                  "Desktop Properties"
	:label-type             :string
	:initial-state          :open
	:show-footer            nil
	:resizable              nil
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   category_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      0
	:width                  427
	:height                 40
	:background-color       ""
	:foreground-color       ""
	:initial-state          :active
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   category
	:owner                  category_controls
	:help                   ""
	:x                      24
	:y                      8
	:width                  199
	:height                 23
	:value-x                95
	:value-y                8
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Category:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Icons" "Menus" "Miscellaneous" "Mouse Settings" )
	:choice-label-types     (:string :string :string :string )
	:choice-colors          ("" "" "" "" )
	:choice-defaults        (nil nil nil nil )
	:initial-selections     (t nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup category "Icons")
		:when                   (Notify )
		:to                     (popup icon_controls)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
		(
		:from                   (popup category "Miscellaneous")
		:when                   (Notify )
		:to                     (popup misc_controls)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
		(
		:from                   (popup category "Menus")
		:when                   (Notify )
		:to                     (popup menu_controls)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
		(
		:from                   (popup category "Mouse Settings")
		:when                   (Notify )
		:to                     (popup mouse_controls)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :control-area
	:name                   mouse_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      39
	:width                  427
	:height                 193
	:background-color       ""
	:foreground-color       ""
	:initial-state          :invisible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   scrollbar_jump
	:owner                  mouse_controls
	:help                   ""
	:x                      24
	:y                      40
	:width                  223
	:height                 23
	:value-x                213
	:value-y                40
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Scrollbar Pointer Jumping:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("" )
	:choice-label-types     (:string )
	:choice-colors          ("" )
	:initial-selections     (nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   popup_jump
	:owner                  mouse_controls
	:help                   ""
	:x                      35
	:y                      73
	:width                  212
	:height                 23
	:value-x                213
	:value-y                73
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Pop-up Pointer Jumping:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("" )
	:choice-label-types     (:string )
	:choice-colors          ("" )
	:initial-selections     (nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   multi_click
	:owner                  mouse_controls
	:help                   ""
	:x                      65
	:y                      106
	:width                  286
	:height                 20
	:value-x                208
	:value-y                106
	:slider-width           50
	:ticks                  0
	:foreground-color       ""
	:label                  "Multi-click Timeout:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             t
	:show-value             t
	:min-value              2
	:max-value              10
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
	:type                   :message
	:name                   seconds
	:owner                  mouse_controls
	:help                   ""
	:x                      361
	:y                      106
	:width                  46
	:height                 13
	:foreground-color       ""
	:label                  "sec/10"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group1
	:x                      60
	:y                      106
	:group-type             :row
	:members                (multi_click seconds )
	:rows                   1
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :labels
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
	:type                   :group
	:name                   group2
	:x                      24
	:y                      40
	:group-type             :column
	:members                (scrollbar_jump popup_jump group1 )
	:rows                   0
	:row-alignment          :top-edges
	:columns                1
	:column-alignment       :labels
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
	:type                   :control-area
	:name                   icon_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      39
	:width                  427
	:height                 193
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
	:name                   icon_location
	:owner                  icon_controls
	:help                   ""
	:x                      80
	:y                      80
	:width                  254
	:height                 23
	:value-x                149
	:value-y                80
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Location:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Top" "Bottom" "Left" "Right" )
	:choice-label-types     (:string :string :string :string )
	:choice-colors          ("" "" "" "" )
	:initial-selections     (nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   misc_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      39
	:width                  427
	:height                 193
	:background-color       ""
	:foreground-color       ""
	:initial-state          :invisible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   beep
	:owner                  misc_controls
	:help                   ""
	:x                      127
	:y                      40
	:width                  248
	:height                 23
	:value-x                172
	:value-y                40
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Beep:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Always" "Notices Only" "Never" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   set_input
	:owner                  misc_controls
	:help                   ""
	:x                      64
	:y                      73
	:width                  298
	:height                 23
	:value-x                172
	:value-y                73
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Set Input Area:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Click SELECT" "Move Pointer" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   scrollbar
	:owner                  misc_controls
	:help                   ""
	:x                      24
	:y                      106
	:width                  235
	:height                 23
	:value-x                172
	:value-y                106
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Scrollbar Placement:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Left" "Right" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group
	:x                      24
	:y                      40
	:group-type             :column
	:members                (beep set_input scrollbar )
	:rows                   0
	:row-alignment          :top-edges
	:columns                1
	:column-alignment       :labels
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
	:type                   :control-area
	:name                   menu_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      39
	:width                  427
	:height                 193
	:background-color       ""
	:foreground-color       ""
	:initial-state          :invisible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   drag_right
	:owner                  menu_controls
	:help                   ""
	:x                      40
	:y                      48
	:width                  213
	:height                 15
	:value-x                189
	:value-y                48
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Drag-Right distance:"
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
	:type                   :message
	:name                   pixels
	:owner                  menu_controls
	:help                   ""
	:x                      263
	:y                      48
	:width                  42
	:height                 13
	:foreground-color       ""
	:label                  "pixels"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group3
	:x                      40
	:y                      48
	:group-type             :row
	:members                (drag_right pixels )
	:rows                   1
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :labels
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
	:type                   :setting
	:name                   select
	:owner                  menu_controls
	:help                   ""
	:x                      40
	:y                      73
	:width                  361
	:height                 23
	:value-x                189
	:value-y                73
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "SELECT Mouse Press"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Selects Default" "Displays Menu" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group4
	:x                      40
	:y                      48
	:group-type             :column
	:members                (group3 select )
	:rows                   0
	:row-alignment          :top-edges
	:columns                1
	:column-alignment       :labels
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
	:type                   :control-area
	:name                   apply_reset_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      232
	:width                  427
	:height                 56
	:background-color       ""
	:foreground-color       ""
	:initial-state          :active
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   apply
	:owner                  apply_reset_controls
	:help                   ""
	:x                      156
	:y                      19
	:width                  53
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply"
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
	:name                   reset
	:owner                  apply_reset_controls
	:help                   ""
	:x                      219
	:y                      19
	:width                  51
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Reset"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group5
	:x                      156
	:y                      19
	:group-type             :row
	:members                (apply reset )
	:rows                   1
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :labels
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
	:type                   :stack
	:name                   stack1
	:owner                  popup
	:members                (icon_controls menu_controls misc_controls mouse_controls )
	:user-data              ()
)
)
