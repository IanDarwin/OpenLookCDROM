;GIL-3
; @(#)psdraw.G	2.5 91/10/15 Copyright 1991 Sun Microsystems
(
(
	:type                   :base-window
	:name                   win
	:owner                  nil
	:width                  576
	:height                 412
	:background-color       ""
	:foreground-color       ""
	:label                  "PSdraw"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              ""
	:icon-label             ""
	:icon-mask-file         ""
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls
	:owner                  win
	:help                   ""
	:x                      0
	:y                      0
	:width                  576
	:height                 52
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   mode
	:owner                  controls
	:help                   ""
	:x                      169
	:y                      10
	:width                  26
	:height                 156
	:value-x                169
	:value-y                10
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  ""
	:label-type             :string
	:notify-handler         mode_proc
	:event-handler          nil
	:events                 ()
	:choices                ("point.pr" "line.pr" "rectangle.pr" "circle.pr" "ellipse.pr" "text.pr" )
	:choice-label-types     (:glyph :glyph :glyph :glyph :glyph :glyph )
	:choice-colors          ("" "" "" "" "" "" )
	:initial-selections     (nil nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (win mode)
		:when                   (Notify )
		:to                     (win mode)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (mode_proc)
		)
	)
)
(
	:type                   :button
	:name                   props_button
	:owner                  controls
	:help                   ""
	:x                      10
	:y                      14
	:width                  89
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Properties..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         props_proc
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (win props_button)
		:when                   (Notify )
		:to                     (win props_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (props_proc)
		)
	)
)
(
	:type                   :button
	:name                   clear_button
	:owner                  controls
	:help                   ""
	:x                      109
	:y                      14
	:width                  50
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Clear"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         clear_proc
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (win clear_button)
		:when                   (Notify )
		:to                     (win clear_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (clear_proc)
		)
	)
)
(
	:type                   :text-field
	:name                   text
	:owner                  controls
	:help                   ""
	:x                      335
	:y                      16
	:width                  232
	:height                 15
	:value-x                375
	:value-y                16
	:value-length           24
	:stored-length          1024
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Text:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :inactive
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group7
	:x                      10
	:y                      10
	:group-type             :row
	:members                (props_button clear_button mode text )
	:rows                   1
	:row-alignment          :horizontal-centers
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
	:type                   :canvas-pane
	:name                   canvas
	:owner                  win
	:help                   ""
	:x                      0
	:y                      52
	:width                  576
	:height                 360
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:draggable              nil
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:horizontal-scrollbar   nil
	:scrollable-width       576
	:vertical-scrollbar     nil
	:scrollable-height      360
	:repaint-proc           canvas_repaint
	:event-handler          canvas_events
	:events                 (:mouse :mouse-drag )
	:drawing-model          :postscript
	:user-data              ()
	:actions                (
		(
		:from                   (win canvas)
		:when                   (Repaint )
		:to                     (win canvas)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (canvas_repaint)
		)
		(
		:from                   (win canvas)
		:when                   (AnyEvent )
		:to                     (win canvas)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (canvas_events)
		)
	)
)
(
	:type                   :popup-window
	:name                   popup
	:owner                  win
	:width                  336
	:height                 347
	:background-color       ""
	:foreground-color       ""
	:label                  "PSdraw: Properties"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              nil
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   props_controls
	:owner                  popup
	:help                   ""
	:x                      0
	:y                      0
	:width                  336
	:height                 347
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   fill
	:owner                  props_controls
	:help                   ""
	:x                      64
	:y                      8
	:width                  23
	:height                 67
	:value-x                97
	:value-y                8
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Fill:"
	:label-type             :string
	:notify-handler         fill_proc
	:event-handler          nil
	:events                 ()
	:choices                ("" )
	:choice-label-types     (:string )
	:choice-colors          ("" )
	:initial-selections     (nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup fill)
		:when                   (Notify )
		:to                     (popup fill)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (fill_proc)
		)
	)
)
(
	:type                   :setting
	:name                   fill_color
	:owner                  props_controls
	:help                   ""
	:x                      26
	:y                      33
	:width                  23
	:height                 126
	:value-x                97
	:value-y                33
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Fill color:"
	:label-type             :string
	:notify-handler         fill_color_proc
	:event-handler          nil
	:events                 ()
	:choices                ("0" "0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9" "1" )
	:choice-label-types     (:string :string :string :string :string :string :string :string :string :string :string )
	:choice-colors          ("" "" "" "" "" "" "" "" "" "" "" )
	:choice-defaults        (nil nil nil nil nil t nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil t nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup fill_color)
		:when                   (Notify )
		:to                     (popup fill_color)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (fill_color_proc)
		)
	)
)
(
	:type                   :setting
	:name                   line_color
	:owner                  props_controls
	:help                   ""
	:x                      16
	:y                      58
	:width                  23
	:height                 136
	:value-x                97
	:value-y                58
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Line Color:"
	:label-type             :string
	:notify-handler         line_color_proc
	:event-handler          nil
	:events                 ()
	:choices                ("0" "0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9" "1" )
	:choice-label-types     (:string :string :string :string :string :string :string :string :string :string :string )
	:choice-colors          ("" "" "" "" "" "" "" "" "" "" "" )
	:choice-defaults        (nil nil nil nil nil nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup line_color)
		:when                   (Notify )
		:to                     (popup line_color)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (line_color_proc)
		)
	)
)
(
	:type                   :slider
	:name                   line_width
	:owner                  props_controls
	:help                   ""
	:x                      13
	:y                      83
	:width                  272
	:height                 20
	:value-x                97
	:value-y                83
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Line Width:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             t
	:show-value             t
	:min-value              0
	:max-value              50
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          5
	:initial-state          :active
	:notify-handler         width_proc
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (popup line_width)
		:when                   (Notify )
		:to                     (popup line_width)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (width_proc)
		)
	)
)
(
	:type                   :group
	:name                   group1
	:x                      13
	:y                      8
	:group-type             :column
	:members                (fill fill_color line_color line_width )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
	:horizontal-spacing     10
	:vertical-spacing       2
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
	:name                   line_style
	:owner                  props_controls
	:help                   ""
	:x                      18
	:y                      109
	:width                  23
	:height                 263
	:value-x                97
	:value-y                109
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Line Style:"
	:label-type             :string
	:notify-handler         style_proc
	:event-handler          nil
	:events                 ()
	:choices                ("Solid" "Outline" "Two Tone" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (t nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup line_style)
		:when                   (Notify )
		:to                     (popup line_style)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (style_proc)
		)
	)
)
(
	:type                   :setting
	:name                   line_dash
	:owner                  props_controls
	:help                   ""
	:x                      17
	:y                      136
	:width                  23
	:height                 303
	:value-x                97
	:value-y                136
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Line Dash:"
	:label-type             :string
	:notify-handler         dash_proc
	:event-handler          nil
	:events                 ()
	:choices                ("Solid" "Dots" "Dashes" "Dot-Dash" )
	:choice-label-types     (:string :string :string :string )
	:choice-colors          ("" "" "" "" )
	:initial-selections     (nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup line_dash)
		:when                   (Notify )
		:to                     (popup line_dash)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dash_proc)
		)
	)
)
(
	:type                   :setting
	:name                   line_cap
	:owner                  props_controls
	:help                   ""
	:x                      26
	:y                      163
	:width                  23
	:height                 244
	:value-x                97
	:value-y                163
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Line Cap:"
	:label-type             :string
	:notify-handler         line_cap_proc
	:event-handler          nil
	:events                 ()
	:choices                ("Butt" "Round" "Projecting" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup line_cap)
		:when                   (Notify )
		:to                     (popup line_cap)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (line_cap_proc)
		)
	)
)
(
	:type                   :setting
	:name                   line_join
	:owner                  props_controls
	:help                   ""
	:x                      24
	:y                      190
	:width                  23
	:height                 224
	:value-x                97
	:value-y                190
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Line Join:"
	:label-type             :string
	:notify-handler         line_join_proc
	:event-handler          nil
	:events                 ()
	:choices                ("Miter" "Round" "Bevel" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup line_join)
		:when                   (Notify )
		:to                     (popup line_join)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (line_join_proc)
		)
	)
)
(
	:type                   :group
	:name                   group2
	:x                      17
	:y                      109
	:group-type             :column
	:members                (line_style line_dash line_cap line_join )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
	:horizontal-spacing     10
	:vertical-spacing       4
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   miter_limit
	:owner                  props_controls
	:help                   ""
	:x                      10
	:y                      219
	:width                  123
	:height                 15
	:value-x                97
	:value-y                219
	:value-length           4
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Miter Limit:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "10"
	:initial-state          :active
	:read-only              nil
	:notify-handler         miter_proc
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (popup miter_limit)
		:when                   (Notify )
		:to                     (popup miter_limit)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (miter_proc)
		)
	)
)
(
	:type                   :group
	:name                   group3
	:x                      10
	:y                      8
	:group-type             :column
	:members                (group1 group2 miter_limit )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
	:horizontal-spacing     10
	:vertical-spacing       6
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
	:name                   font
	:owner                  props_controls
	:help                   ""
	:x                      57
	:y                      250
	:width                  23
	:height                 214
	:value-x                97
	:value-y                250
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Font:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Times-Roman" "Times-Bold" "Times-Italic" "Times-BoldItalic" "Helvetica" "Helvetica-Bold" "Helvetica-Oblique" "Helvetica-BoldOblique" "Courier" "Courier-Bold" "Courier-Oblique" "Courier-BoldOblique" )
	:choice-label-types     (:string :string :string :string :string :string :string :string :string :string :string :string )
	:choice-colors          ("" "" "" "" "" "" "" "" "" "" "" "" )
	:choice-defaults        (nil nil nil nil nil nil nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil nil nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   font_size
	:owner                  props_controls
	:help                   ""
	:x                      25
	:y                      277
	:width                  143
	:height                 15
	:value-x                97
	:value-y                277
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Font Size:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              256
	:min-value              0
	:initial-value          36
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group5
	:x                      25
	:y                      250
	:group-type             :column
	:members                (font font_size )
	:rows                   0
	:row-alignment          :bottom-edges
	:columns                1
	:column-alignment       :labels
	:horizontal-spacing     10
	:vertical-spacing       4
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
	:name                   hold_mode
	:owner                  props_controls
	:help                   ""
	:x                      50
	:y                      308
	:width                  26
	:height                 99
	:value-x                97
	:value-y                308
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Mode:"
	:label-type             :string
	:notify-handler         special_proc
	:event-handler          nil
	:events                 ()
	:choices                ("hold.pr" "join.pr" )
	:choice-label-types     (:glyph :glyph )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popup hold_mode)
		:when                   (Notify )
		:to                     (popup hold_mode)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (special_proc)
		)
	)
)
(
	:type                   :group
	:name                   group4
	:x                      10
	:y                      8
	:group-type             :column
	:members                (group3 group5 hold_mode )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
	:horizontal-spacing     10
	:vertical-spacing       16
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
)
