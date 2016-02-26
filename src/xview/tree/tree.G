;GIL-3
; @(#)tree.G	2.3 91/10/15 Copyright 1991 Sun Microsystems
(
(
	:type                   :base-window
	:name                   window
	:owner                  nil
	:width                  465
	:height                 449
	:background-color       ""
	:foreground-color       ""
	:label                  "Fractal Tree"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              ""
	:icon-mask-file         ""
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls
	:owner                  window
	:help                   ""
	:x                      0
	:y                      0
	:width                  465
	:height                 41
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
	:type                   :button
	:name                   draw
	:owner                  controls
	:help                   ""
	:x                      10
	:y                      10
	:width                  50
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Draw"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         draw
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (window draw)
		:when                   (Notify )
		:to                     (window draw)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (draw)
		)
	)
)
(
	:type                   :button
	:name                   properties
	:owner                  controls
	:help                   ""
	:x                      70
	:y                      10
	:width                  89
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Properties..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (window properties)
		:when                   (Notify )
		:to                     (props)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :group
	:name                   group11
	:x                      10
	:y                      10
	:group-type             :row
	:members                (draw properties )
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
	:owner                  window
	:help                   ""
	:x                      0
	:y                      41
	:width                  465
	:height                 408
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:draggable              nil
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:horizontal-scrollbar   nil
	:scrollable-width       465
	:vertical-scrollbar     nil
	:scrollable-height      408
	:repaint-proc           nil
	:event-handler          nil
	:events                 ()
	:drawing-model          :xview
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   props
	:owner                  window
	:width                  344
	:height                 310
	:background-color       ""
	:foreground-color       ""
	:label                  "Fractal Tree: Properties"
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
	:name                   controls
	:owner                  props
	:help                   ""
	:x                      0
	:y                      0
	:width                  344
	:height                 310
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
	:type                   :slider
	:name                   dir
	:owner                  controls
	:help                   ""
	:x                      14
	:y                      8
	:width                  255
	:height                 20
	:value-x                111
	:value-y                8
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Direction Up:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             t
	:min-value              10
	:max-value              170
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          90
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   dir_up_msg
	:owner                  controls
	:help                   ""
	:x                      279
	:y                      8
	:width                  47
	:height                 13
	:foreground-color       ""
	:label                  "degrees"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group
	:x                      14
	:y                      8
	:group-type             :row
	:members                (dir dir_up_msg )
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
	:type                   :slider
	:name                   dir_left
	:owner                  controls
	:help                   ""
	:x                      73
	:y                      34
	:width                  196
	:height                 20
	:value-x                111
	:value-y                34
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Left:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             t
	:min-value              5
	:max-value              50
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          15
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   dir_left_msg
	:owner                  controls
	:help                   ""
	:x                      279
	:y                      34
	:width                  47
	:height                 13
	:foreground-color       ""
	:label                  "degrees"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group1
	:x                      73
	:y                      34
	:group-type             :row
	:members                (dir_left dir_left_msg )
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
	:type                   :slider
	:name                   dir_right
	:owner                  controls
	:help                   ""
	:x                      64
	:y                      60
	:width                  205
	:height                 20
	:value-x                111
	:value-y                60
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Right:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             t
	:min-value              5
	:max-value              50
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          15
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   dir_right_msg
	:owner                  controls
	:help                   ""
	:x                      279
	:y                      60
	:width                  47
	:height                 13
	:foreground-color       ""
	:label                  "degrees"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group3
	:x                      64
	:y                      60
	:group-type             :row
	:members                (dir_right dir_right_msg )
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
	:name                   group4
	:x                      14
	:y                      8
	:group-type             :column
	:members                (group group1 group3 )
	:rows                   0
	:row-alignment          :bottom-edges
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
	:type                   :slider
	:name                   growth_left
	:owner                  controls
	:help                   ""
	:x                      21
	:y                      94
	:width                  248
	:height                 20
	:value-x                111
	:value-y                94
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Growth Left:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             t
	:min-value              50
	:max-value              85
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          70
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   grow_left_msg
	:owner                  controls
	:help                   ""
	:x                      279
	:y                      94
	:width                  46
	:height                 13
	:foreground-color       ""
	:label                  "percent"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group5
	:x                      21
	:y                      94
	:group-type             :row
	:members                (growth_left grow_left_msg )
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
	:type                   :slider
	:name                   growth_right
	:owner                  controls
	:help                   ""
	:x                      64
	:y                      120
	:width                  205
	:height                 20
	:value-x                111
	:value-y                120
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Right:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             t
	:min-value              50
	:max-value              85
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          70
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   grow_right_msg
	:owner                  controls
	:help                   ""
	:x                      279
	:y                      120
	:width                  46
	:height                 13
	:foreground-color       ""
	:label                  "percent"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group6
	:x                      64
	:y                      120
	:group-type             :row
	:members                (growth_right grow_right_msg )
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
	:name                   group7
	:x                      21
	:y                      94
	:group-type             :column
	:members                (group5 group6 )
	:rows                   0
	:row-alignment          :bottom-edges
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
	:type                   :text-field
	:name                   bark_color
	:owner                  controls
	:help                   ""
	:x                      29
	:y                      154
	:width                  282
	:height                 15
	:value-x                111
	:value-y                154
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Bark Color:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "Brown"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   leaf_color
	:owner                  controls
	:help                   ""
	:x                      30
	:y                      179
	:width                  281
	:height                 15
	:value-x                111
	:value-y                179
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Leaf Color:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "Forest Green"
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
	:name                   group8
	:x                      29
	:y                      154
	:group-type             :column
	:members                (bark_color leaf_color )
	:rows                   0
	:row-alignment          :horizontal-centers
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
	:type                   :text-field
	:name                   leaf_size
	:owner                  controls
	:help                   ""
	:x                      1
	:y                      208
	:width                  181
	:height                 15
	:value-x                111
	:value-y                208
	:value-length           4
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Length of Leaf:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              25
	:min-value              2
	:initial-value          5
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   clear
	:owner                  controls
	:help                   ""
	:x                      3
	:y                      237
	:width                  142
	:height                 23
	:value-x                111
	:value-y                237
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Clear Drawing:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("" )
	:choice-label-types     (:string )
	:choice-colors          ("" )
	:initial-selections     (t )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group9
	:x                      1
	:y                      8
	:group-type             :column
	:members                (group4 group7 group8 leaf_size clear )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
	:horizontal-spacing     10
	:vertical-spacing       14
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   apply
	:owner                  controls
	:help                   ""
	:x                      97
	:y                      280
	:width                  53
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         apply_properties
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (props apply)
		:when                   (Notify )
		:to                     (props apply)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (apply_properties)
		)
	)
)
(
	:type                   :button
	:name                   defaults
	:owner                  controls
	:help                   ""
	:x                      160
	:y                      280
	:width                  69
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Defaults"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         factory_defaults
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (props defaults)
		:when                   (Notify )
		:to                     (props defaults)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (factory_defaults)
		)
	)
)
(
	:type                   :group
	:name                   group2
	:x                      97
	:y                      280
	:group-type             :row
	:members                (apply defaults )
	:rows                   1
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :vertical-centers
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
	:name                   group10
	:x                      1
	:y                      8
	:group-type             :column
	:members                (group9 group2 )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
	:horizontal-spacing     10
	:vertical-spacing       20
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
)
