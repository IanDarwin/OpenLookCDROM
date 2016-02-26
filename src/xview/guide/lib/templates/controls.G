;GIL-3
; @(#)controls.G	2.6 91/10/15 Copyright 1990 Sun Microsystems
(
(
	:type                   :menu
	:name                   abbrev_menu_button_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("One" "Two" "Three" )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :inactive :active )
	:menu-item-defaults     (nil nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (nil nil nil )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   menu_button_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Before" "After" "Top" "Bottom" )
	:menu-item-label-types  (:string :string :string :string )
	:menu-item-states       (:active :active :active :active )
	:menu-item-defaults     (nil nil nil nil )
	:initial-selections     (nil nil nil nil )
	:menu-item-handlers     (nil nil nil nil )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   pinnable_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Load..." "Save" "Save As..." )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :active :active )
	:menu-item-defaults     (nil nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (nil nil nil )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   command_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Cut" "Copy" "Paste" "Delete" "Undo" )
	:menu-item-label-types  (:string :string :string :string :string )
	:menu-item-states       (:active :active :active :inactive :active )
	:menu-item-defaults     (nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil )
	:menu-item-handlers     (nil nil nil nil nil )
	:menu-item-menus        (nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   exclusive_menu
	:help                   ""
	:columns                1
	:menu-type              :exclusive
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Small" "Medium" "Large" )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :active :active )
	:menu-item-defaults     (nil nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (nil nil nil )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   nonexclusive_menu
	:help                   ""
	:columns                1
	:menu-type              :nonexclusive
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Cream" "Sugar" )
	:menu-item-label-types  (:string :string )
	:menu-item-states       (:active :active )
	:menu-item-defaults     (nil nil )
	:initial-selections     (nil nil )
	:menu-item-handlers     (nil nil )
	:menu-item-menus        (nil nil )
	:menu-item-colors       ("" "" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   color_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Red" "Green" "Blue" )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :active :active )
	:menu-item-defaults     (nil nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (nil nil nil )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("Red" "Green" "Blue" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :menu
	:name                   glyph_menu
	:help                   ""
	:columns                2
	:menu-type              :exclusive
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("blank.pr" "gr_25.cursor" "gr_root.cursor" "gr_50.cursor" "gr_75.cursor" "black.cursor" )
	:menu-item-label-types  (:glyph :glyph :glyph :glyph :glyph :glyph )
	:menu-item-states       (:active :active :active :active :active :active )
	:menu-item-defaults     (nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil )
	:menu-item-handlers     (nil nil nil nil nil nil )
	:menu-item-menus        (nil nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :base-window
	:name                   controls
	:owner                  nil
	:width                  898
	:height                 491
	:background-color       ""
	:foreground-color       ""
	:label                  "OPEN LOOK Controls"
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
	:name                   buttons
	:owner                  controls
	:help                   ""
	:x                      0
	:y                      0
	:width                  480
	:height                 84
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   button_label
	:owner                  buttons
	:help                   ""
	:x                      214
	:y                      12
	:width                  53
	:height                 13
	:foreground-color       ""
	:label                  "Buttons"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   glyph_button
	:owner                  buttons
	:help                   ""
	:x                      88
	:y                      32
	:width                  42
	:height                 42
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "alert_bang32.pr"
	:label-type             :glyph
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   button
	:owner                  buttons
	:help                   ""
	:x                      15
	:y                      44
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
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   menu_button
	:owner                  buttons
	:help                   ""
	:x                      145
	:y                      44
	:width                  110
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Menu Button"
	:label-type             :string
	:initial-state          :active
	:menu                   menu_button_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   abbrev_menu_button
	:owner                  buttons
	:help                   ""
	:x                      270
	:y                      46
	:width                  202
	:height                 15
	:constant-width         nil
	:button-type            :abbreviated
	:foreground-color       ""
	:label                  "Abbreviated Menu Button"
	:label-type             :string
	:initial-state          :active
	:menu                   abbrev_menu_button_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   sliders_gauges
	:owner                  controls
	:help                   ""
	:x                      481
	:y                      0
	:width                  417
	:height                 395
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   slider_label
	:owner                  sliders_gauges
	:help                   ""
	:x                      143
	:y                      12
	:width                  131
	:height                 13
	:foreground-color       ""
	:label                  "Sliders and Gauges"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider
	:owner                  sliders_gauges
	:help                   ""
	:x                      48
	:y                      40
	:width                  247
	:height                 20
	:value-x                99
	:value-y                40
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Slider:"
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
	:initial-value          13
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_endboxes
	:owner                  sliders_gauges
	:help                   ""
	:x                      48
	:y                      65
	:width                  284
	:height                 20
	:value-x                99
	:value-y                65
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Slider:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          t
	:show-range             t
	:show-value             t
	:min-value              0
	:max-value              100
	:min-value-string       "Min"
	:max-value-string       "Max"
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          0
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_novalue
	:owner                  sliders_gauges
	:help                   ""
	:x                      48
	:y                      95
	:width                  202
	:height                 39
	:value-x                99
	:value-y                95
	:slider-width           100
	:ticks                  6
	:foreground-color       ""
	:label                  "Slider:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             t
	:show-value             nil
	:min-value              0
	:max-value              100
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        "Min"
	:max-tick-string        "Max"
	:initial-value          0
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   slider1
	:owner                  sliders_gauges
	:help                   ""
	:x                      316
	:y                      95
	:width                  64
	:height                 104
	:value-x                355
	:value-y                95
	:slider-width           50
	:ticks                  0
	:foreground-color       ""
	:label                  "Gain:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :vertical
	:show-endboxes          nil
	:show-range             t
	:show-value             nil
	:min-value              0
	:max-value              100
	:min-value-string       "Min"
	:max-value-string       "Max"
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          0
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_norange
	:owner                  sliders_gauges
	:help                   ""
	:x                      48
	:y                      148
	:width                  209
	:height                 20
	:value-x                99
	:value-y                148
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Slider:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             t
	:min-value              0
	:max-value              100
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          83
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_novalue_norange
	:owner                  sliders_gauges
	:help                   ""
	:x                      48
	:y                      173
	:width                  164
	:height                 20
	:value-x                99
	:value-y                173
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Slider:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          nil
	:show-range             nil
	:show-value             nil
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
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_red
	:owner                  sliders_gauges
	:help                   ""
	:x                      63
	:y                      202
	:width                  253
	:height                 20
	:value-x                99
	:value-y                202
	:slider-width           100
	:ticks                  0
	:foreground-color       "Red"
	:label                  "Red:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          t
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
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_green
	:owner                  sliders_gauges
	:help                   ""
	:x                      48
	:y                      227
	:width                  268
	:height                 20
	:value-x                99
	:value-y                227
	:slider-width           100
	:ticks                  0
	:foreground-color       "Green"
	:label                  "Green:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          t
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
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :slider
	:name                   hslider_blue
	:owner                  sliders_gauges
	:help                   ""
	:x                      57
	:y                      252
	:width                  259
	:height                 20
	:value-x                99
	:value-y                252
	:slider-width           100
	:ticks                  0
	:foreground-color       "Blue"
	:label                  "Blue:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          t
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
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :gauge
	:name                   hgauge
	:owner                  sliders_gauges
	:help                   ""
	:x                      46
	:y                      288
	:width                  173
	:height                 24
	:value-x                99
	:value-y                288
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Gauge:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-range             t
	:min-value              0
	:max-value              100
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          13
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :gauge
	:name                   vgauge
	:owner                  sliders_gauges
	:help                   ""
	:x                      264
	:y                      288
	:width                  140
	:height                 76
	:value-x                361
	:value-y                288
	:slider-width           60
	:ticks                  0
	:foreground-color       ""
	:label                  "Temperature"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :vertical
	:show-range             t
	:min-value              -100
	:max-value              100
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          0
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :gauge
	:name                   hgauge_norange
	:owner                  sliders_gauges
	:help                   ""
	:x                      46
	:y                      320
	:width                  169
	:height                 13
	:value-x                99
	:value-y                320
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "Gauge:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-range             nil
	:min-value              0
	:max-value              100
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          0
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :gauge
	:name                   hgauge_ticks
	:owner                  sliders_gauges
	:help                   ""
	:x                      11
	:y                      351
	:width                  212
	:height                 28
	:value-x                99
	:value-y                351
	:slider-width           100
	:ticks                  4
	:foreground-color       ""
	:label                  "Disk Usage:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-range             t
	:min-value              0
	:max-value              100
	:min-tick-string        "Min"
	:max-tick-string        "Max"
	:initial-value          82
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   messages
	:owner                  controls
	:help                   ""
	:x                      0
	:y                      84
	:width                  480
	:height                 84
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message_label
	:owner                  messages
	:help                   ""
	:x                      206
	:y                      12
	:width                  68
	:height                 13
	:foreground-color       ""
	:label                  "Messages"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   glyph_message
	:owner                  messages
	:help                   ""
	:x                      330
	:y                      44
	:width                  32
	:height                 32
	:foreground-color       "Yellow"
	:label                  "alert_bang32.pr"
	:label-type             :glyph
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   glyph_message2
	:owner                  messages
	:help                   ""
	:x                      412
	:y                      44
	:width                  32
	:height                 32
	:foreground-color       ""
	:label                  "alert_qmark32.pr"
	:label-type             :glyph
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   bold_message
	:owner                  messages
	:help                   ""
	:x                      37
	:y                      54
	:width                  94
	:height                 13
	:foreground-color       ""
	:label                  "Bold Message"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   normal_message
	:owner                  messages
	:help                   ""
	:x                      181
	:y                      54
	:width                  99
	:height                 13
	:foreground-color       ""
	:label                  "Normal Message"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   text_fields
	:owner                  controls
	:help                   ""
	:x                      1
	:y                      168
	:width                  480
	:height                 128
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   text_field_label
	:owner                  text_fields
	:help                   ""
	:x                      203
	:y                      12
	:width                  74
	:height                 13
	:foreground-color       ""
	:label                  "Text Fields"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   textfield_left
	:owner                  text_fields
	:help                   ""
	:x                      27
	:y                      39
	:width                  141
	:height                 15
	:value-x                104
	:value-y                39
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Text Field:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "Tree"
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
	:name                   numeric_textfield_left
	:owner                  text_fields
	:help                   ""
	:x                      248
	:y                      39
	:width                  210
	:height                 15
	:value-x                387
	:value-y                39
	:value-length           4
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Numeric Text Field:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              100
	:min-value              0
	:initial-value          13
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
	:name                   textfield_readonly
	:owner                  text_fields
	:help                   ""
	:x                      23
	:y                      62
	:width                  145
	:height                 15
	:value-x                104
	:value-y                62
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Read Only:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "Bush"
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   numeric_textfield_readonly
	:owner                  text_fields
	:help                   ""
	:x                      306
	:y                      62
	:width                  152
	:height                 15
	:value-x                387
	:value-y                62
	:value-length           4
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Read Only:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              100
	:min-value              0
	:initial-value          0
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   textfield1
	:owner                  text_fields
	:help                   ""
	:x                      124
	:y                      90
	:width                  233
	:height                 32
	:value-x                268
	:value-y                90
	:value-length           8
	:stored-length          80
	:rows                   2
	:foreground-color       ""
	:text-type              :multiline
	:label                  "Multiline Text Field:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "Hello World"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   settings
	:owner                  controls
	:help                   ""
	:x                      0
	:y                      296
	:width                  480
	:height                 195
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   setting_label
	:owner                  settings
	:help                   ""
	:x                      212
	:y                      12
	:width                  56
	:height                 13
	:foreground-color       ""
	:label                  "Settings"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   exclusive_setting
	:owner                  settings
	:help                   ""
	:x                      35
	:y                      48
	:width                  264
	:height                 23
	:value-x                163
	:value-y                48
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Exclusive Setting:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Coffee" "Tea" "Milk" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   check_box
	:owner                  settings
	:help                   ""
	:x                      312
	:y                      48
	:width                  154
	:height                 52
	:value-x                392
	:value-y                48
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Check Box:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Cream" "Sugar" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   nonexclusive_setting
	:owner                  settings
	:help                   ""
	:x                      8
	:y                      81
	:width                  272
	:height                 23
	:value-x                163
	:value-y                81
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :nonexclusive
	:selection-required     t
	:label                  "Nonexclusive Setting:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Cream" "Sugar" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   dogs
	:owner                  settings
	:help                   ""
	:x                      118
	:y                      113
	:width                  197
	:height                 69
	:value-x                163
	:value-y                113
	:rows                   3
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Dogs:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Labrador" "Retriever" "Boxer" "Spaniel" "Collie" "Dachsund" )
	:choice-label-types     (:string :string :string :string :string :string )
	:choice-colors          ("" "" "" "" "" "" )
	:initial-selections     (nil nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   stack
	:owner                  settings
	:help                   ""
	:x                      345
	:y                      115
	:width                  123
	:height                 23
	:value-x                392
	:value-y                115
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Stack:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Coffee" "Tea" "Milk" )
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
	:name                   color
	:owner                  settings
	:help                   ""
	:x                      345
	:y                      154
	:width                  125
	:height                 26
	:value-x                392
	:value-y                154
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Color:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("black.cursor" "black.cursor" "black.cursor" )
	:choice-label-types     (:glyph :glyph :glyph )
	:choice-colors          ("red" "green" "blue" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   menus
	:owner                  controls
	:help                   ""
	:x                      481
	:y                      393
	:width                  417
	:height                 98
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   menu_label
	:owner                  menus
	:help                   ""
	:x                      185
	:y                      12
	:width                  46
	:height                 13
	:foreground-color       ""
	:label                  "Menus"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   command_menu_button
	:owner                  menus
	:help                   ""
	:x                      41
	:y                      38
	:width                  95
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Command"
	:label-type             :string
	:initial-state          :active
	:menu                   command_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   pinnable_menu_button
	:owner                  menus
	:help                   ""
	:x                      163
	:y                      38
	:width                  86
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Pinnable"
	:label-type             :string
	:initial-state          :active
	:menu                   pinnable_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   exclusive_menu_button
	:owner                  menus
	:help                   ""
	:x                      285
	:y                      38
	:width                  88
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Exclusive"
	:label-type             :string
	:initial-state          :active
	:menu                   exclusive_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   nonexclusive_menu_button
	:owner                  menus
	:help                   ""
	:x                      41
	:y                      69
	:width                  112
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Nonexclusive"
	:label-type             :string
	:initial-state          :active
	:menu                   nonexclusive_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   color_menu_button
	:owner                  menus
	:help                   ""
	:x                      163
	:y                      69
	:width                  64
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Color"
	:label-type             :string
	:initial-state          :active
	:menu                   color_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   glyph_menu_button
	:owner                  menus
	:help                   ""
	:x                      285
	:y                      69
	:width                  74
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Glyphs"
	:label-type             :string
	:initial-state          :active
	:menu                   glyph_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
)
