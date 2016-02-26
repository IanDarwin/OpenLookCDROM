;GIL-3
(
(
	:type                   :menu
	:name                   AbbrevButt_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Choice 1" "Choice 2" "Choice 3" "Choice 4" "Choice 5" )
	:menu-item-label-types  (:string :string :string :string :string )
	:menu-item-states       (:active :active :active :active :active )
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
	:type                   :popup-window
	:name                   popup2
	:owner                  nil
	:width                  377
	:height                 393
	:background-color       ""
	:foreground-color       ""
	:label                  "ExampleTool: Properties"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  popup2
	:help                   ""
	:x                      0
	:y                      0
	:width                  377
	:height                 393
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
	:name                   message1
	:owner                  controls1
	:help                   ""
	:x                      65
	:y                      20
	:width                  56
	:height                 13
	:foreground-color       ""
	:label                  "Version:"
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
	:name                   message2
	:owner                  controls1
	:help                   ""
	:x                      129
	:y                      20
	:width                  19
	:height                 13
	:foreground-color       ""
	:label                  "0.1"
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
	:x                      65
	:y                      20
	:group-type             nil
	:members                (message1 message2 )
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
	:name                   message4
	:owner                  controls1
	:help                   ""
	:x                      21
	:y                      55
	:width                  89
	:height                 13
	:foreground-color       ""
	:label                  "FIRST GROUP"
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
	:name                   setting3
	:owner                  controls1
	:help                   ""
	:x                      53
	:y                      85
	:width                  283
	:height                 23
	:value-x                129
	:value-y                85
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Exclusive:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Choice 1" "Choice 2" "Choice 3" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   textfield1
	:owner                  controls1
	:help                   ""
	:x                      23
	:y                      116
	:width                  177
	:height                 15
	:value-x                129
	:value-y                116
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Numeric Field:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              100
	:min-value              0
	:initial-value          0
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message3
	:owner                  controls1
	:help                   ""
	:x                      23
	:y                      164
	:width                  107
	:height                 13
	:foreground-color       ""
	:label                  "SECOND GROUP"
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
	:name                   slider1
	:owner                  controls1
	:help                   ""
	:x                      81
	:y                      196
	:width                  275
	:height                 39
	:value-x                129
	:value-y                196
	:slider-width           190
	:ticks                  12
	:foreground-color       ""
	:label                  "Slider:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-endboxes          t
	:show-range             nil
	:show-value             nil
	:min-value              0
	:max-value              100
	:min-value-string       ""
	:max-value-string       ""
	:min-tick-string        "0"
	:max-tick-string        "100"
	:initial-value          0
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   setting4
	:owner                  controls1
	:help                   ""
	:x                      18
	:y                      243
	:width                  340
	:height                 23
	:value-x                129
	:value-y                243
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :nonexclusive
	:selection-required     t
	:label                  "Non-Exclusive:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Choice 1" "Choice 2" "Choice 3" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   setting2
	:owner                  controls1
	:help                   ""
	:x                      63
	:y                      274
	:width                  154
	:height                 52
	:value-x                129
	:value-y                274
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Options:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:choices                ("Option 1" "Option 2" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   Apply_button
	:owner                  controls1
	:help                   ""
	:x                      120
	:y                      352
	:width                  40
	:height                 19
	:constant-width         t
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply"
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
	:name                   Reset_button
	:owner                  controls1
	:help                   ""
	:x                      190
	:y                      352
	:width                  40
	:height                 19
	:constant-width         t
	:button-type            :normal
	:foreground-color       ""
	:label                  "Reset"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
)
