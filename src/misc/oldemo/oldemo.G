;GIL-3
(
(
	:type                   :menu
	:name                   fileMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Load" "Save" "Wipe out Motif" "Exit" )
	:menu-item-label-types  (:string :string :string :string )
	:menu-item-states       (:active :active :active :active )
	:menu-item-defaults     (nil nil nil nil )
	:initial-selections     (nil nil nil nil )
	:menu-item-handlers     (nil nil nil nil )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (fileMenu "Exit")
		:when                   (Notify )
		:to                     (fileMenu "Exit")
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	exit(0);	/* do it the hard way */")
		)
	)
)
(
	:type                   :menu
	:name                   editmenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Cut" "Copy" "Paste" )
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
	:name                   viewMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Graphic..." "Properties..." )
	:menu-item-label-types  (:string :string )
	:menu-item-states       (:active :active )
	:menu-item-defaults     (nil nil )
	:initial-selections     (nil nil )
	:menu-item-handlers     (nil nil )
	:menu-item-menus        (nil nil )
	:menu-item-colors       ("" "" )
	:pinnable               t
	:user-data              ()
	:actions                ()
)
(
	:type                   :base-window
	:name                   window1
	:owner                  nil
	:width                  520
	:height                 230
	:background-color       ""
	:foreground-color       ""
	:label                  "OPEN LOOK Application"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              nil
	:icon-file              "Book_glyph.icon"
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
	:width                  520
	:height                 50
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
	:type                   :drop-target
	:name                   drop_target1
	:owner                  controls1
	:help                   ""
	:x                      438
	:y                      5
	:width                  18
	:height                 16
	:drop-target_width      12
	:default-drop-site      nil
	:draggable              t
	:droppable              nil
	:label                  ""
	:label-type             :string
	:normal-drop-glyph      "Book_glyph.icon"
	:busy-drop-glyph        ""
	:dnd-cursor             ""
	:dnd-cursor-xhot        0
	:dnd-cursor-yhot        0
	:dnd-accept-cursor      ""
	:dnd-accept-cursor-xhot 0
	:dnd-accept-cursor-yhot 0
	:foreground-color       ""
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   fileButton
	:owner                  controls1
	:help                   ""
	:x                      15
	:y                      11
	:width                  70
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "File"
	:label-type             :string
	:initial-state          :active
	:menu                   fileMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   viewButton2
	:owner                  controls1
	:help                   ""
	:x                      102
	:y                      12
	:width                  78
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "View"
	:label-type             :string
	:initial-state          :active
	:menu                   viewMenu
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
	:x                      195
	:y                      13
	:width                  72
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Edit"
	:label-type             :string
	:initial-state          :active
	:menu                   editmenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group1
	:x                      15
	:y                      11
	:group-type             nil
	:members                (fileButton viewButton2 editButton )
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
	:type                   :canvas-pane
	:name                   canvas1
	:owner                  window1
	:help                   ""
	:x                      0
	:y                      48
	:width                  520
	:height                 88
	:background-color       "BG1"
	:foreground-color       "Black"
	:initial-state          :visible
	:draggable              nil
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:horizontal-scrollbar   nil
	:scrollable-width       520
	:vertical-scrollbar     nil
	:scrollable-height      88
	:repaint-proc           nil
	:event-handler          nil
	:drawing-model          :xview
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls2
	:owner                  window1
	:help                   ""
	:x                      0
	:y                      134
	:width                  520
	:height                 96
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
	:name                   uiSetting
	:owner                  controls2
	:help                   ""
	:x                      22
	:y                      17
	:width                  377
	:height                 25
	:value-x                189
	:value-y                17
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Preferred Interface:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("OPEN LOOK" "Motif" "Other" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (window1 uiSetting "Other")
		:when                   (Notify )
		:to                     (window1)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	puts(\"You draw\");")
		)
		(
		:from                   (window1 uiSetting "OPEN LOOK")
		:when                   (Notify )
		:to                     (window1)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	puts(\"You win\");")
		)
		(
		:from                   (window1 uiSetting "Motif")
		:when                   (Notify )
		:to                     (window1)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	puts(\"You lose\");")
		)
	)
)
(
	:type                   :gauge
	:name                   tempGauge
	:owner                  controls2
	:help                   ""
	:x                      21
	:y                      55
	:width                  306
	:height                 28
	:value-x                203
	:value-y                55
	:slider-width           100
	:ticks                  0
	:foreground-color       ""
	:label                  "GUI War Temperature:"
	:label-type             :string
	:layout-type            :horizontal
	:orientation            :horizontal
	:show-range             t
	:min-value              0
	:max-value              100
	:min-tick-string        ""
	:max-tick-string        ""
	:initial-value          90
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
)
