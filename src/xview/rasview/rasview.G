;GIL-3
;	@(#)rasview.G	2.13 91/10/15 Copyright 1989 Sun Microsystems
(
(
	:type                   :menu
	:name                   file_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Load..." "Clear" )
	:menu-item-label-types  (:string :string )
	:menu-item-states       (:active :active )
	:menu-item-defaults     (nil nil )
	:initial-selections     (nil nil )
	:menu-item-handlers     (rasview_load_handler rasview_close_handler )
	:menu-item-menus        (nil nil )
	:menu-item-colors       ("" "" )
	:pinnable               nil
	:user-data              ()
	:actions                (
		(
		:from                   (file_menu "Load...")
		:when                   (Notify )
		:to                     (file_menu "Load...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (rasview_load_handler)
		)
		(
		:from                   (file_menu "Clear Canvas")
		:when                   (Notify )
		:to                     (file_menu "Clear Canvas")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (rasview_close_handler)
		)
	)
)
(
	:type                   :base-window
	:name                   win
	:owner                  nil
	:width                  581
	:height                 594
	:background-color       ""
	:foreground-color       ""
	:label                  "RasView"
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
	:owner                  win
	:help                   ""
	:x                      0
	:y                      0
	:width                  581
	:height                 40
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
	:name                   file_button
	:owner                  controls
	:help                   ""
	:x                      15
	:y                      10
	:width                  54
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "File"
	:label-type             :string
	:initial-state          :active
	:menu                   file_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :drop-target
	:name                   drop_target1
	:owner                  controls
	:help                   ""
	:x                      540
	:y                      10
	:width                  18
	:height                 16
	:drop-target_width      12
	:default-drop-site      nil
	:draggable              t
	:droppable              t
	:label                  ""
	:label-type             :string
	:normal-drop-glyph      ""
	:busy-drop-glyph        ""
	:dnd-cursor             "drag_cursor.xbm"
	:dnd-cursor-xhot        17
	:dnd-cursor-yhot        24
	:dnd-accept-cursor      "drop_cursor.xbm"
	:dnd-accept-cursor-xhot 17
	:dnd-accept-cursor-yhot 24
	:foreground-color       ""
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                (
		(
		:from                   (win drop_target1)
		:when                   (DroppedUpon )
		:to                     (win drop_target1)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (canvas_drop_function)
		)
		(
		:from                   (win drop_target1)
		:when                   (DraggedFrom )
		:to                     (win drop_target1)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (rasview_drag_function)
		)
	)
)
(
	:type                   :canvas-pane
	:name                   canvas
	:owner                  win
	:help                   ""
	:x                      0
	:y                      40
	:width                  581
	:height                 554
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:draggable              nil
	:droppable              t
	:default-drop-site      nil
	:menu                   nil
	:horizontal-scrollbar   t
	:scrollable-width       581
	:vertical-scrollbar     t
	:scrollable-height      554
	:repaint-proc           rasview_canvas_repaint
	:event-handler          nil
	:events                 ()
	:drawing-model          :xview
	:user-data              ()
	:actions                (
		(
		:from                   (win canvas)
		:when                   (Repaint )
		:to                     (win canvas)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (rasview_canvas_repaint)
		)
		(
		:from                   (win canvas)
		:when                   (DroppedUpon )
		:to                     (win canvas)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (canvas_drop_function)
		)
	)
)
)
