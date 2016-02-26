;GIL-3
;	@(#)sample.G	2.6 91/10/15 Copyright 1989 Sun Microsystems
(
(
	:type                   :menu
	:name                   edit_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Again" "Undo" "" "Cut" "Copy" "Paste" "Delete" "" "Select All" "Clear" )
	:menu-item-label-types  (:string :string :string :string :string :string :string :string :string :string )
	:menu-item-states       (:active :active :active :active :active :active :active :active :active :active )
	:menu-item-defaults     (t nil nil nil nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil nil nil nil nil )
	:menu-item-handlers     (again undo nil cut copy paste delete_func nil select_all clear )
	:menu-item-menus        (nil nil nil nil nil nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" "" "" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (edit_menu "Select All")
		:when                   (Notify )
		:to                     (edit_menu "Select All")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (select_all)
		)
		(
		:from                   (edit_menu "Cut")
		:when                   (Notify )
		:to                     (edit_menu "Cut")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (cut)
		)
		(
		:from                   (edit_menu "Copy")
		:when                   (Notify )
		:to                     (edit_menu "Copy")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (copy)
		)
		(
		:from                   (edit_menu "Paste")
		:when                   (Notify )
		:to                     (edit_menu "Paste")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (paste)
		)
		(
		:from                   (edit_menu "Again")
		:when                   (Notify )
		:to                     (edit_menu "Again")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (again)
		)
		(
		:from                   (edit_menu "Undo")
		:when                   (Notify )
		:to                     (edit_menu "Undo")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (undo)
		)
		(
		:from                   (edit_menu "Clear")
		:when                   (Notify )
		:to                     (edit_menu "Clear")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (clear)
		)
		(
		:from                   (edit_menu "Delete")
		:when                   (Notify )
		:to                     (edit_menu "Delete")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (delete_func)
		)
	)
)
(
	:type                   :menu
	:name                   view_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Grid" )
	:menu-item-label-types  (:string )
	:menu-item-states       (:active )
	:menu-item-defaults     (nil )
	:initial-selections     (nil )
	:menu-item-handlers     (grid )
	:menu-item-menus        (nil )
	:menu-item-colors       ("" )
	:pinnable               nil
	:user-data              ()
	:actions                (
		(
		:from                   (view_menu "Grid")
		:when                   (Notify )
		:to                     (view_menu "Grid")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (grid)
		)
	)
)
(
	:type                   :menu
	:name                   file_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("New" "Load..." "Save" "Save as..." "Import..." "Export..." "Print" "Print Options..." )
	:menu-item-label-types  (:string :string :string :string :string :string :string :string )
	:menu-item-states       (:active :active :active :active :active :active :active :active )
	:menu-item-defaults     (nil nil nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil nil nil )
	:menu-item-handlers     (new_file load save save_as import export print print_options )
	:menu-item-menus        (nil nil nil nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" "" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                (
		(
		:from                   (file_menu "Load...")
		:when                   (Notify )
		:to                     (file_menu "Load...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (load)
		)
		(
		:from                   (file_menu "Save")
		:when                   (Notify )
		:to                     (file_menu "Save")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (save)
		)
		(
		:from                   (file_menu "Save as...")
		:when                   (Notify )
		:to                     (file_menu "Save as...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (save_as)
		)
		(
		:from                   (file_menu "Import...")
		:when                   (Notify )
		:to                     (file_menu "Import...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (import)
		)
		(
		:from                   (file_menu "Print")
		:when                   (Notify )
		:to                     (file_menu "Print")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (print)
		)
		(
		:from                   (file_menu "New")
		:when                   (Notify )
		:to                     (file_menu "New")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (new_file)
		)
		(
		:from                   (file_menu "Print Options...")
		:when                   (Notify )
		:to                     (file_menu "Print Options...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (print_options)
		)
		(
		:from                   (file_menu "Export...")
		:when                   (Notify )
		:to                     (file_menu "Export...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (export)
		)
	)
)
(
	:type                   :menu
	:name                   props_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Selection..." "Program..." )
	:menu-item-label-types  (:string :string )
	:menu-item-states       (:active :active )
	:menu-item-defaults     (t nil )
	:initial-selections     (nil nil )
	:menu-item-handlers     (props_selection props_program )
	:menu-item-menus        (nil nil )
	:menu-item-colors       ("" "" )
	:pinnable               nil
	:user-data              ()
	:actions                (
		(
		:from                   (props_menu "Selection...")
		:when                   (Notify )
		:to                     (props_menu "Selection...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (props_selection)
		)
		(
		:from                   (props_menu "Program...")
		:when                   (Notify )
		:to                     (props_menu "Program...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (props_program)
		)
	)
)
(
	:type                   :base-window
	:name                   basewin
	:owner                  nil
	:width                  437
	:height                 358
	:background-color       ""
	:foreground-color       ""
	:label                  "Sample"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              "sun.icon"
	:icon-mask-file         ""
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls
	:owner                  basewin
	:help                   ""
	:x                      0
	:y                      0
	:width                  437
	:height                 37
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
	:name                   file
	:owner                  controls
	:help                   ""
	:x                      10
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
	:type                   :button
	:name                   view
	:owner                  controls
	:help                   ""
	:x                      72
	:y                      10
	:width                  62
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "View"
	:label-type             :string
	:initial-state          :active
	:menu                   view_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   edit
	:owner                  controls
	:help                   ""
	:x                      142
	:y                      10
	:width                  56
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Edit"
	:label-type             :string
	:initial-state          :active
	:menu                   edit_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   properties
	:owner                  controls
	:help                   ""
	:x                      206
	:y                      10
	:width                  94
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Properties"
	:label-type             :string
	:initial-state          :active
	:menu                   props_menu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :canvas-pane
	:name                   canvas1
	:owner                  basewin
	:help                   ""
	:x                      0
	:y                      37
	:width                  437
	:height                 321
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:draggable              nil
	:droppable              nil
	:default-drop-site      nil
	:menu                   edit_menu
	:horizontal-scrollbar   nil
	:scrollable-width       437
	:vertical-scrollbar     nil
	:scrollable-height      321
	:repaint-proc           nil
	:event-handler          nil
	:events                 ()
	:drawing-model          :xview
	:user-data              ()
	:actions                ()
)
)
