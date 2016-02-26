;GIL-3
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
	:menu-item-handlers     (again undo nil cut copy paste delete nil select_all clear )
	:menu-item-menus        (nil nil nil nil nil nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" "" "" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
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
		:from                   (edit_menu "Delete")
		:when                   (Notify )
		:to                     (edit_menu "Delete")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (delete)
		)
		(
		:from                   (edit_menu "Select All")
		:when                   (Notify )
		:to                     (edit_menu "Select All")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (select_all)
		)
		(
		:from                   (edit_menu "Clear")
		:when                   (Notify )
		:to                     (edit_menu "Clear")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (clear)
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
	:menu-item-labels       ("Full Size" "Half Size" "Quarter Size" )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :active :active )
	:menu-item-defaults     (nil nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (grid nil nil )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                (
		(
		:from                   (view_menu "Full Size")
		:when                   (Notify )
		:to                     (view_menu "Full Size")
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
	:menu-item-labels       ("Load..." "Save" "Save as..." "Print" "Print Options..." )
	:menu-item-label-types  (:string :string :string :string :string )
	:menu-item-states       (:active :active :active :active :active )
	:menu-item-defaults     (nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil )
	:menu-item-handlers     (load save save_as print nil )
	:menu-item-menus        (nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" )
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
		:from                   (file_menu "Print")
		:when                   (Notify )
		:to                     (file_menu "Print")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (print)
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
	:menu-item-labels       ("Selection..." "ExampleTool..." )
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
		:from                   (props_menu "ExampleTool...")
		:when                   (Notify )
		:to                     (props_menu "ExampleTool...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (props_program)
		)
	)
)
(
	:type                   :menu
	:name                   pane-pop-up_menu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             "Example Pane"
	:menu-item-labels       ("Undo" " " "Cut" "Copy" "Paste" "" "Props..." )
	:menu-item-label-types  (:string :string :string :string :string :string :string )
	:menu-item-states       (:active :active :active :active :active :active :active )
	:menu-item-defaults     (nil nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil nil )
	:menu-item-handlers     (nil nil nil nil nil nil nil )
	:menu-item-menus        (nil nil nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                ()
)
(
	:type                   :base-window
	:name                   basewin
	:owner                  nil
	:width                  437
	:height                 358
	:background-color       ""
	:foreground-color       ""
	:label                  "ExampleTool"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              "sun.icon"
	:icon-label             ""
	:icon-mask-file         ""
	:event-handler          nil
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
	:height                 34
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
	:name                   file
	:owner                  controls
	:help                   ""
	:x                      7
	:y                      7
	:width                  30
	:height                 19
	:constant-width         t
	:button-type            :normal
	:foreground-color       ""
	:label                  "File"
	:label-type             :string
	:initial-state          :active
	:menu                   file_menu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   view
	:owner                  controls
	:help                   ""
	:x                      79
	:y                      7
	:width                  30
	:height                 19
	:constant-width         t
	:button-type            :normal
	:foreground-color       ""
	:label                  "View"
	:label-type             :string
	:initial-state          :active
	:menu                   view_menu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   edit
	:owner                  controls
	:help                   ""
	:x                      151
	:y                      7
	:width                  30
	:height                 19
	:constant-width         t
	:button-type            :normal
	:foreground-color       ""
	:label                  "Edit"
	:label-type             :string
	:initial-state          :active
	:menu                   edit_menu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   properties
	:owner                  controls
	:help                   ""
	:x                      223
	:y                      7
	:width                  34
	:height                 19
	:constant-width         t
	:button-type            :normal
	:foreground-color       ""
	:label                  "Props"
	:label-type             :string
	:initial-state          :active
	:menu                   props_menu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group1
	:x                      7
	:y                      7
	:group-type             :row
	:members                (file view edit properties )
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
	:owner                  basewin
	:help                   ""
	:x                      0
	:y                      32
	:width                  437
	:height                 326
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:draggable              nil
	:droppable              nil
	:default-drop-site      nil
	:menu                   pane-pop-up_menu
	:horizontal-scrollbar   nil
	:scrollable-width       437
	:vertical-scrollbar     nil
	:scrollable-height      326
	:repaint-proc           nil
	:event-handler          nil
	:drawing-model          :xview
	:user-data              ()
	:actions                ()
)
)
