;GIL-3
(
(
	:type                   :menu
	:name                   insertMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Before" "After" "Top" "Bottom" )
	:menu-item-label-types  (:string :string :string :string )
	:menu-item-states       (:active :active :active :active )
	:menu-item-defaults     (nil nil nil t )
	:initial-selections     (nil nil nil nil )
	:menu-item-handlers     (dependency_insert_before dependency_insert_after dependency_insert_top dependency_insert_bottom )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (insertMenu "Before")
		:when                   (Notify )
		:to                     (insertMenu "Before")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_insert_before)
		)
		(
		:from                   (insertMenu "After")
		:when                   (Notify )
		:to                     (insertMenu "After")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_insert_after)
		)
		(
		:from                   (insertMenu "Top")
		:when                   (Notify )
		:to                     (insertMenu "Top")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_insert_top)
		)
		(
		:from                   (insertMenu "Bottom")
		:when                   (Notify )
		:to                     (insertMenu "Bottom")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_insert_bottom)
		)
	)
)
(
	:type                   :popup-window
	:name                   dependenciesPopup
	:owner                  mainWindow
	:width                  359
	:height                 326
	:background-color       ""
	:foreground-color       ""
	:label                  "Dependencies"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 t
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   control
	:owner                  dependenciesPopup
	:help                   "This is the dependency window for the current task.  Dependencies are those tasks which much be completed before the current task begins.
"
	:x                      0
	:y                      0
	:width                  359
	:height                 326
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
	:type                   :message
	:name                   dependenciesLabel
	:owner                  control
	:help                   ""
	:x                      16
	:y                      16
	:width                  97
	:height                 13
	:foreground-color       ""
	:label                  "Dependencies"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :scrolling-list
	:name                   dependencyList
	:owner                  control
	:help                   "This is the list of dependencies for the current task.  All dependencies listed must be completed before this task can begin.
"
	:x                      16
	:y                      40
	:width                  300
	:height                 200
	:value-x                16
	:value-y                40
	:rows                   10
	:foreground-color       ""
	:label                  ""
	:title                  ""
	:label-type             :string
	:layout-type            :horizontal
	:read-only              t
	:multiple-selections    nil
	:selection-required     nil
	:initial-state          :active
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:notify-handler         dependency_list_notify
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                (
		(
		:from                   (dependenciesPopup dependencyList)
		:when                   (Notify )
		:to                     (dependenciesPopup dependencyList)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_list_notify)
		)
	)
)
(
	:type                   :button
	:name                   insertButton
	:owner                  control
	:help                   "After entering a name into the dependency field, press here and select \"Top, Bottom, Before, or After\" from the menu.  If you select \"Before\" or \"After\", you must have a dependency selected that you would like to add the new dependency before or after.
"
	:x                      88
	:y                      264
	:width                  67
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Insert"
	:label-type             :string
	:initial-state          :active
	:menu                   insertMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
	)
)
(
	:type                   :button
	:name                   deleteButton
	:owner                  control
	:help                   "Deletes the selected dependency from the list."
	:x                      184
	:y                      264
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Delete"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         dependency_delete
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (dependenciesPopup deleteButton)
		:when                   (Notify )
		:to                     (dependenciesPopup deleteButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_delete)
		)
	)
)
(
	:type                   :text-field
	:name                   dependencyField
	:owner                  control
	:help                   "Enter the dependency that you would like to add to the list here."
	:x                      16
	:y                      304
	:width                  335
	:height                 15
	:value-x                111
	:value-y                304
	:value-length           30
	:stored-length          60
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Dependency:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         dependency_enter_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (dependenciesPopup dependencyField)
		:when                   (Notify )
		:to                     (dependenciesPopup dependencyField)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dependency_enter_callback)
		)
	)
)
)
