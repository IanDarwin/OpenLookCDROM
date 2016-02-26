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
	:menu-item-handlers     (resource_insert_before resource_insert_after resource_insert_top resource_insert_bottom )
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
		:action                 (resource_insert_before)
		)
		(
		:from                   (insertMenu "After")
		:when                   (Notify )
		:to                     (insertMenu "After")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (resource_insert_after)
		)
		(
		:from                   (insertMenu "Top")
		:when                   (Notify )
		:to                     (insertMenu "Top")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (resource_insert_top)
		)
		(
		:from                   (insertMenu "Bottom")
		:when                   (Notify )
		:to                     (insertMenu "Bottom")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (resource_insert_bottom)
		)
	)
)
(
	:type                   :popup-window
	:name                   resourcesPopup
	:owner                  mainWindow
	:width                  343
	:height                 327
	:background-color       ""
	:foreground-color       ""
	:label                  "Resources"
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
	:owner                  resourcesPopup
	:help                   ""
	:x                      0
	:y                      0
	:width                  343
	:height                 327
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
	:name                   resourcesLabel
	:owner                  control
	:help                   ""
	:x                      16
	:y                      16
	:width                  71
	:height                 13
	:foreground-color       ""
	:label                  "Resources"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :scrolling-list
	:name                   resourceList
	:owner                  control
	:help                   "The list of resources for the currently selected task."
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
	:notify-handler         resource_list_notify
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                (
		(
		:from                   (resourcesPopup resourceList)
		:when                   (Notify )
		:to                     (resourcesPopup resourceList)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (resource_list_notify)
		)
	)
)
(
	:type                   :button
	:name                   insertButton
	:owner                  control
	:help                   "Insert a resource before or after the currently selected resource, or at the top or bottom of the list.  "
	:x                      96
	:y                      256
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
	:help                   "Delete the currently selected resource from the resource list for this task."
	:x                      192
	:y                      256
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Delete"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         resource_delete
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (resourcesPopup deleteButton)
		:when                   (Notify )
		:to                     (resourcesPopup deleteButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (resource_delete)
		)
	)
)
(
	:type                   :text-field
	:name                   resourceField
	:owner                  control
	:help                   "Enter the resource that you would like to add here."
	:x                      16
	:y                      296
	:width                  314
	:height                 15
	:value-x                90
	:value-y                296
	:value-length           30
	:stored-length          30
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Resource:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         resource_enter_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (resourcesPopup resourceField)
		:when                   (Notify )
		:to                     (resourcesPopup resourceField)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (resource_enter_callback)
		)
	)
)
)
