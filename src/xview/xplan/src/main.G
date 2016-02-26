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
	:menu-item-labels       ("Load..." "New Project" "Save" "Save As..." )
	:menu-item-label-types  (:string :string :string :string )
	:menu-item-states       (:active :active :active :active )
	:menu-item-defaults     (nil nil nil nil )
	:initial-selections     (nil nil nil nil )
	:menu-item-handlers     (nil new_project_callback save_callback nil )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (fileMenu "New Project")
		:when                   (Notify )
		:to                     (fileMenu "New Project")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (new_project_callback)
		)
		(
		:from                   (fileMenu "Save")
		:when                   (Notify )
		:to                     (fileMenu "Save")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (save_callback)
		)
	)
)
(
	:type                   :menu
	:name                   chartMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("PERT" "Gantt" )
	:menu-item-label-types  (:string :string )
	:menu-item-states       (:active :active )
	:menu-item-defaults     (nil nil )
	:initial-selections     (nil nil )
	:menu-item-handlers     (display_pert_callback display_gantt_callback )
	:menu-item-menus        (nil nil )
	:menu-item-colors       ("" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (chartMenu "PERT")
		:when                   (Notify )
		:to                     (chartMenu "PERT")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (display_pert_callback)
		)
		(
		:from                   (chartMenu "Gantt")
		:when                   (Notify )
		:to                     (chartMenu "Gantt")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (display_gantt_callback)
		)
	)
)
(
	:type                   :menu
	:name                   analyzeMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Find Critical Path" "Check Dependencies" "Date Calculation" )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :active :active )
	:menu-item-defaults     (t nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (find_critical_path_callback check_dependencies_callback calc_early_start_date )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (analyzeMenu "Find Critical Path")
		:when                   (Notify )
		:to                     (analyzeMenu "Find Critical Path")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (find_critical_path_callback)
		)
		(
		:from                   (analyzeMenu "Check Dependencies")
		:when                   (Notify )
		:to                     (analyzeMenu "Check Dependencies")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (check_dependencies_callback)
		)
		(
		:from                   (analyzeMenu "Date Calculation")
		:when                   (Notify )
		:to                     (analyzeMenu "Date Calculation")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (calc_early_start_date)
		)
	)
)
(
	:type                   :menu
	:name                   exportMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Generate Task Sheet" "Generate PERT Chart" "Generate Gantt Chart" "Options..." )
	:menu-item-label-types  (:string :string :string :string )
	:menu-item-states       (:active :active :active :active )
	:menu-item-defaults     (t nil nil nil )
	:initial-selections     (nil nil nil nil )
	:menu-item-handlers     (task_list_callback latex_pert_callback latex_gantt_callback nil )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (exportMenu "Generate Task Sheet")
		:when                   (Notify )
		:to                     (exportMenu "Generate Task Sheet")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (task_list_callback)
		)
		(
		:from                   (exportMenu "Generate PERT Chart")
		:when                   (Notify )
		:to                     (exportMenu "Generate PERT Chart")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (latex_pert_callback)
		)
		(
		:from                   (exportMenu "Generate Gantt Chart")
		:when                   (Notify )
		:to                     (exportMenu "Generate Gantt Chart")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (latex_gantt_callback)
		)
	)
)
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
	:menu-item-handlers     (nil nil nil nil )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
	)
)
(
	:type                   :base-window
	:name                   mainWindow
	:owner                  nil
	:width                  343
	:height                 315
	:background-color       ""
	:foreground-color       ""
	:label                  "xplan"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              ""
	:icon-label             ""
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   control
	:owner                  mainWindow
	:help                   "This is the main window of xplan.  The list in the window is the list of tasks for the project being planned.  The menu options allow you to do various calculations and displays."
	:x                      0
	:y                      0
	:width                  343
	:height                 315
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
	:name                   fileButton
	:owner                  control
	:help                   "File menu, for starting a new project, or loading an old one."
	:x                      16
	:y                      16
	:width                  54
	:height                 19
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
	:name                   chartButton
	:owner                  control
	:help                   "The chart menu, which allows you to display graphically the PERT or Gantt chart for a task network."
	:x                      88
	:y                      16
	:width                  66
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Chart"
	:label-type             :string
	:initial-state          :active
	:menu                   chartMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   analyzeButton
	:owner                  control
	:help                   "The analyize menu, which allows you to do various calculations."
	:x                      168
	:y                      16
	:width                  80
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Analyze"
	:label-type             :string
	:initial-state          :active
	:menu                   analyzeMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   exportButton
	:owner                  control
	:help                   "The menu for the LaTeX export options.
"
	:x                      264
	:y                      16
	:width                  71
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Export"
	:label-type             :string
	:initial-state          :active
	:menu                   exportMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   tasksLabel
	:owner                  control
	:help                   ""
	:x                      16
	:y                      48
	:width                  41
	:height                 13
	:foreground-color       ""
	:label                  "Tasks"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :scrolling-list
	:name                   taskList
	:owner                  control
	:help                   "This is the task list for the current project.  Entries in the list correspond to tasks that much be completed for this project.  The first task in the list must be the task that begins the project, and the last task in the list must be the final task in the project.  All tasks other than the first and last must have dependencies, and must be a dependency of another tasks.  The first task can have no dependencies, and no task can depend on the last."
	:x                      16
	:y                      72
	:width                  300
	:height                 200
	:value-x                16
	:value-y                72
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
	:notify-handler         nil
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   insertButton
	:owner                  control
	:help                   "Insert a new task into the task list, either before or after the currently selected task, or at the top of bottom of the list.
"
	:x                      96
	:y                      288
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
	:help                   "Deletes the currently selected task from the task list."
	:x                      192
	:y                      288
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Delete"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         main_deleteButton_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (mainWindow deleteButton)
		:when                   (Notify )
		:to                     (mainWindow deleteButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (main_deleteButton_callback)
		)
	)
)
)
