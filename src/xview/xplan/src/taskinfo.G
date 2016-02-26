;GIL-3
(
(
	:type                   :popup-window
	:name                   taskinfoPopup
	:owner                  mainWindow
	:width                  543
	:height                 392
	:background-color       ""
	:foreground-color       ""
	:label                  "Task Information"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 t
	:done-handler           taskinfo_taskinfoPopup_done_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (taskinfoPopup)
		:when                   (Done )
		:to                     (taskinfoPopup)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (taskinfo_taskinfoPopup_done_callback)
		)
	)
)
(
	:type                   :control-area
	:name                   control
	:owner                  taskinfoPopup
	:help                   ""
	:x                      0
	:y                      0
	:width                  543
	:height                 392
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
	:type                   :text-field
	:name                   name
	:owner                  control
	:help                   "The name of the task."
	:x                      16
	:y                      24
	:width                  327
	:height                 15
	:value-x                103
	:value-y                24
	:value-length           30
	:stored-length          50
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Task Name:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   options
	:owner                  control
	:help                   "Options on the task.  Is it a milestone?  Is it a deliverable?"
	:x                      368
	:y                      24
	:width                  169
	:height                 52
	:value-x                434
	:value-y                24
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
	:choices                ("Milestone" "Deliverable" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   description
	:owner                  control
	:help                   "The description of the current task."
	:x                      16
	:y                      56
	:width                  313
	:height                 88
	:value-x                16
	:value-y                73
	:value-length           40
	:stored-length          400
	:rows                   5
	:foreground-color       ""
	:text-type              :multiline
	:label                  "Task Description:"
	:label-type             :string
	:layout-type            :vertical
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   duration
	:owner                  control
	:help                   "The duratin of the task in days.
"
	:x                      368
	:y                      88
	:width                  142
	:height                 15
	:value-x                439
	:value-y                88
	:value-length           4
	:stored-length          4
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Duration:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              1800
	:min-value              0
	:initial-value          0
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   floatTime
	:owner                  control
	:help                   "The float time for this task.
"
	:x                      392
	:y                      112
	:width                  115
	:height                 15
	:value-x                436
	:value-y                112
	:value-length           4
	:stored-length          4
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Float:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              1800
	:min-value              0
	:initial-value          0
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   startDateLabel
	:owner                  control
	:help                   ""
	:x                      16
	:y                      160
	:width                  67
	:height                 13
	:foreground-color       ""
	:label                  "Start Date"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   endDateLabel
	:owner                  control
	:help                   ""
	:x                      224
	:y                      160
	:width                  60
	:height                 13
	:foreground-color       ""
	:label                  "End Date"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   plannedStartDate
	:owner                  control
	:help                   "The date you plan to begin this task."
	:x                      32
	:y                      184
	:width                  130
	:height                 15
	:value-x                98
	:value-y                184
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Planned:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   plannedEndDate
	:owner                  control
	:help                   "The date you plan on ending this task.
"
	:x                      240
	:y                      184
	:width                  130
	:height                 15
	:value-x                306
	:value-y                184
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Planned:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   actualStartDate
	:owner                  control
	:help                   "The actual date on which this task started."
	:x                      32
	:y                      216
	:width                  118
	:height                 15
	:value-x                86
	:value-y                216
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Actual:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   actualEndDate
	:owner                  control
	:help                   "The actual date that this task ended."
	:x                      240
	:y                      216
	:width                  118
	:height                 15
	:value-x                294
	:value-y                216
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Actual:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   dependenciesButton
	:owner                  control
	:help                   "This button pops up a list of dependencies for you to view and modify."
	:x                      416
	:y                      232
	:width                  103
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Dependencies"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   forecastStartDate
	:owner                  control
	:help                   "The date used to specify when you think you will be able to begin this task (used when you don't begin by the planned date).
"
	:x                      32
	:y                      248
	:width                  132
	:height                 15
	:value-x                100
	:value-y                248
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Forecast:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   forecastEndDate
	:owner                  control
	:help                   "The date used to specify when you think you will be able to finish this task (used when you don't complete a task by the planned date).
"
	:x                      240
	:y                      248
	:width                  132
	:height                 15
	:value-x                308
	:value-y                248
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Forecast:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   earliestStartDate
	:owner                  control
	:help                   "The earliest date on which this task can start."
	:x                      32
	:y                      280
	:width                  128
	:height                 15
	:value-x                96
	:value-y                280
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Earliest:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   earliestEndDate
	:owner                  control
	:help                   "The earliest date that this task can end."
	:x                      240
	:y                      280
	:width                  128
	:height                 15
	:value-x                304
	:value-y                280
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Earliest:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   resourcesButton
	:owner                  control
	:help                   "Pops up a list of resources for this task.
"
	:x                      424
	:y                      280
	:width                  78
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Resources"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   latestStartDate
	:owner                  control
	:help                   "The latest date this process can be started and still keep from disturbing the critical path."
	:x                      32
	:y                      312
	:width                  117
	:height                 15
	:value-x                85
	:value-y                312
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Latest:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   latestEndDate
	:owner                  control
	:help                   "The latest date this task can finish and still keep from disturbing the critical path."
	:x                      240
	:y                      312
	:width                  117
	:height                 15
	:value-x                293
	:value-y                312
	:value-length           8
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Latest:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   applyButton
	:owner                  control
	:help                   "This button that saves changes in the task information window to the database."
	:x                      216
	:y                      360
	:width                  53
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         taskinfo_taskinfoPopup_applyButton_notify_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (taskinfoPopup applyButton)
		:when                   (Notify )
		:to                     (taskinfoPopup applyButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (taskinfo_taskinfoPopup_applyButton_notify_callback)
		)
	)
)
(
	:type                   :button
	:name                   resetButton
	:owner                  control
	:help                   "Loads the old data back into the task info window.
"
	:x                      304
	:y                      360
	:width                  51
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Reset"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         taskinfo_taskinfoPopup_resetButton_notify_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (taskinfoPopup resetButton)
		:when                   (Notify )
		:to                     (taskinfoPopup resetButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (taskinfo_taskinfoPopup_resetButton_notify_callback)
		)
	)
)
)
