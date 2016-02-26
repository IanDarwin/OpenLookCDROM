;GIL-3
(
(
	:type                   :popup-window
	:name                   filePopup
	:owner                  mainWindow
	:width                  343
	:height                 318
	:background-color       ""
	:foreground-color       ""
	:label                  "Default"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 t
	:done-handler           filePopup_done_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (filePopup)
		:when                   (Done )
		:to                     (filePopup)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (filePopup_done_callback)
		)
	)
)
(
	:type                   :control-area
	:name                   control
	:owner                  filePopup
	:help                   "This is the window in which you select a file that you would like to load/save."
	:x                      0
	:y                      0
	:width                  343
	:height                 318
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
	:name                   directoryField
	:owner                  control
	:help                   "This is the field in which you specify the directory that you would like to enter."
	:x                      24
	:y                      16
	:width                  314
	:height                 15
	:value-x                98
	:value-y                16
	:value-length           30
	:stored-length          256
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Directory:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         directory_prompt_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (filePopup directoryField)
		:when                   (Notify )
		:to                     (filePopup directoryField)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (directory_prompt_notify)
		)
	)
)
(
	:type                   :scrolling-list
	:name                   fileList
	:owner                  control
	:help                   "A listing of files in the specified directory."
	:x                      24
	:y                      40
	:width                  300
	:height                 200
	:value-x                24
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
	:notify-handler         fileList_notify_callback
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                (
		(
		:from                   (filePopup fileList)
		:when                   (Notify )
		:to                     (filePopup fileList)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (fileList_notify_callback)
		)
	)
)
(
	:type                   :text-field
	:name                   fileField
	:owner                  control
	:help                   "This is the field in which you can specify the file you would like to load/save."
	:x                      24
	:y                      256
	:width                  276
	:height                 15
	:value-x                60
	:value-y                256
	:value-length           30
	:stored-length          30
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "File:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         file_prompt_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (filePopup fileField)
		:when                   (Notify )
		:to                     (filePopup fileField)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (file_prompt_notify)
		)
	)
)
(
	:type                   :button
	:name                   applyButton
	:owner                  control
	:help                   "The file named will be stored or saved from/to the disk as appropriate.
"
	:x                      144
	:y                      288
	:width                  63
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Default"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         applyButton_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (filePopup applyButton)
		:when                   (Notify )
		:to                     (filePopup applyButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (applyButton_callback)
		)
	)
)
)
