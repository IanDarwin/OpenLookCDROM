;GIL-3
(
(
	:type                   :popup-window
	:name                   exportPopup
	:owner                  mainWindow
	:width                  343
	:height                 105
	:background-color       ""
	:foreground-color       ""
	:label                  "Export Options"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 t
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (exportPopup)
		:when                   (Done )
		:to                     (exportPopup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Hide)
		)
	)
)
(
	:type                   :control-area
	:name                   control
	:owner                  exportPopup
	:help                   "The main window for the export options.  This is where you specify the filename and directory for the exported LaTeX files.
"
	:x                      0
	:y                      0
	:width                  343
	:height                 105
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
	:name                   directory
	:owner                  control
	:help                   "The directory in which exported LaTeX files will be placed."
	:x                      16
	:y                      16
	:width                  314
	:height                 15
	:value-x                90
	:value-y                16
	:value-length           30
	:stored-length          100
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
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   file
	:owner                  control
	:help                   "The file into which exported LaTeX documents will be placed.
"
	:x                      16
	:y                      48
	:width                  276
	:height                 15
	:value-x                52
	:value-y                48
	:value-length           30
	:stored-length          100
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
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   applyButton
	:owner                  control
	:help                   "Apply your changes to the directory and filename."
	:x                      144
	:y                      80
	:width                  53
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         export_apply_callback
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (exportPopup applyButton)
		:when                   (Notify )
		:to                     (exportPopup applyButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (export_apply_callback)
		)
	)
)
)
