;GIL-3
(
(
	:type                   :base-window
	:name                   window1
	:owner                  nil
	:width                  600
	:height                 700
	:background-color       ""
	:foreground-color       ""
	:label                  "Fax Tool"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              "faxtool.icon"
	:icon-label             "FaxTool"
	:icon-mask-file         "faxtool_mask.icon"
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1)
		:when                   (Resize )
		:to                     (window1)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (main_panel_resize_notify)
		)
	)
)
(
	:type                   :control-area
	:name                   top_controls
	:owner                  window1
	:help                   "Faxtool is a OPENLOOK (TM) front end to FlexFax.

To use this tool select one of the operating mode setting (Send Fax or Status) desired:

Send Fax:	For scheduling a fax for 
		transmission.

Status:		To monitor, view, print, change, 
		or delete pending faxes.

Note: See the online help for each control panel for a detailed description of each mode."
	:x                      0
	:y                      0
	:width                  600
	:height                 57
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   mode
	:owner                  top_controls
	:help                   "Mode:	This setting controls which panel is 
	displayed. There is one for sending faxes 
	and one for displaying and optionally 
	changing the status of pending fax 
	transmissions.

Select the choice desired."
	:x                      214
	:y                      17
	:width                  172
	:height                 23
	:value-x                261
	:value-y                17
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Mode:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Send Fax" "Status" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "white" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (window1 mode "Send Fax")
		:when                   (Notify )
		:to                     (window1 send_controls)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
		(
		:from                   (window1 mode "Status")
		:when                   (Notify )
		:to                     (window1 status_controls)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :drop-target
	:name                   drop_target
	:owner                  top_controls
	:help                   "Drop Target:	You may add files to the Files To 
		Transmit list by dragging them 
		from other tools and releasing 
		them over this drop site. You may 
		also drag them directly onto the 
		Faxtool icon when the window is 
		closed."
	:x                      572
	:y                      17
	:width                  18
	:height                 16
	:drop-target_width      12
	:default-drop-site      t
	:draggable              nil
	:droppable              t
	:label                  ""
	:label-type             :string
	:normal-drop-glyph      ""
	:busy-drop-glyph        ""
	:dnd-cursor             ""
	:dnd-cursor-xhot        0
	:dnd-cursor-yhot        0
	:dnd-accept-cursor      ""
	:dnd-accept-cursor-xhot 0
	:dnd-accept-cursor-yhot 0
	:foreground-color       "firebrick"
	:initial-state          :active
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 drop_target)
		:when                   (DroppedUpon )
		:to                     (window1 drop_target)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (drop_notify)
		)
	)
)
(
	:type                   :button
	:name                   props_button
	:owner                  top_controls
	:help                   "Properties:	Press this button to access the 
		Properties Popup.

The Properties Popup allows you to customize FaxTool commands for Viewing,  Printing and Previewing of Faxes, as well as to set defaults 
for Fax Host and Fax Spooling Directory."
	:x                      11
	:y                      19
	:width                  89
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "white"
	:label                  "Properties..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 props_button)
		:when                   (Notify )
		:to                     (prop_popup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :group
	:name                   top_objects
	:x                      11
	:y                      17
	:group-type             nil
	:members                (mode drop_target props_button )
	:rows                   0
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :left-edges
	:horizontal-spacing     10
	:vertical-spacing       10
	:anchor-object          top_controls
	:anchor-point           :center
	:reference-point        :center
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   send_controls
	:owner                  window1
	:help                   "This panel is used to create and schedule a fax for transmission.  Capabilities include:

Creation of an optional cover page.

Definition of documents to send via Drag and Drop, File-chooser popup, or manual entry.

Selection of transmission options including:

	Resolution
	Paper Size
	Sender Notification
	Transmission Time

To use this panel:

1) Select Coverpage - Yes or No, and, if Yes, fill in the desired fields.  If you elect to send a coverpage, you must minimally fill in the Name: field.

2) Select the document or documents to be sent using DnD or the Select Files Popup.

3) Select the transmission options desired.

4) Optionally, press Preview to view the fax before sending.

5) Press Send.

Note: The Clear button resets all fields to their defaults and clears the scrolling list.  Reset restores the fields the the values contained before the last Clear."
	:x                      0
	:y                      57
	:width                  600
	:height                 643
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   cover
	:owner                  send_controls
	:help                   "Coverpage:	Select Yes if you want a 
		coverpage.

		Select No if you do not want a 
		coverpage."
	:x                      100
	:y                      25
	:width                  147
	:height                 23
	:value-x                190
	:value-y                25
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  "Coverpage?:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("No" "Yes" )
	:choice-label-types     (:string :string )
	:choice-colors          ("Yellow" "white" )
	:choice-defaults        (nil t )
	:initial-selections     (nil t )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (window1 cover "Yes")
		:when                   (Notify )
		:to                     (window1 add_dir_button)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (window1 cover "No")
		:when                   (Notify )
		:to                     (window1 add_dir_button)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 cover "No")
		:when                   (Notify )
		:to                     (window1 to)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 cover "Yes")
		:when                   (Notify )
		:to                     (window1 to)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (window1 cover "No")
		:when                   (Notify )
		:to                     (window1 company)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 cover "Yes")
		:when                   (Notify )
		:to                     (window1 company)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (window1 cover "No")
		:when                   (Notify )
		:to                     (window1 re)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 cover "Yes")
		:when                   (Notify )
		:to                     (window1 re)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (window1 cover "No")
		:when                   (Notify )
		:to                     (window1 comments)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 cover "Yes")
		:when                   (Notify )
		:to                     (window1 comments)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
	)
)
(
	:type                   :button
	:name                   directory_button
	:owner                  send_controls
	:help                   "Directory Button:	Press this button to 
			activate the Directory 
			popup.  

The Directory popup allows you to select a 
recipient from your personal Fax Directory 
stored in ~/.faxtoolrc."
	:x                      267
	:y                      27
	:width                  82
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "white"
	:label                  "Directory..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 directory_button)
		:when                   (Notify )
		:to                     (directory_popup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :button
	:name                   add_dir_button
	:owner                  send_controls
	:help                   "Add To Directory:	This button automatically 
			stores the contents of 
			the Fax Number, Name, 
			and Company fields in 
			the scrolling list on 
			the Directory popup.

Note: You must still access the popup and press Save to premanently store the added entry."
	:x                      369
	:y                      27
	:width                  120
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Green"
	:label                  "Add To Directory"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         add_dir_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 add_dir_button)
		:when                   (Notify )
		:to                     (window1 add_dir_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (add_dir_notify)
		)
	)
)
(
	:type                   :group
	:name                   cover_top_objects
	:x                      100
	:y                      25
	:group-type             :row
	:members                (cover directory_button add_dir_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     20
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
	:type                   :text-field
	:name                   fax
	:owner                  send_controls
	:help                   "Fax Number:	The phone number of the 
		receipient's fax machine.

(Mandatory)

(Maximum Length - 25 characters)"
	:x                      98
	:y                      58
	:width                  292
	:height                 15
	:value-x                190
	:value-y                58
	:value-length           25
	:stored-length          25
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Fax Number:"
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
	:name                   to
	:owner                  send_controls
	:help                   "Name:	The name of the receipient.

(Mandatory if Coverpage is Yes)

(Maximum Length - 80 characters)"
	:x                      140
	:y                      83
	:width                  250
	:height                 15
	:value-x                190
	:value-y                83
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Name:"
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
	:name                   company
	:owner                  send_controls
	:help                   "Company:	The name of the receipient's 
		company.

(Optional)

(Maximum Length - 80 characters)"
	:x                      115
	:y                      108
	:width                  275
	:height                 15
	:value-x                190
	:value-y                108
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Company:"
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
	:name                   re
	:owner                  send_controls
	:help                   "Regarding:	A one line summary of the subject 
		of this fax.

(Optional)

(Maximum Length - 80 characters)"
	:x                      110
	:y                      133
	:width                  280
	:height                 15
	:value-x                190
	:value-y                133
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Regarding:"
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
	:name                   comments
	:owner                  send_controls
	:help                   "Comments:	You may enter a brief message on 
		the coverpage by typing it into 
		the Comments box.

(Optional)

(Maximum Length - 512 characters)"
	:x                      106
	:y                      158
	:width                  397
	:height                 84
	:value-x                190
	:value-y                158
	:value-length           40
	:stored-length          512
	:rows                   6
	:foreground-color       ""
	:text-type              :multiline
	:label                  "Comments:"
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
	:type                   :group
	:name                   coverpage
	:x                      98
	:y                      25
	:group-type             :column
	:members                (cover_top_objects fax to company re comments )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :scrolling-list
	:name                   file_list
	:owner                  send_controls
	:help                   "Files To 
Transmit:	This list contains all of the 
		files to be sent, in the order in 
		which they will be transmitted.

To add a file to the list:

1) Press the Select Files button and choose the 
   file(s) desired using the resulting popup.

2) Drag the file desired from another tool (such 
   as FileManager) either to the Drop Target (in 
   the upper right hand corner of this panel), or 
   directly onto the tool icon when the tool is 
   closed.

3) Manually add a line to the list by clicking 
   the right mouse button over the list and 
   selecting Edit List on the resulting menu. 
   This places you in edit mode.  Click right 
   again and select Insert.  You may then type 
   the file or pathname directly into the 
   resulting row, which will be underscored. When 
   you are finised, click right again to 
   redisplay the menu, and select End Editing.

To change a list entry:

1) Click the right mouse button over the list and 
   select Edit List on the resulting menu.  This 
   places you in edit mode.  

2) Select the row that you wish to change.  It 
   will become highlighted. 

3) Click right again and select Change to modify 
   the row entry.  You may then type the changes 
   directly into the row, which will be 
   underscored.

4) When you are finised, click right again to 
   redisplay the menu, and select End Editing.

To delete a list entry:

1) Click the right mouse button over the list and 
   select Edit List on the resulting menu.  This 
   places you in edit mode.  

2) Select the row that you wish to change.  It 
   will become highlighted. 

3) Click right again and select Delete. 

4) When you are finised, click right again to 
   redisplay the menu, and select End Editing.

(Mandatory - There must be at least one file in 
             the list to send a fax)"
	:x                      66
	:y                      272
	:width                  450
	:height                 116
	:value-x                66
	:value-y                272
	:rows                   4
	:foreground-color       ""
	:label                  ""
	:title                  "Files To Transmit               "
	:label-type             :string
	:layout-type            :vertical
	:read-only              nil
	:multiple-selections    t
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
	:name                   files_button
	:owner                  send_controls
	:help                   "Select File:	Press this button to display the 
		File Chooser popup.  

Files selected with the popup will be placed in the Files To Transmit list."
	:x                      253
	:y                      398
	:width                  95
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "white"
	:label                  "Select Files..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         browse_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 files_button)
		:when                   (Notify )
		:to                     (window1 files_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (browse_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   file_objects
	:x                      66
	:y                      272
	:group-type             :column
	:members                (file_list files_button )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
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
	:type                   :setting
	:name                   resolution
	:owner                  send_controls
	:help                   "Resolution:	Select Normal for low resolution 
		or Fine for high resolution.

(Mandatory)

(Default - Normal)"
	:x                      170
	:y                      447
	:width                  187
	:height                 23
	:value-x                255
	:value-y                447
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Resolution:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Normal" "Fine" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "yellow" )
	:initial-selections     (t nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   page_size
	:owner                  send_controls
	:help                   "Page Size:	Select the page size for your 
		document(s).  The choices are 
		Letter, A4, or B4.

(Mandatory)

(Default - Letter)"
	:x                      180
	:y                      480
	:width                  189
	:height                 23
	:value-x                255
	:value-y                480
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Page Size:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Letter" "A4" "B4" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("white" "white" "white" )
	:initial-selections     (t nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   notify
	:owner                  send_controls
	:help                   "Notify Sender:	This setting determines if the 
		sender is notified, via email, of 
		various transmission events.

Select On Delivery, to be notified when the fax has been successfully sent.

Select On Retry, to be notified each time the fax transmission fails and is rescheduled.

Select both choices, to enable both options.

(Optional)

(Default - No Notification)"
	:x                      151
	:y                      513
	:width                  273
	:height                 23
	:value-x                255
	:value-y                513
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :nonexclusive
	:selection-required     t
	:label                  "Notify Sender:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("On Delivery" "On Retry" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "yellow" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   time_unit
	:owner                  send_controls
	:help                   "Time To Send:	This option determines when the 
		fax is sent.  To schedule a fax 
		for future transmission, enter 
		the desired delay and select 
		Minutes or Hours.

(Optional)

(Default - 0 Minutes - Send Immediately)"
	:x                      364
	:y                      546
	:width                  85
	:height                 23
	:value-x                364
	:value-y                546
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  ""
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Minutes" "Hours" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "white" )
	:choice-defaults        (t nil )
	:initial-selections     (t nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   send
	:owner                  send_controls
	:help                   "Time To Send:	This option determines when the 
		fax is sent.  To schedule a fax 
		for future transmission, enter 
		the desired delay and select 
		Minutes or Hours.

(Optional)

(Default - 0 Minutes - Send Immediately)"
	:x                      151
	:y                      550
	:width                  136
	:height                 15
	:value-x                255
	:value-y                550
	:value-length           4
	:stored-length          4
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Time To Send:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       nil
	:initial-value          "Now"
	:initial-state          :active
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   time
	:owner                  send_controls
	:help                   "Time To Send:	This option determines when the 
		fax is sent.  To schedule a fax 
		for future transmission, enter 
		the desired delay and select 
		Minutes or Hours.

(Optional)

(Default - 0 Minutes - Send Immediately)"
	:x                      292
	:y                      550
	:width                  67
	:height                 15
	:value-x                308
	:value-y                550
	:value-length           2
	:stored-length          2
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "+"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              99
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
	:type                   :group
	:name                   send_time
	:x                      151
	:y                      546
	:group-type             :row
	:members                (send time time_unit )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     5
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
	:type                   :group
	:name                   options
	:x                      151
	:y                      447
	:group-type             :column
	:members                (resolution page_size notify send_time )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :button
	:name                   send_button
	:owner                  send_controls
	:help                   "Send:	Press this button to submit your fax for 
	transmission.  

The status of your request will appear in the lower left hand corner of the window."
	:x                      63
	:y                      599
	:width                  48
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "green"
	:label                  "Send"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 send_button)
		:when                   (Notify )
		:to                     (window1 preview_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (send_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   preview_button
	:owner                  send_controls
	:help                   "Preview:	Press this button to preview your 
		fax (including cover page) prior 
		to sending it.

Note: Preview does not attempt to send the fax."
	:x                      161
	:y                      599
	:width                  76
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "white"
	:label                  "Preview..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         preview_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 preview_button)
		:when                   (Notify )
		:to                     (window1 preview_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (preview_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   send_lower_left_buttons
	:x                      63
	:y                      599
	:group-type             :row
	:members                (send_button preview_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     50
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
	:type                   :button
	:name                   clear_button
	:owner                  send_controls
	:help                   "Clear:	Press this button to reset the all fields 
	on the panel to their defaults.  The 
	Files To Transmit list is also cleared."
	:x                      387
	:y                      599
	:width                  50
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "firebrick"
	:label                  "Clear"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 clear_button)
		:when                   (Notify )
		:to                     (window1 comments)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (clear_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   reset_button
	:owner                  send_controls
	:help                   "Reset:	Press this button to restore all fields 
	and the Files To Transmit list to their 
	values prior to the last use of the Clear 
	button."
	:x                      487
	:y                      599
	:width                  51
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Yellow"
	:label                  "Reset"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 reset_button)
		:when                   (Notify )
		:to                     (window1 comments)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (reset_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   send_lower_right_buttons
	:x                      387
	:y                      599
	:group-type             :row
	:members                (clear_button reset_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     50
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
	:type                   :group
	:name                   send_lower_buttons
	:x                      63
	:y                      599
	:group-type             :row
	:members                (send_lower_left_buttons send_lower_right_buttons )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     150
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
	:type                   :group
	:name                   send_objects
	:x                      63
	:y                      25
	:group-type             :column
	:members                (coverpage file_objects options send_lower_buttons )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
	:horizontal-spacing     10
	:vertical-spacing       30
	:anchor-object          send_controls
	:anchor-point           :center
	:reference-point        :center
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   status_controls
	:owner                  window1
	:help                   "This panel is used to check the status of the fax queue, print and/or view received faxes, and optionally modify the fax queue entries of jobs awaiting transmission. 
 
Capabilities include:

Query of the entire queue or just entries for 
this user.

Query of the Receive Queue or the Transmit Queue.

Viewing of received faxes.

Printing of received faxes.

Deletion of received faxes and faxes awaiting 
transmission.

Modification of transmission options including:

	Transmission Time
	Sender Notification

To use this panel:

1) Select the query options desired.

2) For the Send Queue:

	a) Select an entry from the Fax Queue 
	   list for modification or deletion.

	b) Press Delete to remove the entry and 
	   cancel the request.

	or,

	c) Select the transmission options 
	   desired.

	d) Press Modify to change the options.

3) For the Receive Queue:

	a) Select an entry from the Fax Queue 
	   list for viewing or printing.

	b) Press View to display the fax.

	or,

	c) Press Print to print the fax.

	or,

	d) Press Delete to delete the fax.

Note: View, Print, and Delete are only active 
for received faxes for which you have the 
appropriate permissions.
"
	:x                      0
	:y                      57
	:width                  600
	:height                 643
	:background-color       ""
	:foreground-color       ""
	:initial-state          :invisible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   query_mode
	:owner                  status_controls
	:help                   "Query For:	This setting determines the scope 
		of the query.

Select All Jobs, to see all pending transmit requests for all users.

Select My Jobs Only, to see only those jobs submitted by this user.  In this mode, you may choose to view either the Receive Queue or the 
Send Queue.

(Mandatory)

(Default - My Jobs Only)"
	:x                      166
	:y                      29
	:width                  235
	:height                 23
	:value-x                244
	:value-y                29
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Query For:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("All Jobs" "My Jobs Only" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "white" )
	:initial-selections     (nil t )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (window1 query_mode "All Jobs")
		:when                   (Notify )
		:to                     (window1 query_mode "All Jobs")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (all_jobs_notify)
		)
		(
		:from                   (window1 query_mode "All Jobs")
		:when                   (Notify )
		:to                     (window1 query_type)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 query_mode "My Jobs Only")
		:when                   (Notify )
		:to                     (window1 query_type)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
	)
)
(
	:type                   :setting
	:name                   query_type
	:owner                  status_controls
	:help                   "Job Queue:	This setting determines which 
		queue is displayed.

Select Receive Queue, to view the status of received faxes.

Select Send Queue, to view the status of faxes waiting to be sent.

(Optional)

(Default - Send Queue)"
	:x                      161
	:y                      62
	:width                  279
	:height                 23
	:value-x                244
	:value-y                62
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Job Queue:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Receive Queue" "Send Queue" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "white" )
	:initial-selections     (nil t )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (window1 query_type "Receive Queue")
		:when                   (Notify )
		:to                     (window1 query_type "Receive Queue")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (receive_queue_notify)
		)
		(
		:from                   (window1 query_type "Send Queue")
		:when                   (Notify )
		:to                     (window1 query_type "Send Queue")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (send_queue_notify)
		)
	)
)
(
	:type                   :group
	:name                   query_objects
	:x                      161
	:y                      29
	:group-type             :column
	:members                (query_mode query_type )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :scrolling-list
	:name                   jobs_list
	:owner                  status_controls
	:help                   "Fax Queue:	This list displays the status of 
		all jobs based on the query 
		criteria selected using the 
		Query For and Job Queue choices.

To update the list, select the desired query criteria and press the Query button.

To modify a list entry, select it, choose the desired changes from the choices at the bottom of the panel, and press Modify.

To delete a list entry, select it and press Delete."
	:x                      35
	:y                      115
	:width                  511
	:height                 350
	:value-x                35
	:value-y                132
	:rows                   17
	:foreground-color       ""
	:label                  ""
	:title                  "Job  Modem Destination     Time-To-Send     Sender         Status                        "
	:label-type             :string
	:layout-type            :vertical
	:read-only              t
	:multiple-selections    nil
	:selection-required     nil
	:initial-state          :active
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:notify-handler         jobs_list_notify
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                (
		(
		:from                   (window1 jobs_list)
		:when                   (Notify )
		:to                     (window1 jobs_list)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (jobs_list_notify)
		)
	)
)
(
	:type                   :button
	:name                   status_view_button
	:owner                  status_controls
	:help                   "View:	Press this buttton to view the fax 
	selected from the Fax Queue list.

This button only works for received faxes."
	:x                      89
	:y                      475
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "white"
	:label                  "View..."
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         view_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 status_view_button)
		:when                   (Notify )
		:to                     (window1 status_view_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (view_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   status_print_button
	:owner                  status_controls
	:help                   "Print:	Press this button to print the fax 
	selected in the Fax Queue list.

Note: This button only works for received faxes."
	:x                      196
	:y                      475
	:width                  48
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "white"
	:label                  "Print"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         print_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 status_print_button)
		:when                   (Notify )
		:to                     (window1 status_print_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (print_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   job_queue_left_buttons
	:x                      89
	:y                      475
	:group-type             :row
	:members                (status_view_button status_print_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     50
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
	:type                   :button
	:name                   modify_button
	:owner                  status_controls
	:help                   "Modify:		Press this button to change 
		the attributes of the selected 
		transmit job to the values 
		selected using the Reschedule, 
		Time To Send, and Notify Sender 
		choices at the bottom of the 
		panel."
	:x                      344
	:y                      475
	:width                  60
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "yellow"
	:label                  "Modify"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         modify_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 modify_button)
		:when                   (Notify )
		:to                     (window1 modify_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (modify_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   delete_button
	:owner                  status_controls
	:help                   "Delete:		Press this button to cancel and 
		delete the job selected in the 
		Fax Queue list.

Note:  This action cannot be undone!"
	:x                      454
	:y                      475
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "firebrick"
	:label                  "Delete"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         delete_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window1 delete_button)
		:when                   (Notify )
		:to                     (window1 delete_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (delete_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   job_queue_right_buttons
	:x                      344
	:y                      475
	:group-type             :row
	:members                (modify_button delete_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     50
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
	:type                   :group
	:name                   job_queue_buttons
	:x                      89
	:y                      475
	:group-type             :row
	:members                (job_queue_left_buttons job_queue_right_buttons )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :labels
	:horizontal-spacing     100
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
	:type                   :group
	:name                   job_queue_objects
	:x                      35
	:y                      132
	:group-type             :column
	:members                (jobs_list job_queue_buttons )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
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
	:type                   :setting
	:name                   status_send_mode
	:owner                  status_controls
	:help                   "Reschedule:	This setting determines if the 
		selected job will have its 
		scheduled transmit time adjusted.

Select Now to cause the job to be scheduled immediately (\"Pushed\").

Select Specify to use the Time To Send options to provide an exact send time.

(Optional)

(Default - Nothing Selected - No Change)"
	:x                      138
	:y                      524
	:width                  193
	:height                 23
	:value-x                228
	:value-y                524
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     nil
	:label                  "Reschedule:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Now" "Specify" )
	:choice-label-types     (:string :string )
	:choice-colors          ("White" "white" )
	:initial-selections     (nil nil )
	:initial-state          :inactive
	:user-data              ()
	:actions                (
		(
		:from                   (window1 status_send_mode "Now")
		:when                   (Notify )
		:to                     (window1 status_send_time_objects)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (window1 status_send_mode "Specify")
		:when                   (Notify )
		:to                     (window1 status_send_time_objects)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (window1 status_send_mode "Specify")
		:when                   (Deselect )
		:to                     (window1 status_send_time_objects)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
	)
)
(
	:type                   :setting
	:name                   status_send_time_unit
	:owner                  status_controls
	:help                   "Time To Send:	This option determines when the 
		fax is sent.  To reschedule a 
		fax for future transmission, 
		enter the desired delay and 
		select Minutes or Hours.

Note: This choice has no effect unless 
      Reschedule: Specify is selected.

(Optional)

(Default - 0 Minutes - Send Immediately)"
	:x                      337
	:y                      557
	:width                  85
	:height                 23
	:value-x                337
	:value-y                557
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :stack
	:selection-required     t
	:label                  ""
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Minutes" "Hours" )
	:choice-label-types     (:string :string )
	:choice-colors          ("white" "white" )
	:choice-defaults        (nil nil )
	:initial-selections     (nil nil )
	:initial-state          :inactive
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   status_send
	:owner                  status_controls
	:help                   "Time To Send:	This option determines when the 
		fax is sent.  To reschedule a 
		fax for future transmission, 
		enter the desired delay and 
		select Minutes or Hours.

Note: This choice has no effect unless 
      Reschedule: Specify is selected.

(Optional)

(Default - 0 Minutes - Send Immediately)"
	:x                      124
	:y                      561
	:width                  136
	:height                 15
	:value-x                228
	:value-y                561
	:value-length           4
	:stored-length          4
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Time To Send:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       nil
	:initial-value          "Now"
	:initial-state          :inactive
	:read-only              t
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   status_send_time
	:owner                  status_controls
	:help                   "Time To Send:	This option determines when the 
		fax is sent.  To reschedule a 
		fax for future transmission, 
		enter the desired delay and 
		select Minutes or Hours.

Note: This choice has no effect unless 
      Reschedule: Specify is selected.

(Optional)

(Default - 0 Minutes - Send Immediately)"
	:x                      265
	:y                      561
	:width                  67
	:height                 15
	:value-x                281
	:value-y                561
	:value-length           2
	:stored-length          2
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "+"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              99
	:min-value              0
	:initial-value          0
	:initial-state          :inactive
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   status_send_time_objects
	:x                      124
	:y                      557
	:group-type             :row
	:members                (status_send status_send_time status_send_time_unit )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     5
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
	:type                   :group
	:name                   reschedule_objects
	:x                      124
	:y                      524
	:group-type             :column
	:members                (status_send_mode status_send_time_objects )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :setting
	:name                   status_notify
	:owner                  status_controls
	:help                   "Notify Sender:	This setting determines if and 
		when the sender is notified, via 
		email, of changes to the status 
		of a scheduled fax transmission.

Select None to change the current notification to no notification.

Select On Delivery to change the current notification to notification when the fax has been successfully sent.

Select On Retry to change the current notification to notification each time the fax transmission fails and has to be rescheduled.

Select Both to change the current notification to both Delivery and Resend notification.

(Optional)

(Default - Nothing Selected - No Change)"
	:x                      124
	:y                      590
	:width                  352
	:height                 23
	:value-x                228
	:value-y                590
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     nil
	:label                  "Notify Sender:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("None" "On Delivery" "On Retry" "Both" )
	:choice-label-types     (:string :string :string :string )
	:choice-colors          ("white" "white" "yellow" "yellow" )
	:initial-selections     (nil nil nil nil )
	:initial-state          :inactive
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   status_lower_objects
	:x                      124
	:y                      524
	:group-type             :column
	:members                (reschedule_objects status_notify )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :group
	:name                   status_objects
	:x                      35
	:y                      29
	:group-type             :column
	:members                (query_objects job_queue_objects status_lower_objects )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
	:horizontal-spacing     10
	:vertical-spacing       30
	:anchor-object          status_controls
	:anchor-point           :center
	:reference-point        :center
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :stack
	:name                   layered_pane1
	:owner                  window1
	:members                (send_controls status_controls )
	:user-data              ()
)
(
	:type                   :popup-window
	:name                   directory_popup
	:owner                  window1
	:width                  508
	:height                 638
	:background-color       ""
	:foreground-color       ""
	:label                  "Fax Tool -  Directory"
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
		:from                   (directory_popup)
		:when                   (Resize )
		:to                     (directory_popup)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (directory_popup_resize)
		)
	)
)
(
	:type                   :control-area
	:name                   directory_controls
	:owner                  directory_popup
	:help                   "This panel allows you to create and maintain 
your own personal Fax \"Phonebook\".

To use this panel:


To Add Entries
--------------

1) Enter the desired information in the Name, 
   Company and Fax Number fields on this panel 
   and press Add.

or,

2) Enter the desired information on the main 
   panel and press Add To Directory.


To Change An Entry
------------------

1) Select the desired entry from the list.

2) Make the desired changes to the Name, Company 
   and/or Fax Number fields.

3) Press Change.


To Delete An Entry
------------------

1) Select the desired entry from the list.

2) Press Delete.


To Save Your Changes
--------------------

1) Press Save.

2) The entire list will be saved to ~/.faxtoolrc.


To Select An Entry
------------------

1) Select the desired entry from the list.

2) Press Select.

3) The fields on the main panel will be set
   using the selected entry.

"
	:x                      0
	:y                      0
	:width                  508
	:height                 638
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :scrolling-list
	:name                   directory_list
	:owner                  directory_controls
	:help                   "Directory List:	This is your personal Fax 
		Directory.

To use it, select an entry and press Select to 
copy its values to the main panel.

See the online help for this panel for detailed 
examples of how to use this popup."
	:x                      32
	:y                      31
	:width                  425
	:height                 404
	:value-x                32
	:value-y                31
	:rows                   20
	:foreground-color       ""
	:label                  ""
	:title                  "Name                     Company                  Fax Number                    "
	:label-type             :string
	:layout-type            :horizontal
	:read-only              t
	:multiple-selections    nil
	:selection-required     nil
	:initial-state          :active
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:notify-handler         directory_list_notify
	:event-handler          nil
	:initial-list-values    ()
	:initial-list-glyphs    ()
	:initial-selections     ()
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup directory_list)
		:when                   (Notify )
		:to                     (directory_popup directory_list)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (directory_list_notify)
		)
	)
)
(
	:type                   :button
	:name                   dir_add_button
	:owner                  directory_controls
	:help                   "Add Button:	Press to add the contents of the 
		Name, Company and Fax Number
		fields to the scrolling list."
	:x                      123
	:y                      445
	:width                  42
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "green"
	:label                  "Add"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         dir_add_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_add_button)
		:when                   (Notify )
		:to                     (directory_popup dir_add_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_add_notify)
		)
	)
)
(
	:type                   :button
	:name                   dir_change_button
	:owner                  directory_controls
	:help                   "Change Button:	Press to change the contents of 
		the selected entry to the values 
		in the Name, Company and Fax 
		Number fields."
	:x                      215
	:y                      445
	:width                  64
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Yellow"
	:label                  "Change"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         dir_change_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_change_button)
		:when                   (Notify )
		:to                     (directory_popup dir_change_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_change_notify)
		)
	)
)
(
	:type                   :button
	:name                   dir_delete_button
	:owner                  directory_controls
	:help                   "Delete Button:	Press to delete the selected 
		entry from the scrolling list."
	:x                      329
	:y                      445
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Firebrick"
	:label                  "Delete"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         dir_delete_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_delete_button)
		:when                   (Notify )
		:to                     (directory_popup dir_delete_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_delete_notify)
		)
	)
)
(
	:type                   :group
	:name                   dir_buttons
	:x                      123
	:y                      445
	:group-type             :row
	:members                (dir_add_button dir_change_button dir_delete_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :labels
	:horizontal-spacing     50
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
	:type                   :group
	:name                   dir_list_objects
	:x                      32
	:y                      31
	:group-type             :column
	:members                (directory_list dir_buttons )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
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
	:type                   :text-field
	:name                   dir_name
	:owner                  directory_controls
	:help                   "Name:	The name of the receipient.

(Mandatory if Company is blank)

(Maximum Length - 25 characters)"
	:x                      150
	:y                      494
	:width                  250
	:height                 15
	:value-x                200
	:value-y                494
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Name:"
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
	:name                   dir_company
	:owner                  directory_controls
	:help                   "Company:	The name of the receipient's 
		company.

(Mandatory if Name is blank)

(Maximum Length - 25 characters)"
	:x                      125
	:y                      519
	:width                  275
	:height                 15
	:value-x                200
	:value-y                519
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Company:"
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
	:name                   dir_fax
	:owner                  directory_controls
	:help                   "Fax Number:	The phone number of the 
		receipient's fax machine.

(Mandatory)

(Maximum Length - 25 characters)"
	:x                      108
	:y                      544
	:width                  292
	:height                 15
	:value-x                200
	:value-y                544
	:value-length           25
	:stored-length          25
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Fax Number:"
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
	:type                   :group
	:name                   dir_fields
	:x                      108
	:y                      494
	:group-type             :column
	:members                (dir_name dir_company dir_fax )
	:rows                   0
	:row-alignment          :top-edges
	:columns                1
	:column-alignment       :labels
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
	:type                   :button
	:name                   dir_select_button
	:owner                  directory_controls
	:help                   "Select Button:	Press to copy the values from 
		the current selection to the 
		Fax Number, Name, and Company 
		fields on the main panel."
	:x                      41
	:y                      589
	:width                  54
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Green"
	:label                  "Select"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         dir_select_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_select_button)
		:when                   (Notify )
		:to                     (directory_popup dir_select_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_select_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   dir_save_button
	:owner                  directory_controls
	:help                   "Save Button:	Press to save the entire list to 
		~/.faxtoolrc."
	:x                      145
	:y                      589
	:width                  46
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Green"
	:label                  "Save"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         dir_save_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_save_button)
		:when                   (Notify )
		:to                     (directory_popup dir_save_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_save_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   dir_lower_left_buttons
	:x                      41
	:y                      589
	:group-type             :row
	:members                (dir_select_button dir_save_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     50
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
	:type                   :button
	:name                   dir_clear_button
	:owner                  directory_controls
	:help                   "Clear Button:	Press to clear the Name, Company 
		and Fax Number fields."
	:x                      316
	:y                      589
	:width                  50
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "firebrick"
	:label                  "Clear"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         dir_clear_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_clear_button)
		:when                   (Notify )
		:to                     (directory_popup dir_clear_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_clear_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   dir_reset_button
	:owner                  directory_controls
	:help                   "Reset Button:	Press to reset the Name, Company 
		and Fax Number fields to the 
		values prior to the last Clear, 
		Add, Change, or Delete."
	:x                      416
	:y                      589
	:width                  51
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "yellow"
	:label                  "Reset"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         dir_reset_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (directory_popup dir_reset_button)
		:when                   (Notify )
		:to                     (directory_popup dir_reset_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_reset_button_notify)
		)
	)
)
(
	:type                   :group
	:name                   dir_lower_right_buttons
	:x                      316
	:y                      589
	:group-type             :row
	:members                (dir_clear_button dir_reset_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     50
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
	:type                   :group
	:name                   dir_lower_buttons
	:x                      41
	:y                      589
	:group-type             :row
	:members                (dir_lower_left_buttons dir_lower_right_buttons )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :vertical-centers
	:horizontal-spacing     125
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
	:type                   :group
	:name                   dir_objects
	:x                      32
	:y                      31
	:group-type             :column
	:members                (dir_list_objects dir_fields dir_lower_buttons )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
	:horizontal-spacing     10
	:vertical-spacing       30
	:anchor-object          directory_controls
	:anchor-point           :center
	:reference-point        :center
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   prop_popup
	:owner                  window1
	:width                  402
	:height                 230
	:background-color       ""
	:foreground-color       ""
	:label                  "Fax Tool - Properties"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              nil
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   prop_controls
	:owner                  prop_popup
	:help                   "This panel is used to set defaults and options 
for FaxTool. All fields are mandatory.  See help 
for the individual fields for more information."
	:x                      0
	:y                      0
	:width                  402
	:height                 230
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   fax_host
	:owner                  prop_controls
	:help                   "Fax Host:	The name of the machine which is 
		the Fax Server (running faxd) 
		for your site.

(Mandatory)

(Maximum Length - 40 characters)

(Default - localhost)

"
	:x                      115
	:y                      23
	:width                  148
	:height                 15
	:value-x                183
	:value-y                23
	:value-length           10
	:stored-length          40
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Fax Host:"
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
	:name                   fax_dir
	:owner                  prop_controls
	:help                   "Fax Spooling Directory:	The path on Fax Host 
			used for fax spooling.

(Mandatory)

(Maximum Length - 80 characters)

(Default: /var/spool/fax)"
	:x                      19
	:y                      48
	:width                  364
	:height                 15
	:value-x                183
	:value-y                48
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Fax Spooling Directory:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "/var/spool/fax"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   prop_upper_fields
	:x                      19
	:y                      23
	:group-type             :column
	:members                (fax_host fax_dir )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :text-field
	:name                   view_cmd
	:owner                  prop_controls
	:help                   "View Filter:	Enter the name of the program to 
		be run when the View Button is 
		pressed.

Note: The command specified here is invoked using:
 
	\"fax2ps SpoolDirectory/recvq/SelectedEntry
 	2> /dev/null 1> TemporaryFile; 
	YourViewFilter TemporaryFile; 
	rm -f TemporaryFile\"

(Mandatory)

(Maximum Length - 80 characters)

(Default - pageview)"
	:x                      70
	:y                      93
	:width                  284
	:height                 15
	:value-x                154
	:value-y                93
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "View Filter:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "pageview"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   print_cmd
	:owner                  prop_controls
	:help                   "Print Filter:	Enter the name of the program to 
		be run when the Print Button is 
		pressed.

Note: The command specified here is invoked using:

	\"fax2ps SpoolDirectory/recvq/SelectedEntry
 	2> /dev/null | YourViewFilter\"

(Mandatory)

(Maximum Length - 80 characters)

(Default - pl)"
	:x                      70
	:y                      118
	:width                  284
	:height                 15
	:value-x                154
	:value-y                118
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Print Filter:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "pl"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   preview_cmd
	:owner                  prop_controls
	:help                   "Preview Filter:	Enter the name of the program to 
		be run when the Preview Button is 
		pressed.

Note: The command specified here is invoked using:
 
	\"YourPreviewFilter FilesInTransmitList\"

(Mandatory)

(Maximum Length - 80 characters)

(Default - cmdtool gs -q)"
	:x                      49
	:y                      143
	:width                  305
	:height                 15
	:value-x                154
	:value-y                143
	:value-length           25
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Preview Filter:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "cmdtool gs -q"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   prop_lower_fields
	:x                      49
	:y                      93
	:group-type             :column
	:members                (view_cmd print_cmd preview_cmd )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :labels
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
	:type                   :button
	:name                   prop_save_button
	:owner                  prop_controls
	:help                   "Save Button:	Press to apply your changes to 
		the current FaxTool and save 
		them to ~/.faxtoolrc."
	:x                      21
	:y                      188
	:width                  46
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "Green"
	:label                  "Save"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         dir_save_button_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (prop_popup prop_save_button)
		:when                   (Notify )
		:to                     (prop_popup prop_save_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (dir_save_button_notify)
		)
	)
)
(
	:type                   :button
	:name                   prop_apply_button
	:owner                  prop_controls
	:help                   "Apply Button:	Press to apply your changes to 
		the current FaxTool."
	:x                      97
	:y                      188
	:width                  53
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "green"
	:label                  "Apply"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         prop_apply_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (prop_popup prop_apply_button)
		:when                   (Notify )
		:to                     (prop_popup prop_apply_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (prop_apply_notify)
		)
	)
)
(
	:type                   :group
	:name                   prop_left_buttons
	:x                      21
	:y                      188
	:group-type             :row
	:members                (prop_save_button prop_apply_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :labels
	:horizontal-spacing     30
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
	:type                   :button
	:name                   prop_clear_button
	:owner                  prop_controls
	:help                   "Clear Button:	Press to clear all fields on the 
		Properties popup."
	:x                      250
	:y                      188
	:width                  50
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "firebrick"
	:label                  "Clear"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         prop_clear_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (prop_popup prop_clear_button)
		:when                   (Notify )
		:to                     (prop_popup prop_clear_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (prop_clear_notify)
		)
	)
)
(
	:type                   :button
	:name                   prop_reset_button
	:owner                  prop_controls
	:help                   "Reset Button:	Press to reset all fields on the 
		Properties popup to the values 
		last Applied or Saved."
	:x                      330
	:y                      188
	:width                  51
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       "yellow"
	:label                  "Reset"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         prop_reset_notify
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (prop_popup prop_reset_button)
		:when                   (Notify )
		:to                     (prop_popup prop_reset_button)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (prop_reset_notify)
		)
	)
)
(
	:type                   :group
	:name                   prop_right_buttons
	:x                      250
	:y                      188
	:group-type             :row
	:members                (prop_clear_button prop_reset_button )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :labels
	:horizontal-spacing     30
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
	:type                   :group
	:name                   prop_lower_buttons
	:x                      21
	:y                      188
	:group-type             :row
	:members                (prop_left_buttons prop_right_buttons )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
	:column-alignment       :labels
	:horizontal-spacing     100
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
	:type                   :group
	:name                   prop_objects
	:x                      19
	:y                      23
	:group-type             :column
	:members                (prop_upper_fields prop_lower_fields prop_lower_buttons )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
	:column-alignment       :vertical-centers
	:horizontal-spacing     10
	:vertical-spacing       30
	:anchor-object          prop_controls
	:anchor-point           :center
	:reference-point        :center
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
)
