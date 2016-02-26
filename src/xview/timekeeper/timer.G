;GIL-3
(
(
	:type                   :base-window
	:name                   baseFrame
	:owner                  nil
	:width                  450
	:height                 450
	:background-color       ""
	:foreground-color       ""
	:label                  "Time Usage Reporter"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  baseFrame
	:help                   ""
	:x                      0
	:y                      0
	:width                  450
	:height                 450
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
	:type                   :scrolling-list
	:name                   topicList
	:owner                  controls1
	:help                   ""
	:x                      16
	:y                      16
	:width                  430
	:height                 290
	:value-x                16
	:value-y                39
	:rows                   10
	:foreground-color       ""
	:label                  ""
	:title                  "Topic"
	:label-type             :string
	:layout-type            :vertical
	:read-only              nil
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
	:actions                (
		(
		:from                   (baseFrame topicList)
		:when                   (Notify )
		:to                     (baseFrame topicList)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :button
	:name                   startButton
	:owner                  controls1
	:help                   "Click this button to begin attributing your time to the selected Topic."
	:x                      16
	:y                      339
	:width                  55
	:height                 20
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Start"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         startTopic
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame startButton)
		:when                   (Notify )
		:to                     (baseFrame startButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (startTopic)
		)
	)
)
(
	:type                   :button
	:name                   stopButton
	:owner                  controls1
	:help                   "Click this button to end attributing your time to the selected topic."
	:x                      354
	:y                      339
	:width                  54
	:height                 20
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Stop"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         stopTopic
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame stopButton)
		:when                   (Notify )
		:to                     (baseFrame stopButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (stopTopic)
		)
	)
)
(
	:type                   :text-field
	:name                   newTextfield
	:owner                  controls1
	:help                   "To add a new topic to your list, type it in this text field and then click on ADD."
	:x                      16
	:y                      370
	:width                  328
	:height                 21
	:value-x                124
	:value-y                370
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "New Topic:"
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
	:name                   addButton
	:owner                  controls1
	:help                   "To add a new topic, type it in the \"New Topic\" text field and then click this button."
	:x                      354
	:y                      370
	:width                  52
	:height                 20
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Add"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame addButton)
		:when                   (Notify )
		:to                     (baseFrame topicList)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (addTopic)
		)
	)
)
(
	:type                   :button
	:name                   rereadButton
	:owner                  controls1
	:help                   ""
	:x                      16
	:y                      401
	:width                  145
	:height                 20
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Re-read Topics"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         rereadTopics
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame rereadButton)
		:when                   (Notify )
		:to                     (baseFrame rereadButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (rereadTopics)
		)
	)
)
(
	:type                   :button
	:name                   deleteButton
	:owner                  controls1
	:help                   "Click this button to DELETE the selected topic from your personal list of topics."
	:x                      354
	:y                      401
	:width                  69
	:height                 20
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Delete"
	:label-type             :string
	:initial-state          :inactive
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame deleteButton)
		:when                   (Notify )
		:to                     (baseFrame topicList)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (delTopic)
		)
	)
)
(
	:type                   :group
	:name                   group1
	:x                      16
	:y                      339
	:group-type             :row-column
	:members                (startButton stopButton newTextfield addButton rereadButton deleteButton )
	:rows                   3
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :left-edges
	:horizontal-spacing     10
	:vertical-spacing       10
	:anchor-object          topicList
	:anchor-point           :south-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        10
	:user-data              ()
	:actions                ()
)
)
