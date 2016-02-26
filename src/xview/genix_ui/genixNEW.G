;GIL-3
(
(
	:type                   :menu
	:name                   fileMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             "File"
	:menu-item-labels       ("Save" "Load" "Exit" )
	:menu-item-label-types  (:string :string :string )
	:menu-item-states       (:active :active :active )
	:menu-item-defaults     (nil nil nil )
	:initial-selections     (nil nil nil )
	:menu-item-handlers     (globalSave globalLoad globalExit )
	:menu-item-menus        (nil nil nil )
	:menu-item-colors       ("" "" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (fileMenu "Save")
		:when                   (Notify )
		:to                     (fileMenu "Save")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (globalSave)
		)
		(
		:from                   (fileMenu "Load")
		:when                   (Notify )
		:to                     (fileMenu "Load")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (globalLoad)
		)
		(
		:from                   (fileMenu "Exit")
		:when                   (Notify )
		:to                     (fileMenu "Exit")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (globalExit)
		)
	)
)
(
	:type                   :menu
	:name                   viewMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             "View"
	:menu-item-labels       ("People" "Tree" )
	:menu-item-label-types  (:string :string )
	:menu-item-states       (:active :active )
	:menu-item-defaults     (nil nil )
	:initial-selections     (nil nil )
	:menu-item-handlers     (nil nil )
	:menu-item-menus        (nil nil )
	:menu-item-colors       ("" "" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (viewMenu)
		:when                   (Notify )
		:to                     (relationsWindow)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
		(
		:from                   (viewMenu "Tree")
		:when                   (Notify )
		:to                     (treeWindow)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :menu
	:name                   editMenu
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Relations" )
	:menu-item-label-types  (:string )
	:menu-item-states       (:active )
	:menu-item-defaults     (nil )
	:initial-selections     (nil )
	:menu-item-handlers     (nil )
	:menu-item-menus        (nil )
	:menu-item-colors       ("" )
	:pinnable               t
	:user-data              ()
	:actions                (
		(
		:from                   (editMenu "Relations")
		:when                   (Notify )
		:to                     (relationsWindow)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :base-window
	:name                   rootsWindow
	:owner                  nil
	:width                  605
	:height                 434
	:background-color       ""
	:foreground-color       ""
	:label                  "GenIX -- Genealogy for UNIX and X"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              t
	:icon-file              "genix.icon"
	:icon-label             ""
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   menubarPanel
	:owner                  rootsWindow
	:help                   ""
	:x                      0
	:y                      0
	:width                  605
	:height                 30
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
	:owner                  menubarPanel
	:help                   "This menu allows you to save your work, load a
new file, and exit the program."
	:x                      24
	:y                      8
	:width                  40
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "File"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   viewButton
	:owner                  menubarPanel
	:help                   "This menu controls what parts of the information
structure will be visible: the optional relations
and tree windows."
	:x                      88
	:y                      8
	:width                  62
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "View"
	:label-type             :string
	:initial-state          :active
	:menu                   viewMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   editButton
	:owner                  menubarPanel
	:help                   ""
	:x                      160
	:y                      8
	:width                  56
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Edit"
	:label-type             :string
	:initial-state          :active
	:menu                   editMenu
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   locateButton
	:owner                  menubarPanel
	:help                   "Click this button to locate a person by name."
	:x                      226
	:y                      8
	:width                  65
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Locate..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (rootsWindow locateButton)
		:when                   (Notify )
		:to                     (locateWindow)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :control-area
	:name                   persPanel
	:owner                  rootsWindow
	:help                   ""
	:x                      0
	:y                      30
	:width                  605
	:height                 404
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
	:type                   :message
	:name                   persTitle
	:owner                  persPanel
	:help                   ""
	:x                      224
	:y                      32
	:width                  146
	:height                 13
	:foreground-color       ""
	:label                  "Personal Information"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   surnameTF
	:owner                  persPanel
	:help                   "Enter the person's surname."
	:x                      48
	:y                      104
	:width                  232
	:height                 15
	:value-x                120
	:value-y                104
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Surname:"
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
	:name                   gnameTF
	:owner                  persPanel
	:help                   "Enter the person's given (first) name and any
known middle names or initials."
	:x                      9
	:y                      128
	:width                  271
	:height                 15
	:value-x                120
	:value-y                128
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Given Name(s):"
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
	:name                   bdateTF
	:owner                  persPanel
	:help                   ""
	:x                      40
	:y                      176
	:width                  240
	:height                 15
	:value-x                120
	:value-y                176
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Birth Date:"
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
	:name                   bplaceTF
	:owner                  persPanel
	:help                   ""
	:x                      35
	:y                      200
	:width                  245
	:height                 15
	:value-x                120
	:value-y                200
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Birth Place:"
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
	:name                   ddateTF
	:owner                  persPanel
	:help                   ""
	:x                      18
	:y                      232
	:width                  264
	:height                 15
	:value-x                122
	:value-y                232
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Date of Death:"
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
	:name                   dplaceTF
	:owner                  persPanel
	:help                   ""
	:x                      11
	:y                      256
	:width                  269
	:height                 15
	:value-x                120
	:value-y                256
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Place of Death:"
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
	:name                   sexChoice
	:owner                  persPanel
	:help                   ""
	:x                      88
	:y                      352
	:width                  139
	:height                 23
	:value-x                122
	:value-y                352
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Sex:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Male" "Female" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   applyButton
	:owner                  persPanel
	:help                   ""
	:x                      344
	:y                      352
	:width                  133
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply these values"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         personApply
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (rootsWindow applyButton)
		:when                   (Notify )
		:to                     (rootsWindow applyButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (personApply)
		)
	)
)
(
	:type                   :popup-window
	:name                   relationsWindow
	:owner                  rootsWindow
	:width                  550
	:height                 522
	:background-color       ""
	:foreground-color       ""
	:label                  "Relationships"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  relationsWindow
	:help                   ""
	:x                      0
	:y                      0
	:width                  550
	:height                 522
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
	:name                   unionTitle
	:owner                  controls1
	:help                   "This read-only field will display the identifier
for this union, if an existing union (marriage)
is known.
"
	:x                      200
	:y                      24
	:width                  174
	:height                 13
	:foreground-color       ""
	:label                  "Relationship Information"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :scrolling-list
	:name                   leftList
	:owner                  controls1
	:help                   ""
	:x                      8
	:y                      56
	:width                  200
	:height                 380
	:value-x                8
	:value-y                56
	:rows                   20
	:foreground-color       ""
	:label                  ""
	:title                  ""
	:label-type             :string
	:layout-type            :horizontal
	:read-only              t
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
	:type                   :scrolling-list
	:name                   rightList
	:owner                  controls1
	:help                   ""
	:x                      378
	:y                      56
	:width                  200
	:height                 380
	:value-x                378
	:value-y                56
	:rows                   20
	:foreground-color       ""
	:label                  ""
	:title                  ""
	:label-type             :string
	:layout-type            :horizontal
	:read-only              t
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
	:type                   :setting
	:name                   relChoice
	:owner                  controls1
	:help                   ""
	:x                      237
	:y                      224
	:width                  91
	:height                 132
	:value-x                237
	:value-y                241
	:rows                   0
	:columns                1
	:layout-type            :vertical
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "<- IS ... OF ->"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("(No relation)" "Mother" "Father" "Spouse" "Child" )
	:choice-label-types     (:string :string :string :string :string )
	:choice-colors          ("" "" "" "" "" )
	:initial-selections     (nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   relationsListGroup
	:x                      8
	:y                      56
	:group-type             :row
	:members                (leftList relChoice rightList )
	:rows                   1
	:row-alignment          :horizontal-centers
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
	:type                   :text-field
	:name                   unionidTF
	:owner                  controls1
	:help                   ""
	:x                      167
	:y                      456
	:width                  135
	:height                 15
	:value-x                238
	:value-y                456
	:value-length           8
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Union ID:"
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
	:name                   unionapplyButton
	:owner                  controls1
	:help                   ""
	:x                      345
	:y                      456
	:width                  53
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Apply"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   relationsButtonGroup
	:x                      167
	:y                      456
	:group-type             nil
	:members                (unionidTF unionapplyButton )
	:rows                   0
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :left-edges
	:horizontal-spacing     10
	:vertical-spacing       10
	:anchor-object          relationsListGroup
	:anchor-point           :south
	:reference-point        :north
	:horizontal-offset      0
	:vertical-offset        20
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   treeWindow
	:owner                  rootsWindow
	:width                  238
	:height                 101
	:background-color       ""
	:foreground-color       ""
	:label                  "Family Tree Display"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls2
	:owner                  treeWindow
	:help                   ""
	:x                      0
	:y                      0
	:width                  238
	:height                 101
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
	:name                   message1
	:owner                  controls2
	:help                   ""
	:x                      8
	:y                      40
	:width                  219
	:height                 13
	:foreground-color       ""
	:label                  "Draw function not available yet."
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   locateWindow
	:owner                  rootsWindow
	:width                  400
	:height                 150
	:background-color       ""
	:foreground-color       ""
	:label                  "Locate"
	:label-type             :string
	:initial-state          :invisible
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
)
