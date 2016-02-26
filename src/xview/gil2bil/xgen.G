;GIL-3
(
(
	:type                   :base-window
	:name                   baseFrame
	:owner                  nil
	:width                  679
	:height                 702
	:background-color       ""
	:foreground-color       ""
	:label                  "SoftQuad Licensing: Client Site Information"
	:label-type             :string
	:initial-state          :open
	:show-footer            t
	:resizable              nil
	:icon-file              "xgenlice.icon"
	:icon-label             "XGenLicense"
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   clPanel
	:owner                  baseFrame
	:help                   ""
	:x                      0
	:y                      0
	:width                  679
	:height                 702
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
	:name                   quitButton
	:owner                  clPanel
	:help                   "This button completely terminates the License Generator program."
	:x                      32
	:y                      16
	:width                  53
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Quit"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         quit
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame quitButton)
		:when                   (Notify )
		:to                     (baseFrame quitButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (quit)
		)
	)
)
(
	:type                   :button
	:name                   aboutButton
	:owner                  clPanel
	:help                   "This button displays a small window with program and version number information."
	:x                      528
	:y                      16
	:width                  73
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "About..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame aboutButton)
		:when                   (Notify )
		:to                     (aboutWindow)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :scrolling-list
	:name                   clListList
	:owner                  clPanel
	:help                   ""
	:x                      56
	:y                      48
	:width                  400
	:height                 460
	:value-x                56
	:value-y                48
	:rows                   22
	:foreground-color       ""
	:label                  ""
	:title                  ""
	:label-type             :string
	:layout-type            :horizontal
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
	:actions                ()
)
(
	:type                   :button
	:name                   clListNewLoadButton
	:owner                  clPanel
	:help                   ""
	:x                      176
	:y                      512
	:width                  149
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Re-use Client Info"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         doLoadClFromList
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame clListNewLoadButton)
		:when                   (Notify )
		:to                     (baseFrame clListNewLoadButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doLoadClFromList)
		)
	)
)
(
	:type                   :text-field
	:name                   clCompany
	:owner                  clPanel
	:help                   ""
	:x                      26
	:y                      544
	:width                  309
	:height                 17
	:value-x                111
	:value-y                544
	:value-length           28
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
	:name                   clContact
	:owner                  clPanel
	:help                   ""
	:x                      338
	:y                      544
	:width                  283
	:height                 17
	:value-x                461
	:value-y                544
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Contact Name:"
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
	:name                   clPhone
	:owner                  clPanel
	:help                   ""
	:x                      26
	:y                      584
	:width                  247
	:height                 17
	:value-x                153
	:value-y                584
	:value-length           15
	:stored-length          30
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Phone Number:"
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
	:name                   clFAXnum
	:owner                  clPanel
	:help                   ""
	:x                      330
	:y                      584
	:width                  232
	:height                 17
	:value-x                442
	:value-y                584
	:value-length           15
	:stored-length          30
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "FAX Number: "
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
	:name                   clEmail
	:owner                  clPanel
	:help                   ""
	:x                      26
	:y                      624
	:width                  247
	:height                 17
	:value-x                153
	:value-y                624
	:value-length           15
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "E-Mail Address"
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
	:name                   contactFeatGroup
	:x                      26
	:y                      544
	:group-type             nil
	:members                (clCompany clContact clPhone clFAXnum clEmail )
	:rows                   0
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
	:type                   :button
	:name                   clientOKbutton
	:owner                  clPanel
	:help                   ""
	:x                      248
	:y                      672
	:width                  179
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "OK; now make licenses"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (baseFrame clientOKbutton)
		:when                   (Notify )
		:to                     (baseFrame)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doClientInfo)
		)
		(
		:from                   (baseFrame clientOKbutton)
		:when                   (Notify )
		:to                     (licePopup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :popup-window
	:name                   licePopup
	:owner                  baseFrame
	:width                  743
	:height                 520
	:background-color       ""
	:foreground-color       ""
	:label                  "SoftQuad Licensing: Generate Licenses"
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
	:name                   controls2
	:owner                  licePopup
	:help                   ""
	:x                      0
	:y                      0
	:width                  743
	:height                 520
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
	:type                   :setting
	:name                   ed_lftype
	:owner                  controls2
	:help                   ""
	:x                      30
	:y                      50
	:width                  504
	:height                 25
	:value-x                174
	:value-y                50
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "License File Mode"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("UNIX" "PC - Old Style" "MS-Win/Mac - New Style" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup ed_lftype "MS-Win/Mac - New Style")
		:when                   (Notify )
		:to                     (licePopup ed_HostnameText)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_lftype "PC - Old Style")
		:when                   (Notify )
		:to                     (licePopup ed_HostnameText)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_lftype "UNIX")
		:when                   (Notify )
		:to                     (licePopup ed_HostnameText)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (licePopup ed_lftype "MS-Win/Mac - New Style")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	lf_mode = lf_suo;")
		)
		(
		:from                   (licePopup ed_lftype "PC - Old Style")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_lftype "PC - Old Style")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	lf_mode = lf_oldpc;")
		)
		(
		:from                   (licePopup ed_lftype "UNIX")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (licePopup ed_lftype "UNIX")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	lf_mode = lf_unix;")
		)
		(
		:from                   (licePopup ed_lftype "MS-Win/Mac - New Style")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
	)
)
(
	:type                   :setting
	:name                   ed_nodality
	:owner                  controls2
	:help                   ""
	:x                      30
	:y                      83
	:width                  205
	:height                 25
	:value-x                107
	:value-y                83
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Nodality:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Floating" "Fixed" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup ed_nodality "Floating")
		:when                   (Notify )
		:to                     (licePopup ed_HostnameText)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (licePopup ed_nodality "Fixed")
		:when                   (Notify )
		:to                     (licePopup ed_HostnameText)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_nodality "Fixed")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_nodality "Floating")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (licePopup ed_nodality "Fixed")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	ltype = NODELICENSE;")
		)
		(
		:from                   (licePopup ed_nodality "Floating")
		:when                   (Notify )
		:to                     (licePopup ed_nusers)
		:function_type          ExecuteCode
		:arg_type               (:string)
		:action                 ("	ltype = FLOATLICENSE;")
		)
	)
)
(
	:type                   :setting
	:name                   ed_feat
	:owner                  controls2
	:help                   ""
	:x                      30
	:y                      116
	:width                  672
	:height                 25
	:value-x                99
	:value-y                116
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Feature"
	:label-type             :string
	:notify-handler         nil
	:event-handler          eventFeature
	:choices                ("A/E" "AETABLE" "AERULESB" "AE-RUN" "AE-SCRRUN" "AE-SCRDEV" "RB-RUN" "Other:" )
	:choice-label-types     (:string :string :string :string :string :string :string :string )
	:choice-colors          ("" "" "" "" "" "" "" "" )
	:initial-selections     (nil nil nil nil nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup ed_feat "AETABLE")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "A/E")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "AE-RUN")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "AE-SCRDEV")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "AE-SCRRUN")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "Other:")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
		(
		:from                   (licePopup ed_feat "AERULESB")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "RB-RUN")
		:when                   (Notify )
		:to                     (licePopup otherType)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_feat "AETABLE")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "1.0")
		)
		(
		:from                   (licePopup ed_feat "AE-RUN")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "3.0")
		)
		(
		:from                   (licePopup ed_feat "AE-SCRDEV")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "3.0")
		)
		(
		:from                   (licePopup ed_feat "AE-SCRRUN")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "3.0")
		)
		(
		:from                   (licePopup ed_feat "A/E")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "1.0")
		)
		(
		:from                   (licePopup ed_feat "Other:")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "???")
		)
		(
		:from                   (licePopup ed_feat "AERULESB")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "3.0")
		)
		(
		:from                   (licePopup ed_feat "RB-RUN")
		:when                   (Notify )
		:to                     (licePopup ed_versnum)
		:function_type          :user_defined
		:arg_type               (:string)
		:action                 (SetValueString "3.0")
		)
		(
		:from                   (licePopup ed_feat)
		:when                   (AnyEvent )
		:to                     (licePopup ed_feat)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (eventFeature)
		)
	)
)
(
	:type                   :text-field
	:name                   otherType
	:owner                  controls2
	:help                   ""
	:x                      638
	:y                      120
	:width                  80
	:height                 17
	:value-x                638
	:value-y                120
	:value-length           10
	:stored-length          40
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  ""
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :inactive
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   ed_featGroup
	:x                      30
	:y                      116
	:group-type             :row
	:members                (ed_feat otherType )
	:rows                   1
	:row-alignment          :horizontal-centers
	:columns                0
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
	:name                   ed_topGroup
	:x                      30
	:y                      50
	:group-type             :column
	:members                (ed_lftype ed_nodality ed_featGroup )
	:rows                   0
	:row-alignment          :horizontal-centers
	:columns                1
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
	:name                   ed_HostnameText
	:owner                  controls2
	:help                   ""
	:x                      50
	:y                      210
	:width                  272
	:height                 17
	:value-x                162
	:value-y                210
	:value-length           20
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Server Name:"
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
	:name                   ed_HostidText
	:owner                  controls2
	:help                   ""
	:x                      391
	:y                      210
	:width                  187
	:height                 17
	:value-x                458
	:value-y                210
	:value-length           15
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "HostID:"
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
	:name                   ed_versnum
	:owner                  controls2
	:help                   ""
	:x                      50
	:y                      245
	:width                  120
	:height                 17
	:value-x                118
	:value-y                245
	:value-length           4
	:stored-length          10
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Version"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          "1.0"
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   ed_pwText
	:owner                  controls2
	:help                   ""
	:x                      391
	:y                      245
	:width                  215
	:height                 17
	:value-x                478
	:value-y                245
	:value-length           16
	:stored-length          16
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Password:"
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
	:type                   :setting
	:name                   ed_isNoExpdt
	:owner                  controls2
	:help                   ""
	:x                      50
	:y                      280
	:width                  233
	:height                 25
	:value-x                132
	:value-y                280
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Duration:"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Perpetual" "Expires" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup ed_isNoExpdt "Perpetual")
		:when                   (Notify )
		:to                     (licePopup ed_expdate)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Disable)
		)
		(
		:from                   (licePopup ed_isNoExpdt "Expires")
		:when                   (Notify )
		:to                     (licePopup ed_expdate)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Enable)
		)
	)
)
(
	:type                   :text-field
	:name                   ed_expdate
	:owner                  controls2
	:help                   ""
	:x                      391
	:y                      280
	:width                  332
	:height                 17
	:value-x                627
	:value-y                280
	:value-length           12
	:stored-length          12
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Expiry Date (dd-mmm-yyyy):"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :inactive
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   ed_midGroup
	:x                      50
	:y                      210
	:group-type             nil
	:members                (ed_HostnameText ed_HostidText ed_versnum ed_pwText ed_isNoExpdt ed_expdate )
	:rows                   0
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
	:type                   :text-field
	:name                   ed_nusers
	:owner                  controls2
	:help                   ""
	:x                      40
	:y                      336
	:width                  222
	:height                 17
	:value-x                183
	:value-y                336
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Number of users:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              100
	:min-value              1
	:initial-value          1
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   genThisLiceButton
	:owner                  controls2
	:help                   ""
	:x                      50
	:y                      384
	:width                  172
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Generate this License"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         doLicense
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup genThisLiceButton)
		:when                   (Notify )
		:to                     (licePopup genThisLiceButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doLicense)
		)
	)
)
(
	:type                   :button
	:name                   liWriteLFButton
	:owner                  controls2
	:help                   ""
	:x                      330
	:y                      384
	:width                  170
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Save in \"./licensefile\""
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         WriteLF
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup liWriteLFButton)
		:when                   (Notify )
		:to                     (licePopup liWriteLFButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (WriteLF)
		)
	)
)
(
	:type                   :button
	:name                   liWriteAltLFButton
	:owner                  controls2
	:help                   ""
	:x                      487
	:y                      384
	:width                  83
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Save as..."
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         WriteAltLF
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup liWriteAltLFButton)
		:when                   (Notify )
		:to                     (licePopup liWriteAltLFButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (WriteAltLF)
		)
	)
)
(
	:type                   :text-field
	:name                   ed_nlice
	:owner                  controls2
	:help                   ""
	:x                      208
	:y                      388
	:width                  122
	:height                 17
	:value-x                290
	:value-y                388
	:value-length           3
	:stored-length          8
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Licenses: "
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
	:type                   :group
	:name                   genActGrp1
	:x                      50
	:y                      384
	:group-type             :row
	:members                (genThisLiceButton ed_nlice liWriteLFButton liWriteAltLFButton )
	:rows                   1
	:row-alignment          :bottom-edges
	:columns                0
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
	:name                   edLiceFileButton
	:owner                  controls2
	:help                   ""
	:x                      49
	:y                      413
	:width                  167
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "View/Edit licensefile"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         doXEditLiceFile
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup edLiceFileButton)
		:when                   (Notify )
		:to                     (licePopup edLiceFileButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doXEditLiceFile)
		)
		(
		:from                   (licePopup edLiceFileButton)
		:when                   (Notify )
		:to                     (xeditPopup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Show)
		)
	)
)
(
	:type                   :button
	:name                   mailButton
	:owner                  controls2
	:help                   ""
	:x                      203
	:y                      413
	:width                  132
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Mail Licensefile"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         doMail
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup mailButton)
		:when                   (Notify )
		:to                     (licePopup mailButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doMail)
		)
	)
)
(
	:type                   :button
	:name                   printLicefileButton
	:owner                  controls2
	:help                   ""
	:x                      325
	:y                      413
	:width                  137
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Print Licensefile"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         doPrint
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup printLicefileButton)
		:when                   (Notify )
		:to                     (licePopup printLicefileButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doPrint)
		)
	)
)
(
	:type                   :button
	:name                   faxButton
	:owner                  controls2
	:help                   ""
	:x                      451
	:y                      413
	:width                  128
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "FAX Licensefile"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         doFAX
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup faxButton)
		:when                   (Notify )
		:to                     (licePopup faxButton)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doFAX)
		)
	)
)
(
	:type                   :group
	:name                   genActGrp2
	:x                      49
	:y                      413
	:group-type             :row
	:members                (edLiceFileButton mailButton printLicefileButton faxButton )
	:rows                   1
	:row-alignment          :bottom-edges
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
	:type                   :button
	:name                   doneWithLiceFileButton
	:owner                  controls2
	:help                   ""
	:x                      208
	:y                      442
	:width                  220
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "All done with this licensefile"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (licePopup doneWithLiceFileButton)
		:when                   (Notify )
		:to                     (licePopup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Hide)
		)
		(
		:from                   (licePopup doneWithLiceFileButton)
		:when                   (Notify )
		:to                     (xeditPopup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Hide)
		)
		(
		:from                   (licePopup doneWithLiceFileButton)
		:when                   (Notify )
		:to                     (xeditPopup)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (doneWithThisLicefile)
		)
	)
)
(
	:type                   :group
	:name                   genActGroup
	:x                      41
	:y                      384
	:group-type             :column
	:members                (genActGrp1 genActGrp2 doneWithLiceFileButton )
	:rows                   0
	:row-alignment          :top-edges
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
	:type                   :popup-window
	:name                   xeditPopup
	:owner                  baseFrame
	:width                  623
	:height                 190
	:background-color       ""
	:foreground-color       ""
	:label                  "SoftQuad Licensing: View/Edit Current Licensefile"
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
	:type                   :text-pane
	:name                   editLiceFileTextsw
	:owner                  xeditPopup
	:help                   ""
	:x                      0
	:y                      0
	:width                  623
	:height                 150
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:read-only              nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls3
	:owner                  xeditPopup
	:help                   ""
	:x                      0
	:y                      150
	:width                  623
	:height                 40
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
	:name                   edLiceDoneButton
	:owner                  controls3
	:help                   ""
	:x                      280
	:y                      10
	:width                  58
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Done"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (xeditPopup edLiceDoneButton)
		:when                   (Notify )
		:to                     (xeditPopup)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Hide)
		)
	)
)
(
	:type                   :popup-window
	:name                   aboutWindow
	:owner                  baseFrame
	:width                  600
	:height                 180
	:background-color       ""
	:foreground-color       ""
	:label                  "About SoftQuad GenLicense"
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
	:type                   :text-pane
	:name                   aboutTextpane
	:owner                  aboutWindow
	:help                   ""
	:x                      0
	:y                      0
	:width                  600
	:height                 115
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            t
	:read-only              t
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  aboutWindow
	:help                   ""
	:x                      0
	:y                      115
	:width                  600
	:height                 65
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
	:name                   aboutOKButton
	:owner                  controls1
	:help                   "This button dismisses the About... dialog."
	:x                      264
	:y                      16
	:width                  42
	:height                 21
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "OK"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (aboutWindow aboutOKButton)
		:when                   (Notify )
		:to                     (aboutWindow)
		:function_type          :user_defined
		:arg_type               ()
		:action                 (Hide)
		)
	)
)
)
