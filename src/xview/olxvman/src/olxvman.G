;GIL-2
(
(
	:type                   :menu
	:name                   OptionMenu
	:help                   ""
	:columns                1
	:label                  ""
	:label-type             :string
	:menu-type              :command
	:menu-handler           nil
	:menu-title             "Options"
	:menu-item-labels       ("Search..." "Show Version" "Print Current Page" "Quit" )
	:menu-item-label-types  (:string :string :string :string )
	:menu-item-defaults     (nil nil nil nil )
	:menu-item-handlers     (SearchHandler ShowVersionHandler PrintHandler QuitHandler )
	:menu-item-menus        (nil nil nil nil )
	:menu-item-colors       ("" "" "" "" )
	:pinnable               t
	:user-data              ()
)
(
	:type                   :base-window
	:name                   OLXVMan
	:owner                  nil
	:width                  456
	:height                 280
	:background-color       ""
	:foreground-color       ""
	:label                  "OLXV Man"
	:label-type             :string
	:mapped                 t
	:show-footer            t
	:resizable              t
	:icon-file              "olxvman.icon"
	:icon-mask-file         ""
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  OLXVMan
	:help                   ""
	:x                      0
	:y                      0
	:width                  456
	:height                 280
	:background-color       ""
	:foreground-color       ""
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   OptionButton
	:owner                  controls1
	:help                   "The Options available for OLXV Man are as follows:

Search - Brings up the search window which allows the manual pages to be searched for either a specific page, or pages with a specific topic.

Show Version - Displays the current version of the program.

Print Current Page - Prints the manual page that is in the current manual page display window. The current manual page display window is the window that is unpinned. Manual pages in a pinned display window cannot be printed.

Quit - Exits OLXV Man."
	:x                      8
	:y                      12
	:constant-width         nil
	:button-type            :normal
	:width                  80
	:height                 19
	:foreground-color       ""
	:label                  "Options"
	:label-type             :string
	:menu                   OptionMenu
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :scrolling-list
	:name                   ManualSections
	:owner                  controls1
	:help                   "The Manual Sections list provides a listing of the manual sections that are available. Selecting a manual section will cause the manual pages for the section to be displayed in the Manual Pages list."
	:x                      4
	:y                      56
	:width                  200
	:height                 217
	:foreground-color       ""
	:label                  "Manual Sections"
	:label-type             :string
	:layout-type            :vertical
	:rows                   10
	:read-only              t
	:multiple-selections    nil
	:selection-required     t
	:menu                   nil
	:notify-handler         ManualSectionNotify
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :scrolling-list
	:name                   ManualPages
	:owner                  controls1
	:help                   "The Manual Pages list is a listing of all manual pages that are available in the current section. A manual page can be displayed by selecting an item in the list.

If the manual page is not formatted for display, it will be formatted. If the directory required for storing manual formatted manual pages has the correct permissions, the user will be prompted to save the manual page."
	:x                      232
	:y                      56
	:width                  200
	:height                 217
	:foreground-color       ""
	:label                  "Manual Pages"
	:label-type             :string
	:layout-type            :vertical
	:rows                   10
	:read-only              t
	:multiple-selections    nil
	:selection-required     t
	:menu                   nil
	:notify-handler         ManualPageNotify
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :popup-window
	:name                   SearchPopup
	:owner                  OLXVMan
	:width                  207
	:height                 83
	:background-color       ""
	:foreground-color       ""
	:label                  "Manual Search"
	:label-type             :string
	:mapped                 nil
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :control-area
	:name                   controls2
	:owner                  SearchPopup
	:help                   ""
	:x                      0
	:y                      0
	:width                  207
	:height                 83
	:background-color       ""
	:foreground-color       ""
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :text-field
	:name                   SearchField
	:owner                  controls2
	:help                   "The search string is a lower case string that represents either the manual page or topic that will be the focus of the search."
	:x                      12
	:y                      8
	:width                  184
	:height                 32
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Please enter search string:"
	:label-type             :string
	:value-x                12
	:value-y                25
	:layout-type            :vertical
	:value-length           23
	:stored-length          80
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   ManualButton
	:owner                  controls2
	:help                   "The Manual Page search will locate a manual page on the entered command and display it in the current manual page text window."
	:x                      8
	:y                      56
	:constant-width         nil
	:button-type            :normal
	:width                  96
	:height                 19
	:foreground-color       ""
	:label                  "Manual Page"
	:label-type             :string
	:menu                   nil
	:notify-handler         SearchNotify
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :button
	:name                   AproposButton
	:owner                  controls2
	:help                   "Apropos search looks for manual pages that contain the text string requested. This function is the equivalent of \"man -k\"."
	:x                      128
	:y                      56
	:constant-width         nil
	:button-type            :normal
	:width                  67
	:height                 19
	:foreground-color       ""
	:label                  "Apropos"
	:label-type             :string
	:menu                   nil
	:notify-handler         SearchNotify
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :popup-window
	:name                   ManualPagePopup
	:owner                  OLXVMan
	:width                  506
	:height                 675
	:background-color       ""
	:foreground-color       ""
	:label                  "Manual Page"
	:label-type             :string
	:mapped                 nil
	:show-footer            t
	:resizable              t
	:pinned                 nil
	:done-handler           ManualPagePinned
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
(
	:type                   :text-pane
	:name                   ManualPageText
	:owner                  ManualPagePopup
	:help                   "The manual page text is displayed in a scrolling text window that cannot be edited."
	:x                      0
	:y                      0
	:width                  506
	:height                 675
	:background-color       ""
	:foreground-color       ""
	:show-border            t
	:read-only              t
	:event-handler          nil
	:events                 ()
	:user-data              ()
)
)
