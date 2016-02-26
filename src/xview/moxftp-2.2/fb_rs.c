#include "defs.h"

#if defined(MOTIF)
String fallback_resources[] = {
"Mftp.wcChildren:          	layout\n",
"Mftp.wcPopups:			Shellstatus, Shellcommand\n",
"Mftp.title:                    	Mftp\n",
"Mftp.allowShellResize:		FALSE\n",
"Mftp*wcTrace:                   FALSE\n",
"*labelFontList:			-*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*buttonFontList:		-*-times-medium-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*textFontList:		        -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*menuBar*fontList:		-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*menuBar1*fontList:		-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*menuBar2*fontList:		-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*archie_menubar*fontList:	-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*rate.fontList:		        -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*.*.list.fontList:		-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*width_in_chars:                90\n",
"*layout.wcClass:		XmForm\n",
"*layout.wcChildren:            	menuBar, \
				status, \
                                dir, \
				rowcol, \
                                connect, dir_display, command, glob,\
				   search, next,reconnect,\
				   archie_command, rate, \
                                percent, \
				listsw\n",
"*layout.horizontalSpacing:	0\n",
"*menuBar.wcCreate:              XmCreateMenuBar\n",
"*menuBar.leftAttachment:        XmATTACH_FORM\n",
"*menuBar.rightAttachment:	XmATTACH_FORM\n",
"*menuBar.topAttachment:		XmATTACH_FORM\n",
"*menuBar.wcChildren:		quitm, quitmMenu, \
				options, optionsmenu, \
				fileopts, fileoptsmenu, \
				filesopts, filesoptsmenu, \
			        helpm, helpMenu\n",
"*quitm.wcConstructor:		XmCreateCascadeButton\n",
"*quitm.wcCallback:            	help_register\n",
"*quitm.labelString:             Quit\n",
"*quitm.mnemonic:		F\n",
"*quitm.helpCallback:		Help()\n",
"*quitmMenu.wcConstructor:	XmCreatePulldownMenu\n",
"*quitmMenu.wcCallback:    	WcSetValue(*quitm.subMenuId: *quitmMenu)\n",
"*quitmMenu.wcManaged:     	False\n",
"*quitmMenu.wcChildren:		abort, quit\n",
"*abort.wcConstructor:        	XmCreateCascadeButton\n",
"*abort.labelString:		Abort\n",
"*abort.mnemonic:		A\n",
"*abort.activateCallback:	abort\n",
"*quit.wcConstructor:        	XmCreateCascadeButton\n",
"*quit.labelString:		Quit\n",
"*quit.mnemonic:			Q\n",
"*quit.activateCallback:		quit\n",
"*options.wcCreate:		XmCascadeButton\n",
"*options.wcCallback:    	help_register\n",
"*options.labelString:		Options\n",
"*optionsmenu.wcCreate:		XmCreatePulldownMenu\n",
"*optionsmenu.wcCallback:    	help_register \
				WcSetValue(*options.subMenuId: *optionsmenu)\n",
"*optionsmenu.wcPopups:		listingMenu, sortmenu\n",
"*optionsmenu.wcChildren:	op_1, op_2, opline, listing, sort\n",
"*op_1.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*op_1.labelString:              Ignore Errors\n",
"*op_1.valueChangedCallback:	op(IgnoreErrors)\n",
"*op_1.indicatorType:		XmONE_OF_MANY\n",
"*op_1.visibleWhenOff:		TRUE\n",
"*op_1.set:			FALSE\n",
"*op_1.indicatorType:		XmN_OF_MANY\n",
"*op_2.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*op_2.wcCallback:    		op(NoAutoDir)\n",
"*op_2.labelString:              No Auto Directory Listing\n",
"*op_2.valueChangedCallback:	op(NoAutoDir)\n",
"*op_2.indicatorType:		XmONE_OF_MANY\n",
"*op_2.visibleWhenOff:		TRUE\n",
"*op_2.set:			FALSE\n",
"*op_2.indicatorType:		XmN_OF_MANY\n",
"*opline.wcConstructor:       	XmCreateSeparatorGadget\n",
"*opline.sepatorType:     	SINGLE_LINE \n",
"*listing.wcCreate:              XmCascadeButton\n",
"*listing.labelString:           Short Listing\n",
"*listing.mnemonic: 		L\n",
"*listing.subMenuId:		*listingMenu\n",
"*listingMenu.wcConstructor:     XmCreatePulldownMenu\n",
"*listingMenu.wcChildren:	lmenu1, lmenu2, lmenu3, lmenu4, \
				lmenu5, lmenu6\n",
"*lmenu1.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*lmenu1.wcCallback:         	toggle(listing, SHORT)\n",
"*lmenu1.labelString:            Short listing\n",
"*lmenu1.valueChangedCallback:	listing_type(SHORT) \n",
"*lmenu1.indicatorType:		XmONE_OF_MANY\n",
"*lmenu1.visibleWhenOff:		TRUE\n",
"*lmenu1.set:			TRUE\n",
"*lmenu2.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*lmenu2.wcCallback:         	toggle(listing, MEDIUM)\n",
"*lmenu2.labelString:            Medium listing\n",
"*lmenu2.valueChangedCallback:	listing_type(MEDIUM) \n",
"*lmenu2.indicatorType:		XmONE_OF_MANY\n",
"*lmenu2.visibleWhenOff:		TRUE\n",
"*lmenu3.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*lmenu3.wcCallback:         	toggle(listing, LONG)\n",
"*lmenu3.labelString:            Long listing\n",
"*lmenu3.valueChangedCallback:	listing_type(LONG) \n",
"*lmenu3.indicatorType:		XmONE_OF_MANY\n",
"*lmenu3.visibleWhenOff:		TRUE\n",
"*lmenu4.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*lmenu4.wcCallback:         	toggle(listing, TRANSLATIONS)\n",
"*lmenu4.labelString:            Translation listing\n",
"*lmenu4.valueChangedCallback:	listing_type(TRANSLATIONS) \n",
"*lmenu4.indicatorType:		XmONE_OF_MANY\n",
"*lmenu4.visibleWhenOff:		TRUE\n",
"*lmenu5.wcConstructor:       	XmCreateSeparatorGadget\n",
"*lmenu5.sepatorType:     	SINGLE_LINE \n",
"*lmenu6.wcConstructor:	        XmCreateCascadeButtonGadget\n",
"*lmenu6.labelString:            Translations\n",
"*lmenu6.activateCallback:	List_Translations\n",
"*sort.wcCreate:              	XmCreateCascadeButton\n",
"*sort.wcCallback:            	help_register\n",
"*sort.labelString:          	Sort Options \n",
"*sort.mnemonic: 		S\n",
"*sort.helpCallback:		Help()\n",
"*sort.subMenuId:		*sortmenu\n",
"*sortmenu.wcConstructor:        XmCreatePulldownMenu\n",
"*sortmenu.wcChildren:		smbyname, smbysize, smbyage, smline, \
			        smbytype, smnormal\n",
"*smbyname.wcConstructor:        XmCreateToggleButtonGadget\n",
"*smbyname.wcCallback:		toggle(sort_type, SORT_BY_NAME)\n",
"*smbyname.labelString:          Sort By Name\n",
"*smbyname.valueChangedCallback:	listing_type(SORT_BY_NAME) \n",
"*smbyname.indicatorType:	XmONE_OF_MANY\n",
"*smbyname.set:			TRUE\n",
"*smbyname.visibleWhenOff:	TRUE\n",
"*smbysize.wcConstructor:        XmCreateToggleButtonGadget\n",
"*smbysize.wcCallback:		toggle(sort_type, SORT_BY_SIZE)\n",
"*smbysize.labelString:          Sort By Size\n",
"*smbysize.valueChangedCallback:	listing_type(SORT_BY_SIZE) \n",
"*smbysize.indicatorType:	XmONE_OF_MANY\n",
"*smbysize.visibleWhenOff:	TRUE\n",
"*smbyage.wcConstructor:	        XmCreateToggleButtonGadget\n",
"*smbyage.wcCallback:		toggle(sort_type, SORT_BY_AGE)\n",
"*smbyage.labelString:           Sort By Age\n",
"*smbyage.valueChangedCallback:	listing_type(SORT_BY_AGE) \n",
"*smbyage.indicatorType:		XmONE_OF_MANY\n",
"*smbyage.visibleWhenOff:	TRUE\n",
"*smline.wcConstructor:       	XmCreateSeparatorGadget\n",
"*smline.sepatorType:     	SINGLE_LINE \n",
"*smbytype.wcConstructor:        XmCreateToggleButtonGadget\n",
"*smbytype.labelString:          Sort By Type\n",
"*smbytype.valueChangedCallback:	listing_type(SORT_BY_TYPE) \n",
"*smbytype.indicatorType:	XmN_OF_MANY\n",
"*smbytype.visibleWhenOff:	TRUE\n",
"*smnormal.wcConstructor:        XmCreateToggleButtonGadget\n",
"*smnormal.labelString:          Normal\n",
"*smnormal.valueChangedCallback:listing_type(NORMAL) \n",
"*smnormal.indicatorType:	XmN_OF_MANY\n",
"*smnormal.visibleWhenOff:	TRUE\n",
"*smnormal.set:			TRUE\n",
"*fileopts.wcCreate:            	XmCreateCascadeButton\n",
"*fileopts.wcCallback:         	noop(get put dir connect action Sensitive)\
                             	help_register\n",
"*fileopts.labelString:         	File Options\n",
"*fileopts.mnemonic: 		F\n",
"*fileopts.helpCallback:		Help()\n",
"*fileopts.cascadingCallback:	Single_File_Actions\n",
"*fileoptsmenu.wcConstructor:    XmCreatePulldownMenu\n",
"*fileoptsmenu.wcCallback:    	WcSetValue(*fileopts.subMenuId: *fileoptsmenu)\n",
"*fileoptsmenu.wcChildren:	filem_UP, filem_CD, filem_GET,  \
				filem_VIEW, filem_PUT, \
				filem_Ascii, filem_Binary,  filem_Tenex, \
				filem_Default, \
				filem_ignore, filem_use, \
				filem_DIR\n",
"*filem_UP.wcConstructor:        XmCreateCascadeButtonGadget\n",
"*filem_UP.wcCallback:           Register_action(up)\n",
"*filem_UP.labelString:          Up\n",
"*filem_UP.activateCallback:     SetFileAction_menu()\n",
"*filem_CD.wcConstructor:        XmCreateCascadeButtonGadget\n",
"*filem_CD.wcCallback:           Register_action(cd)\n",
"*filem_CD.labelString:          Cd\n",
"*filem_CD.activateCallback:     SetFileAction_menu()\n",
"*filem_GET.wcConstructor:       XmCreateCascadeButtonGadget\n",
"*filem_GET.wcCallback:          Register_action(get)\n",
"*filem_GET.labelString:         Get\n",
"*filem_GET.activateCallback:    SetFileAction_menu()\n",
"*filem_VIEW.wcConstructor:      XmCreateCascadeButtonGadget\n",
"*filem_VIEW.wcCallback:         Register_action(view)\n",
"*filem_VIEW.labelString:        View\n",
"*filem_VIEW.activateCallback:   SetFileAction_menu()\n",
"*filem_PUT.wcConstructor:       XmCreateCascadeButtonGadget\n",
"*filem_PUT.wcCallback:          Register_action(put)\n",
"*filem_PUT.labelString:         Put\n",
"*filem_PUT.activateCallback:    SetFileAction_menu()\n",
"*filem_Ascii.wcConstructor:  	XmCreateCascadeButtonGadget\n",
"*filem_Ascii.wcCallback:        Register_action(ascii)\n",
"*filem_Ascii.labelString:       Ascii\n",
"*filem_Ascii.activateCallback:	SetFileAction_menu()\n",
"*filem_Binary.wcConstructor:  	XmCreateCascadeButtonGadget\n",
"*filem_Binary.wcCallback:       Register_action(binary)\n",
"*filem_Binary.labelString:      Binary\n",
"*filem_Binary.activateCallback:	SetFileAction_menu()\n",
"*filem_Tenex.wcConstructor:  	XmCreateCascadeButtonGadget\n",
"*filem_Tenex.wcCallback:        Register_action(tenex)\n",
"*filem_Tenex.labelString:       Tenex\n",
"*filem_Tenex.activateCallback:	SetFileAction_menu()\n",
"*filem_Default.wcConstructor: 	XmCreateCascadeButtonGadget\n",
"*filem_Default.wcCallback:      Register_action(default)\n",
"*filem_Default.labelString:     Default Mode\n",
"*filem_Default.activateCallback: SetFileAction_menu()\n",
"*filem_ignore.wcConstructor:   	XmCreateCascadeButtonGadget\n",
"*filem_ignore.wcCallback: 	Register_action(ignore)\n",
"*filem_ignore.labelString:     	Ignore\n",
"*filem_ignore.activateCallback:	SetFileAction_menu()\n",
"*filem_use.wcConstructor:      	XmCreateCascadeButtonGadget\n",
"*filem_use.wcCallback:    	Register_action(use)\n",
"*filem_use.labelString:        	Don't ignore\n",
"*filem_use.activateCallback:   	SetFileAction_menu()\n",
"*filem_DIR.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*filem_DIR.wcCallback:		Register_action(dir)\n",
"*filem_DIR.labelString:		Dir\n",
"*filem_DIR.activateCallback:	SetFileAction_menu()\n",
"*filesopts.wcCreate:           	XmCreateCascadeButton\n",
"*filesopts.wcCallback:          help_register \
        noop(get put dir connect action Sensitive ifsensitive) \n",
"*filesopts.labelString:        	Multi File Options\n",
"*filesopts.mnemonic: 		i\n",
"*filesopts.helpCallback:	Help()\n",
"*filesopts.cascadingCallback:	Single_File_Actions\n",
"*filesopts.sensitive:        	FALSE\n",
"*filesoptsmenu.wcConstructor:   XmCreatePulldownMenu\n",
"*filesoptsmenu.wcCallback:    	WcSetValue(*filesopts.subMenuId: *filesoptsmenu)\n",
"*filesoptsmenu.wcChildren: 	filesm_CLEAR filesm_GET_ALL filesm_PUT_ALL\n",
"*filesm_CLEAR.wcConstructor:    XmCreateCascadeButtonGadget\n",
"*filesm_CLEAR.wcCallback:       Register_action(clear_all)\n",
"*filesm_CLEAR.labelString:      Clear File Selections\n",
"*filesm_CLEAR.activateCallback: SetFileAction_menu()\n",
"*filesm_GET_ALL.wcConstructor:   XmCreateCascadeButtonGadget\n",
"*filesm_GET_ALL.wcCallback:      Register_action(get_all)\n",
"*filesm_GET_ALL.labelString:     Get Selected Files\n",
"*filesm_GET_ALL.activateCallback:SetFileAction_menu()\n",
"*filesm_PUT_ALL.wcConstructor:   XmCreateCascadeButtonGadget\n",
"*filesm_PUT_ALL.wcCallback:      Register_action(put_all)\n",
"*filesm_PUT_ALL.labelString:     Put Selected Files\n",
"*filesm_PUT_ALL.activateCallback:SetFileAction_menu()\n",
"*helpm.wcConstructor:		XmCreateCascadeButton\n",
"*helpm.wcCallback:    	        WcSetValue(*menuBar.menuHelpWidget: *helpm) \
                             	help_register\n",
"*helpm.labelString:             Help\n",
"*helpm.helpCallback:		Help()\n",
"*helpm.mnemonic:		H\n",
"*helpMenu.wcConstructor:	XmCreatePulldownMenu\n",
"*helpMenu.wcCallback:    	WcSetValue(*helpm.subMenuId: *helpMenu)\n",
"*helpMenu.wcManaged:     	False\n",
"*helpMenu.wcChildren:		HelpA, HelpGeneral\n",
"*HelpA.wcConstructor:   	XmCreateCascadeButton\n",
"*HelpA.labelString:		Context Sensitive Help\n",
"*HelpA.mnemonic:		C\n",
"*HelpA.activateCallback:	Help(c)\n",
"*HelpGeneral.wcConstructor:   	XmCreateCascadeButton\n",
"*HelpGeneral.labelString:	General Help\n",
"*HelpGeneral.mnemonic:		G\n",
"*HelpGeneral.activateCallback:	Help()\n",
"*status.wcCreate:               XmLabel\n",
"*status.wcCallback:		help_register set_width(1)\n",
"*status.labelString:            \\ \n",
"*status.Alignment:   	        XmALIGNMENT_BEGINNING\n",
"*status.leftAttachment:	        XmATTACH_FORM\n",
"*status.rightAttachment:	XmATTACH_FORM\n",
"*status.topWidget:             *menuBar\n",
"*status.topAttachment:         	XmATTACH_WIDGET\n",
"*status.recomputeSize:		FALSE\n",
"*status.borderWidth:            2\n",
"*status.helpCallback:		Help()\n",
"*status.topOffset:		4\n",
"*status.translations:     	#override \
				<Key>osfHelp:  Help() \\n\
                                <Btn1Down>: WcPopupACT(*Shellstatus) \
					    SetStatusIcon()\n",
"*dir.wcCreate:         		XmLabel\n",
"*dir.wcCallback:		help_register set_width(1)\n",
"*dir.labelString:          	\\  \n",
"*dir.recomputeSize:		FALSE\n",
"*dir.Alignment:   	        XmALIGNMENT_BEGINNING\n",
"*dir.leftAttachment:	        XmATTACH_FORM\n",
"*dir.rightAttachment:		XmATTACH_FORM\n",
"*dir.topWidget:                 *status\n",
"*dir.topAttachment:          	XmATTACH_WIDGET\n",
"*dir.borderWidth:        	2\n",
"*dir.topOffset:			4\n",
"*dir.helpCallback:		Help() \n",
"*rowcol.wcCreate:		XmCreateBulletinBoard\n",
"*rowcol.wcChildren:		host_name, system_name, default_mode\n",
"*rowcol.topWidget:           	*dir\n",
"*rowcol.topAttachment:      	XmATTACH_WIDGET\n",
"*rowcol.leftAttachment:	        XmATTACH_FORM\n",
"*rowcol.rightAttachment:	XmATTACH_FORM\n",
"*rowcol.marginWidth:		0\n",
"*rowcol.marginHeight:		0\n",
"*rowcol.translations:     	#override \
		<Expose>:    	resizebb() \\n\
		<Configure>:   	resizebb()\n",
"*host_name.wcCreate:        	XmLabel\n",
"*host_name.wcCallback:		help_register set_width(3, -8)\n",
"*host_name.labelString:         \\ \n",
"*host_name.Alignment:           XmALIGNMENT_BEGINNING\n",
"*host_name.topWidget:           *dir\n",
"*host_name.topAttachment:      	XmATTACH_WIDGET\n",
"*host_name.shadowThickness:    	0\n",
"*host_name.recomputeSize:	FALSE\n",
"*host_name.helpCallback:	Help()\n",
"*system_name.wcCreate:     	XmLabel\n",
"*system_name.wcCallback:	help_register set_width(3, -8)\n",
"*system_name.labelString:       \\  \n",
"*system_name.leftWidget:        *host_name\n",
"*system_name.leftAttachment: 	XmATTACH_WIDGET\n",
"*system_name.topWidget:         *dir\n",
"*system_name.topAttachment:    	XmATTACH_WIDGET\n",
"*system_name.recomputeSize:	FALSE\n",
"*system_name.helpCallback:	Help()\n",
"*default_mode.wcCreate:      	XmLabel\n",
"*default_mode.wcCallback:	help_register set_width(3, -8) \n",
"*default_mode.labelString:      \\     \n",
"*default_mode.leftWidget:      *system_name\n",
"*default_mode.leftAttachment: 	XmATTACH_WIDGET\n",
"*default_mode.topWidget:       *dir\n",
"*default_mode.topAttachment:   	XmATTACH_WIDGET\n",
"*default_mode.recomputeSize:	FALSE\n",
"*default_mode.Alignment:        XmALIGNMENT_END\n",
"*default_mode.helpCallback:	Help()\n",
"*connect.wcCreate:		XmPushButton\n",
"*connect.wcCallback:		help_register help_register(system_list) \
                                help_register(dotxftp) \
                                help_register(netrc) \
				help_register(Trademarks) \
			    	help_register(list_key_input) \
			    	help_register(mftp_fonts) \
     			        help_register(Shellcommand) \
     			        help_register(Shellview) \
     			        help_register(Shelltran) \
				help_register(Shellstatus) \
				help_register(Shellhelp) \
				help_register(Shellconnect) \
                                help_register(Shellglobdialog) \
                                help_register(Shellsearchdialog) \
                                help_register(Shellsearchhostdialog) \
                                help_register(op_listing) \
                                help_register(op_sort)\n",
"*connect.labelString:           Login\n",
"*connect.topWidget:             *rowcol\n",
"*connect.topAttachment:        	XmATTACH_WIDGET\n",
"*connect.activateCallback:	connect_disconnect() Set_noop(connect)\n",
"*connect.recomputeSize:		FALSE\n",
"*connect.helpCallback:		Help()\n",
"*dir_display.wcCreate:		XmPushButton\n",
"*dir_display.wcCallback:        noop(get put dir connect notconnected action Sensitive) \
                                help_register\n",
"*dir_display.labelString:	Remote\n",
"*dir_display.leftWidget:	*connect\n",
"*dir_display.leftAttachment:   	XmATTACH_WIDGET\n",
"*dir_display.topWidget:         *rowcol\n",
"*dir_display.topAttachment:   	XmATTACH_WIDGET\n",
"*dir_display.recomputeSize:	FALSE\n",
"*dir_display.activateCallback:	remote_local_toggle\n",
"*dir_display.helpCallback:	Help()\n",
"*dir_display.sensitive:        	FALSE\n",
"*command.wcCreate:              XmPushButton\n",
"*command.wcCallback:            help_register\n",
"*command.labelString:           Command Shell\n",
"*command.leftWidget:		*dir_display\n",
"*command.leftAttachment:   	XmATTACH_WIDGET\n",
"*command.topWidget:             *rowcol\n",
"*command.topAttachment:         XmATTACH_WIDGET\n",
"*command.recomputeSize:         FALSE\n",
"*command.helpCallback:          Help()\n",
"*command.activateCallback: 	WcPopupCB(*Shellcommand) \
				SetIcons(*Shellcommand)\n",
"*glob.wcCreate:             	XmPushButton\n",
"*glob.wcCallback:               CreateGlobDialog \
        noop(get put dir connect notconnected action Sensitive ifsensitive) \
        help_register\n",
"*glob.labelString:           	Glob\n",
"*glob.leftWidget:  	     	*command\n",
"*glob.leftAttachment:   	XmATTACH_WIDGET\n",
"*glob.topWidget:             	*rowcol\n",
"*glob.topAttachment:         	XmATTACH_WIDGET\n",
"*glob.recomputeSize:         	FALSE\n",
"*glob.helpCallback:          	Help()\n",
"*glob.activateCallback:      	WcManageCB(*Shellglobdialog) \n",
"*Shellglobdialog.defaultPosition:  FALSE\n",
"*Shellglobdialog.mapCallback:	PositionDialog()\n",
"*Shellglobdialog.translations:  #override \
		<Expose>:    	travers()\n",
"*Shellglobdialog.*.Text.translations:  #override \
   <Key>osfHelp:  		PrimitiveHelp() \\n\
   <Key>Help:                   PrimitiveHelp() \\n\
   <Key>F1:                     PrimitiveHelp() \\n\
   <Key>Return:                 WcUnmanageACT(*Shellglobdialog) \
                                set_glob_text(glob) \\n\
   Ctrl<Key>r:                  WcUnmanageACT(*Shellglobdialog) \
                                set_glob_text(reg) \\n\
   Ctrl<Key>g:                  WcUnmanageACT(*Shellglobdialog) \
                                set_glob_text(glob) \\n\
   Ctrl<Key>c:                  WcUnmanageACT(*Shellglobdialog)\n",
"*search.wcCreate:             	XmPushButton\n",
"*search.wcCallback:             CreateSearchDialog \
        noop(get put dir connect notconnected action Sensitive ifsensitive) \
	help_register\n",
"*search.labelString:           	Search\n",
"*search.leftWidget:  	     	*glob\n",
"*search.leftAttachment:   	XmATTACH_WIDGET\n",
"*search.topWidget:             	*rowcol\n",
"*search.topAttachment:         	XmATTACH_WIDGET\n",
"*search.recomputeSize:         	FALSE\n",
"*search.helpCallback:          	Help()\n",
"*search.activateCallback:       WcManageCB(*Shellsearchdialog) \n",
"*Shellsearchdialog.defaultPosition:  FALSE\n",
"*Shellsearchdialog.mapCallback:	PositionDialog()\n",
"*Shellsearchdialog.translations:  #override \
                <Expose>:       travers()\n",
"*Shellsearchdialog.*.Text.translations:  #override \
   <Key>osfHelp:  		PrimitiveHelp() \\n\
   <Key>Help:                   PrimitiveHelp() \\n\
   <Key>F1:                     PrimitiveHelp() \\n\
   <Key>Return:                 WcUnmanageACT(*Shellsearchdialog) \
                                set_search_text(glob) \\n\
   Ctrl<Key>r:                  WcUnmanageACT(*Shellsearchdialog) \
                                set_search_text(reg) \\n\
   Ctrl<Key>g:                  WcUnmanageACT(*Shellsearchdialog) \
                                set_search_text(glob) \\n\
   Ctrl<Key>c:                  WcUnmanageACT(*Shellsearchdialog)\n",
"*next.wcCreate:             	XmPushButton\n",
"*next.wcCallback:  \
        noop(get put dir connect notconnected action Sensitive ifsensitive) \
        help_register\n",
"*next.labelString:           	Next\n",
"*next.leftWidget:  	     	*search\n",
"*next.leftAttachment:   	XmATTACH_WIDGET\n",
"*next.topWidget:             	*rowcol\n",
"*next.topAttachment:         	XmATTACH_WIDGET\n",
"*next.recomputeSize:         	FALSE\n",
"*next.helpCallback:          	Help()\n",
"*next.activateCallback:      	search_next()\n",
"*next.sensitive:                FALSE\n",
"*reconnect.wcCreate:      	XmPushButton\n",
"*reconnect.wcCallback:          help_register\n",
"*reconnect.labelString:         Reconnect\n",
"*reconnect.leftWidget:		*next\n",
"*reconnect.leftAttachment: 	XmATTACH_WIDGET\n",
"*reconnect.topWidget:       	*rowcol\n",
"*reconnect.topAttachment:   	XmATTACH_WIDGET\n",
"*reconnect.recomputeSize:      	FALSE\n",
"*reconnect.helpCallback:       	Help()\n",
"*reconnect.sensitive:          	FALSE\n",
"*reconnect.activateCallback:   	Reconnect\n",
"*archie_command.wcCreate:    	XmPushButton\n",
"*archie_command.wcCallback:    	archie_noop() help_register\n",
"*archie_command.labelString:   	Archie\n",
"*archie_command.y:             	3\n",
"*archie_command.leftWidget:    	*reconnect\n",
"*archie_command.leftAttachment:	XmATTACH_WIDGET\n",
"*archie_command.topWidget:     	*rowcol\n",
"*archie_command.topAttachment: 	XmATTACH_WIDGET\n",
"*archie_command.helpCallback:  	Help()\n",
"*archie_command.activateCallback: archie()\n",
"*rate.wcCreate:                 LableQUICKClass\n",
"*rate.label:			\\ \n",
"*rate.wcCallback:               help_register \n",
"*rate.borderWidth:              0\n",
"*rata.internalWidth:		0\n",
"*rate.topOffset:		3\n",
"*rate.justify:                  right\n",
"*rate.topWidget:                *rowcol\n",
"*rate.editable:                 FALSE\n",
"*rate.cursorPositionVisible:    FALSE\n",
"*rate.traversalOn:              FALSE\n",
"*rate.topAttachment:            XmATTACH_WIDGET\n",
"*rate.leftWidget:               *archie_command\n",
"*rate.leftAttachment:           XmATTACH_WIDGET\n",
"*rate.rightAttachment:          XmATTACH_FORM\n",
"*rate.shadowThickness:          0\n",
"*rate.recomputeSize:            FALSE\n",
"*rate.helpCallback:             Help()\n",
"*percent.wcCreate:          	XmDrawingArea\n",
"*percent.wcCallback:		help_register set_width(1)\n",
"*percent.height:		3\n",
"*percent.topOffset:		2\n",
"*percent.MarginHeight:		2\n",
"*percent.MarginWidth:		2\n",
"*percent.Alignment:   	        XmALIGNMENT_BEGINNING\n",
"*percent.topWidget:            *dir_display\n",
"*percent.helpCallback:	        Help()\n",
"*percent.topAttachment:         XmATTACH_WIDGET\n",
"*percent.leftAttachment:        XmATTACH_FORM\n",
"*percent.rightAttachment:	XmATTACH_FORM\n",
"*listsw.wcCreate:		MyListSW\n",
"*listsw.wcCallback:             \
	noop(get put dir notconnected action connect) \
                                help_register \
				set_width(1) \
				CreateContinueDialog() \
				WcCreatePopups(*list, fmenu)\n",
"*listsw.topWidget:        	*percent\n",
"*listsw.height:                 400\n",
"*listsw.labelw:                 TRUE\n",
"*listsw.topAttachment:    	XmATTACH_WIDGET\n",
"*listsw.rightAttachment:  	XmATTACH_FORM\n",
"*listsw.leftAttachment:   	XmATTACH_FORM\n",
"*listsw.bottomAttachment:	XmATTACH_FORM\n",
"*listsw.topOffset:		2\n",
"*listsw.list.Callback:          list_notify\n",
"*listsw.list.columnSpacing:     0\n",
"*listsw.list.defaultColumns:    1\n",
"*listsw.list.rowSpacing:        0\n",
"*listsw.list.helpCallback:	help()\n",
"*listsw.list.translations:     	#override \
   <Key>F1:    		PrimitiveHelp() \\n\
  ~Ctrl ~Shift <Key>h:	Listop(Left)\\n\
  ~Ctrl ~Shift <Key>k:  Listop(Up)\\n\
  ~Ctrl ~Shift <Key>l:  Listop(Right)\\n\
  ~Ctrl ~Shift <Key>j:	Listop(Down)\\n\
  ~Ctrl   <Key>dollar:  Listop(End)\\n\
  ~Ctrl ~Shift <Key>0:	Listop(Start)\\n\
  ~Ctrl  Shift <Key>m:	Listop(Down)\\n\
   Ctrl ~Shift <Key>f:  Listop(NextPage)\\n\
   Ctrl ~Shift <Key>b:  Listop(PrevPage)\\n\
   Ctrl ~Shift <Key>n:	Listop(Down)\\n\
   Ctrl ~Shift <Key>j:	Listop(Down)\\n\
   Ctrl ~Shift <Key>p:	Listop(Up)\\n\
  ~Shift  ~Meta ~Alt <Key>space: Listop(Select) \\n\
   Ctrl ~Shift <Key>t:	remote_local_toggle()\\n\
  ~Ctrl  Shift <Key>l:	Listing_type(LONG)\\n\
  ~Ctrl  Shift <Key>s: 	Listing_type(SHORT)\\n\
  ~Ctrl  Shift <Key>t:	Listing_type(TRANSLATIONS)\\n\
  ~Ctrl  <Key>greater:	Listop(NextPage)\\n\
  ~Ctrl  <Key>less:  	Listop(PrevPage)\\n\
   Ctrl  <Key>greater: 	Listop(Bottom)\\n\
   Ctrl  <Key>less:     Listop(Top)\\n\
  ~Ctrl ~Shift <Key>u:	SetFileAction(up)\\n\
  ~Ctrl ~Shift <Key>c:	SetFileAction(cd)\\n\
  ~Ctrl ~Shift <Key>g:	SetFileAction(get)\\n\
  ~Ctrl ~Shift <Key>p:	SetFileAction(put)\\n\
  ~Ctrl ~Shift <Key>a:	SetFileAction(ascii)\\n\
  ~Ctrl ~Shift <Key>b:	SetFileAction(binary)\\n\
  ~Ctrl ~Shift <Key>d:	SetFileAction(default)\\n\
  ~Ctrl ~Shift <Key>t:	SetFileAction(tenex)\\n\
   Ctrl        <Key>s:  search_next()\\n\
   Ctrl        <Key>g:  search_clear()\\n\
   Button1<Motion>:     Set(M)\\n\
   ~Ctrl <Btn1Down>:   	Set()\\n\
   ~Ctrl <Btn1Up>:      Notify()\\n\
   Ctrl <Btn1Down>:     Set(x) Open_file() \\n\
  <Key>osfHelp:  	PrimitiveHelp()\\n\
   Ctrl <Btn3Down>: 	Set(x) \
       	      		Single_File_Actions() \
              		MyPopupACT(*fmenu) \\n\
  ~Ctrl <Btn3Down>: 	Single_File_Actions() \
              		MyPopupACT(*fmenu) \n\n",
"*fmenu.wcConstructor:		XmCreatePopupMenu\n",
"*fmenu.wcChildren:              fmenu_title, fmenuline, \
                                fmenu_UP, fmenu_Cd, \
			        fmenu_GET, fmenu_VIEW,\
				fmenu_PUT,\
				fmenu_Dir, \
				fmenu_modes, \
			        fmenuline, fmenuline,\
				fmenu_title2, \
				fmenuline, \
				fmenu_clear_all, \
				fmenu_get_all, fmenu_put_all, fmenu_delete_all\n",
"*fmenu.wcPopups:		modesMenu\n",
"*fmenu.unmapCallback:	        Clear_List_Entry()\n",
"*fmenu_title.wcConstructor:     XmCreateLabelGadget\n",
"*fmenu_title.labelString:	Single File Options\n",
"*fmenu_UP.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*fmenu_UP.wcCallback:           Register_action(up)\n",
"*fmenu_UP.labelString:          Up\n",
"*fmenu_UP.activateCallback:	SetFileAction()\n",
"*fmenu_Cd.wcConstructor:        XmCreateCascadeButtonGadget\n",
"*fmenu_Cd.wcCallback:           Register_action(cd)\n",
"*fmenu_Cd.labelString:          Cd\n",
"*fmenu_Cd.activateCallback:	SetFileAction()\n",
"*fmenu_GET.wcConstructor:    	XmCreateCascadeButtonGadget\n",
"*fmenu_GET.wcCallback:          Register_action(get)\n",
"*fmenu_GET.labelString:         Get\n",
"*fmenu_GET.activateCallback:	SetFileAction()\n",
"*fmenu_VIEW.wcConstructor:    	XmCreateCascadeButtonGadget\n",
"*fmenu_VIEW.wcCallback:         Register_action(view)\n",
"*fmenu_VIEW.labelString:        View\n",
"*fmenu_VIEW.activateCallback:	SetFileAction()\n",
"*fmenu_PUT.wcConstructor:    	XmCreateCascadeButtonGadget\n",
"*fmenu_PUT.wcCallback:          Register_action(put)\n",
"*fmenu_PUT.labelString:         Put\n",
"*fmenu_PUT.activateCallback:	SetFileAction()\n",
"*fmenu_modes.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*fmenu_modes.labelString:       File Modes\n",
"*fmenu_modes.subMenuId:         *modesMenu\n",
"*modesMenu.wcConstructor:     	XmCreatePulldownMenu\n",
"*modesMenu.wcChildren:        	fmenu_Ascii, fmenu_Binary, fmenu_Tenex, \
				fmenu_Default, fmenu_ignore, fmenu_use\n",
"*fmenu_Ascii.wcConstructor:  	XmCreateCascadeButtonGadget\n",
"*fmenu_Ascii.wcCallback:        Register_action(ascii)\n",
"*fmenu_Ascii.labelString:       Ascii\n",
"*fmenu_Ascii.activateCallback:	SetFileAction()\n",
"*fmenu_Binary.wcConstructor:  	XmCreateCascadeButtonGadget\n",
"*fmenu_Binary.wcCallback:       Register_action(binary)\n",
"*fmenu_Binary.labelString:      Binary\n",
"*fmenu_Binary.activateCallback:	SetFileAction()\n",
"*fmenu_Tenex.wcConstructor:  	XmCreateCascadeButtonGadget\n",
"*fmenu_Tenex.wcCallback:        Register_action(tenex)\n",
"*fmenu_Tenex.labelString:       Tenex\n",
"*fmenu_Tenex.activateCallback:	SetFileAction()\n",
"*fmenu_Default.wcConstructor: 	XmCreateCascadeButtonGadget\n",
"*fmenu_Default.wcCallback:      Register_action(default)\n",
"*fmenu_Default.labelString:     Default Mode\n",
"*fmenu_Default.activateCallback: SetFileAction()\n",
"*fmenu_ignore.wcConstructor:    XmCreateCascadeButtonGadget\n",
"*fmenu_ignore.wcCallback:       Register_action(ignore)\n",
"*fmenu_ignore.labelString:      Ignore\n",
"*fmenu_ignore.activateCallback: SetFileAction()\n",
"*fmenu_use.wcConstructor:       XmCreateCascadeButtonGadget\n",
"*fmenu_use.wcCallback:          Register_action(use)\n",
"*fmenu_use.labelString:         Don't ignore\n",
"*fmenu_use.activateCallback:    SetFileAction()\n",
"*fmenu_Dir.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*fmenu_Dir.wcCallback:		Register_action(dir)\n",
"*fmenu_Dir.labelString:		Dir\n",
"*fmenu_Dir.activateCallback:	SetFileAction()\n",
"*fmenuline.wcConstructor:       XmCreateSeparatorGadget\n",
"*fmenuline.sepatorType:     	SINGLE_LINE \n",
"*fmenu_title2.wcConstructor:     XmCreateLabelGadget\n",
"*fmenu_title2.labelString:       Multi File Options\n",
"*fmenu_clear_all.wcConstructor:		XmCreateCascadeButtonGadget\n",
"*fmenu_clear_all.wcCallback:    	Register_action(clear_all)\n",
"*fmenu_clear_all.labelString:        	Clear File Selections\n",
"*fmenu_clear_all.activateCallback:	SetFileAction()\n",
"*fmenu_get_all.wcConstructor:		XmCreateCascadeButtonGadget\n",
"*fmenu_get_all.wcCallback:    		Register_action(get_all)\n",
"*fmenu_get_all.labelString:        	Get Selected Files\n",
"*fmenu_get_all.activateCallback:	SetFileAction()\n",
"*fmenu_put_all.wcConstructor:		XmCreateCascadeButtonGadget\n",
"*fmenu_put_all.wcCallback:    		Register_action(put_all)\n",
"*fmenu_put_all.sensitive:        	FALSE\n",
"*fmenu_put_all.labelString:          	Put Selected Files\n",
"*fmenu_put_all.activateCallback:	SetFileAction()\n",
"*fmenu_delete_all.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*fmenu_delete_all.labelString:  	Delete Selected Files\n",
"*fmenu_delete_all.sensitive:        	FALSE\n",
"!*fmenu_delete_all.activateCallback:	delete\n",
"*Shellconnect.wcCreate:         TopLevelShell\n",
"*Shellconnect.wcChildren:       connectlayout\n",
"*Shellconnect.title:            Connect...  \n",
"*Shellconnect.iconName:        	Mftp\n",
"*Shellconnect*deleteResponse:	unmap\n",
"*Shellconnect.helpCallback:	Help()\n",
"*connectlayout.wcCreate:        XmForm\n",
"*connectlayout.wcChildren:      menuBar1, DoBoxConnect, DoHide,\
				     DoArchie, DoRetry, DoGateway,\
				hostsw, \
				hostlabel, hosttext,\
				logonlabel, logontext,\
				passwordlabel, passwordtext,\
				remotedirlabel, remotedirtext,\
				localdirlabel, localdirtext,\
				gatewaylabel, gatewaytext\n",
"*menuBar1.wcCreate:             XmCreateMenuBar\n",
"*menuBar1.leftAttachment:   	XmATTACH_FORM\n",
"*menuBar1.rightAttachment:      XmATTACH_FORM\n",
"*menuBar1.topAttachment:        XmATTACH_FORM\n",
"*menuBar1.wcChildren:           DoAnonymous, anonymous\n",
"*DoBoxConnect.wcCreate:	  	XmPushButton\n",
"*DoBoxConnect.traversalOn:	FALSE\n",
"*DoBoxConnect.labelString:     	Connect\n",
"*DoBoxConnect.Alignment:        XmALIGNMENT_BEGINNING\n",
"*DoBoxConnect.y:		3\n",
"*DoBoxConnect.helpCallback:	Help()\n",
"*DoBoxConnect.activateCallback: WcSetSensitiveCB(\"*connect\") \
			 	Login()\n",
"*DoBoxConnect.topWidget:        *menuBar1\n",
"*DoBoxConnect.topAttachment:   	XmATTACH_WIDGET\n",
"*DoHide.wcCreate:		XmPushButton\n",
"*DoHide.traversalOn:		FALSE\n",
"*DoHide.labelString:     	Hide\n",
"*DoHide.y:			3\n",
"*DoHide.leftWidget:             *DoBoxConnect\n",
"*DoHide.leftAttachment:      	XmATTACH_WIDGET\n",
"*DoHide.topWidget:    		*menuBar1\n",
"*DoHide.topAttachment:   	XmATTACH_WIDGET\n",
"*Dohide.helpCallback:		Help()\n",
"*DoHide.activateCallback: 	Clear_noop(connect) \
			        Set_noop(notconnected) \
			        WcSetSensitiveCB(\"*connect\") \
			        WcPopDownCB(~)\n",
"*DoArchie.wcCreate:		XmPushButton\n",
"*DoArchie.wcCallback: 	        archie_noop()\n",
"*DoArchie.traversalOn:		FALSE\n",
"*DoArchie.labelString:    	Archie\n",
"*DoArchie.y:			3\n",
"*DoArchie.leftWidget:       	*DoHide\n",
"*DoArchie.leftAttachment:       XmATTACH_WIDGET\n",
"*DoArchie.topWidget:    	*menuBar1\n",
"*DoArchie.topAttachment:   	XmATTACH_WIDGET\n",
"*DoArchie.helpCallback:		Help()\n",
"*DoArchie.activateCallback:     archie()\n",
"*DoRetry.wcCreate:		XmToggleButton\n",
"*DoRetry.traversalOn:		FALSE\n",
"*DoRetry.labelString:     	Retry\n",
"*DoRetry.y:			3\n",
"*DoRetry.leftWidget:            *DoArchie\n",
"*DoRetry.leftAttachment:      	XmATTACH_WIDGET\n",
"*DoRetry.topWidget:    		*menuBar1\n",
"*DoRetry.topAttachment:   	XmATTACH_WIDGET\n",
"*DoRetry.armCallback:      	Set_retry\n",
"*DoRetry.disarmCallback:      	Set_retry\n",
"*DoRetry.helpCallback:		Help()\n",
"*DoGateway.wcCreate:		XmToggleButton\n",
"*DoGateway.traversalOn:		FALSE\n",
"*DoGateway.labelString:     	Use ftp gateway\n",
"*DoGateway.y:			3\n",
"*DoGateway.leftWidget:          *DoRetry\n",
"*DoGateway.leftAttachment:   	XmATTACH_WIDGET\n",
"*DoGateway.topWidget:    	*menuBar1\n",
"*DoGateway.topAttachment:   	XmATTACH_WIDGET\n",
"*DoGateway.armCallback:      	Set_use_gateway\n",
"*DoGateway.disarmCallback:     	Set_use_gateway\n",
"*DoGateway.helpCallback:	Help()\n",
"*hostsw.wcCreate:		MyListSW\n",
"*hostsw.wcCallback:             help_register \
				set_width(1) \
				CreateHostSearchDialog()\n",
"*hostsw.height:                 100\n",
"*hostsw.labelw:                 FALSE\n",
"*hostsw.topWidget:       	*DoBoxConnect\n",
"*hostsw.topAttachment:   	XmATTACH_WIDGET\n",
"*hostsw.rightAttachment:  	XmATTACH_FORM\n",
"*hostsw.leftAttachment:   	XmATTACH_FORM\n",
"*hostsw.topOffset:		2\n",
"*hostsw.list.Callback:          SelectHost\n",
"*hostsw.list.columnSpacing:     0\n",
"*hostsw.list.defaultColumns:    1\n",
"*hostsw.list.forceColumns:	TRUE\n",
"*hostsw.list.rowSpacing:        0\n",
"*hostsw.list.helpCallback:	help()\n",
"*hostsw.list.mulitselect:	FALSE\n",
"*hostsw.list.translations:     	#override \
  Ctrl ~Shift <Key>n:		Listop(Down)\\n\
 ~Ctrl ~Shift <Key>j:		Listop(Down)\\n\
  Ctrl ~Shift <Key>p:		Listop(Up)\\n\
 ~Ctrl ~Shift <Key>k:		Listop(Up)\\n\
  Ctrl ~Shift <Key>f:		Listop(Right)\\n\
 ~Ctrl ~Shift <Key>l:		Listop(Right)\\n\
  Ctrl ~Shift <Key>b:		Listop(Left)\\n\
 ~Ctrl ~Shift <Key>h:		Listop(Left)\\n\
  Ctrl ~Shift <Key>a:	        Listop(Start)\\n\
  Ctrl ~Shift <Key>e:         	Listop(End)\\n\
  Ctrl  <Key>s:                 search_host()\\n\
  Ctrl  <Key>g:                 search_host(clear)\\n\
 ~Ctrl  <Key>greater: 	        Listop(NextPage)\\n\
 ~Ctrl  <Key>less:  	        Listop(PrevPage)\\n\
  Ctrl  <Key>greater: 		Listop(Bottom)\\n\
  Ctrl  <Key>less:     		Listop(Top)\\n\
  Ctrl <Key>osfBeginLine:       Listop(Top)\\n\
  Ctrl ~Shift <Key>osfBeginLine:Listop(Start)\\n\
  Ctrl <Key>osfEndLine:         Listop(Bottom)\\n\
  <Key>osfEndLine:              Listop(End)\\n\
 ~Ctrl <Key>osfPageDown:        Listop(NextPage)\\n\
 ~Ctrl <Key>osfPageUp:          Listop(PrevPage)\\n\
 ~Ctrl ~Shift <Key>osfLeft:     Listop(Left)\\n\
 ~Ctrl ~Shift <Key>osfRight:    Listop(Right)\\n\
 ~Ctrl ~Shift <Key>osfUp:       Listop(Up)\\n\
 ~Ctrl ~Shift <Key>osfDown:     Listop(Down)\\n\
 ~Shift ~Meta ~Alt <Key>space:  Listop(Select)\\n\
 <Btn1Down>(1):                 Set() Notify()\\n\
 <Btn1Down>(2):                 Login()\\n\
 <Btn1Up>:			Noop()\n\n",
"<Key>osfHelp:  		PrimitiveHelp()\n",
"*Shellsearchhostdialog.defaultPosition:  FALSE\n",
"*Shellsearchhostdialog.mapCallback:	PositionDialog()\n",
"*Shellsearchhostdialog.translations:  #override \
                <Expose>:       travers()\n",
"*Shellsearchhostdialog.*.Text.translations:  #override \
   <Key>osfHelp:  		PrimitiveHelp() \\n\
   <Key>Help:                   PrimitiveHelp() \\n\
   <Key>F1:                     PrimitiveHelp() \\n\
   <Key>Return:                 WcUnmanageACT(*Shellsearchhostdialog) \
                                set_search_host(reg) \\n\
   Ctrl<Key>r:                  WcUnmanageACT(*Shellsearchhostdialog) \
                                set_search_host(reg) \\n\
   Ctrl<Key>c:                  WcUnmanageACT(*Shellsearchhostdialog)\n",
"*hostlabel.wcCreate:     	XmLabel\n",
"*hostlabel.width:           	140\n",
"*hostlabel.alignment:		XmALIGNMENT_END\n",
"*hostlabel.labelString:     	Host:\n",
"*hostlabel.Alignment:   	XmALIGNMENT_BEGINNING\n",
"*hostlabel.topWidget:       	*hostsw\n",
"*hostlabel.topAttachment:   	XmATTACH_WIDGET\n",
"*hostlabel.recomputeSize:	FALSE\n",
"*hostlabel.topOffset:		4\n",
"*hosttext.wcCreate:		XmTextField\n",
"*hosttext.wcCallback:       	init_connect_info(host) \
				SetHostList\n",
"*hosttext.width:            	300\n",
"*hosttext.editable:		TRUE\n",
"*hosttext.topWidget:        	*hostsw\n",
"*hosttext.topAttachment:  	XmATTACH_WIDGET\n",
"*hosttext.leftWidget:      	*hostlabel\n",
"*hosttext.leftAttachment:  	XmATTACH_WIDGET\n",
"*hosttext.rightAttachment:  	XmATTACH_FORM\n",
"*hosttext.topOffset:		4\n",
"*hosttext.translations:  	#override \
	 	<Key>Return:    	   Login() \n",
"*logonlabel.wcCreate:     	XmLabel\n",
"*logonlabel.width:           	140\n",
"*logonlabel.alignment:		XmALIGNMENT_END\n",
"*logonlabel.labelString:     	Login:\n",
"*logonlabel.Alignment:   	XmALIGNMENT_BEGINNING\n",
"*logonlabel.topWidget:       	*hosttext\n",
"*logonlabel.topAttachment:   	XmATTACH_WIDGET\n",
"*logonlabel.recomputeSize:	FALSE\n",
"*logontext.wcCreate:		XmTextField\n",
"*logontext.wcCallback:       	init_connect_info(login)\n",
"*logontext.width:            	300\n",
"*logontext.editable:		TRUE\n",
"*logontext.topWidget:        	*hosttext\n",
"*logontext.topAttachment:  	XmATTACH_WIDGET\n",
"*logontext.leftWidget:      	*logonlabel\n",
"*logontext.leftAttachment:  	XmATTACH_WIDGET\n",
"*logontext.rightAttachment:  	XmATTACH_FORM\n",
"*logontext.translations:  	#override \
	 	<Key>Return:    	   Login() \n",
"*passwordlabel.wcCreate:     	XmLabel\n",
"*passwordlabel.width:           140\n",
"*passwordlabel.alignment:	XmALIGNMENT_END\n",
"*passwordlabel.labelString:     Password:\n",
"*passwordlabel.Alignment:   	XmALIGNMENT_BEGINNING\n",
"*passwordlabel.topWidget:       *logontext\n",
"*passwordlabel.topAttachment:   XmATTACH_WIDGET\n",
"*passwordlabel.recomputeSize:	FALSE\n",
"*passwordtext.wcCreate:		XmTextField\n",
"*passwordtext.wcCallback:       init_connect_info(password)\n",
"*passwordtext.width:            300\n",
"*passwordtext.editable:		TRUE\n",
"*passwordtext.topWidget:        *logontext\n",
"*passwordtext.topAttachment:  	XmATTACH_WIDGET\n",
"*passwordtext.leftWidget:      	*passwordlabel\n",
"*passwordtext.leftAttachment:  	XmATTACH_WIDGET\n",
"*passwordtext.rightAttachment: 	XmATTACH_FORM\n",
"*passwordtext.translations:  	#override \
		<Key>osfHelp:              PrimitiveHelp()\\n\
	        Shift ~Meta ~Alt <Key>Tab: prev-tab-group()\\n\
	         ~Meta ~Alt <Key>Tab:      next-tab-group()\\n\
	        <Key>osfBackSpace: 	   delete_char() \\n\
	 	<Key>Return:    	   Login() \\n\
		<Key>:             	   insert_char() \n",
"*remotedirlabel.wcCreate:        XmLabel\n",
"*remotedirlabel.width:           140\n",
"*remotedirlabel.alignment:	 XmALIGNMENT_END\n",
"*remotedirlabel.labelString:     Remote Directory:\n",
"*remotedirlabel.Alignment:   	 XmALIGNMENT_BEGINNING\n",
"*remotedirlabel.topWidget:       *passwordtext\n",
"*remotedirlabel.topAttachment:   XmATTACH_WIDGET\n",
"*remotedirlabel.recomputeSize:	 FALSE\n",
"*remotedirtext.wcCreate:	XmTextField\n",
"*remotedirtext.wcCallback:      init_connect_info(remotedir)\n",
"*remotedirtext.width:           300\n",
"*remotedirtext.editable:  	TRUE\n",
"*remotedirtext.topWidget:      	*passwordtext\n",
"*remotedirtext.topAttachment:  	XmATTACH_WIDGET\n",
"*remotedirtext.leftWidget:     	*remotedirlabel\n",
"*remotedirtext.leftAttachment: 	XmATTACH_WIDGET\n",
"*remotedirtext.rightAttachment: XmATTACH_FORM\n",
"*remotedirtext.translations:  	#override \
	 	<Key>Return:    	   Login() \n",
"*localdirlabel.wcCreate:  	XmLabel\n",
"*localdirlabel.width:           140\n",
"*localdirlabel.alignment:	XmALIGNMENT_END\n",
"*localdirlabel.labelString:     Local Directory:\n",
"*localdirlabel.Alignment:   	XmALIGNMENT_BEGINNING\n",
"*localdirlabel.topWidget:       *remotedirtext\n",
"*localdirlabel.topAttachment:   XmATTACH_WIDGET\n",
"*localdirlabel.recomputeSize:	FALSE\n",
"*localdirtext.wcCreate:	     	XmTextField\n",
"*localdirtext.wcCallback:       init_connect_info(localdir)\n",
"*localdirtext.width:            300\n",
"*localdirtext.editable:		TRUE\n",
"*localdirtext.topWidget:        *remotedirtext\n",
"*localdirtext.topAttachment:  	XmATTACH_WIDGET\n",
"*localdirtext.leftWidget:      	*localdirlabel\n",
"*localdirtext.leftAttachment:  	XmATTACH_WIDGET\n",
"*localdirtext.rightAttachment:  XmATTACH_FORM\n",
"*localdirtext.translations:  	#override \
	 	<Key>Return:    	   Login() \n",
"*gatewaylabel.wcCreate:        XmLabel\n",
"*gatewaylabel.width:           140\n",
"*gatewaylabel.alignment:       XmALIGNMENT_END\n",
"*gatewaylabel.labelString:     Gateway:\n",
"*gatewaylabel.Alignment:       XmALIGNMENT_BEGINNING\n",
"*gatewaylabel.topWidget:       *localdirtext\n",
"*gatewaylabel.topAttachment:   XmATTACH_WIDGET\n",
"*gatewaylabel.recomputeSize:   FALSE\n",
"*gatewaytext.wcCreate:         XmTextField\n",
"*gatewaytext.wcCallback:       init_connect_info(gateway)\n",
"*gatewaytext.width:            300\n",
"*gatewaytext.editable:         TRUE\n",
"*gatewaytext.topWidget:        *localdirtext\n",
"*gatewaytext.topAttachment:    XmATTACH_WIDGET\n",
"*gatewaytext.leftWidget:       *gatewaylabel\n",
"*gatewaytext.leftAttachment:   XmATTACH_WIDGET\n",
"*gatewaytext.rightAttachment:  XmATTACH_FORM\n",
"*gatewaytext.translations:     #override \
                <Key>Return:               Login()\n",
"*DoAnonymous.wcConstructor:     XmCreateCascadeButton\n",
"*DoAnonymous.labelString:       Anonymous\n",
"*DoAnonymous.mnemonic:		A\n",
"*anonymous.wcConstructor:	XmCreatePulldownMenu\n",
"*anonymous.wcChildren:		anonGuest, anonMail, anonUser\n",
"*anonymous.wcManaged:		FALSE\n",
"*anonymous.wcCallback:        	WcSetValue(*DoAnonymous.subMenuId: *anonymous)\n",
"*anonGuest.wcConstructor:       XmCreateCascadeButtonGadget\n",
"*anonGuest.labelString:		guest\n",
"*anonGuest.activateCallback:	Anonymous(guest)\n",
"*anonMail.wcConstructor:        XmCreateCascadeButtonGadget\n",
"*anonMail.labelString:		Mail Address\n",
"*anonMail.activateCallback:	Anonymous(MAIL)\n",
"*anonUser.wcConstructor:        XmCreateCascadeButtonGadget\n",
"*anonUser.labelString:		Name\n",
"*anonUser.activateCallback:	Anonymous(NAME)\n",
"*Shellarchie.wcConstructor:    	XtCreateTopLevelShell\n",
"*Shellarchie.wcChildren: 	archie_layout\n",
"*Shellarchie.title:      	Archie\n",
"*Shellarchie.iconName:         	Mftp\n",
"*Shellarchie*deleteResponse:    unmap\n",
"*Shellarchie.helpCallback:	Help()\n",
"*archie_layout.wcCreate:       	XmForm\n",
"*archie_layout.wcChildren:     	archie_menubar, \
			        archie_hide, archie_search, archie_abort,\
				     archielabel, archietext \
				archie_lw\n",
"*archie_menubar.wcCreate:      	XmCreateMenuBar\n",
"*archie_menubar.leftAttachment:	XmATTACH_FORM\n",
"*archie_menubar.rightAttachment:XmATTACH_FORM\n",
"*archie_menubar.topAttachment: 	XmATTACH_FORM\n",
"*archie_menubar.wcChildren:     ArchieHits,  archiehits, \
				ArchieHost,  archiehost, \
				ArchieNice,  archienice, \
			        ArchieSearch, archiesearch, \
			        ArchieSort, archiesort \n",
"*ArchieSearch.wcConstructor:    XmCreateCascadeButton\n",
"*ArchieSearch.labelString:      Search Types\n",
"*ArchieSearch.mnemonic:       	S\n",
"*archiesearch.wcConstructor:	XmCreatePulldownMenu\n",
"*archiesearch.wcCallback:      	\
			WcSetValue(*ArchieSearch.subMenuId: *archiesearch)\n",
"*archiesearch.wcChildren:	archie_csss, archie_esm, archie_res, \
				archie_ciss\n",
"*archiesearch.wcManaged:	FALSE\n",
"*archie_csss.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_csss.wcCallback:        Register_archie(csss)\n",
"*archie_csss.labelString:	Case Sensitive substring search\n",
"*archie_csss.activateCallback:	do_archie\n",
"*archie_esm.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_esm.wcCallback:         Register_archie(esm)\n",
"*archie_esm.labelString:	Exact String Match\n",
"*archie_esm.activateCallback:	do_archie\n",
"*archie_res.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_res.wcCallback:         Register_archie(res)\n",
"*archie_res.labelString:	Regular Expession Search\n",
"*archie_res.activateCallback:	do_archie\n",
"*archie_ciss.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_ciss.wcCallback:  	Register_archie(ciss)\n",
"*archie_ciss.labelString:	Case Insensitive Substring Search\n",
"*archie_ciss.activateCallback:	do_archie\n",
"*ArchieHits.wcConstructor:      XmCreateCascadeButton\n",
"*ArchieHits.labelString:        Hits\n",
"*ArchieHits.mnemonic:       	H\n",
"*archiehits.wcConstructor:	XmCreatePulldownMenu\n",
"*archiehits.wcCallback:      	WcSetValue(*ArchieHits.subMenuId: *archiehits)\n",
"*archiehits.wcChildren:		archie_hit_95, archie_hit_200, \
				archie_hit_400 archie_hit_800\n",
"*archiehits.wcManaged:		FALSE\n",
"*archie_hit_95.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_hit_95.wcCallback:  	Register_archie(95)\n",
"*archie_hit_95.labelString:	95\n",
"*archie_hit_95.activateCallback:do_archie\n",
"*archie_hit_200.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_hit_200.wcCallback:  	Register_archie(200)\n",
"*archie_hit_200.labelString:	200\n",
"*archie_hit_200.activateCallback:do_archie\n",
"*archie_hit_400.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_hit_400.wcCallback:  	Register_archie(400)\n",
"*archie_hit_400.labelString:	400\n",
"*archie_hit_400.activateCallback:do_archie\n",
"*archie_hit_800.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_hit_800.wcCallback:  	Register_archie(800)\n",
"*archie_hit_800.labelString:	800\n",
"*archie_hit_800.activateCallback:do_archie\n",
"*ArchieNice.wcConstructor:      XmCreateCascadeButton\n",
"*ArchieNice.labelString:        Nice\n",
"*ArchieNice.mnemonic:       	N\n",
"*archienice.wcConstructor:	XmCreatePulldownMenu\n",
"*archienice.wcCallback:      	WcSetValue(*ArchieNice.subMenuId: *archienice)\n",
"*archienice.wcChildren:		archie_nice_default, archie_nice_nice, \
				archie_nice_nicer,  archie_nice_very, \
				archie_nice_exterm,  archie_nice_nicest\n",
"*archienice.wcManaged:		FALSE\n",
"*archie_nice_default.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_nice_default.wcCallback:  	Register_archie(nice_default)\n",
"*archie_nice_default.labelString:	Nice Default (0)\n",
"*archie_nice_default.activateCallback:	do_archie\n",
"*archie_nice_nice.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_nice_nice.wcCallback:  		Register_archie(nice_nice)\n",
"*archie_nice_nice.labelString:		Nice (500)\n",
"*archie_nice_nice.activateCallback:	do_archie\n",
"*archie_nice_nicer.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_nice_nicer.wcCallback:  	Register_archie(nice_nicer)\n",
"*archie_nice_nicer.labelString:		Nicer (1000)\n",
"*archie_nice_nicer.activateCallback:	do_archie\n",
"*archie_nice_very.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_nice_very.wcCallback:  		Register_archie(nice_very)\n",
"*archie_nice_very.labelString:		Very Nice (5000)\n",
"*archie_nice_very.activateCallback:	do_archie\n",
"*archie_nice_exterm.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_nice_exterm.wcCallback:  	Register_archie(nice_exterm)\n",
"*archie_nice_exterm.labelString:	Extermly Nice (1000)\n",
"*archie_nice_exterm.activateCallback:	do_archie\n",
"*archie_nice_nicest.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_nice_nicest.wcCallback:  	Register_archie(nice_nicest)\n",
"*archie_nice_nicest.labelString:	Nicest (32765)\n",
"*archie_nice_nicest.activateCallback:	do_archie\n",
"*ArchieSort.wcConstructor:      XmCreateCascadeButton\n",
"*ArchieSort.labelString:        Sort\n",
"*ArchieSort.mnemonic:       	S\n",
"*archiesort.wcConstructor:	XmCreatePulldownMenu\n",
"*archiesort.wcCallback:      	WcSetValue(*ArchieSort.subMenuId: *archiesort)\n",
"*archiesort.wcChildren:		archie_sort_age, archie_sort_name,\
				archie_sort_size,\
				archie_sort_sep,\
				archie_sort_normal,\
				archie_sort_reverse\n",
"*archie_sep_sort.sepatorType:   SINGLE_LINE \n",
"*archiesort.wcManaged:		FALSE\n",
"*archie_sort_age.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_sort_age.wcCallback:  	Register_archie(sort_by_age)\n",
"*archie_sort_age.labelString:	Age\n",
"*archie_sort_age.activateCallback:  do_archie\n",
"*archie_sort_name.wcConstructor:XmCreateCascadeButtonGadget\n",
"*archie_sort_name.wcCallback:  	Register_archie(sort_by_name)\n",
"*archie_sort_name.labelString:	Name\n",
"*archie_sort_name.activateCallback:  do_archie\n",
"*archie_sort_size.wcConstructor:XmCreateCascadeButtonGadget\n",
"*archie_sort_size.wcCallback:  	Register_archie(sort_by_size)\n",
"*archie_sort_size.labelString:	Size\n",
"*archie_sort_size.activateCallback:  do_archie\n",
"*archie_sort_sep.wcConstructor: XmCreateSeparatorGadget\n",
"*archie_sort_sep.sepatorType:   SINGLE_LINE \n",
"*archie_sort_normal.wcConstructor:XmCreateCascadeButtonGadget\n",
"*archie_sort_normal.wcCallback:	Register_archie(sort_normal)\n",
"*archie_sort_normal.labelString:Normal\n",
"*archie_sort_normal.activateCallback:  do_archie\n",
"*archie_sort_reverse.wcConstructor:XmCreateCascadeButtonGadget\n",
"*archie_sort_reverse.wcCallback:Register_archie(sort_reverse)\n",
"*archie_sort_reverse.labelString: Reverse\n",
"*archie_sort_reverse.activateCallback:  do_archie\n",
"*ArchieHost.wcConstructor:      XmCreateCascadeButton\n",
"*ArchieHost.labelString:        Hosts\n",
"*ArchieHost.mnemonic:       	H\n",
"*archiehost.wcConstructor:	XmCreatePulldownMenu\n",
"*archiehost.wcCallback:      	WcSetValue(*ArchieHost.subMenuId: *archiehost)\
				archie_noop() archie_hosts()\n",
"*archiehost.wcChildren:	\n",
"*archiehost.wcManaged:		FALSE\n",
"*archie_host.wcConstructor:	XmCreateCascadeButtonGadget\n",
"*archie_host.wcCallback:  	Register_archie(host)\n",
"*archie_host.labelString:	\n",
"*archie_host.activateCallback:  do_archie\n",
"*archie_sep_host.wcConstructor: XmCreateSeparatorGadget\n",
"*archie_sep_host.wcCallback:	archie_hosts()\n",
"*archie_sep_host.sepatorType:   SINGLE_LINE \n",
"*archie_hide.wcCreate:		XmPushButton\n",
"*archie_hide.labelString:     	Hide\n",
"*archie_hide.activateCallback: 	WcPopDownCB(~)\n",
"*archie_hide.leftAttachment:    XmATTACH_FORM\n",
"*archie_hide.topWidget:         *archie_menubar\n",
"*archie_hide.topAttachment:   	XmATTACH_WIDGET\n",
"*archie_hide.helpCallback:	Help()\n",
"*archie_search.wcCreate:	XmPushButton\n",
"*archie_search.wcCallback:      Register_archie(do_search)\n",
"*archie_search.labelString:    	Search\n",
"*archie_search.activateCallback:do_archie\n",
"*archie_search.leftAttachment:  XmATTACH_FORM\n",
"*archie_search.topWidget:       *archie_menubar\n",
"*archie_search.topAttachment:   XmATTACH_WIDGET\n",
"*archie_search.leftWidget:      *archie_hide\n",
"*archie_search.leftAttachment:  XmATTACH_WIDGET\n",
"*archie_search.helpCallback:	Help()\n",
"*archie_abort.wcCreate:		XmPushButton\n",
"*archie_abort.labelString:    	Abort Search\n",
"*archie_abort.activateCallback: abort_archie\n",
"*archie_abort.leftAttachment:   XmATTACH_FORM\n",
"*archie_abort.topWidget:        *archie_menubar\n",
"*archie_abort.topAttachment:    XmATTACH_WIDGET\n",
"*archie_abort.leftWidget:       *archie_search\n",
"*archie_abort.leftAttachment:   XmATTACH_WIDGET\n",
"*archie_abort.helpCallback:	Help()\n",
"*archielabel.wcCreate:        	XmLabel\n",
"*archielabel.width:           	140\n",
"*archielabel.x:           	5\n",
"*archielabel.alignment:       	XmALIGNMENT_END\n",
"*archielabel.labelString:     	Archie Search Item:\n",
"*archielabel.Alignment:       	XmALIGNMENT_BEGINNING\n",
"*archielabel.topWidget:       	*archie_menubar\n",
"*archielabel.topAttachment:   	XmATTACH_WIDGET\n",
"*archielabel.leftWidget:        *archie_abort\n",
"*archielabel.leftAttachment:    XmATTACH_WIDGET\n",
"*archielabel.recomputeSize:   	FALSE\n",
"*archietext.wcCreate:  	      	XmTextField\n",
"*archietext.wcCallback:       	Register_archie(search_text) \n",
"*archietext.width:            	700\n",
"*archietext.editable:         	TRUE\n",
"*archietext.topWidget:        	*archie_menubar\n",
"*archietext.topAttachment:      XmATTACH_WIDGET\n",
"*archietext.leftWidget:         *archielabel\n",
"*archietext.leftAttachment:     XmATTACH_WIDGET\n",
"*archietext.rightAttachment:    XmATTACH_FORM\n",
"*archie_lw.wcCreate:		MyListSW\n",
"*archie_lw.wcCallback:          \n",
"*archie_lw.topWidget:           *archietext\n",
"*archie_lw.height:              300\n",
"*archie_lw.width:           	700\n",
"*archie_lw.labelw:              TRUE\n",
"*archie_lw.topAttachment:       XmATTACH_WIDGET\n",
"*archie_lw.rightAttachment:     XmATTACH_FORM\n",
"*archie_lw.leftAttachment:      XmATTACH_FORM\n",
"*archie_lw.bottomAttachment:    XmATTACH_FORM\n",
"*archie_lw.borderWidth:         1\n",
"*archie_lw.topOffset:           2\n",
"*archie_lw.list.columnSpacing:  0\n",
"*archie_lw.list.defaultColumns: 1\n",
"*archie_lw.list.Callback:       archie_notify\n",
"*archie_lw.list.borderWidth:    0\n",
"*archie_lw.list.columnSpacing:  0\n",
"*archie_lw.list.rowSpacing:     0\n",
"*archie_lw.helpCallback:        help()\n",
"*help_title.wcCreate:       	XmLabel\n",
"*help_title.Alignment:          XmALIGNMENT_BEGINNING\n",
"*Shellhelp.wcConstructor:       XtCreateTopLevelShell\n",
"*Shellhelp.wcChildren:        	helplayout\n",
"*Shellhelp.title:            	Help\n",
"*Shellhelp.iconName:           	Mftp\n",
"*Shellhelp*deleteResponse:      unmap\n",
"*ShellHelp.helpCallback:	Help()\n",
"*helplayout.wcCreate:        	XmForm\n",
"*helplayout.wcChildren:       	menuBar2, \
				help_quit, help_title, help_text\n",
"*menuBar2.wcCreate:             XmCreateMenuBar\n",
"*menuBar2.leftAttachment:       XmATTACH_FORM\n",
"*menuBar2.rightAttachment:      XmATTACH_FORM\n",
"*menuBar2.topAttachment:        XmATTACH_FORM\n",
"*menuBar2.wcChildren:           Do_help_options, helpmenu\n",
"*Do_help_options.wcConstructor: XmCreateCascadeButton\n",
"*Do_help_options.labelString:   Selections\n",
"*Do_help_options.mnemonic:      S\n",
"*Do_help_options.helpCallback:  Help()\n",
"*helpmenu.wcConstructor:       	XmCreatePulldownMenu\n",
"*helpmenu.wcCallback:          	\
	WcSetValue(*Do_help_options.subMenuId: *helpmenu)\n",
"*helpmenu.wcManaged:           	FALSE\n",
"*help_sub.wcConstructor: 	XmCreateCascadeButton\n",
"*help_menu.wcConstructor:      	XmCreatePulldownMenu\n",
"*help_menu_title.wcConstructor: XmCreateLabelGadget\n",
"*help_menu_title.labelString:   Help Menu\n",
"*helpmenuline.wcConstructor:    XmCreateSeparatorGadget\n",
"*helpmenuline.sepatorType:      SINGLE_LINE\n",
"*helpmenu1.wcConstructor:       XmCreateCascadeButtonGadget\n",
"*helpmenu1.wcCallback:          SetHelpTitle()\n",
"*helpmenu1.labelString:         \\ \n",
"*helpmenu1.activateCallback:    help_by_title(), help_once\n",
"*help_quit.wcCreate:		XmPushButton\n",
"*help_quit.labelString:     	Hide\n",
"*help_quit.activateCallback: 	WcPopDownCB(~)\n",
"*help_quit.helpCallback:	Help()\n",
"*help_quit.topWidget:        	*menuBar2\n",
"*help_quit.topAttachment:   	XmATTACH_WIDGET\n",
"*help_title.wcCreate:       	XmLabel\n",
"*help_title.Alignment:          XmALIGNMENT_BEGINNING\n",
"*help_title.width:		370\n",
"*help_title.labelString:	\\ \n",
"*help_title.leftWidget:         *help_quit\n",
"*help_title.leftAttachment:  	XmATTACH_WIDGET\n",
"*help_title.topWidget:        	*menuBar2\n",
"*help_title.topAttachment:   	XmATTACH_WIDGET\n",
"*help_title.recomputeSize:	FALSE\n",
"*help_title.y:			4\n",
"*help_title.topOffset:		4\n",
"*help_title.helpCallback:	Help()\n",
"*help_text.wcConstructor:     	XmCreateScrolledText\n",
"!*help_text.wcCallback:          set_help\n",
"*help_text.rows:		10\n",
"*help_text.editable:		FALSE\n",
"*help_text.editMode:		XmMULTI_LINE_EDIT\n",
"*help_text.helpCallback:	Help()\n",
"*help_textSW.width:             630\n",
"*help_textSW.hight:             400\n",
"*help_textSW.topWidget:       	*help_title\n",
"*help_textSW.topOffset:		4	\n",
"*help_textSW.y:			4\n",
"*help_textSW.leftAttachment:    XmATTACH_FORM\n",
"*help_textSW.rightAttachment:   XmATTACH_FORM\n",
"*help_textSW.topAttachment:   	XmATTACH_WIDGET\n",
"*help_textSW.bottomAttachment:	XmATTACH_FORM\n",
"*Shelltran.wcConstructor:       XtCreateTopLevelShell\n",
"*Shelltran.wcChildren:          tran_layout\n",
"*Shelltran.title:               Remote File Translations Examples\n",
"*Shelltran.iconName:           	Mftp\n",
"*Shelltran*deleteResponse:   	unmap\n",
"*Shelltran.helpCallback:	Help()\n",
"*tran_layout.wcCreate:          XmForm\n",
"*tran_layout.wcChildren:       	tran_quit, tran_text\n",
"*tran_quit.wcCreate:		XmPushButton\n",
"*tran_quit.labelString:     	Hide\n",
"*tran_quit.activateCallback: 	WcPopDownCB(~)\n",
"*tran_quit.helpCallback:	Help()\n",
"*tran_text.wcConstructor:     	XmCreateScrolledText\n",
"*tran_text.wcCallback:          set_tran\n",
"*tran_text.rows:		10\n",
"*tran_text.editMode:		XmMULTI_LINE_EDIT\n",
"*tran_text.editable:		FALSE\n",
"*tran_text.helpCallback:	Help()\n",
"*tran_textSW.width:             500\n",
"*tran_textSW.hight:             400\n",
"*tran_textSW.topWidget:       	*tran_quit\n",
"*tran_textSW.topOffset:		4	\n",
"*tran_textSW.y:			4\n",
"*tran_textSW.topWidget:       	*tran_quit\n",
"*tran_textSW.leftAttachment:    XmATTACH_FORM\n",
"*tran_textSW.rightAttachment:    XmATTACH_FORM\n",
"*tran_textSW.topAttachment:   	XmATTACH_WIDGET\n",
"*tran_textSW.bottomAttachment:	XmATTACH_FORM\n",
"*Shellstatus.wcConstructor:	XtCreateTopLevelShell\n",
"*Shellstatus.wcChildren:	status_layout\n",
"*Shellstatus.title:		Status Message Log\n",
"*Shellstatus.iconName:         	Mftp\n",
"*Shellstatus*deleteResponse:    unmap\n",
"*Shellstatus.helpCallback:	Help()\n",
"*status_layout.wcCreate:	XmForm\n",
"*status_layout.wcChildren:	status_quit, status_clear, status_text\n",
"*status_quit.wcCreate:		XmPushButton\n",
"*status_quit.labelString:     	Hide\n",
"*status_quit.activateCallback: 	WcPopDownCB(~)\n",
"*status_quit.helpCallback:	Help()\n",
"*status_clear.wcCreate:		XmPushButton\n",
"*status_clear.labelString:     	Clear Text\n",
"*status_clear.activateCallback: Clear_Text(*status_text)\n",
"*status_clear.helpCallback:	Help()\n",
"*status_clear.leftWidget:	*status_quit\n",
"*status_clear.leftAttachment:  	XmATTACH_WIDGET\n",
"*status_text.wcConstructor: 	XmCreateScrolledText\n",
"*status_text.rows:		10\n",
"*status_text.editMode:		XmMULTI_LINE_EDIT\n",
"*status_text.autoShowCursorPosition: TRUE\n",
"*status_text.editable:		FALSE\n",
"*status_text.helpCallback:	Help()\n",
"*status_textSW.width:           500\n",
"*status_textSW.hight:           400\n",
"*status_textSW.topWidget:      	*status_quit\n",
"*status_textSW.topOffset:	4	\n",
"*status_textSW.y:		4\n",
"*status_textSW.leftAttachment:	XmATTACH_FORM\n",
"*status_textSW.rightAttachment:	XmATTACH_FORM\n",
"*status_textSW.topAttachment:	XmATTACH_WIDGET\n",
"*status_textSW.bottomAttachment: XmATTACH_FORM\n",
"*Shellcommand.wcConstructor:    XtCreateTopLevelShell\n",
"*Shellcommand.wcChildren: 	command_layout\n",
"*Shellcommand.title:      	Ftp command \n",
"*Shellcommand.iconName:        	Mftp\n",
"*Shellcommand*deleteResponse:   unmap\n",
"*Shellcommand.helpCallback:	Help()\n",
"*command_layout.wcCreate:	XmForm\n",
"*command_layout.wcChildren: 	ftp, command_quit, command_clear\n",
"*command_quit.wcCreate:		XmPushButton\n",
"*command_quit.wcCallback: \
   WcSetValue(*ftpSW.topWidget:       	*command_quit) \
   WcSetValue(*ftpSW.leftAttachment:	XmATTACH_FORM) \
   WcSetValue(*ftpSW.rightAttachment:	XmATTACH_FORM) \
   WcSetValue(*ftpSW.topAttachment:	XmATTACH_WIDGET) \
   WcSetValue(*ftpSW.bottomAttachment: 	XmATTACH_FORM)\n",
"*command_quit.labelString:     	Hide\n",
"*command_quit.activateCallback: WcPopDownCB(~)\n",
"*command_quit.helpCallback:	Help()\n",
"*command_clear.wcCreate:	XmPushButton\n",
"*command_clear.labelString:    	Clear Text\n",
"*command_clear.activateCallback: Clear_Text(*ftp)\n",
"*command_clear.helpCallback:	Help()\n",
"*command_clear.leftWidget:	*command_quit\n",
"*command_clear.leftAttachment:  XmATTACH_WIDGET\n",
"*ftp.wcConstructor:     	XmCreateScrolledText\n",
"*ftp.wcCallback:		noop(get put dir action connect notconnected) \
                                help_register \
				set_width(1)\n",
"*ftp.helpCallback:		Help()\n",
"*ftp.rows:			10\n",
"*ftp.editMode:			XmMULTI_LINE_EDIT\n",
"*ftp.translations:     		#override \
	<Key>osfBackSpace: InsertSpace() delete-previous-character() \\n\
	<Key>Return:    end-of-file() newline() Dispatch() \\n\
	<Key>osfHelp:   PrimitiveHelp()\\n\
        Shift ~Meta ~Alt <Key>Tab:     prev-tab-group()\\n\
        ~Meta ~Alt <Key>Tab:           next-tab-group()\\n\
	<Key>:          end-of-file() self-insert() \n",
"*ftpSW.hight:                 	400\n",
"*ftpSW.helpCallback:     	Help()\n",
"*ftpSW.topOffset:		4	\n",
"*ftpSW.y:			4\n",
"*Shellview.wcConstructor:       XtCreateTopLevelShell\n",
"*Shellview.wcCallback:       	help_register\n",
"*Shellview.wcChildren:          view_layout\n",
"*Shellview.title:               View File\n",
"*Shellview.iconName:           	Mftp\n",
"*Shellview*deleteResponse:   	destroy\n",
"*Shellview.helpCallback:	Help()\n",
"*view_layout.wcCreate:          XmForm\n",
"*view_layout.wcChildren:       	view_quit, view_text\n",
"*view_quit.wcCreate:		XmPushButton\n",
"*view_quit.labelString:     	Dismiss\n",
"*view_quit.activateCallback:    WcDestroyCB(~)\n",
"*view_quit.helpCallback:	Help()\n",
"*view_text.wcConstructor:     	XmCreateScrolledText\n",
"*view_text.wcCallback:          set_view_file \
				set_view_text_top(^view_quit)\n",
"*view_text.rows:		15\n",
"*view_text.editMode:		XmMULTI_LINE_EDIT\n",
"*view_text.editable:		FALSE\n",
"*view_text.helpCallback:	Help()\n",
"*view_textSW.topOffset:		3\n",
"*view_textSW.width:             600\n",
"*view_textSW.leftAttachment:    XmATTACH_FORM\n",
"*view_textSW.rightAttachment:   XmATTACH_FORM\n",
"*view_textSW.topAttachment:     XmATTACH_WIDGET\n",
"*view_textSW.bottomAttachment:	XmATTACH_FORM\n",
"*help_General.help_text:\
General Help\\n\
XXXX is a X front end to ftp.\\n\
\\n\
XXXX allows retrieval  or  transmission  of  selected  files  and\\n\
directory trees.\\n\
\\n\
The screen display for XXXX consists of 5 sections:  a  menu  bar\\n\
containing  a  quit  menu,   option menu, file option menu, mutli\\n\
file option menu, and help menu; a status window; a  remote/local\\n\
directory window; a series of buttons login,  remote/local direc-\\n\
tory, command, glob, search, next, reconnect and  archie;  and  a\\n\
scrolled list window.\\n\
\\n\
The status window display the current actions and error messages.\\n\
\\n\
The remote/local directory window display the  remote/local  name\\n\
of the displayed directory.\\n\
\\n\
The login button is used to initiate logins.\\n\
\\n\
The remote/local button toggles between remote and  local  direc-\\n\
tory display's.\\n\
\\n\
The command shell button is used to bring up a shell window  that\\n\
contains a direct interface to ftp.\\n\
\\n\
The glob button is used to select a set of files based  on  shell\\n\
glob syntax or regular expression syntax through a dialog.\\n\
\\n\
The search button is used to find a file or set of files .  based\\n\
on  shell glob syntax or regular expression syntax through a dia-\\n\
log.\\n\
\\n\
The next button will find the next file based on the glob a regu-\\n\
lar expression set by the search button.\\n\
\\n\
The reconnect button will  restart  the  ftp  session  after  the\\n\
foreign host has disconnected due to a inactivity disconnect.\\n\
\\n\
The archie command will bring up a dialog to run a archie command\\n\
if the archie command is in the users search path.\\n\
\\n\
All buttons and menu selections are done with the left mouse but-\\n\
ton.\\n\
\\n\
A file can be selected by clicking the left mouse button  on  the\\n\
file.  Multi file selection are accomplished by clicking the left\\n\
mouse button on the first file and then dragging the mouses  over\\n\
the  files  to  be  selected.   Selected  files  are displayed in\\n\
reverse video.  The current selection has a square border  around\\n\
it.\\n\
\\n\
The scrolled list window has a popup menu that can  be  activated\\n\
by holding down the right mouse button. You can also use the key-\\n\
board to select the listing options, local/remote  display,  sort\\n\
options,  files  or directories, and actions to apply to selected\\n\
files.\\n\
\\n\
You can click the left mouse button with the control key  pressed\\n\
on  a  directory to cd to it.  If you click the left mouse button\\n\
with the control key pressed on a file and it is  a  remote  file\\n\
then the file will be transferred to the local host or if it is a\\n\
local file then it is transferred to the remote host.\n",
"*netrc.help_text:\
moxftprc or netrc\\n\
XXXX will look for  ~/.moxftprc if not found then  it  will  look\\n\
for  ~/.netrc.   The format of of \".moxftprc\" is the same as that\\n\
of \".netrc\"  with  the  addition  of  three  new   tokens  called\\n\
\"remote_dir\", \"local_dir\", and \"note\".  \"note\" should be the last\\n\
token of a entry.\\n\
\\n\
It is not advisable to put  your  password  in  the  \".netrc\"  or\\n\
\".moxftprc\" files.\\n\
example:\\n\
machine ftp.chpc.utexas.edu\\n\
 login anonymous\\n\
 password jones@\\n\
 note Home of xmoftp\\n\
machine ftp.utexas.edu\\n\
 login anonymous\\n\
 password jones@\\n\
 remote_dir /packages/X\\n\
 note Lots of Networking Information\n",
"*xftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by xftp are defined by the following resources:\\n\
 Xftp*font:\\\n\
     -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Command.font:\\\n\
    -*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Text*font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Label*font:\\\n\
    -*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*LabelQUICK*font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*listsw*list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*hostsw*list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*oftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by oftp are defined by the following resources:\\n\
 Oftp*font:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Oftp*listsw.*.list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Oftp*hostsw.*.list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*mftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by mftp are defined by the following resources:\\n\
 Mftp*labelFontList:\\\n\
-*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*buttonFontList:\\\n\
-*-times-medium-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*textFontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar1*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar2*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*archie_menubar*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*rate.fontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp.*.*.list.fontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*quit.help_text:\
Quit Button\\n\
Quit XXXX. Any pending actions will be terminated.\n",
"*listsw*help_text:\
Directory List Window\\n\
The current  local/remote directory  listing is  displayed  here.\\n\
There  are  four  types of listing formats sort, medium, long and\\n\
translations.  It is not always possible for there to be  a  long\\n\
or medium listing format for a remote system.   In such cases the\\n\
short listing format is used.\\n\
\\n\
A file or directory entry can be selected by  clicking  the  left\\n\
most mouse button on the entry.\\n\
\\n\
The right mouse button will select an entry and bring up  a  menu\\n\
with  a  list  of actions that can be applied to that entry.  The\\n\
actions are:\\n\
\\n\
UP      - Go up the directory tree\\n\
Cd      - Cd to selected directory\\n\
Get     - Get remote file\\n\
Put     - Put local file\\n\
View    - View remote file.\\n\
Ascii   - Set transfer mode of file to ascii\\n\
Binary  - Set transfer mode of file to binary\\n\
Tenex   - Set transfer mode to tenix\\n\
Default - Set transfer mode to default\\n\
Ignore  - Ignore file when retriving directory.\\n\
Don't Ignore - Do not ignore file retriving directory.\\n\
Dir     - Refresh directy listing.\\n\
\\n\
The menu also contains actions that can be taken on all  selected\\n\
files:\\n\
\\n\
Clear File Selections - Unselect all files and directories\\n\
Get Selected files    - Get all selected remote files and  direc-\\n\
tories.\\n\
Put Selected files    - Put all selected local files  and  direc-\\n\
tories.\\n\
\\n\
Also see the keyboard input help selection under general help.\n",
"*ftp*help_text:\
Ftp Window\\n\
This is the ftp window.  You can use the following  ftp  commands\\n\
in this window:\\n\
\\n\
 ascii\\n\
 binary\\n\
 delete\\n\
 dir\\n\
 cd <remote directory>\\n\
 help\\n\
 image\\n\
 get <remote file> [<local file>]\\n\
 reg <regular expression>\\n\
 regget\\n\
 regput\\n\
 lcd\\n\
 ls\\n\
 lmkdir\\n\
 lpwd\\n\
 mkdir\\n\
 put\\n\
 pwd\\n\
 quote\\n\
 remotehelp\\n\
 site\\n\
 tenex\n",
"*abort.help_text:\
Abort Button\\n\
Abort ftp. Since there is no reliable way to abort ftp  you  will\\n\
have to login again.\n",
"*op_sort.help_text:\
Option SubMenu - Sort\\n\
The sort option menu can be used to select the type of sort  that\\n\
is  done  on  directories.  Files  can be sorted by age, name, or\\n\
size.  The sort can be reverse or normal sort order.   File  also\\n\
can be sorted by type then age, name, or size.\n",
"*options.help_text:\
Options Menu\\n\
The options menu options, to turn on or of error ignoring  during\\n\
transfers  of  multiple  files,  to turn on or off auto directory\\n\
listing and  two submenus Listing and Sort to change listing for-\\n\
mats  or sort options.  See help on submenus Listing and Sort for\\n\
more information on Listing and Sort submenus.\n",
"*op_listing.help_text:\
Option SubMenu - Listing\\n\
Select the listing options.  There are four list options plus the\\n\
example translations table option:\\n\
\\n\
Short Listing\\n\
Medium Listing\\n\
Long Listing\\n\
Translation Listing\\n\
Translations\\n\
\\n\
The short listing format  displays  the  filename  only.  If  the\\n\
remote  file  system  is an UNIX system then a directory will end\\n\
with \"/\", a link with \"@\" and of offline file with \"%\".   If  the\\n\
remote  file system is not an unix file system then a d is placed\\n\
before the file name to indicate that it is a directory.\\n\
\\n\
The Medium  Listing  format  is  system  dependent.   It  usually\\n\
includes the file length.\\n\
\\n\
The Long Listing format is system dependent.  It usually includes\\n\
the file length, type and protections.\\n\
\\n\
The Translation Listing format will display the remote  to  local\\n\
or  the  local  to  remote  translation  for  the directory being\\n\
displayed.  It also shows the mode the file will  be  transferred\\n\
in.   If XXXX does not know how to translate the filename it will\\n\
leave the translation blank.\\n\
\\n\
The Translations menu option will produce a list of example local\\n\
and remote files and their translations.\n",
"*dir.help_text:\
Directory Window\\n\
The current selected local or remote directory name is  displayed\\n\
here.\n",
"*connect.help_text:\
Login/Close Button\\n\
Login to remote host or close the connection from a remote  host.\\n\
If the option is login, a menu will popup allowing you to set the\\n\
remote host name, the remote host login  name,  the  remote  host\\n\
password, the remote directory name, and the local directory name\\n\
to use at login time.\\n\
\\n\
The retry button informs XXXX to keep retrying connection every 5\\n\
minutes until it is able to log into the remote hosts.\\n\
\\n\
XXXX understands the ftp .netrc file format. It use this to  gen-\\n\
erate  a  menu  that  will  set the hostname, login name, and (if\\n\
specified) the password for the selected host.\\n\
\\n\
A comment for the specified host can be added to the  .xftp  file\\n\
found  in  the  login  directory  using the \"note\" directive; for\\n\
example:\\n\
\\n\
note dinosaur.cc.utexas.edu UTD\\n\
note ftp.uu.net Has most anything that any one would want.\\n\
\\n\
This will be displayed beside the host entry in the host menu.\n",
"*status.help_text:\
Status Window\\n\
Display status information.  Clicking the right mouse  button  on\\n\
the  status  window  will  popup the Status Message Log.  You can\\n\
then view all of the previous status messages.\n",
"*host_name.help_text:\
System Name Window\\n\
The host name of the connected  or  selected  host  is  displayed\\n\
here.\n",
"*system_name.help_text:\
System Type Window\\n\
The System type is displayed here.\n",
"*default_mode.help_text:\
Default Transfer Mode Window\\n\
The default transfer mode is displayed in this window.\n",
"*dir_display.help_text:\
Local/Remote Button\\n\
Toggle between current  local/remote  directories.   A  directory\\n\
listing  is  displayed  of the selected local/remote directory in\\n\
the directory list window.\n",
"*dotxftp.help_text:\
XXXX initialization file\\n\
XXXX reads the \".xftp\" initialization file in the home  directory\\n\
when  it  first starts up.  The \".xftp\" file can contain the fol-\\n\
lowing directives:\\n\
\\n\
trans        <machine type>\\n\
examples_r   <remote file>\\n\
examples_e   <local file>\\n\
unix         <regular expression>\\n\
             <source> [<conversion type>]\\n\
back         <regular expression>\\n\
             <source> [<conversion type>]\\n\
end\\n\
viewer       <audio|ps|picture|tar|text> <comand>\\n\
\\n\
The note directive allows you add a note that is displayed in the\\n\
host  list  menu  in the login window.  It is used in conjunction\\n\
with the \"~/.netrc\" file.\\n\
\\n\
The trans directive start a translation table block of  commands.\\n\
You  can  only  specify the examples_r, examples_e, unix and back\\n\
directive in a translation table block.  The end  directive  ends\\n\
the translation table block.\\n\
\\n\
The examples_r and examples_e directives are used to  generate  a\\n\
example of the translations specified by the unix and back direc-\\n\
tives.\\n\
\\n\
The unix and back directive are used to specific rewriting  rules\\n\
for translating file form the remote system file name to unix and\\n\
back.  You can specify \"ascii\", \"binary\" and \"tenex\" as  <conver-\\n\
sion type>\\n\
\\n\
The examples_r, examples_e,  unix  and  back  directives  can  be\\n\
repeated 50 times each.\\n\
\\n\
The following is example of a translation table  that  you  might\\n\
want for a Vax VMS system running MULTINET.\\n\
\\n\
trans        VMS MULTINET\\n\
examples_r   XFTP_TAR.Z;1\\n\
unix         ([a-z0-9_,]+)_TAR.Z;[0-9]+\\n\
             1.tar.Z binary\\n\
examples_e   xftp.tar.Z\\n\
back         ([A-Z0-9_,]+).tar.Z\\n\
             1_TAR.Z  binary\\n\
end\\n\
\\n\
The unix directive specifies a regular expression to apply to the\\n\
remote  file  name.   If  it matches then the string \"1.tar.Z\" is\\n\
used as the source  to  rewrite  the  file  name.  This  examples\\n\
translate  \"XFTP_TAR.Z;1\"  to the unix file name \"xftp.tar.Z\" and\\n\
specifies that the file is to be transferred in binary mode.\\n\
\\n\
The back directive specifies a regular expression to apply to the\\n\
local unix file.  If it matches then the string \"1_TAR.Z\" is used\\n\
as  the  source  to  rewrite  the  file  name.   The  unix   file\\n\
\"xftp.tar.Z\" should be rewritten as \"XFTP_TAR.Z\".  The file would\\n\
be transferred in binary mode.\\n\
\\n\
The viewer directive spicfies a program  to  execute  to  view  a\\n\
audio, postscript, tar, text and picture files.  XXXX regogonizes\\n\
the filename  extensions  .aiff  and  .au  as  audio  files;  the\\n\
filename   extensions .gif, .tiff, .rgp and .jpg as pictures; the\\n\
the filename extesions .ps as postscript; and the filname  exten-\\n\
sion\\n\
example:\\n\
viewer ps ghostview\\n\
viewer text xless\\n\
viewer pitcure xv\n",
"*list_key_input.help_text:\
Keyboard Input\\n\
The Directory List Window allows the following keyboard input.\\n\
\\n\
   <Key>Help:           Help Menu\\n\
   <Key>F1:             Help Menu\\n\
\\n\
  ~Ctrl ~Shift <Key>h:  Previous item\\n\
  ~Ctrl ~Shift <Key>j:  Down one item\\n\
  ~Ctrl ~Shift <Key>k:  Up one item\\n\
   Ctrl ~Shift <Key>l:  Next item\\n\
\\n\
  ~Ctrl ~Shift <Key>0:  Fisrt item in line\\n\
   Ctrl ~Shift <Key>$:  Last item in line\\n\
\\n\
   Ctrl ~Shift <Key>f:  Next page\\n\
   Ctrl ~Shift <Key>b:  Previous page\\n\
   Ctrl ~Shift <Key>n:  Down one item\\n\
   Ctrl ~Shift <Key>p:  Up one item\\n\
\\n\
   Ctrl ~Shift <Key>j:  Down one item\\n\
  ~Ctrl  Shift <Key>m:  Down one item\\n\
\\n\
           <Key>space:  Select item\\n\
\\n\
   Ctrl ~Shift <Key>t:  Toggle to remote/local directory\\n\
\\n\
  ~Ctrl  Shift <Key>l:  Set long listing format\\n\
  ~Ctrl  Shift <Key>s:  Set short listing format\\n\
  ~Ctrl  Shift <Key>t:  Set translation listing format\\n\
\\n\
  ~Ctrl  <Key>>:        Next page\\n\
  ~Ctrl  <Key><:  Previous page\\n\
   Ctrl  <Key>>:        Bottom\\n\
   Ctrl  <Key><:     Top\\n\
\\n\
  ~Ctrl ~Shift <Key>a:Set file transfer mode to type Ascii\\n\
  ~Ctrl ~Shift <Key>b:Set file transfer mode to type binary\\n\
  ~Ctrl ~Shift <Key>t:Set file transfer mod to tenex\\n\
  ~Ctrl ~Shift <Key>d:Use default transfer mode\\n\
\\n\
  ~Ctrl ~Shift <Key>u:Go to parent directory\\n\
  ~Ctrl ~Shift <Key>c:Change dir to directory\\n\
\\n\
  ~Ctrl ~Shift <Key>g:Get file\\n\
  ~Ctrl ~Shift <Key>p:Put file\\n\
\\n\
   Ctrl        <Key>s:  Search Next\\n\
   Ctrl        <Key>g:  Clear Search Pattern\n",
"*quitm.help_text:\
Quit Menu\\n\
The quit menu contains the abort and quit options.\\n\
\\n\
Since there is no reliable way to abort  ftp  you  will  have  to\\n\
login again after aborting a ftp connection.\n",
"*items.help_text:\
Display Items\\n\
The item display display the count of the following items,  block\\n\
devices,  char   devices,  links,  sockets, files, offline_files,\\n\
selected items, and the total number of items.\n",
"*command.help_text:\
Command Button\\n\
The command button brings up the command shell.  Commands can  be\\n\
given directly to ftp through this shell.\n",
"*hide.help_text:\
Hide Shell\\n\
Hide the current shell.\n",
"*help_quit.help_text:\
Hide Shell\\n\
Hide the help shell.\n",
"*tran_quit.help_text:\
Hide Shell\\n\
Hide the translation shell.\n",
"*status_quit.help_text:\
Hide Shell\\n\
Hide the status shell.\n",
"*command_quit.help_text:\
Hide Shell\\n\
Hide the command shell.\n",
"*Shellconnect.help_text:\
Connect Shell\\n\
Used to specify login  information,  remote  host,  user  number,\\n\
password, local directory and remote directory for XXXX.\n",
"*hosts.help_text:\
Host List Menu\\n\
List of host found in $HOME/.netrc.\n",
"*anonymous.help_text:\
Anonymous login menu\\n\
Can be used to set the login user anonymous and initial password.\\n\
The password can be set to guest, mail address, or user name.\n",
"*DoBoxConnect.help_text:\
Connect button\\n\
Initiate connection.\n",
"*DoHide.help_text:\
Hide Shell\\n\
Hide the connect shell.\n",
"*Shellhelp.help_text:\
Help Shell\\n\
Display text of help message.\n",
"*Shelltran.help_text:\
Translation Shell\\n\
Display translations used with non UNIX systems.\n",
"*Shellstatus.help_text:\
Status Shell\\n\
Display log of status messages.\n",
"*Shellcommand.help_text:\
Ftp Command Shell\\n\
The ftp command shell.\n",
"*Shellview.help_text:\
View Shell\\n\
Shell window brought up to view a text file. If the file ends  in\\n\
.Z  it  will  be uncompressed before viewing if uncompress is the\\n\
users path.  If the file ends in .gz it will unzip if  gunzip  is\\n\
in the users path.\n",
"*Trademarks.help_text:\
Trademarks\\n\
OPEN LOOK is a trademark of AT&T\\n\
UNIX is a registered trademark of AT&T\\n\
The X Window System is a trademark of the Massachusetts Institute\\n\
of Technology.\n",
"*helpm.help_text:\
Help Menu\\n\
The help menu provides a context sensitive help selection  and  a\\n\
general help selection.\\n\
\\n\
If you select the context sensitive  help  selection  the  cursor\\n\
will  change  to  a  cross bar.  You can then position the cursor\\n\
over the object that you want help on and click left  most  mouse\\n\
botton.   If  the  help  system  knows  about  the object it will\\n\
display the help text in the help shell. If it does not  it  will\\n\
display the general help message in the help shell.\n",
"*fileopts.help_text:\
Single File Options Menu\\n\
The single file options menu allows the following  operations  on\\n\
the high lighted file:\\n\
\\n\
 Up           - cd to parent directory\\n\
 Cd           - cd to high lighted directory\\n\
 Get          - get high lighted file or directory\\n\
 View         - view high lighted file\\n\
 Put          - put high lighted file or directory\\n\
 Ascii        - transfer high lighted file in ascii mode\\n\
 Binary       - transfer high lighted file in binary mode\\n\
 Tenex        - transfer high lighted file in tenex mode\\n\
 Default      - transfer high lighted using default transfer mode\\n\
 Ignore       - ignore  high lighted directory/file when\\n\
                transferring contents of a directory\\n\
 Don't ignore - don't ignore high lighted directory/file when\\n\
                transferring contents of a directory\n",
"*filesopts.help_text:\
Multi File Options Menu\\n\
The multi file options menu allows the  following  operations  on\\n\
the selected files:\\n\
\\n\
Clear File Selections - Clear all file selections in current\\n\
        directory\\n\
Get Selected Files    - Get selected file in current directory\\n\
Put Selected Files    - Put selected file in current directory\n",
"*archie_command.help_text:\
Archie Button\\n\
The archie button brings up arche interface shell.\n",
"*DoArchie.help_text:\
Archie Button\\n\
The archie button brings up arche interface shell.\n",
"*DoGateway.help_text:\
Gateway Button\\n\
Enable suns passthrough ftp gateway.\\n\
slag The Search Host List Dialog is activated  by  the  following\\n\
keys in the host list window:\\n\
   Ctrl        <Key>s:  Search Next\\n\
   Ctrl        <Key>g:  Clear Search Pattern\\n\
slag *Shellsearchhostdialog.help_text:\\n\
Search Host List Dialog\\n\
Set search string for regular expression search of the host  list\\n\
in the Connect Shell.\\n\
The Search Host List Dialog has the following keyboard input:\\n\
  <Key>Return:      Start search\\n\
   Ctrl<Key>r:      Start search\\n\
   Ctrl<Key>c:      Abort search\n",
"*Shellsearchdialog.help_text:\
Search Dialog\\n\
Set search string for regular expression  search  or  shell  glob\\n\
search of file.\\n\
The Search Dialog has the following keyboard input:\\n\
   <Key>Return:    Start glob search\\n\
   Ctrl<Key>r:     Start regualar expression search\\n\
   Ctrl<Key>g:     Start glob search\\n\
   Ctrl<Key>c:     Abort Search\n",
"*Shellglobdialog.help_text:\
Glob Dialog\\n\
Select files based on shell glob expression  or  regular  expres-\\n\
sions.\n",
"*reconnect.help_text:\
Recconect Button\\n\
The recconect button allows the continuation of ftp session after\\n\
the server has disconnected the seesion.\n",
"*glob.help_text:\
Glob Button\\n\
The Glob button will bring up a glob dialog which will allow  the\\n\
selection/deselection of files based on a regular expression or a\\n\
shell glob expression  search.\n",
"*search.help_text:\
Search Button\\n\
The Search button will bring up a search  dialog  to  search  the\\n\
current  directory   for the specified item.   The  search can be\\n\
based on regular expression  or shell globing.  The  Search  Next\\n\
button  will  search  for  the next item that matches the regular\\n\
expression or shell glob.\n",
"*next.help_text:\
Search Next Button\\n\
Search for the next item that matches the regular  expression  or\\n\
shell globing expression.\n",
NULL,
};
#endif
#if defined(XAW)
String fallback_resources[] = {
"Xftp.wcChildren:		layout	\n",
"Xftp.wcPopups:			Shellhelp, Shellconnect, Shelltran, \
				Shellstatus, Shellcommand\n",
"Xftp*wcTrace:			FALSE\n",
"Xftp.title:			Xftp\n",
"*.shapeStyle:			Rectangle\n",
"*.font:			       -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*Command.font:                 -*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*Text*font:                    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*Label*font:		       -*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*LabelQUICK*font:	       -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*listsw*list.font:             -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*hostsw*list.font:             -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*width_in_chars:		90\n",
"*translations: 	#override \
		<Key>Help:      help() \\n\
		<Key>F1:        help()  \n",
"*layout.wcCreate: 		formWidgetClass\n",
"*layout.wcChildren: 		quitm, options, \
				    fileopts, filesopts, helpm, \
				status, \
				dir, \
				host_name, system_name, default_mode, \
			        connect, dir_display, command, glob, \
				search, next, reconnect, archie_command, rate,\
				percent, \
				listsw\n",
"*layout.defaultDistance:	2\n",
"*quitm.wcCreate: 	   	menuButtonWidgetClass\n",
"*quitm.wcCallback:        	help_register\n",
"*quitm.label: 			Quit\n",
"*quitm.shapeStyle:		Rectangle\n",
"*quitm.right: 			chainleft\n",
"*quitm.left: 	        	chainleft\n",
"*quitm.bottom: 			chaintop\n",
"*quitm.top: 			chaintop\n",
"*quitm.wcPopups:		qmenu\n",
"*quitm.menuName:          	qmenu\n",
"*quitm.horizDistance:		4\n",
"*quitm.translations:          	#override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \n",
"*qmenu.wcCreate:		simpleMenuWidgetClass\n",
"*qmenu.wcManged:		False\n",
"*qmenu.wcChildren:	        quit, abort\n",
"*quit.wcCreate:			SmeBSB\n",
"*quit.label:			Quit\n",
"*quit*callback:			quit\n",
"*abort.wcCreate:		SmeBSB\n",
"*abort.label:			Abort\n",
"*abort*callback:		abort\n",
"*options.wcCreate:		menuButtonWidgetClass\n",
"*options.wcCallback:		noop(get put dir connect action Sensitive) \
				help_register\n",
"*options.wcPopups:		op_menu\n",
"*options.menuName:          	op_menu\n",
"*options.label: 		Options\n",
"*options.shapeStyle:		Rectangle\n",
"*options.right: 		chainleft\n",
"*options.left: 		        chainleft\n",
"*options.bottom: 		chaintop\n",
"*options.top: 			chaintop\n",
"*options.fromHoriz:		*quitm\n",
"*options.horizDistance:		4\n",
"*options.translations:          #override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \n",
"*op_menu.wcCreate:		simpleMenuWidgetClass\n",
"*op_menu.wcManged:		False\n",
"*op_menu.wcChildren:	        op_line, op_1, op_2, op_line, op_3, op_4\n",
"*op_menu.wcPopups:		lmenu, smenu\n",
"*op_menu.label:			Options\n",
"*op_menu.*.leftMargin:          20\n",
"*op_menu.translations:  	#override \\n\
    <BtnMotion>: 		highlight() menu_cascade() \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \\n\
    <LeaveWindow>:		unhighlight() \n",
"*op_1.wcCreate:			SmeBSB\n",
"*op_1.label:			Ignore Errors\n",
"*op_1*callback:			op(IgnoreErrors)\n",
"*op_2.wcCreate:			SmeBSB\n",
"*op_2.WcCallback:		op(NoAutoDir)\n",
"*op_2.label:			No Auto Directy Listings\n",
"*op_2*callback:			op(NoAutoDir)\n",
"*op_line.wcCreate:		SmeLine\n",
"*op_3.wcCreate:			SmeBSB\n",
"*op_3.wcCallback:		bind_menu(*lmenu)\n",
"*op_3.label:			Listing Options\n",
"*op_4.label:			Sort Options\n",
"*op_4.wcCreate:			SmeBSB\n",
"*op_4.wcCallback:		bind_menu(*smenu)\n",
"*lmenu.wcCreate:		simpleMenuWidgetClass\n",
"*lmenu.wcManged:		False\n",
"*lmenu.wcChildren:	        lmenuline, lmenu1, lmenu2, \
				           lmenu3, lmenu4, \
				           lmenuline, lmenu5\n",
"*lmenu.translations:  		#override \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \n\n",
"<LeaveWindow>:		unhighlight() \n",
"*lmenu.*.leftMargin:            20\n",
"*lmenuline.wcCreate:		SmeLine\n",
"*lmenu1.wcCreate:		SmeBSB\n",
"*lmenu1.wcCallback:             toggle(listing, SHORT) \
			        mark_menu()\n",
"*lmenu1.label:			Short listing\n",
"*lmenu1*callback:		listing_type(SHORT)\n",
"*lmenu2.wcCreate:		SmeBSB\n",
"*lmenu2.wcCallback:             toggle(listing, MEDIUM)\n",
"*lmenu2.label:			Medium listing\n",
"*lmenu2*callback:		listing_type(MEDIUM)\n",
"*lmenu3.wcCreate:		SmeBSB\n",
"*lmenu3.wcCallback:             toggle(listing, LONG)\n",
"*lmenu3.label:			Long listing\n",
"*lmenu3*callback:		listing_type(LONG)\n",
"*lmenu4.wcCreate:		SmeBSB\n",
"*lmenu4.wcCallback:             toggle(listing, TRANSLATIONS)\n",
"*lmenu4.label:			Translation  listing\n",
"*lmenu4*callback:		listing_type(TRANSLATIONS)\n",
"*lmenu5.wcCreate:		SmeBSB\n",
"*lmenu5.label:			Translations\n",
"*lmenu5*callback:		List_Translations\n",
"*smenu.wcCreate:		simpleMenuWidgetClass\n",
"*smenu.wcManged:		False\n",
"*smenu.wcChildren:	        smenuline, \
				smbyname, smbysize, smbyage, smenuline, \
				smbytype, smnormal\n",
"*smenu.justify:			center\n",
"*smenu.translations:  		#override \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \\n\
    <LeaveWindow>:		unhighlight() \n",
"*smenu.*.leftMargin:		20\n",
"*smenuline.wcCreate:		SmeLine\n",
"*smbyname.wcCreate:	        SmeBSB\n",
"*smbyname.wcCallback:		mark_menu() \
				toggle(sort_type, SORT_BY_NAME)\n",
"*smbyname.label:            	Sort By Name\n",
"*smbyname.callback:		listing_type(SORT_BY_NAME)\n",
"*smbysize.wcCreate:	        SmeBSB\n",
"*smbysize.wcCallback:		toggle(sort_type, SORT_BY_SIZE)\n",
"*smbysize.label:            	Sort By Size\n",
"*smbysize.callback:		listing_type(SORT_BY_SIZE)\n",
"*smbyage.wcCreate:	        SmeBSB\n",
"*smbyage.wcCallback:		toggle(sort_type, SORT_BY_AGE)\n",
"*smbyage.label:            	Sort By Age\n",
"*smbyage.callback:		listing_type(SORT_BY_AGE)\n",
"*smbytype.wcCreate:	        SmeBSB\n",
"*smbytype.label:            	Sort By Type\n",
"*smbytype.callback:		listing_type(SORT_BY_TYPE)\n",
"*smnormal.wcCreate:	        SmeBSB\n",
"*smnormal.wcCallback:		mark_menu()\n",
"*smnormal.label:            	Normal\n",
"*smnormal.callback:		listing_type(NORMAL)\n",
"*fileopts.wcCreate: 		menuButtonWidgetClass\n",
"*fileopts.wcCallback:	\
		noop(get put dir connect notconnected action Sensitive) \
		help_register\n",
"*fileopts.label: 		File Options\n",
"*fileopts.shapeStyle:		Rectangle\n",
"*fileopts.right: 		chainleft\n",
"*fileopts.left: 	        chainleft\n",
"*fileopts.bottom: 		chaintop\n",
"*fileopts.top: 			chaintop\n",
"*fileopts.horizDistance:	4\n",
"*fileopts.wcPopups:		fileoptsmenu\n",
"*fileopts.menuName:          	fileoptsmenu\n",
"*fileopts.callback:		Single_File_Actions()\n",
"*fileopts.fromHoriz:		*options\n",
"*fileopts.translations:		#override \
				<Key>Help: help() \\n\
				<Key>F1: help() \\n\
\n",
"*fileoptsmenu.wcCreate:        	simpleMenuWidgetClass\n",
"*fileoptsmenu.wcManged:         False\n",
"*fileoptsmenu.wcChildren:       filem_UP, filem_CD, filem_GET, filem_VIEW, \
                                filem_PUT, filem_Ascii, filem_Binary, \
				filem_Tenex, filem_Default, \
				filem_ignore, filem_use, filem_Dir\n",
"*filem_UP.wcCreate:	        SmeBSB\n",
"*filem_UP.wcCallback:		Register_action(up)\n",
"*filem_UP.justify:		left\n",
"*filem_UP.label:            	Up\n",
"*filem_UP.callback:		SetFileAction_menu()\n",
"*filem_CD.wcCreate:	        SmeBSB\n",
"*filem_CD.wcCallback:		Register_action(cd)\n",
"*filem_CD.justify:		left\n",
"*filem_CD.label:            	Cd\n",
"*filem_CD.callback:		SetFileAction_menu()\n",
"*filem_GET.wcCreate:	        SmeBSB\n",
"*filem_GET.wcCallback:		Register_action(get)\n",
"*filem_GET.justify:		left\n",
"*filem_GET.label:            	Get\n",
"*filem_GET.callback:		SetFileAction_menu()\n",
"*filem_VIEW.wcCreate:	        SmeBSB\n",
"*filem_VIEW.wcCallback:		Register_action(view)\n",
"*filem_VIEW.justify:		left\n",
"*filem_VIEW.label:            	Veiw\n",
"*filem_VIEW.callback:		SetFileAction_menu()\n",
"*filem_PUT.wcCreate:	        SmeBSB\n",
"*filem_PUT.wcCallback:		Register_action(put)\n",
"*filem_PUT.justify:		left\n",
"*filem_PUT.label:            	Put\n",
"*filem_PUT.callback:		SetFileAction_menu()\n",
"*filem_Ascii.wcCreate:		SmeBSB\n",
"*filem_Ascii.wcCallback:        Register_action(ascii)\n",
"*filem_Ascii.label:		Ascii\n",
"*filem_Ascii.callback:         	SetFileAction_menu()\n",
"*filem_Binary.wcCreate:	        SmeBSB\n",
"*filem_Binary.wcCallback:       Register_action(binary)\n",
"*filem_Binary.label:		Binary\n",
"*filem_Binary.callback: 	SetFileAction_menu()\n",
"*filem_Tenex.wcCreate:	        SmeBSB\n",
"*filem_Tenex.wcCallback:        Register_action(tenex)\n",
"*filem_Tenex.label:		Tenex\n",
"*filem_Tenex.callback: 	        SetFileAction_menu()\n",
"*filem_Default.wcCreate:	SmeBSB\n",
"*filem_Default.wcCallback:      Register_action(default)\n",
"*filem_Default.label:		Default\n",
"*filem_Default.callback: 	SetFileAction_menu()\n",
"*filem_ignore.wcCreate:    	SmeBSB\n",
"*filem_ignore.wcCallback:       Register_action(ignore)\n",
"*filem_ignore.label:      	Ignore\n",
"*filem_ignore.Callback: 	SetFileAction_menu()\n",
"*filem_use.wcCreate:       	SmeBSB\n",
"*filem_use.wcCallback:          Register_action(use)\n",
"*filem_use.label:         	Don't ignore\n",
"*filem_use.Callback:    	SetFileAction_menu()\n",
"*filem_Dir.wcCreate:       	SmeBSB\n",
"*filem_Dir.wcCallback:          Register_action(dir)\n",
"*filem_Dir.label:         	Dir\n",
"*filem_Dir.Callback:    	SetFileAction_menu()\n",
"*filesopts.wcCreate: 		menuButtonWidgetClass\n",
"*filesopts.wcCallback:          help_register \
        noop(get put dir connect action Sensitive ifsensitive) \n",
"*filesopts.sensitive:           FALSE\n",
"*filesopts.label: 		Multi File Options\n",
"*filesopts.shapeStyle:		Rectangle\n",
"*filesopts.right: 		chainleft\n",
"*filesopts.left: 	        chainleft\n",
"*filesopts.bottom: 		chaintop\n",
"*filesopts.top: 		chaintop\n",
"*filesopts.horizDistance:	4\n",
"*filesopts.wcPopups:		filesoptsmenu\n",
"*filesopts.menuName:          	filesoptsmenu\n",
"*filesopts.callback:		Single_File_Actions()\n",
"*filesopts.fromHoriz:		*fileopts\n",
"*filesopts.translations:	#override \
				<Key>Help: help() \\n\
				<Key>F1: help() \n",
"*filesoptsmenu.wcCreate:       	simpleMenuWidgetClass\n",
"*filesoptsmenu.wcManged:	False\n",
"*filesoptsmenu.wcChildren:      filesm_CLEAR filesm_GET_ALL filesm_PUT_ALL\n",
"*filesm_CLEAR.wcCreate:	        SmeBSB\n",
"*filesm_CLEAR.wcCallback:	Register_action(clear_all)\n",
"*filesm_CLEAR.justify:		left\n",
"*filesm_CLEAR.label:            Clear File Selections\n",
"*filesm_CLEAR.callback:		SetFileAction_menu()\n",
"*filesm_GET_ALL.wcCreate:       SmeBSB\n",
"*filesm_GET_ALL.wcCallback:	Register_action(get_all)\n",
"*filesm_GET_ALL.justify:	left\n",
"*filesm_GET_ALL.label:         	Get Selected Files\n",
"*filesm_GET_ALL.callback:	SetFileAction_menu()\n",
"*filesm_PUT_ALL.wcCreate:	SmeBSB\n",
"*filesm_PUT_ALL.wcCallback:	Register_action(put_all)\n",
"*filesm_PUT_ALL.justify:	left\n",
"*filesm_PUT_ALL.label:         	Put Selected Files\n",
"*filesm_PUT_ALL.callback:	SetFileAction_menu()\n",
"*helpm.wcCreate:		menuButtonWidgetClass\n",
"*helpm.wcCallback:		help_register\n",
"*helpm.label:			help\n",
"*helpm.shapeStyle:		Rectangle\n",
"*helpm.right:			chainleft\n",
"*helpm.left:			chainleft\n",
"*helpm.bottom:			chaintop\n",
"*helpm.top:			chaintop\n",
"*helpm.fromHoriz:		*filesopts\n",
"*helpm.horizDistance:           4\n",
"*helpm.wcPopups:		helpme\n",
"*helpm.menuName:          	helpme\n",
"*helpm.translations:		#override \
				<Key>Help: help() \\n\
				<Key>F1: help() \n",
"*helpme.wcCreate:		simpleMenuWidgetClass\n",
"*helpme.wcManged:		False\n",
"*helpme.wcChildren:		help_context, help_general\n",
"*help_context.wcCreate:		SmeBSB\n",
"*help_context.label:		Context Sensitive Help\n",
"*help_context.callback:		help(c)\n",
"*help_general.wcCreate:		SmeBSB\n",
"*help_general.label:		General Help\n",
"*help_general.callback:		help()\n",
"*status.wcCreate:		Label\n",
"*status.wcCallback:		help_register set_width(1)\n",
"*status.label:			\n",
"*status.justify:		left\n",
"*status.right: 			chainright\n",
"*status.left: 			chainleft\n",
"*status.bottom: 		chaintop\n",
"*status.top: 			chaintop\n",
"*status.fromVert:		*quitm\n",
"*status.borderWidth: 		2\n",
"*status.translations:           #override \
				<Key>Help: help() \\n\
				<Key>F1: help()  \\n\
                                <Btn1Down>: WcPopupACT(*Shellstatus) \
					    SetStatusIcon()\n",
"*dir.wcCreate:			Label\n",
"*dir.wcCallback:		help_register set_width(1)\n",
"*dir.label:			\n",
"*dir.justify:			left\n",
"*dir.fromVert: 			*status\n",
"*dir.right: 			chainright\n",
"*dir.left: 			chainleft\n",
"*dir.bottom: 		  	chaintop\n",
"*dir.top: 			chaintop\n",
"*dir.borderWidth: 		2\n",
"*host_name.wcCreate:		Label\n",
"*host_name.wcCallback:		help_register set_width(3, -6)\n",
"*host_name.label:			\n",
"*host_name.justify:		left\n",
"*host_name.fromVert: 		*dir\n",
"*host_name.right: 		chainleft\n",
"*host_name.left:		chainleft\n",
"*host_name.bottom: 		chaintop\n",
"*host_name.top: 		chaintop\n",
"*host_name.borderWidth: 	0\n",
"*system_name.wcCreate:		Label\n",
"*system_name.wcCallback:	help_register set_width(3, -6)\n",
"*system_name.label:			\n",
"*system_name.justify:		center\n",
"*system_name.fromVert: 		*dir\n",
"*system_name.fromHoriz:		*host_name\n",
"*system_name.right: 		chainright\n",
"*system_name.left:		chainleft\n",
"*system_name.bottom: 		chaintop\n",
"*system_name.top: 		chaintop\n",
"*system_name.borderWidth: 	0\n",
"*default_mode.wcCreate:		Label\n",
"*default_mode.wcCallback:	help_register set_width(3, -6) \n",
"*default_mode.label:			\n",
"*default_mode.justify:		right\n",
"*default_mode.fromVert: 	*dir\n",
"*default_mode.fromHoriz:	*system_name\n",
"*default_mode.right: 		chainright\n",
"*default_mode.left:		chainright\n",
"*default_mode.bottom: 		chaintop\n",
"*default_mode.top: 		chaintop\n",
"*default_mode.borderWidth: 	0\n",
"*connect.wcCreate: 		commandWidgetClass\n",
"*connect.wcCallback:		help_register help_register(system_list) \
			        help_register(dotxftp) \
				help_register(netrc) \
                                help_register(Trademarks) \
				help_register(xftp_fonts) \
				help_register(list_key_input) \
                                help_register(Shellcommand) \
                                help_register(Shellview) \
                                help_register(Shelltran) \
                                help_register(Shellconnect) \
                                help_register(Shellstatus) \
                                help_register(Shellhelp) \
                                help_register(Shellglobdialog) \
                                help_register(Shellsearchdialog) \
                                help_register(Shellsearchhostdialog) \
                                help_register(op_listing) \
                                help_register(op_sort)\n",
"*connect.label:			Login\n",
"*connect.shapeStyle:		Rectangle\n",
"*connect.callback:		connect_disconnect() Set_noop(connect)\n",
"*connect.fromVert: 		*host_name\n",
"*connect.right: 		chainleft\n",
"*connect.bottom: 		chaintop\n",
"*connect.top: 			chaintop\n",
"*dir_display.wcCreate: 		commandWidgetClass\n",
"*dir_display.wcCallback: \
		noop(get put dir connect notconnected action Sensitive) \
		help_register\n",
"*dir_display.label: 		Remote\n",
"*dir_display.shapeStyle:	Rectangle\n",
"*dir_display.right: 		chainleft\n",
"*dir_display.left: 	        chainleft\n",
"*dir_display.bottom: 		chaintop\n",
"*dir_display.top: 		chaintop\n",
"*dir_display.fromHoriz:		*connect\n",
"*dir_display.fromVert: 		*host_name\n",
"*dir_display.horizDistance:	4\n",
"*dir_display.callback:	        remote_local_toggle\n",
"*command.wcCreate: 		commandWidgetClass\n",
"*command.wcCallback:		help_register\n",
"*command.label:			Command Shell\n",
"*command.shapeStyle:		Rectangle\n",
"*command.fromHoriz:		*dir_display\n",
"*command.fromVert: 		*host_name\n",
"*command.left: 	        	chainleft\n",
"*command.right: 		chainleft\n",
"*command.bottom: 		chaintop\n",
"*command.top: 			chaintop\n",
"*command.horizDistance:		4\n",
"*command.callback:		WcPopupCB(*Shellcommand) \
                                SetIcons(*Shellcommand)\n",
"*glob.wcCreate:        		commandWidgetClass\n",
"*glob.wcCallback:		CreateGlobDialog \
	noop(get put dir connect notconnected action Sensitive) \
	help_register\n",
"*glob.label:        		Glob\n",
"*glob.fromHoriz:		*command\n",
"*glob.fromVert: 		*host_name\n",
"*glob.left: 	       		chainleft\n",
"*glob.right: 			chainleft\n",
"*glob.bottom: 			chaintop\n",
"*glob.top: 			chaintop\n",
"*glob.horizDistance:		4\n",
"*glob.sensitive:		False\n",
"*glob.callback:			PositionDialog(*Shellglobdialog) \
				WcPopupCB(*Shellglobdialog) \n",
"*search.wcCreate:      		commandWidgetClass\n",
"*search.wcCallback:		CreateSearchDialog \
	noop(get put dir connect notconnected action Sensitive) \
	help_register\n",
"*search.label:        		Search\n",
"*search.fromHoriz:		*glob\n",
"*search.fromVert: 		*host_name\n",
"*search.left: 	       		chainleft\n",
"*search.right: 			chainleft\n",
"*search.bottom: 		chaintop\n",
"*search.top: 			chaintop\n",
"*search.horizDistance:		4\n",
"*search.sensitive:		False\n",
"*search.callback:		PositionDialog(*Shellsearchdialog) \
				WcPopupCB(*Shellsearchdialog) \n",
"*Shellsearchdialog*.translations:   #override \
   <Key>Help:  			help() \\n\
   <Key>F1:    			help() \n",
"*Shellsearchdialog*value.translations:   #override \
   <Key>Help:  			help() \\n\
   <Key>F1:    			help() \\n\
   <Key>Return:          	WcPopdownACT(*Shellsearchdialog) \
				set_search_text(glob) \\n\
   Ctrl<Key>r:          	WcPopdownACT(*Shellsearchdialog) \
				set_search_text(reg) \\n\
   Ctrl<Key>g:          	WcPopdownACT(*Shellsearchdialog) \
				set_search_text(glob) \\n\
   Ctrl<Key>c:			WcPopdownACT(*Shellsearchdialog) \\n\
   Ctrl<Key>:     		no-op(RingBell) \n",
"*next.wcCreate:      		commandWidgetClass\n",
"*next.wcCallback: \
	noop(get put dir connect notconnected action Sensitive ifsensitive) \
	help_register\n",
"*next.label:        		Search Next\n",
"*next.fromHoriz:		*search\n",
"*next.fromVert: 		*host_name\n",
"*next.left: 	       		chainleft\n",
"*next.right: 			chainleft\n",
"*next.bottom: 			chaintop\n",
"*next.top: 			chaintop\n",
"*next.horizDistance:		4\n",
"*next.callback:			search_next()\n",
"*next.sensitive:		FALSE\n",
"*reconnect.wcCreate:           	commandWidgetClass\n",
"*reconnect.wcCallback:		help_register\n",
"*reconnect.label:        	Reconnect\n",
"*reconnect.fromHoriz:		*next\n",
"*reconnect.fromVert: 		*host_name\n",
"*reconnect.left: 	       	chainleft\n",
"*reconnect.right: 		chainleft\n",
"*reconnect.bottom: 		chaintop\n",
"*reconnect.top: 		chaintop\n",
"*reconnect.horizDistance:	4\n",
"*reconnect.callback:		Reconnect\n",
"*reconnect.sensitive:		False\n",
"*archie_command.wcCreate: 	commandWidgetClass\n",
"*archie_command.wcCallback:	archie_noop() help_register\n",
"*archie_command.label:		Archie\n",
"*archie_command.shapeStyle:	Rectangle\n",
"*archie_command.fromHoriz:	*reconnect\n",
"*archie_command.fromVert: 	*host_name\n",
"*archie_command.left: 	        chainleft\n",
"*archie_command.right: 		chainleft\n",
"*archie_command.bottom: 	chaintop\n",
"*archie_command.top: 		chaintop\n",
"*archie_command.horizDistance:	4\n",
"*archie_command.callback:	archie()\n",
"*rate.wcCreate:			LableQUICKClass\n",
"*rate.wcCallback:               help_register \
	       			set_width(1) \n",
"*rate.resize:                   FALSE\n",
"*rate.label:			\\ \n",
"*rate.justify:                  right\n",
"*rate.fromVert: 		*host_name\n",
"*rate.bottom: 			chaintop\n",
"*rate.top: 			chaintop\n",
"*rate.left: 	        	chainleft\n",
"*rate.right: 	        	chainright\n",
"*rate.horizDistance:		5\n",
"*rate.borderWidth: 		0\n",
"*percent.wcCreate:		Label\n",
"*percent.wcCallback:	        set_width(1)\n",
"*percent.height:		3\n",
"*percent.label:			\n",
"*percent.fromVert: 		*connect\n",
"*percent.right: 		chainright\n",
"*percent.bottom: 		chaintop\n",
"*percent.top: 			chaintop\n",
"*percent.borderWidth: 		0\n",
"*percent.translations:     	<Expose>:       resize_percent() \n",
"*listsw.wcCreate:		MyListSW\n",
"*listsw.wcCallback:	        noop(get put dir action connect notconnected) \
	       			set_width(1) \
				CreateContinueDialog() \
	        		help_register \n",
"*listsw.wcPopups:		fmenu\n",
"*listsw.height:			400\n",
"*listsw.min:			100\n",
"*listsw.fromVert:               *percent\n",
"*listsw.right: 			chainright\n",
"*listsw.left: 			chainleft\n",
"*listsw.top: 			chaintop\n",
"*listsw.bottom: 		chainbottom\n",
"*listsw.labelw:			TRUE\n",
"*listsw.list.width:		10\n",
"*listsw.list.height:		10\n",
"*listsw.list.Callback:          list_notify \n",
"*listsw.list.columnSpacing:	0\n",
"*listsw.list.defaultColumns:	1\n",
"*listsw.list.rowSpacing:	0\n",
"*listsw.list.translations:    	#override \
   <Key>Help:  		help() \\n\
   <Key>F1:    		help() \\n\
  ~Ctrl ~Shift <Key>h:	Listop(Left)\\n\
  ~Ctrl ~Shift <Key>k:  Listop(Up)\\n\
  ~Ctrl ~Shift <Key>l:  Listop(Right)\\n\
  ~Ctrl ~Shift <Key>j:	Listop(Down)\\n\
  ~Ctrl   <Key>dollar: Listop(End)\\n\
  ~Ctrl ~Shift <Key>0:	Listop(Start)\\n\
  ~Ctrl  Shift <Key>m:	Listop(Down)\\n\
   Ctrl ~Shift <Key>f:  Listop(NextPage)\\n\
   Ctrl ~Shift <Key>b:  Listop(PrevPage)\\n\
   Ctrl ~Shift <Key>n:	Listop(Down)\\n\
   Ctrl ~Shift <Key>j:	Listop(Down)\\n\
   Ctrl ~Shift <Key>p:	Listop(Up)\\n\
  ~Shift  ~Meta ~Alt <Key>space: Listop(Select) \\n\
   Ctrl ~Shift <Key>t:	remote_local_toggle()\\n\
  ~Ctrl  Shift <Key>l:	Listing_type(LONG)\\n\
  ~Ctrl  Shift <Key>s: 	Listing_type(SHORT)\\n\
  ~Ctrl  Shift <Key>t:	Listing_type(TRANSLATIONS)\\n\
  ~Ctrl  <Key>greater:	Listop(NextPage)\\n\
  ~Ctrl  <Key>less:  	Listop(PrevPage)\\n\
   Ctrl  <Key>greater: 	Listop(Bottom)\\n\
   Ctrl  <Key>less:     Listop(Top)\\n\
  ~Ctrl ~Shift <Key>u:	SetFileAction(up)\\n\
  ~Ctrl ~Shift <Key>c:	SetFileAction(cd)\\n\
  ~Ctrl ~Shift <Key>g:	SetFileAction(get)\\n\
  ~Ctrl ~Shift <Key>p:	SetFileAction(put)\\n\
  ~Ctrl ~Shift <Key>a:	SetFileAction(ascii)\\n\
  ~Ctrl ~Shift <Key>b:	SetFileAction(binary)\\n\
  ~Ctrl ~Shift <Key>d:	SetFileAction(default)\\n\
  ~Ctrl ~Shift <Key>t:	SetFileAction(tenex)\\n\
   Ctrl        <Key>s:  search_next()\\n\
   Ctrl        <Key>g:  search_clear()\\n\
   Button1<Motion>:     Set(M)\\n\
   ~Ctrl <Btn1Down>:    Set() \\n\
   ~Ctrl <Btn1Up>:      Notify()\\n\
   Ctrl <Btn1Down>:     Set(x) Open_file() \\n\
  ~Ctrl <Btn3Down>: 	Single_File_Actions() \
			XawPositionSimpleMenu(fmenu) \
			MyPopup(fmenu) \\n\
   Ctrl <Btn3Down>: 	Set(x) \
	       		Single_File_Actions() \
			XawPositionSimpleMenu(fmenu) \
			MyPopup(fmenu)\n",
"*fmenu.wcCreate:		simpleMenuWidgetClass\n",
"!*fmenu.popdownCallback:	Clear_List_Entry()\n",
"*fmenu.wcManged:		False\n",
"*fmenu.wcPopups:		fmenu_modem\n",
"*fmenu.wcChildren:		fmenuline, \
				fmenu_UP, fmenu_Cd, fmenu_GET, fmenu_VIEW, \
				fmenu_PUT,\
				fmenu_dir, \
				fmenu_mode, \
				amenuline, \
				fmenu_clear_all, \
                                fmenu_get_all, fmenu_put_all, fmenu_delete_all\n",
"*fmenu*ShapeStyle:		XmuShapeOval\n",
"*fmenu.label:			File Actions\n",
"*fmenu.translations:  		#override \\n\
    <BtnMotion>:                highlight() menu_cascade() \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \n\n",
"<LeaveWindow>:		unhighlight() \n",
"*fmenuline.wcCreate:		SmeLine\n",
"*fmenu_UP.wcCreate:		SmeBSB\n",
"*fmenu_UP.wcCallback:           Register_action(up)\n",
"*fmenu_UP.label:		Up\n",
"*fmenu_UP.callback:		SetFileAction()\n",
"*fmenu_Cd.wcCreate:		SmeBSB\n",
"*fmenu_Cd.wcCallback:           Register_action(cd)\n",
"*fmenu_Cd.label:		Cd\n",
"*fmenu_Cd.callback:             SetFileAction()\n",
"*fmenu_GET.wcCreate:		SmeBSB\n",
"*fmenu_GET.wcCallback:          Register_action(get)\n",
"*fmenu_GET.label:		Get\n",
"*fmenu_GET.callback:   		SetFileAction()\n",
"*fmenu_VIEW.wcCreate:		SmeBSB\n",
"*fmenu_VIEW.wcCallback:         Register_action(view)\n",
"*fmenu_VIEW.label:		View\n",
"*fmenu_VIEW.callback:   	SetFileAction()\n",
"*fmenu_PUT.wcCreate:		SmeBSB\n",
"*fmenu_PUT.wcCallback:          Register_action(put)\n",
"*fmenu_PUT.label:		Put\n",
"*fmenu_PUT.callback:           	SetFileAction()\n",
"*fmenu_mode.wcCreate:           SmeBSB\n",
"*fmenu_mode.wcCallback:         bind_menu(*fmenu_modem)\n",
"*fmenu_mode.label:              File Modes\n",
"*fmenu_modem.wcCreate:		simpleMenuWidgetClass\n",
"*fmenu_modem.wcManged:		False\n",
"*fmenu_modem.wcChildren:        fmenu_Ascii, fmenu_Binary, fmenu_Tenex, \
				fmenu_Default, \
				fmenu_ignore, fmenu_use, \n",
"*fmenu_modem.translations:	#override \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \n\n",
"<LeaveWindow>:		unhighlight() \n",
"*fmenu_modem.*.leftMargin:      20\n",
"*fmenu_Ascii.wcCreate:		SmeBSB\n",
"*fmenu_Ascii.wcCallback:        Register_action(ascii)\n",
"*fmenu_Ascii.label:		Ascii\n",
"*fmenu_Ascii.callback:         	SetFileAction()\n",
"*fmenu_Binary.wcCreate:	        SmeBSB\n",
"*fmenu_Binary.wcCallback:       Register_action(binary)\n",
"*fmenu_Binary.label:		Binary\n",
"*fmenu_Binary.callback: 	SetFileAction()\n",
"*fmenu_Tenex.wcCreate:	        SmeBSB\n",
"*fmenu_Tenex.wcCallback:        Register_action(tenex)\n",
"*fmenu_Tenex.label:		Tenex\n",
"*fmenu_Tenex.callback: 	        SetFileAction()\n",
"*fmenu_Default.wcCreate:	SmeBSB\n",
"*fmenu_Default.wcCallback:      Register_action(default)\n",
"*fmenu_Default.label:		Default\n",
"*fmenu_Default.callback: 	SetFileAction()\n",
"*fmenu_ignore.wcCreate:    	SmeBSB\n",
"*fmenu_ignore.wcCallback:       Register_action(ignore)\n",
"*fmenu_ignore.label:      	Ignore\n",
"*fmenu_ignore.Callback: 	SetFileAction()\n",
"*fmenu_use.wcCreate:       	SmeBSB\n",
"*fmenu_use.wcCallback:          Register_action(use)\n",
"*fmenu_use.label:         	Don't ignore\n",
"*fmenu_use.Callback:    	SetFileAction()\n",
"*fmenu_dir.wcCreate:       	SmeBSB\n",
"*fmenu_dir.wcCallback:          Register_action(dir)\n",
"*fmenu_dir.label:         	Dir\n",
"*fmenu_dir.Callback:    	SetFileAction()\n",
"*amenuline.wcCreate:		SmeLine\n",
"*fmenu_clear_all.wcCreate:	SmeBSB\n",
"*fmenu_clear_all.wcCallback:	Register_action(clear_all)\n",
"*fmenu_clear_all.label:		Clear File Selesctions\n",
"*fmenu_clear_all*callback:	SetFileAction()\n",
"*fmenu_put_all.wcCreate:	SmeBSB\n",
"*fmenu_put_all.wcCallback:	Register_action(put_all)\n",
"*fmenu_put_all.label:		Put Selected Files\n",
"*fmenu_put_all*callback:	SetFileAction()\n",
"*fmenu_put_all.sensitive:	False\n",
"*fmenu_get_all.wcCreate:	SmeBSB\n",
"*fmenu_get_all.wcCallback:	Register_action(get_all)\n",
"*fmenu_get_all.label:		Get Selected Files\n",
"*fmenu_get_all*callback:	SetFileAction()\n",
"*fmenu_delete_all.wcCreate:	SmeBSB\n",
"*fmenu_delete_all.label:	Delete Selected Files\n",
"*fmenu_delete_all.sensitive:	False\n",
"!*fmenu_get_all*callback:	delete\n",
"*Shellarchie.wcCreate:		TopLevelShell\n",
"*Shellarchie.wcChildren:	archie_layout\n",
"*Shellarchie.title:		Archie\n",
"*Shellarchie.destroyCallback:	help_dead\n",
"*archie_layout.wcCreate:	formWidgetClass\n",
"*archie_layout.wcChildren:	ArchieHits, ArchieHost, ArchieNice,\
				    ArchieSearch, ArchieSort,\
				archie_hide, archie_search, archie_abort,\
				   archie_label, archietext, \
				archie_lw\n",
"*ArchieHits.wcCreate: 		commandWidgetClass\n",
"*ArchieHits.wcPopups:		archiehits\n",
"*ArchieHits.label: 		Hits\n",
"*ArchieHits.SapeStyle:		Rectangle\n",
"*ArchieHits.right: 		chainleft\n",
"*ArchieHits.left: 	        chainleft\n",
"*ArchieHits.bottom: 		chaintop\n",
"*ArchieHits.top: 		chaintop\n",
"*ArchieHits.horizDistance:	4\n",
"*ArchieHits.translations: 	#override \
			<Key>Help: help() \\n\
			<Key>F1:   help() \\n\
                        <Btn1Down>:  XawPositionSimpleMenu(archiehits) \
                                     XtMenuPopup(archiehits) \\n\
                        <Btn3Down>:  XawPositionSimpleMenu(archiehits) \
                                     XtMenuPopup(archiehits) \n",
"*archiehits.wcCreate:		simpleMenuWidgetClass\n",
"*archiehits.wcManged:		False\n",
"*archiehits.wcChildren: 	archie_hit_95, archie_hit_200, \
                                archie_hit_400 archie_hit_800\n",
"*archiehits.wcManaged:          FALSE\n",
"*archie_hit_95.wcCreate:	SmeBSB\n",
"*archie_hit_95.wcCallback:	Register_archie(95)\n",
"*archie_hit_95.label:		95\n",
"*archie_hit_95*callback:	do_archie\n",
"*archie_hit_200.wcCreate:	SmeBSB\n",
"*archie_hit_200.wcCallback:	Register_archie(200)\n",
"*archie_hit_200.label:		200\n",
"*archie_hit_200*callback:	do_archie\n",
"*archie_hit_400.wcCreate:	SmeBSB\n",
"*archie_hit_400.wcCallback:	Register_archie(400)\n",
"*archie_hit_400.label:		400\n",
"*archie_hit_400*callback:	do_archie\n",
"*archie_hit_800.wcCreate:	SmeBSB\n",
"*archie_hit_800.wcCallback:	Register_archie(800)\n",
"*archie_hit_800.label:		800\n",
"*archie_hit_800*callback:	do_archie\n",
"*ArchieHost.wcCreate: 		commandWidgetClass\n",
"*ArchieHost.wcPopups:		archiehost\n",
"*ArchieHost.label: 		Host\n",
"*ArchieHost.SapeStyle:		Rectangle\n",
"*ArchieHost.right: 		chainleft\n",
"*ArchieHost.left: 	        chainleft\n",
"*ArchieHost.bottom: 		chaintop\n",
"*ArchieHost.top: 		chaintop\n",
"*ArchieHost.horizDistance:	4\n",
"*ArchieHost.fromHoriz:		*ArchieHits\n",
"*ArchieHost.translations: 	#override \
			<Key>Help: help() \\n\
			<Key>F1:   help() \\n\
                        <Btn1Down>:  XawPositionSimpleMenu(archiehost) \
                                     XtMenuPopup(archiehost) \\n\
                        <Btn3Down>:  XawPositionSimpleMenu(archiehost) \
                                     XtMenuPopup(archiehost) \n",
"*archiehost.wcCreate:		simpleMenuWidgetClass\n",
"*archiehost.wcCallback:		archie_hosts()\n",
"*archiehost.wcManged:		False\n",
"*archiehost.wcChildren: 	\n",
"*archiehost.wcManaged:          FALSE\n",
"*archie_host.wcCreate:		SmeBSB\n",
"*archie_host.wcCallback:	Register_archie(host)\n",
"*archie_host.label:		\n",
"*archie_host*callback:		do_archie\n",
"*ArchieNice.wcCreate: 		commandWidgetClass\n",
"*ArchieNice.wcPopups:		archienice\n",
"*ArchieNice.label: 		Nice\n",
"*ArchieNice.SapeStyle:		Rectangle\n",
"*ArchieNice.right: 		chainleft\n",
"*ArchieNice.left: 	        chainleft\n",
"*ArchieNice.bottom: 		chaintop\n",
"*ArchieNice.top: 		chaintop\n",
"*ArchieNice.horizDistance:	4\n",
"*ArchieNice.fromHoriz:		*ArchieHost\n",
"*ArchieNice.translations: 	#override \
			<Key>Help: help() \\n\
			<Key>F1:   help() \\n\
                        <Btn1Down>:  XawPositionSimpleMenu(archienice) \
                                     XtMenuPopup(archienice) \\n\
                        <Btn3Down>:  XawPositionSimpleMenu(archienice) \
                                     XtMenuPopup(archienice) \n",
"*archienice.wcCreate:		simpleMenuWidgetClass\n",
"*archienice.wcManged:		False\n",
"*archienice.wcChildren: 	archie_nice_default, archie_nice_nice,\
                                archie_nice_nicer,  archie_nice_very,\
                                archie_nice_exterm,  archie_nice_nicest\n",
"*archienice.wcManaged:          FALSE\n",
"*archie_nice_default.wcCreate:	SmeBSB\n",
"*archie_nice_default.wcCallback:Register_archie(nice_default)\n",
"*archie_nice_default.label:	Nice Default      (0)\n",
"*archie_nice_default*callback:	do_archie\n",
"*archie_nice_nice.wcCreate:	SmeBSB\n",
"*archie_nice_nice.wcCallback:	Register_archie(nice_nice)\n",
"*archie_nice_nice.label:	Nice            (500)\n",
"*archie_nice_nice*callback:	do_archie\n",
"*archie_nice_nicer.wcCreate:	SmeBSB\n",
"*archie_nice_nicer.wcCallback:	Register_archie(nice_nicer)\n",
"*archie_nice_nicer.label:	Nice           (1000)\n",
"*archie_nice_nicer*callback:	do_archie\n",
"*archie_nice_very.wcCreate:	SmeBSB\n",
"*archie_nice_very.wcCallback:	Register_archie(nice_very)\n",
"*archie_nice_very.label:	Very Nice      (5000)\n",
"*archie_nice_very*callback:	do_archie\n",
"*archie_nice_exterm.wcCreate:	SmeBSB\n",
"*archie_nice_exterm.wcCallback:	Register_archie(nice_exterm)\n",
"*archie_nice_exterm.label:	Extermly Nice (10000)\n",
"*archie_nice_exterm*callback:	do_archie\n",
"*archie_nice_nicest.wcCreate:	SmeBSB\n",
"*archie_nice_nicest.wcCallback:Register_archie(nice_nicest)\n",
"*archie_nice_nicest.label:	Extermly Nice (32765)\n",
"*archie_nice_nicest*callback:	do_archie\n",
"*ArchieSearch.wcCreate: 	commandWidgetClass\n",
"*ArchieSearch.wcPopups:		archiesearch\n",
"*ArchieSearch.label: 		Search Types\n",
"*ArchieSearch.SapeStyle:	Rectangle\n",
"*ArchieSearch.right: 		chainleft\n",
"*ArchieSearch.left: 	        chainleft\n",
"*ArchieSearch.bottom: 		chaintop\n",
"*ArchieSearch.top: 		chaintop\n",
"*ArchieSearch.horizDistance:	4\n",
"*ArchieSearch.fromHoriz:	*ArchieNice\n",
"*ArchieSearch.translations: 	#override \
			<Key>Help: help() \\n\
			<Key>F1:   help() \\n\
                        <Btn1Down>:  XawPositionSimpleMenu(archiesearch) \
                                     XtMenuPopup(archiesearch) \\n\
                        <Btn3Down>:  XawPositionSimpleMenu(archiesearch) \
                                     XtMenuPopup(archiesearch) \n",
"*archiesearch.wcCreate:		simpleMenuWidgetClass\n",
"*archiesearch.wcManged:		False\n",
"*archiesearch.wcChildren: 	archie_csss, archie_esm, archie_res,\
                        	archie_ciss\n",
"*archiesearch.wcManaged: 	FALSE\n",
"*archie_csss.wcCreate:		SmeBSB\n",
"*archie_csss.wcCallback:        Register_archie(csss)\n",
"*archie_csss.label:		Case Sensitive substring search\n",
"*archie_csss*callback:		do_archie\n",
"*archie_esm.wcCreate:		SmeBSB\n",
"*archie_esm.wcCallback:         Register_archie(esm)\n",
"*archie_esm.label:		Exact String Match\n",
"*archie_esm*callback:		do_archie\n",
"*archie_res.wcCreate:		SmeBSB\n",
"*archie_res.wcCallback:         Register_archie(res)\n",
"*archie_res.label:		Regular Expession Search\n",
"*archie_res*callback:		do_archie\n",
"*archie_ciss.wcCreate:		SmeBSB\n",
"*archie_ciss.wcCallback:	Register_archie(ciss)\n",
"*archie_ciss.label:		Case Insensitive Substring Search\n",
"*archie_ciss*callback:		do_archie\n",
"*ArchieSort.wcCreate: 		commandWidgetClass\n",
"*ArchieSort.wcPopups:		archiesort\n",
"*ArchieSort.label: 		Sort\n",
"*ArchieSort.SapeStyle:		Rectangle\n",
"*ArchieSort.right: 		chainleft\n",
"*ArchieSort.left: 	        chainleft\n",
"*ArchieSort.bottom: 		chaintop\n",
"*ArchieSort.top: 		chaintop\n",
"*ArchieSort.horizDistance:	4\n",
"*ArchieSort.fromHoriz:		*ArchieSearch\n",
"*ArchieSort.translations: 	#override \
			<Key>Help: help() \\n\
			<Key>F1:   help() \\n\
                        <Btn1Down>:  XawPositionSimpleMenu(archiesort) \
                                     XtMenuPopup(archiesort) \\n\
                        <Btn3Down>:  XawPositionSimpleMenu(archiesort) \
                                     XtMenuPopup(archiesort) \n",
"*archiesort.wcCreate:		simpleMenuWidgetClass\n",
"*archiesort.wcManged:		False\n",
"*archiesort.wcChildren:         archie_sort_age, archie_sort_name,\
                                archie_sort_size,\
                                archie_sort_sep,\
                                archie_sort_normal,\
                                archie_sort_reverse\n",
"*archiesort.wcManaged: 		FALSE\n",
"*archie_sort_age.wcCreate:	SmeBSB\n",
"*archie_sort_age.wcCallback:	Register_archie(sort_by_age)\n",
"*archie_sort_age.label:		Age\n",
"*archie_sort_age*callback:	do_archie\n",
"*archie_sort_name.wcCreate:	SmeBSB\n",
"*archie_sort_name.wcCallback:	Register_archie(sort_by_name)\n",
"*archie_sort_name.label:	Name\n",
"*archie_sort_name*callback:	do_archie\n",
"*archie_sort_size.wcCreate:	SmeBSB\n",
"*archie_sort_size.wcCallback:	Register_archie(sort_by_size)\n",
"*archie_sort_size.label:	Size\n",
"*archie_sort_size*callback:	do_archie\n",
"*archie_sort_sep.wcCreate:	SmeLine\n",
"*archie_sort_normal.wcCreate:	SmeBSB\n",
"*archie_sort_normal.wcCallback:	Register_archie(sort_normal)\n",
"*archie_sort_normal.label:	Normal\n",
"*archie_sort_normal*callback:	do_archie\n",
"*archie_sort_reverse.wcCreate:	SmeBSB\n",
"*archie_sort_reverse.wcCallback:Register_archie(sort_reverse)\n",
"*archie_sort_reverse.label:	Reverse\n",
"*archie_sort_reverse*callback:	do_archie\n",
"*archie_hide.wcCreate: 		commandWidgetClass\n",
"*archie_hide.label:		Hide\n",
"*archie_hide.shapeStyle:	Rectangle\n",
"*archie_hide.left: 		chainleft\n",
"*archie_hide.right: 		chainleft\n",
"*archie_hide.bottom: 		chaintop\n",
"*archie_hide.top: 		chaintop\n",
"*archie_hide.callback:          WcPopDownCB(~)\n",
"*archie_hide.fromVert:		*ArchieHits\n",
"*archie_hide.horizDistance:	4\n",
"*archie_search.wcCreate: 	commandWidgetClass\n",
"*archie_search.wcCallback:      Register_archie(do_search)\n",
"*archie_search.label:		Search\n",
"*archie_search.shapeStyle:	Rectangle\n",
"*archie_search.left: 		chainleft\n",
"*archie_search.right: 		chainleft\n",
"*archie_search.bottom:		chaintop\n",
"*archie_search.top:		chaintop\n",
"*archie_search.callback:	do_archie\n",
"*archie_search.fromVert:	*ArchieHits\n",
"*archie_search.fromHoriz:	*archie_hide\n",
"*archie_search.horizDistance:	4\n",
"*archie_abort.wcCreate: 	commandWidgetClass\n",
"*archie_abort.label:		Abort Search\n",
"*archie_abort.shapeStyle:	Rectangle\n",
"*archie_abort.left: 		chainleft\n",
"*archie_abort.right: 		chainleft\n",
"*archie_abort.bottom:		chaintop\n",
"*archie_abort.top:		chaintop\n",
"*archie_abort.callback:		abort_archie\n",
"*archie_abort.fromVert:		*ArchieHits\n",
"*archie_abort.fromHoriz:	*archie_search\n",
"*archie_abort.horizDistance:	4\n",
"*archie_label.wcCreate:		Label\n",
"*archie_label.label:		Archie Search Item:\n",
"*archie_label.resize:		FALSE\n",
"*archie_label.width:		140	\n",
"*archie_label.justify:		left\n",
"*archie_label.right: 		chainleft\n",
"*archie_label.left: 		chainleft\n",
"*archie_label.bottom: 		chaintop\n",
"*archie_label.top: 		chaintop\n",
"*archie_label.fromVert:		*ArchieHits\n",
"*archie_label.fromHoriz:	*archie_abort\n",
"*archie_label.horizDistance:	4\n",
"*archie_label.borderWidth: 	0\n",
"*archietext.wcCreate:  		AsciiText\n",
"*archietext.wcCallback:         Register_archie(search_text)\n",
"*archietext.width:		435\n",
"*archietext*editType:           edit\n",
"*archietext.input:              TRUE\n",
"*archietext.border:             black\n",
"*archietext.right:              chainright\n",
"*archietext.left:               chainleft\n",
"*archietext.bottom:             chaintop\n",
"*archietext.top:                chaintop\n",
"*archietext.fromVert:		*ArchieHits\n",
"*archietext.fromHoriz:		*archie_label\n",
"*archietext.translations:       #override \
                                <Key>Help: help() \\n\
                                <Key>F1:   help() \\n\
                                <Key>Return:    no-op(RingBell) \\n\
                                Ctrl<Key>J:     no-op(RingBell) \\n\
                                Ctrl<Key>M:     no-op(RingBell) \\n\
                                Ctrl<Key>O:     no-op(RingBell) \\n\
                                Ctrl<Key>V:     no-op(RingBell) \\n\
\n",
"*archie_lw.wcCreate:            MyListSW\n",
"*archie_lw.height:              300\n",
"*archie_lw.width:		800\n",
"*archie_lw.labelw:              TRUE\n",
"*archie_lw.left: 		chainleft\n",
"*archie_lw.right: 		chainright\n",
"*archie_lw.bottom: 		chainbottom\n",
"*archie_lw.top: 		chaintop\n",
"*archie_lw.fromVert:		*archie_hide\n",
"*archie_lw.list.Callback:       archie_notify\n",
"*archie_lw.list.height:		10\n",
"*archie_lw.list.width:		10\n",
"*archie_lw.list.columnSpacing:  0\n",
"*archie_lw.list.defaultColumns: 1\n",
"*archie_lw.list.rowSpacing:     0\n",
"*archie_lw.translations:       #override \
                                <Key>Help: help() \\n\
                                <Key>F1:   help() \n",
"*Shellhelp.wcCreate:            XtCreateTopLevelShell\n",
"*Shellhelp.wcChildren:		help_layout\n",
"*Shellhelp.title:		Help\n",
"*Shellhelp.destroyCallback:	help_dead\n",
"*help_layout.wcCreate:		formWidgetClass\n",
"*help_layout.wcChildren:	help_quit, help_options, help_title, help_text\n",
"*help_quit.wcCreate: 		commandWidgetClass\n",
"*help_quit.label:		Hide\n",
"*help_quit.shapeStyle:		Rectangle\n",
"*help_quit.left: 		chainleft\n",
"*help_quit.right: 		chainleft\n",
"*help_quit.bottom: 		chaintop\n",
"*help_quit.top: 		chaintop\n",
"*help_quit.horizDistance:	4\n",
"*help_quit.callback:            WcPopDownCB(~)\n",
"*help_options.wcCreate: 	commandWidgetClass\n",
"*help_options.label:		Selections\n",
"*help_options.shapeStyle:	Rectangle\n",
"*help_options.left: 		chainleft\n",
"*help_options.right: 		chainleft\n",
"*help_options.bottom: 		chaintop\n",
"*help_options.top: 		chaintop\n",
"*help_options.fromHoriz:	*help_quit\n",
"*help_options.wcPopups:		helpmenu\n",
"*help_options.horizDistance:	4\n",
"*help_options.translations:     #override \
                                <Btn1Down>:  XawPositionSimpleMenu(helpmenu) \
                                             XtMenuPopup(helpmenu) \\n\
                                <Btn3Down>:  XawPositionSimpleMenu(helpmenu) \
                                             XtMenuPopup(helpmenu)\n",
"*help_title.wcCreate:		Label\n",
"*help_title.label:			\n",
"*help_title.resize:		FALSE\n",
"*help_title.width:		350\n",
"*help_title.justify:		left\n",
"*help_title.right: 		chainright\n",
"*help_title.left: 		chainleft\n",
"*help_title.bottom: 		chaintop\n",
"*help_title.top: 		chaintop\n",
"*help_title.fromHoriz:		*help_options\n",
"*help_title.horizDistance:	4\n",
"*help_title.borderWidth: 	0\n",
"*helpmenu.wcCreate:		simpleMenuWidgetClass\n",
"*helpmenu.label:		Help Menu\n",
"*helpmenu*ShapeStyle:		XmuShapeOval\n",
"*helpmenu.translations:  	#override \\n\
    <BtnMotion>: 		highlight() menu_cascade() \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \\n\
    <LeaveWindow>:		unhighlight()\n",
"*help_sub.wcCreate:             SmeBSB\n",
"*help_menu.wcManged:            False\n",
"*help_menu.translations:  	#override \\n\
    <BtnMotion>: 		highlight() menu_cascade() \\n\
    <BtnUp>:			PopDownSubs() notify() unhighlight() \\n\
    <LeaveWindow>:		unhighlight() \n",
"*helpmenu1.wcCreate:		SmeBSB\n",
"*helpmenu1.label:			\n",
"*helpmenu1.callback:		help_by_title()  help_once()\n",
"*help_text.wcCreate:		AsciiText\n",
"*help_text.width:		630\n",
"*help_text.height:		300\n",
"*help_text*scrollVertical:	whenNeeded\n",
"*help_text*scrollHorizontal: 	whenNeeded\n",
"*help_text*editType: 		read\n",
"*help_text.borderWidth:		1\n",
"*help_text.fromVert: 		*help_quit\n",
"*help_text.left: 		chainleft\n",
"*help_text.right: 		chainright\n",
"*help_text.bottom: 		chainbottom\n",
"*help_text.top: 		chaintop\n",
"*Shelltran.wcCreate:		TopLevelShell\n",
"*Shelltran.wcChildren:		tran_layout\n",
"*Shelltran.title:		Remote File Translations Examples\n",
"*tran_layout.wcCreate:		formWidgetClass\n",
"*tran_layout.wcChildren:	tran_quit, tran_text\n",
"*tran_quit.wcCreate: 		commandWidgetClass\n",
"*tran_quit.label:		Hide\n",
"*tran_quit.shapeStyle:		Rectangle\n",
"*tran_quit.left: 		chainleft\n",
"*tran_quit.right: 		chainleft\n",
"*tran_quit.bottom: 		chaintop\n",
"*tran_quit.top: 		chaintop\n",
"*tran_quit.callback:            WcPopDownCB(~)\n",
"*tran_text.wcCreate:		AsciiText\n",
"*tran_text.wcCallback:	        set_tran\n",
"*tran_text.width:		500\n",
"*tran_text.height:		300\n",
"*tran_text*scrollVertical:	whenNeeded\n",
"*tran_text*scrollHorizontal: 	whenNeeded\n",
"*tran_text*editType: 		read\n",
"*tran_text.borderWidth:		1\n",
"*tran_text.fromVert: 		*tran_quit\n",
"*tran_text.left: 		chainleft\n",
"*tran_text.right: 		chainright\n",
"*tran_text.bottom: 		chainbottom\n",
"*tran_text.top: 		chaintop\n",
"*Shellconnect.wcCreate:		TopLevelShell\n",
"*Shellconnect.wcChildren:	Shconnectlayout\n",
"*Shellconnect.title:		Connect...\n",
"*Shconnectlayout.wcCreate: 	formWidgetClass\n",
"*Shconnectlayout.wcChildren:	DoAnonymous, \
				DoConnect, DoHide, DoRetry, DoGateway,\
				   DoArchie,\
			        hostsw, \
                                hostlabel, hosttext,\
                                loginlabel, logintext,\
                                passwordlabel, passwordtext,\
                                remotedirlabel, remotedirtext,\
                                localdirlabel, localdirtext,\
				gatewaylabel, gatewaytext\n",
"*DoAnonymous.wcCreate: 		menuButtonWidgetClass\n",
"*DoAnonymous.label:		Anonymous Login\n",
"*DoAnonymous.shapeStyle:	Rectangle\n",
"*DoAnonymous.menuName:		anonymous\n",
"*DoAnonymous.right:		chainleft\n",
"*DoAnonymous.left:		chainleft\n",
"*DoAnonymous.bottom:		chaintop\n",
"*DoAnonymous.top:		chaintop\n",
"*DoAnonymous.wcPopups:		anonymous\n",
"*anonymous.wcCreate:		simpleMenuWidgetClass\n",
"*anonymous.wcManged:	 	False\n",
"*anonymous.wcChildren:     	anonGuest, anonMail, anonLogin\n",
"*anonGuest.wcCreate:	        SmeBSB\n",
"*anonGuest.label:            	Guest\n",
"*anonGuest.callback:		Anonymous(guest)\n",
"*anonMail.wcCreate:	        SmeBSB\n",
"*anonMail.label:            	Mail Address\n",
"*anonMail.callback:		Anonymous(MAIL)\n",
"*anonLogin.wcCreate:	        SmeBSB\n",
"*anonLogin.label:            	Login Name\n",
"*anonLogin.callback:		Anonymous(NAME)\n",
"*DoConnect.wcCreate: 		commandWidgetClass\n",
"*DoConnect.label:		Connect\n",
"*DoConnect.right:		chainleft\n",
"*DoConnect.fromVert:		*DoAnonymous\n",
"*DoConnect.bottom:		chaintop\n",
"*DoConnect.top:			chaintop\n",
"*DoConnect.shapeStyle:		Rectangle\n",
"*DoConnect.callback:		Login()\n",
"*DoHide.wcCreate: 		commandWidgetClass\n",
"*DoHide.label:			Hide\n",
"*DoHide.shapeStyle:		Rectangle\n",
"*DoHide.fromHoriz:		*DoConnect\n",
"*DoHide.fromVert:		*DoAnonymous\n",
"*DoHide.right:			chainleft\n",
"*DoHide.left:			chainleft\n",
"*DoHide.bottom:			chaintop\n",
"*DoHide.top:			chaintop\n",
"*DoHide.callback:		WcPopDownCB(~) \
				Clear_noop(connect) \
				WcSetSensitiveCB(\"*connect\")\n",
"*DoRetry.wcCreate: 		toggleWidgetClass\n",
"*DoRetry.label:			Retry\n",
"*DoRetry.callback:		Set_retry\n",
"*DoRetry.shapeStyle:		Rectangle\n",
"*DoRetry.fromVert:		*DoAnonymous\n",
"*DoRetry.fromHoriz:		*DoHide\n",
"*DoRetry.right:			chainleft\n",
"*DoRetry.left:			chainleft\n",
"*DoRetry.bottom:		chaintop\n",
"*DoRetry.top:			chaintop\n",
"*DoGateway.wcCreate: 		toggleWidgetClass\n",
"*DoGateway.label:		Use ftp gateway\n",
"*DoGateway.callback:		Set_use_gateway\n",
"*DoGateway.shapeStyle:		Rectangle\n",
"*DoGateway.fromVert:		*DoAnonymous\n",
"*DoGateway.fromHoriz:		*DoRetry\n",
"*DoGateway.right:		chainleft\n",
"*DoGateway.left:		chainleft\n",
"*DoGateway.bottom:		chaintop\n",
"*DoGateway.top:			chaintop\n",
"*DoArchie.wcCreate: 		commandWidgetClass\n",
"*DoArchie.wcCallback:		archie_noop()\n",
"*DoArchie.label:		Archie\n",
"*DoArchie.shapeStyle:		Rectangle\n",
"*DoArchie.fromVert: 		*DoAnonymous\n",
"*DoArchie.fromHoriz:		*DoGateway\n",
"*DoArchie.left: 	     	chainleft\n",
"*DoArchie.right: 		chainleft\n",
"*DoArchie.bottom: 		chaintop\n",
"*DoArchie.top: 			chaintop\n",
"*DoArchie.horizDistance:	4\n",
"*DoArchie.callback:		archie()\n",
"*hostsw.wcCreate:               MyListSW\n",
"*hostsw.wcCallback:             help_register \
			        CreateHostSearchDialog() \n",
"*hostsw.height:                 100\n",
"*hostsw.width:			424\n",
"*hostsw.min:			100\n",
"*hostsw.labelw:                 FALSE\n",
"*hostsw.fromVert:		*DoConnect\n",
"*hostsw.top:			chaintop\n",
"*hostsw.bottom:			chainbottom\n",
"*hostsw.right:			chainright\n",
"*hostsw.left:			chainleft\n",
"*hostsw.list.Callback:          SelectHost\n",
"*hostsw.list.width:		10\n",
"*hostsw.list.height:		10\n",
"*hostsw.list.columnSpacing:     0\n",
"*hostsw.list.defaultColumns:    1\n",
"*hostsw.list.forceColumns:      FALSE\n",
"*hostsw.list.mulitselect:       FALSE\n",
"*hostsw.list.rowSpacing:        0\n",
"*hostsw.list.translations:      #override \
  Ctrl ~Shift <Key>n:           Listop(Down)\\n\
 ~Ctrl ~Shift <Key>j:           Listop(Down)\\n\
  Ctrl ~Shift <Key>p:           Listop(Up)\\n\
 ~Ctrl ~Shift <Key>k:           Listop(Up)\\n\
  Ctrl ~Shift <Key>f:           Listop(Right)\\n\
 ~Ctrl ~Shift <Key>l:           Listop(Right)\\n\
  Ctrl ~Shift <Key>b:           Listop(Left)\\n\
 ~Ctrl ~Shift <Key>h:           Listop(Left)\\n\
  Ctrl ~Shift <Key>a:           Listop(Start)\\n\
  Ctrl ~Shift <Key>e:           Listop(End)\\n\
 ~Ctrl  <Key>greater:           Listop(NextPage)\\n\
 ~Ctrl  <Key>less:              Listop(PrevPage)\\n\
  Ctrl  <Key>greater:           Listop(Bottom)\\n\
  Ctrl  <Key>less:              Listop(Top)\\n\
  Ctrl  <Key>s:  		search_host()\\n\
  Ctrl  <Key>g: 		search_host(clear)\\n\
 ~Shift ~Meta ~Alt <Key>space:  Listop(Select)\\n\
 <Btn1Down>(1):                 Set() Notify()\\n\
 <Btn1Down>(2):                 Login()\\n\
 <Key>Help:                 	help() \n\n",
"*Shellsearchhostdialog*value.translations:   #override \
   <Key>Help:                   help() \\n\
   <Key>F1:                     help() \\n\
   <Key>Return:                 WcPopdownACT(*Shellsearchhostdialog) \
                                set_search_host(reg) \\n\
   Ctrl<Key>r:                  WcPopdownACT(*Shellsearchhostdialog) \
                                set_search_host(reg) \\n\
   Ctrl<Key>c:                  WcPopdownACT(*Shellsearchhostdialog) \\n\
   Ctrl<Key>:                   no-op(RingBell)\n",
"*hmenu.wcCreate:		simpleMenuWidgetClass\n",
"*hmenu.wcChildren:		hmenuline\n",
"*hmenu.label:			Hosts\n",
"*hmenu_more.wcCreate:		simpleMenuWidgetClass\n",
"*hmenu_more.wcChildren:		hmenuline1\n",
"*hmenu_more.label:		Hosts\n",
"*hmenuline.wcCreate:		SmeLine\n",
"*hmenuline1.wcCreate:		SmeLine\n",
"*hmenuline1.wcCallback:        	CreateHostList()\n",
"*hmenu1.wcCreate:		SmeBSB\n",
"*hmenu1.wcCallback:             SetHostName()\n",
"*hmenu1.label:			\n",
"*hmenu1.callback:		GetHostName()\n",
"*hostlabel.wcCreate:		Label\n",
"*hostlabel.width:		120\n",
"*hostlabel.justify:		right\n",
"*hostlabel.label:		Remote host:\n",
"*hostlabel.borderWidth:		0\n",
"*hostlabel.fromVert:		*hostsw\n",
"*hostlabel.right:		chainleft\n",
"*hostlabel.left:		chainleft\n",
"*hostlabel.bottom:		chainbottom\n",
"*hostlabel.top:			chainbottom\n",
"*hosttext.wcCreate:		AsciiText\n",
"*hosttext.wcCallback:		init_connect_info(host) \
				SetHostList\n",
"*hosttext.width:		300\n",
"*hosttext*editType: 		edit\n",
"*hosttext.borderWidth:		1\n",
"*hosttext.input:		TRUE\n",
"*hosttext.border:		black\n",
"*hosttext.fromVert:		*hostsw\n",
"*hosttext.fromHoriz:		*hostlabel\n",
"*hosttext.right:		chainright\n",
"*hosttext.left:			chainleft\n",
"*hosttext.bottom:		chainbottom\n",
"*hosttext.top:			chainbottom\n",
"*hosttext.translations: 	#override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \\n\
				<Key>Return:    Login() \\n\
				Ctrl<Key>J:     insert-char() \\n\
				Ctrl<Key>M:     insert-char() \\n\
				Ctrl<Key>O:     insert-char() \\n\
				Ctrl<Key>P:     warp_pointer(up) \\n\
				Ctrl<Key>N:     warp_pointer(down) \\n\
				Ctrl<Key>S:	no-op(RingBell) \\n\
				Ctrl<Key>V:     insert-char() \\n\
\n",
"*loginlabel.wcCreate:		Label\n",
"*loginlabel.width:		120\n",
"*loginlabel.justify:		right\n",
"*loginlabel.label:		Login:\n",
"*loginlabel.borderWidth:	0\n",
"*loginlabel.fromVert:		*hostlabel\n",
"*loginlabel.right:		chainleft\n",
"*loginlabel.left:		chainleft\n",
"*loginlabel.bottom:		chainbottom\n",
"*loginlabel.top:		chainbottom\n",
"*logintext.wcCreate:		AsciiText\n",
"*logintext.wcCallback:		init_connect_info(login)\n",
"*logintext.width:		300\n",
"*logintext*editType: 		edit\n",
"*logintext.input:		TRUE\n",
"*logintext.border:		black\n",
"*logintext.fromVert:		*hostlabel\n",
"*logintext.fromHoriz:		*loginlabel\n",
"*logintext.right:		chainright\n",
"*logintext.left:		chainleft\n",
"*logintext.bottom:		chainbottom\n",
"*logintext.top:			chainbottom\n",
"*logintext.translations: 	#override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \\n\
				<Key>Return:    Login() \\n\
				Ctrl<Key>J:     no-op(RingBell) \\n\
				Ctrl<Key>M:     no-op(RingBell) \\n\
				Ctrl<Key>O:     no-op(RingBell) \\n\
				Ctrl<Key>V:     no-op(RingBell) \\n\
				Ctrl<Key>S:	no-op(RingBell) \\n\
				Ctrl<Key>P:     warp_pointer(up) \\n\
				Ctrl<Key>N:     warp_pointer(down)\n",
"*passwordlabel.wcCreate:	Label\n",
"*passwordlabel.width:		120\n",
"*passwordlabel.justify:		right\n",
"*passwordlabel.label:		Password:\n",
"*passwordlabel.borderWidth:	0\n",
"*passwordlabel.fromVert:	*loginlabel\n",
"*passwordlabel.right:		chainleft\n",
"*passwordlabel.left:		chainleft\n",
"*passwordlabel.bottom:		chainbottom\n",
"*passwordlabel.top:		chainbottom\n",
"*passwordtext.wcCreate:		AsciiText\n",
"*passwordtext.wcCallback:	init_connect_info(password)\n",
"*passwordtext.width:		300\n",
"*passwordtext*editType: 	edit\n",
"*passwordtext.borderWidth:	1\n",
"*passwordtext.input:		TRUE\n",
"*passwordtext.border:		black\n",
"*passwordtext.fromVert:		*loginlabel\n",
"*passwordtext.fromHoriz:	*passwordlabel\n",
"*passwordtext.right:		chainright\n",
"*passwordtext.left:		chainleft\n",
"*passwordtext.bottom:		chainbottom\n",
"*passwordtext.top:		chainbottom\n",
"*passwordtext.translations: 	#override \
				<Key>Help:  help() \\n\
				<Key>F1:    help() \\n\
				<Key>Return:    Login() \\n\
				Ctrl<Key>F:     no-op(RingBell) \\n\
				Ctrl<Key>J:     no-op(RingBell) \\n\
				Ctrl<Key>M:     no-op(RingBell) \\n\
				Ctrl<Key>O:     no-op(RingBell) \\n\
				Ctrl<Key>V:     no-op(RingBell) \\n\
				Ctrl<Key>S:	no-op(RingBell) \\n\
				Ctrl<Key>P:     warp_pointer(up) \\n\
				Ctrl<Key>N:     warp_pointer(down) \\n\
			        <Key>Delete:    delete_char() \\n\
			        <Key>BackSpace: delete_char() \\n\
		                <Key>:          my_insert_char()\n",
"*remotedirlabel.wcCreate:	Label\n",
"*remotedirlabel.width:		120\n",
"*remotedirlabel.justify:	right\n",
"*remotedirlabel.label:		Remote Directory:\n",
"*remotedirlabel.borderWidth:	0\n",
"*remotedirlabel.fromVert:	*passwordlabel\n",
"*remotedirlabel.right:		chainleft\n",
"*remotedirlabel.left:		chainleft\n",
"*remotedirlabel.bottom:		chainbottom\n",
"*remotedirlabel.top:		chainbottom\n",
"*remotedirtext.wcCreate:	AsciiText\n",
"*remotedirtext.wcCallback:	init_connect_info(remotedir)\n",
"*remotedirtext.width:		300\n",
"*remotedirtext*editType: 	edit\n",
"*remotedirtext.borderWidth:	1\n",
"*remotedirtext.input:		TRUE\n",
"*remotedirtext.border:		black\n",
"*remotedirtext.fromVert:	*passwordlabel\n",
"*remotedirtext.fromHoriz:	*remotedirlabel\n",
"*remotedirtext.right:		chainright\n",
"*remotedirtext.left:		chainleft\n",
"*remotedirtext.bottom:		chainbottom\n",
"*remotedirtext.top:		chainbottom\n",
"*remotedirtext.translations: 	#override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \\n\
				<Key>Return:    Login() \\n\
				Ctrl<Key>F:     no-op(RingBell) \\n\
				Ctrl<Key>J:     no-op(RingBell) \\n\
				Ctrl<Key>M:     no-op(RingBell) \\n\
				Ctrl<Key>O:     no-op(RingBell) \\n\
				Ctrl<Key>S:	no-op(RingBell) \\n\
				Ctrl<Key>V:     no-op(RingBell) \\n\
				Ctrl<Key>Z:     no-op(RingBell) \\n\
				Ctrl<Key>P:     warp_pointer(up) \\n\
				Ctrl<Key>N:     warp_pointer(down)\n",
"*localdirlabel.wcCreate:	Label\n",
"*localdirlabel.width:		120\n",
"*localdirlabel.justify:		right\n",
"*localdirlabel.label:		Local Directory:\n",
"*localdirlabel.borderWidth:	0\n",
"*localdirlabel.fromVert:	*remotedirlabel\n",
"*localdirlabel.right:		chainleft\n",
"*localdirlabel.left:		chainleft\n",
"*localdirlabel.bottom:		chainbottom\n",
"*localdirlabel.top:		chainbottom\n",
"*localdirtext.wcCreate:		AsciiText\n",
"*localdirtext.wcCallback:	init_connect_info(localdir)\n",
"*localdirtext.width:		300\n",
"*localdirtext*editType: 	edit\n",
"*localdirtext.borderWidth:	1\n",
"*localdirtext.input:		TRUE\n",
"*localdirtext.border:		black\n",
"*localdirtext.fromVert:		*remotedirlabel\n",
"*localdirtext.fromHoriz:	*localdirlabel\n",
"*localdirtext.right:		chainright\n",
"*localdirtext.left:		chainleft\n",
"*localdirtext.bottom:		chainbottom\n",
"*localdirtext.top:		chainbottom\n",
"*localdirtext.translations: 	#override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \\n\
				<Key>Return:    Login() \\n\
				Ctrl<Key>F:     no-op(RingBell) \\n\
				Ctrl<Key>J:     no-op(RingBell) \\n\
				Ctrl<Key>M:     no-op(RingBell) \\n\
				Ctrl<Key>O:     no-op(RingBell) \\n\
				Ctrl<Key>S:	no-op(RingBell) \\n\
				Ctrl<Key>V:     no-op(RingBell) \\n\
				Ctrl<Key>P:     warp_pointer(up) \\n\
				Ctrl<Key>N:     warp_pointer(down)\n",
"*gatewaylabel.wcCreate:		Label\n",
"*gatewaylabel.width:		120\n",
"*gatewaylabel.justify:		right\n",
"*gatewaylabel.label:		Gateway:\n",
"*gatewaylabel.borderWidth:	0\n",
"*gatewaylabel.fromVert:		*localdirlabel\n",
"*gatewaylabel.right:		chainleft\n",
"*gatewaylabel.left:		chainleft\n",
"*gatewaylabel.bottom:		chainbottom\n",
"*gatewaylabel.top:		chainbottom\n",
"*gatewaytext.wcCreate:		AsciiText\n",
"*gatewaytext.wcCallback:	init_connect_info(gateway)\n",
"*gatewaytext.width:		300\n",
"*gatewaytext*editType: 		edit\n",
"*gatewaytext.borderWidth:	1\n",
"*gatewaytext.input:		TRUE\n",
"*gatewaytext.border:		black\n",
"*gatewaytext.fromVert:		*localdirlabel\n",
"*gatewaytext.fromHoriz:		*gatewaylabel\n",
"*gatewaytext.right:		chainright\n",
"*gatewaytext.left:		chainleft\n",
"*gatewaytext.bottom:		chainbottom\n",
"*gatewaytext.top:		chainbottom\n",
"*gatewaytext.translations: 	#override \
				<Key>Help: help() \\n\
				<Key>F1:   help() \\n\
				<Key>Return:    Login() \\n\
				Ctrl<Key>F:     no-op(RingBell) \\n\
				Ctrl<Key>J:     no-op(RingBell) \\n\
				Ctrl<Key>M:     no-op(RingBell) \\n\
				Ctrl<Key>S:	no-op(RingBell) \\n\
				Ctrl<Key>O:     no-op(RingBell) \\n\
				Ctrl<Key>V:     no-op(RingBell) \\n\
				Ctrl<Key>P:     warp_pointer(up) \\n\
				Ctrl<Key>N:     warp_pointer(down)\n",
"*Shellstatus.wcCreate:		TopLevelShell\n",
"*Shellstatus.wcCallback:	NoWindowGroup()\n",
"*Shellstatus.wcChildren:	status_layout\n",
"*Shellstatus.title:		Status Message Log\n",
"*status_layout.wcCreate:	formWidgetClass\n",
"*status_layout.wcChildren:	status_quit, status_clear,\
			        status_text\n",
"*status_quit.wcCreate: 		commandWidgetClass\n",
"*status_quit.label:		Hide\n",
"*status_quit.shapeStyle:	Rectangle\n",
"*status_quit.left: 		chainleft\n",
"*status_quit.right: 		chainleft\n",
"*status_quit.bottom: 		chaintop\n",
"*status_quit.top: 		chaintop\n",
"*status_quit.callback:          WcPopDownCB(~)\n",
"*status_clear.wcCreate:         commandWidgetClass\n",
"*status_clear.label:		Clear Text\n",
"*status_clear.shapeStyle:	Rectangle\n",
"*status_clear.Callback: 	Clear_Text(*status_text)\n",
"*status_clear.right: 		chainleft\n",
"*status_clear.left: 		chainleft\n",
"*status_clear.bottom: 		chaintop\n",
"*status_clear.top: 		chaintop\n",
"*status_clear.fromHoriz:	*status_quit\n",
"*status_text.wcConstructor:     CreateStatusWindow\n",
"*status_text.width:		600\n",
"*status_text.height:		200\n",
"*status_text*scrollVertical:	whenNeeded\n",
"*status_text*scrollHorizontal: 	whenNeeded\n",
"*status_text.borderWidth:	1\n",
"*status_text.fromVert: 		*status_quit\n",
"*status_text.left: 		chainleft\n",
"*status_text.right: 		chainright\n",
"*status_text.bottom: 		chainbottom\n",
"*status_text.top: 		chaintop\n",
"*Shellcommand.wcCreate:     	TopLevelShell\n",
"*Shellcommand.wcCallback:	NoWindowGroup() \
			 	help_register\n",
"*Shellcommand.wcChildren:       command_layout\n",
"*Shellcommand.title:            command_layout\n",
"*command_layout.wcCreate:	formWidgetClass\n",
"*command_layout.wcChildren:  	command_quit, command_clear, \
				ftp\n",
"*command_quit.wcCreate:        	commandWidgetClass\n",
"*command_quit.label:           	Hide\n",
"*command_quit.shapeStyle:      	Rectangle\n",
"*command_quit.left:            	chainleft\n",
"*command_quit.right:           	chainleft\n",
"*command_quit.bottom:          	chaintop\n",
"*command_quit.top:             	chaintop\n",
"*command_quit.callback:      	WcPopDownCB(~)\n",
"*command_clear.wcCreate:    	commandWidgetClass\n",
"*command_clear.label:		Clear Text\n",
"*command_clear.shapeStyle:	Rectangle\n",
"*command_clear.callback: 	Clear_Text(*ftp)\n",
"*command_clear.right: 		chainleft\n",
"*command_clear.left: 		chainleft\n",
"*command_clear.bottom: 		chaintop\n",
"*command_clear.top: 		chaintop\n",
"*command_clear.fromHoriz:	*command_quit\n",
"*ftp.wcConstructor:         	CreateDialogWindow\n",
"*ftp.wcCallback: \
		noop(get put dir action connect notconnected Sensitive) \
		help_register\n",
"*ftp.width:                     600\n",
"*ftp.height:                    200\n",
"*ftp.skipAdjust:		TRUE\n",
"*ftp*scrollVertical:	 	always\n",
"*ftp*scrollHorizontal: 		whenNeeded\n",
"*ftp*AsciiText*XtNwrap:		XawtextWrapNever\n",
"*ftp.min:			200\n",
"*ftp.fromVert: 			*command_quit\n",
"*ftp.left: 			chainleft\n",
"*ftp.right: 			chainright\n",
"*ftp.bottom: 			chainbottom\n",
"*ftp.top:			chaintop\n",
"*ftp.translations:     		#override \
		Ctrl<Key>W:     DeleteWord()\\n\
        	Ctrl<Key>U:     DeleteLine()\\n\
        	Ctrl<Key>H:     InsertSpace() delete-previous-character()\\n\
        	<Key>Delete:    InsertSpace() delete-previous-character()\\n\
        	<Key>BackSpace: InsertSpace() delete-previous-character()\\n\
                <Key>Help:      help() \\n\
                <Key>F1:        help() \\n\
        	<Key>Return:    newline() Dispatch()\n",
"*Shellview.wcCreate:		TopLevelShell\n",
"*Shellview.wcCallback:		NoWindowGroup() \
				help_register\n",
"*Shellview.wcChildren:        	view_layout\n",
"*Shellview.title:             	View File\n",
"*view_layout.wcCreate:       	formWidgetClass\n",
"*view_layout.wcChildren:      	view_quit, view_text\n",
"*view_quit.wcCreate:          	commandWidgetClass\n",
"*view_quit.label:             	Dismiss\n",
"*view_quit.shapeStyle:        	Rectangle\n",
"*view_quit.left:              	chainleft\n",
"*view_quit.right:             	chainleft\n",
"*view_quit.bottom:            	chaintop\n",
"*view_quit.top:               	chaintop\n",
"*view_quit.callback:          	WcDestroyCB(~)\n",
"*view_text.wcCreate:     	AsciiText\n",
"*view_text.wcCallback:		set_view_file \
      			        set_form_vert(^view_quit)\n",
"*view_text.width:             	600\n",
"*view_text.height:            	200\n",
"*view_text*scrollVertical:    	whenNeeded\n",
"*view_text*scrollHorizontal:  	whenNeeded\n",
"*view_text.borderWidth:       	1\n",
"*view_text.left:              	chainleft\n",
"*view_text.right:             	chainright\n",
"*view_text.bottom:            	chainbottom\n",
"*view_text.top:               	chaintop\n",
"*help_General.help_text:\
General Help\\n\
XXXX is a X front end to ftp.\\n\
\\n\
XXXX allows retrieval  or  transmission  of  selected  files  and\\n\
directory trees.\\n\
\\n\
The screen display for XXXX consists of 5 sections:  a  menu  bar\\n\
containing  a  quit  menu,   option menu, file option menu, mutli\\n\
file option menu, and help menu; a status window; a  remote/local\\n\
directory window; a series of buttons login,  remote/local direc-\\n\
tory, command, glob, search, next, reconnect and  archie;  and  a\\n\
scrolled list window.\\n\
\\n\
The status window display the current actions and error messages.\\n\
\\n\
The remote/local directory window display the  remote/local  name\\n\
of the displayed directory.\\n\
\\n\
The login button is used to initiate logins.\\n\
\\n\
The remote/local button toggles between remote and  local  direc-\\n\
tory display's.\\n\
\\n\
The command shell button is used to bring up a shell window  that\\n\
contains a direct interface to ftp.\\n\
\\n\
The glob button is used to select a set of files based  on  shell\\n\
glob syntax or regular expression syntax through a dialog.\\n\
\\n\
The search button is used to find a file or set of files .  based\\n\
on  shell glob syntax or regular expression syntax through a dia-\\n\
log.\\n\
\\n\
The next button will find the next file based on the glob a regu-\\n\
lar expression set by the search button.\\n\
\\n\
The reconnect button will  restart  the  ftp  session  after  the\\n\
foreign host has disconnected due to a inactivity disconnect.\\n\
\\n\
The archie command will bring up a dialog to run a archie command\\n\
if the archie command is in the users search path.\\n\
\\n\
All buttons and menu selections are done with the left mouse but-\\n\
ton.\\n\
\\n\
A file can be selected by clicking the left mouse button  on  the\\n\
file.  Multi file selection are accomplished by clicking the left\\n\
mouse button on the first file and then dragging the mouses  over\\n\
the  files  to  be  selected.   Selected  files  are displayed in\\n\
reverse video.  The current selection has a square border  around\\n\
it.\\n\
\\n\
The scrolled list window has a popup menu that can  be  activated\\n\
by holding down the right mouse button. You can also use the key-\\n\
board to select the listing options, local/remote  display,  sort\\n\
options,  files  or directories, and actions to apply to selected\\n\
files.\\n\
\\n\
You can click the left mouse button with the control key  pressed\\n\
on  a  directory to cd to it.  If you click the left mouse button\\n\
with the control key pressed on a file and it is  a  remote  file\\n\
then the file will be transferred to the local host or if it is a\\n\
local file then it is transferred to the remote host.\n",
"*netrc.help_text:\
moxftprc or netrc\\n\
XXXX will look for  ~/.moxftprc if not found then  it  will  look\\n\
for  ~/.netrc.   The format of of \".moxftprc\" is the same as that\\n\
of \".netrc\"  with  the  addition  of  three  new   tokens  called\\n\
\"remote_dir\", \"local_dir\", and \"note\".  \"note\" should be the last\\n\
token of a entry.\\n\
\\n\
It is not advisable to put  your  password  in  the  \".netrc\"  or\\n\
\".moxftprc\" files.\\n\
example:\\n\
machine ftp.chpc.utexas.edu\\n\
 login anonymous\\n\
 password jones@\\n\
 note Home of xmoftp\\n\
machine ftp.utexas.edu\\n\
 login anonymous\\n\
 password jones@\\n\
 remote_dir /packages/X\\n\
 note Lots of Networking Information\n",
"*xftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by xftp are defined by the following resources:\\n\
 Xftp*font:\\\n\
     -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Command.font:\\\n\
    -*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Text*font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Label*font:\\\n\
    -*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*LabelQUICK*font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*listsw*list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*hostsw*list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*oftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by oftp are defined by the following resources:\\n\
 Oftp*font:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Oftp*listsw.*.list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Oftp*hostsw.*.list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*mftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by mftp are defined by the following resources:\\n\
 Mftp*labelFontList:\\\n\
-*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*buttonFontList:\\\n\
-*-times-medium-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*textFontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar1*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar2*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*archie_menubar*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*rate.fontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp.*.*.list.fontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*quit.help_text:\
Quit Button\\n\
Quit XXXX. Any pending actions will be terminated.\n",
"*listsw*help_text:\
Directory List Window\\n\
The current  local/remote directory  listing is  displayed  here.\\n\
There  are  four  types of listing formats sort, medium, long and\\n\
translations.  It is not always possible for there to be  a  long\\n\
or medium listing format for a remote system.   In such cases the\\n\
short listing format is used.\\n\
\\n\
A file or directory entry can be selected by  clicking  the  left\\n\
most mouse button on the entry.\\n\
\\n\
The right mouse button will select an entry and bring up  a  menu\\n\
with  a  list  of actions that can be applied to that entry.  The\\n\
actions are:\\n\
\\n\
UP      - Go up the directory tree\\n\
Cd      - Cd to selected directory\\n\
Get     - Get remote file\\n\
Put     - Put local file\\n\
View    - View remote file.\\n\
Ascii   - Set transfer mode of file to ascii\\n\
Binary  - Set transfer mode of file to binary\\n\
Tenex   - Set transfer mode to tenix\\n\
Default - Set transfer mode to default\\n\
Ignore  - Ignore file when retriving directory.\\n\
Don't Ignore - Do not ignore file retriving directory.\\n\
Dir     - Refresh directy listing.\\n\
\\n\
The menu also contains actions that can be taken on all  selected\\n\
files:\\n\
\\n\
Clear File Selections - Unselect all files and directories\\n\
Get Selected files    - Get all selected remote files and  direc-\\n\
tories.\\n\
Put Selected files    - Put all selected local files  and  direc-\\n\
tories.\\n\
\\n\
Also see the keyboard input help selection under general help.\n",
"*ftp*help_text:\
Ftp Window\\n\
This is the ftp window.  You can use the following  ftp  commands\\n\
in this window:\\n\
\\n\
 ascii\\n\
 binary\\n\
 delete\\n\
 dir\\n\
 cd <remote directory>\\n\
 help\\n\
 image\\n\
 get <remote file> [<local file>]\\n\
 reg <regular expression>\\n\
 regget\\n\
 regput\\n\
 lcd\\n\
 ls\\n\
 lmkdir\\n\
 lpwd\\n\
 mkdir\\n\
 put\\n\
 pwd\\n\
 quote\\n\
 remotehelp\\n\
 site\\n\
 tenex\n",
"*abort.help_text:\
Abort Button\\n\
Abort ftp. Since there is no reliable way to abort ftp  you  will\\n\
have to login again.\n",
"*op_sort.help_text:\
Option SubMenu - Sort\\n\
The sort option menu can be used to select the type of sort  that\\n\
is  done  on  directories.  Files  can be sorted by age, name, or\\n\
size.  The sort can be reverse or normal sort order.   File  also\\n\
can be sorted by type then age, name, or size.\n",
"*options.help_text:\
Options Menu\\n\
The options menu options, to turn on or of error ignoring  during\\n\
transfers  of  multiple  files,  to turn on or off auto directory\\n\
listing and  two submenus Listing and Sort to change listing for-\\n\
mats  or sort options.  See help on submenus Listing and Sort for\\n\
more information on Listing and Sort submenus.\n",
"*op_listing.help_text:\
Option SubMenu - Listing\\n\
Select the listing options.  There are four list options plus the\\n\
example translations table option:\\n\
\\n\
Short Listing\\n\
Medium Listing\\n\
Long Listing\\n\
Translation Listing\\n\
Translations\\n\
\\n\
The short listing format  displays  the  filename  only.  If  the\\n\
remote  file  system  is an UNIX system then a directory will end\\n\
with \"/\", a link with \"@\" and of offline file with \"%\".   If  the\\n\
remote  file system is not an unix file system then a d is placed\\n\
before the file name to indicate that it is a directory.\\n\
\\n\
The Medium  Listing  format  is  system  dependent.   It  usually\\n\
includes the file length.\\n\
\\n\
The Long Listing format is system dependent.  It usually includes\\n\
the file length, type and protections.\\n\
\\n\
The Translation Listing format will display the remote  to  local\\n\
or  the  local  to  remote  translation  for  the directory being\\n\
displayed.  It also shows the mode the file will  be  transferred\\n\
in.   If XXXX does not know how to translate the filename it will\\n\
leave the translation blank.\\n\
\\n\
The Translations menu option will produce a list of example local\\n\
and remote files and their translations.\n",
"*dir.help_text:\
Directory Window\\n\
The current selected local or remote directory name is  displayed\\n\
here.\n",
"*connect.help_text:\
Login/Close Button\\n\
Login to remote host or close the connection from a remote  host.\\n\
If the option is login, a menu will popup allowing you to set the\\n\
remote host name, the remote host login  name,  the  remote  host\\n\
password, the remote directory name, and the local directory name\\n\
to use at login time.\\n\
\\n\
The retry button informs XXXX to keep retrying connection every 5\\n\
minutes until it is able to log into the remote hosts.\\n\
\\n\
XXXX understands the ftp .netrc file format. It use this to  gen-\\n\
erate  a  menu  that  will  set the hostname, login name, and (if\\n\
specified) the password for the selected host.\\n\
\\n\
A comment for the specified host can be added to the  .xftp  file\\n\
found  in  the  login  directory  using the \"note\" directive; for\\n\
example:\\n\
\\n\
note dinosaur.cc.utexas.edu UTD\\n\
note ftp.uu.net Has most anything that any one would want.\\n\
\\n\
This will be displayed beside the host entry in the host menu.\n",
"*status.help_text:\
Status Window\\n\
Display status information.  Clicking the right mouse  button  on\\n\
the  status  window  will  popup the Status Message Log.  You can\\n\
then view all of the previous status messages.\n",
"*host_name.help_text:\
System Name Window\\n\
The host name of the connected  or  selected  host  is  displayed\\n\
here.\n",
"*system_name.help_text:\
System Type Window\\n\
The System type is displayed here.\n",
"*default_mode.help_text:\
Default Transfer Mode Window\\n\
The default transfer mode is displayed in this window.\n",
"*dir_display.help_text:\
Local/Remote Button\\n\
Toggle between current  local/remote  directories.   A  directory\\n\
listing  is  displayed  of the selected local/remote directory in\\n\
the directory list window.\n",
"*dotxftp.help_text:\
XXXX initialization file\\n\
XXXX reads the \".xftp\" initialization file in the home  directory\\n\
when  it  first starts up.  The \".xftp\" file can contain the fol-\\n\
lowing directives:\\n\
\\n\
trans        <machine type>\\n\
examples_r   <remote file>\\n\
examples_e   <local file>\\n\
unix         <regular expression>\\n\
             <source> [<conversion type>]\\n\
back         <regular expression>\\n\
             <source> [<conversion type>]\\n\
end\\n\
viewer       <audio|ps|picture|tar|text> <comand>\\n\
\\n\
The note directive allows you add a note that is displayed in the\\n\
host  list  menu  in the login window.  It is used in conjunction\\n\
with the \"~/.netrc\" file.\\n\
\\n\
The trans directive start a translation table block of  commands.\\n\
You  can  only  specify the examples_r, examples_e, unix and back\\n\
directive in a translation table block.  The end  directive  ends\\n\
the translation table block.\\n\
\\n\
The examples_r and examples_e directives are used to  generate  a\\n\
example of the translations specified by the unix and back direc-\\n\
tives.\\n\
\\n\
The unix and back directive are used to specific rewriting  rules\\n\
for translating file form the remote system file name to unix and\\n\
back.  You can specify \"ascii\", \"binary\" and \"tenex\" as  <conver-\\n\
sion type>\\n\
\\n\
The examples_r, examples_e,  unix  and  back  directives  can  be\\n\
repeated 50 times each.\\n\
\\n\
The following is example of a translation table  that  you  might\\n\
want for a Vax VMS system running MULTINET.\\n\
\\n\
trans        VMS MULTINET\\n\
examples_r   XFTP_TAR.Z;1\\n\
unix         ([a-z0-9_,]+)_TAR.Z;[0-9]+\\n\
             1.tar.Z binary\\n\
examples_e   xftp.tar.Z\\n\
back         ([A-Z0-9_,]+).tar.Z\\n\
             1_TAR.Z  binary\\n\
end\\n\
\\n\
The unix directive specifies a regular expression to apply to the\\n\
remote  file  name.   If  it matches then the string \"1.tar.Z\" is\\n\
used as the source  to  rewrite  the  file  name.  This  examples\\n\
translate  \"XFTP_TAR.Z;1\"  to the unix file name \"xftp.tar.Z\" and\\n\
specifies that the file is to be transferred in binary mode.\\n\
\\n\
The back directive specifies a regular expression to apply to the\\n\
local unix file.  If it matches then the string \"1_TAR.Z\" is used\\n\
as  the  source  to  rewrite  the  file  name.   The  unix   file\\n\
\"xftp.tar.Z\" should be rewritten as \"XFTP_TAR.Z\".  The file would\\n\
be transferred in binary mode.\\n\
\\n\
The viewer directive spicfies a program  to  execute  to  view  a\\n\
audio, postscript, tar, text and picture files.  XXXX regogonizes\\n\
the filename  extensions  .aiff  and  .au  as  audio  files;  the\\n\
filename   extensions .gif, .tiff, .rgp and .jpg as pictures; the\\n\
the filename extesions .ps as postscript; and the filname  exten-\\n\
sion\\n\
example:\\n\
viewer ps ghostview\\n\
viewer text xless\\n\
viewer pitcure xv\n",
"*list_key_input.help_text:\
Keyboard Input\\n\
The Directory List Window allows the following keyboard input.\\n\
\\n\
   <Key>Help:           Help Menu\\n\
   <Key>F1:             Help Menu\\n\
\\n\
  ~Ctrl ~Shift <Key>h:  Previous item\\n\
  ~Ctrl ~Shift <Key>j:  Down one item\\n\
  ~Ctrl ~Shift <Key>k:  Up one item\\n\
   Ctrl ~Shift <Key>l:  Next item\\n\
\\n\
  ~Ctrl ~Shift <Key>0:  Fisrt item in line\\n\
   Ctrl ~Shift <Key>$:  Last item in line\\n\
\\n\
   Ctrl ~Shift <Key>f:  Next page\\n\
   Ctrl ~Shift <Key>b:  Previous page\\n\
   Ctrl ~Shift <Key>n:  Down one item\\n\
   Ctrl ~Shift <Key>p:  Up one item\\n\
\\n\
   Ctrl ~Shift <Key>j:  Down one item\\n\
  ~Ctrl  Shift <Key>m:  Down one item\\n\
\\n\
           <Key>space:  Select item\\n\
\\n\
   Ctrl ~Shift <Key>t:  Toggle to remote/local directory\\n\
\\n\
  ~Ctrl  Shift <Key>l:  Set long listing format\\n\
  ~Ctrl  Shift <Key>s:  Set short listing format\\n\
  ~Ctrl  Shift <Key>t:  Set translation listing format\\n\
\\n\
  ~Ctrl  <Key>>:        Next page\\n\
  ~Ctrl  <Key><:  Previous page\\n\
   Ctrl  <Key>>:        Bottom\\n\
   Ctrl  <Key><:     Top\\n\
\\n\
  ~Ctrl ~Shift <Key>a:Set file transfer mode to type Ascii\\n\
  ~Ctrl ~Shift <Key>b:Set file transfer mode to type binary\\n\
  ~Ctrl ~Shift <Key>t:Set file transfer mod to tenex\\n\
  ~Ctrl ~Shift <Key>d:Use default transfer mode\\n\
\\n\
  ~Ctrl ~Shift <Key>u:Go to parent directory\\n\
  ~Ctrl ~Shift <Key>c:Change dir to directory\\n\
\\n\
  ~Ctrl ~Shift <Key>g:Get file\\n\
  ~Ctrl ~Shift <Key>p:Put file\\n\
\\n\
   Ctrl        <Key>s:  Search Next\\n\
   Ctrl        <Key>g:  Clear Search Pattern\n",
"*quitm.help_text:\
Quit Menu\\n\
The quit menu contains the abort and quit options.\\n\
\\n\
Since there is no reliable way to abort  ftp  you  will  have  to\\n\
login again after aborting a ftp connection.\n",
"*items.help_text:\
Display Items\\n\
The item display display the count of the following items,  block\\n\
devices,  char   devices,  links,  sockets, files, offline_files,\\n\
selected items, and the total number of items.\n",
"*command.help_text:\
Command Button\\n\
The command button brings up the command shell.  Commands can  be\\n\
given directly to ftp through this shell.\n",
"*hide.help_text:\
Hide Shell\\n\
Hide the current shell.\n",
"*help_quit.help_text:\
Hide Shell\\n\
Hide the help shell.\n",
"*tran_quit.help_text:\
Hide Shell\\n\
Hide the translation shell.\n",
"*status_quit.help_text:\
Hide Shell\\n\
Hide the status shell.\n",
"*command_quit.help_text:\
Hide Shell\\n\
Hide the command shell.\n",
"*Shellconnect.help_text:\
Connect Shell\\n\
Used to specify login  information,  remote  host,  user  number,\\n\
password, local directory and remote directory for XXXX.\n",
"*hosts.help_text:\
Host List Menu\\n\
List of host found in $HOME/.netrc.\n",
"*anonymous.help_text:\
Anonymous login menu\\n\
Can be used to set the login user anonymous and initial password.\\n\
The password can be set to guest, mail address, or user name.\n",
"*DoBoxConnect.help_text:\
Connect button\\n\
Initiate connection.\n",
"*DoHide.help_text:\
Hide Shell\\n\
Hide the connect shell.\n",
"*Shellhelp.help_text:\
Help Shell\\n\
Display text of help message.\n",
"*Shelltran.help_text:\
Translation Shell\\n\
Display translations used with non UNIX systems.\n",
"*Shellstatus.help_text:\
Status Shell\\n\
Display log of status messages.\n",
"*Shellcommand.help_text:\
Ftp Command Shell\\n\
The ftp command shell.\n",
"*Shellview.help_text:\
View Shell\\n\
Shell window brought up to view a text file. If the file ends  in\\n\
.Z  it  will  be uncompressed before viewing if uncompress is the\\n\
users path.  If the file ends in .gz it will unzip if  gunzip  is\\n\
in the users path.\n",
"*Trademarks.help_text:\
Trademarks\\n\
OPEN LOOK is a trademark of AT&T\\n\
UNIX is a registered trademark of AT&T\\n\
The X Window System is a trademark of the Massachusetts Institute\\n\
of Technology.\n",
"*helpm.help_text:\
Help Menu\\n\
The help menu provides a context sensitive help selection  and  a\\n\
general help selection.\\n\
\\n\
If you select the context sensitive  help  selection  the  cursor\\n\
will  change  to  a  cross bar.  You can then position the cursor\\n\
over the object that you want help on and click left  most  mouse\\n\
botton.   If  the  help  system  knows  about  the object it will\\n\
display the help text in the help shell. If it does not  it  will\\n\
display the general help message in the help shell.\n",
"*fileopts.help_text:\
Single File Options Menu\\n\
The single file options menu allows the following  operations  on\\n\
the high lighted file:\\n\
\\n\
 Up           - cd to parent directory\\n\
 Cd           - cd to high lighted directory\\n\
 Get          - get high lighted file or directory\\n\
 View         - view high lighted file\\n\
 Put          - put high lighted file or directory\\n\
 Ascii        - transfer high lighted file in ascii mode\\n\
 Binary       - transfer high lighted file in binary mode\\n\
 Tenex        - transfer high lighted file in tenex mode\\n\
 Default      - transfer high lighted using default transfer mode\\n\
 Ignore       - ignore  high lighted directory/file when\\n\
                transferring contents of a directory\\n\
 Don't ignore - don't ignore high lighted directory/file when\\n\
                transferring contents of a directory\n",
"*filesopts.help_text:\
Multi File Options Menu\\n\
The multi file options menu allows the  following  operations  on\\n\
the selected files:\\n\
\\n\
Clear File Selections - Clear all file selections in current\\n\
        directory\\n\
Get Selected Files    - Get selected file in current directory\\n\
Put Selected Files    - Put selected file in current directory\n",
"*archie_command.help_text:\
Archie Button\\n\
The archie button brings up arche interface shell.\n",
"*DoArchie.help_text:\
Archie Button\\n\
The archie button brings up arche interface shell.\n",
"*DoGateway.help_text:\
Gateway Button\\n\
Enable suns passthrough ftp gateway.\\n\
slag The Search Host List Dialog is activated  by  the  following\\n\
keys in the host list window:\\n\
   Ctrl        <Key>s:  Search Next\\n\
   Ctrl        <Key>g:  Clear Search Pattern\\n\
slag *Shellsearchhostdialog.help_text:\\n\
Search Host List Dialog\\n\
Set search string for regular expression search of the host  list\\n\
in the Connect Shell.\\n\
The Search Host List Dialog has the following keyboard input:\\n\
  <Key>Return:      Start search\\n\
   Ctrl<Key>r:      Start search\\n\
   Ctrl<Key>c:      Abort search\n",
"*Shellsearchdialog.help_text:\
Search Dialog\\n\
Set search string for regular expression  search  or  shell  glob\\n\
search of file.\\n\
The Search Dialog has the following keyboard input:\\n\
   <Key>Return:    Start glob search\\n\
   Ctrl<Key>r:     Start regualar expression search\\n\
   Ctrl<Key>g:     Start glob search\\n\
   Ctrl<Key>c:     Abort Search\n",
"*Shellglobdialog.help_text:\
Glob Dialog\\n\
Select files based on shell glob expression  or  regular  expres-\\n\
sions.\n",
"*reconnect.help_text:\
Recconect Button\\n\
The recconect button allows the continuation of ftp session after\\n\
the server has disconnected the seesion.\n",
"*glob.help_text:\
Glob Button\\n\
The Glob button will bring up a glob dialog which will allow  the\\n\
selection/deselection of files based on a regular expression or a\\n\
shell glob expression  search.\n",
"*search.help_text:\
Search Button\\n\
The Search button will bring up a search  dialog  to  search  the\\n\
current  directory   for the specified item.   The  search can be\\n\
based on regular expression  or shell globing.  The  Search  Next\\n\
button  will  search  for  the next item that matches the regular\\n\
expression or shell glob.\n",
"*next.help_text:\
Search Next Button\\n\
Search for the next item that matches the regular  expression  or\\n\
shell globing expression.\n",
NULL,
};
#endif
#if defined(OPENWINDOW)
String fallback_resources[] = {
"Oftp.wcChildren:          	layout\n",
"Oftp.wcPopups:			Shellstatus, Shellhelp, Shellconnect,\
				Shellcommand\n",
"Oftp.title:                    	Oftp\n",
"Oftp.allowShellResize:		FALSE\n",
"Oftp*wcTrace:                   FALSE\n",
"Oftp*background:        	grey\n",
"*width_in_chars:                85\n",
"*font:                         -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*listsw.*.list.font:           -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*hostsw.*.list.font:           -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*translations:  #override \
                <Key>Help:      help() \\n\
                <Key>F1:        help()\n",
"*layout.wcClass:		Form\n",
"*layout.wcChildren:            	quitm, options, \
				   fileopts, filesopts, helpm, \
				status, \
                                dir, \
				host_name, system_name, default_mode, \
                                connect, dir_display, command, glob, search, \
				    next, reconnect, archie_command, rate,\
                                percent, \
				listsw\n",
"*quitm.wcClass:			menuButtonWidgetClass\n",
"*quitm.wcCallback:    		WcCreateChildren( this*pane, quit, abort) \
                             	help_register \n",
"*quitm.label:              	Quit\n",
"*quitm*pane.abort.wcClass:	oblongButtonGadgetClass\n",
"*quitm*pane.abort.Label:	Abort\n",
"*quitm*pane.abort.select:	abort\n",
"*quitm*pane.quit.wcClass:	oblongButtonGadgetClass\n",
"*quitm*pane.quit.Label:		Quit\n",
"*quitm*pane.quit.select: 	Quit\n",
"*options.wcClass:  		menuButtonWidgetClass            \n",
"*options.wcCallback:            noop(get put dir connect action Sensitive) \
      				help_register \
   WcCreateChildren( this*pane, op_1, op_2, line, op_3, op_4)\n",
"*options.xRefName:		quitm\n",
"*options.xAddWidth:		TRUE\n",
"*options.xOffset:		4\n",
"*options.Label:                 Options\n",
"*options.translations:  	#override \
                <Key>Help:      help() \\n\
                <Key>F1:        help()\n",
"*options*pane.op_1.wcCreate:	oblongButtonGadgetClass\n",
"*options*pane.op_1.label:       Ignore Errors\n",
"*options*pane.op_1*select:      op(IgnoreErrors)\n",
"*options*pane.op_2.wcCreate:	oblongButtonGadgetClass\n",
"*options*pane.op_2.wcCallback:  op(NoAutoDir)\n",
"*options*pane.op_2.label:       No Auto Directory Listings\n",
"*options*pane.op_2*select:      op(NoAutoDir)\n",
"*options*pane.line.wcCreate:   	CreateSep\n",
"*options*pane.op_3.wcClass:  	menuButtonWidgetClass\n",
"*options*pane.op_3.wcCallback: \
   WcCreateChildren( this*pane, lmenu1, lmenu2, lmenu3, lmenu4, sep, lmenu6) \
                             	help_register \n",
"*options*pane.op_3.label:    	Listing Options\n",
"*options*pane.op_4.wcClass:  	menuButtonWidgetClass\n",
"*options*pane.op_4.wcCallback: \
   WcCreateChildren( this*pane, smbyname, smbysize, smbyage, line, \
				smbytype, smnormal) \
                             	help_register \n",
"*options*pane.op_4.label:    	Sort Options\n",
"*options*pane.op_3*pane.lmenu1.wcClass:	oblongButtonGadgetClass\n",
"*options*pane.op_3*pane.lmenu1.wcCallback:toggle(listing, SHORT) \
                               		 mark_menu()\n",
"*options*pane.op_3*pane.lmenu1.Label:  	Short listing\n",
"*options*pane.op_3*pane.lmenu1.select:	listing_type(SHORT) \n",
"*options*pane.op_3*pane.lmenu2.wcClass:	oblongButtonGadgetClass\n",
"*options*pane.op_3*pane.lmenu2.wcCallback:toggle(listing, MEDIUM)\n",
"*options*pane.op_3*pane.lmenu2.label:  	Medium listing\n",
"*options*pane.op_3*pane.lmenu2.select:	listing_type(MEDIUM) \n",
"*options*pane.op_3*pane.lmenu3.wcClass:	oblongButtonGadgetClass\n",
"*options*pane.op_3*pane.lmenu3.wcCallback:toggle(listing, LONG)\n",
"*options*pane.op_3*pane.lmenu3.label:  	Long listing\n",
"*options*pane.op_3*pane.lmenu3.select:	listing_type(LONG) \n",
"*options*pane.op_3*pane.lmenu4.wcClass:	oblongButtonGadgetClass\n",
"*options*pane.op_3*pane.lmenu4.wcCallback:toggle(listing, TRANSLATIONS)\n",
"*options*pane.op_3*pane.lmenu4.label:	Translation listing\n",
"*options*pane.op_3*pane.lmenu4.select:	listing_type(TRANSLATIONS) \n",
"*options*pane.op_3*pane.sep.wcCreate:	CreateSep\n",
"*options*pane.op_3*pane.lmenu6.wcClass: oblongButtonGadgetClass\n",
"*options*pane.op_3*pane.lmenu6.label:   Translations\n",
"*options*pane.op_3*pane.lmenu6.select:	List_Translations\n",
"*options*pane.op_4*pane.smbyname.wcClass: oblongButtonGadgetClass\n",
"*options*pane.op_4*pane.smbyname.wcCallback:toggle(sort_type, SORT_BY_NAME)\n",
"*options*pane.op_4*pane.smbyname.sensitive: 	FALSE\n",
"*options*pane.op_4*pane.smbyname.label:		Sort By Name\n",
"*options*pane.op_4*pane.smbyname.select:	listing_type(SORT_BY_NAME) \n",
"*options*pane.op_4*pane.smbysize.wcClass:   	oblongButtonGadgetClass\n",
"*options*pane.op_4*pane.smbysize.wcCallback:toggle(sort_type, SORT_BY_SIZE)\n",
"*options*pane.op_4*pane.smbysize.label: 	Sort By Size\n",
"*options*pane.op_4*pane.smbysize.select:	listing_type(SORT_BY_SIZE) \n",
"*options*pane.op_4*pane.smbyage.wcClass:   	oblongButtonGadgetClass\n",
"*options*pane.op_4*pane.smbyage.wcCallback:toggle(sort_type, SORT_BY_AGE)\n",
"*options*pane.op_4*pane.smbyage.label: 		Sort By Age\n",
"*options*pane.op_4*pane.smbyage.select:		listing_type(SORT_BY_AGE) \n",
"*options*pane.op_4*pane.line.wcCreate:   	CreateSep\n",
"*options*pane.op_4*pane.smbytype.wcClass:    	oblongButtonGadgetClass\n",
"*options*pane.op_4*pane.smbytype.label:      	Sort By Type\n",
"*options*pane.op_4*pane.smbytype.select:	listing_type(SORT_BY_TYPE) \n",
"*options*pane.op_4*pane.smnormal.wcClass:    	oblongButtonGadgetClass\n",
"*options*pane.op_4*pane.smnormal.label:  	Reverse\n",
"*options*pane.op_4*pane.smnormal.select:	listing_type(NORMAL) \n",
"*fileopts.wcCreate:             menuButtonWidgetClass\n",
"*fileopts.wcCallback:           noop(get put dir connect action Sensitive) \
                             	help_register \
   WcCreateChildren( this*pane, filem_UP, filem_CD, filem_GET, filem_VIEW, \
                                filem_PUT, filem_Ascii, filem_Binary, \
                                filem_Tenex, filem_Default \
				filem_ignore, filem_use, filem_DIR) \n",
"*fileopts.label:       		File Options \n",
"*fileopts.xRefName:		options\n",
"*fileopts.xAddWidth:		TRUE\n",
"*fileopts.xOffset:		4\n",
"*fileopts*pane.filem_UP.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_UP.wcCallback: 	Register_action(up)\n",
"*fileopts*pane.filem_UP.label:		Up\n",
"*fileopts*pane.filem_UP.select:		SetFileAction_menu()\n",
"*fileopts*pane.filem_CD.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_CD.wcCallback: 	Register_action(cd)\n",
"*fileopts*pane.filem_CD.label:		Cd\n",
"*fileopts*pane.filem_CD.select:		SetFileAction_menu()\n",
"*fileopts*pane.filem_GET.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_GET.wcCallback: 	Register_action(get)\n",
"*fileopts*pane.filem_GET.label:		Get\n",
"*fileopts*pane.filem_GET.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_VIEW.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_VIEW.wcCallback: 	Register_action(view)\n",
"*fileopts*pane.filem_VIEW.label:	View\n",
"*fileopts*pane.filem_VIEW.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_PUT.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_PUT.wcCallback: 	Register_action(put)\n",
"*fileopts*pane.filem_PUT.label:		Put\n",
"*fileopts*pane.filem_PUT.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_Ascii.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_Ascii.wcCallback: 	Register_action(ascii)\n",
"*fileopts*pane.filem_Ascii.label:	Ascii\n",
"*fileopts*pane.filem_Ascii.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_Binary.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_Binary.wcCallback: Register_action(binary)\n",
"*fileopts*pane.filem_Binary.label:	Binary\n",
"*fileopts*pane.filem_Binary.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_Tenex.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_Tenex.wcCallback: 	Register_action(tenex)\n",
"*fileopts*pane.filem_Tenex.label:	Tenex\n",
"*fileopts*pane.filem_Tenex.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_Default.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_Default.wcCallback:Register_action(default)\n",
"*fileopts*pane.filem_Default.label:	Default\n",
"*fileopts*pane.filem_Default.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_ignore.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_ignore.wcCallback: Register_action(ignore)\n",
"*fileopts*pane.filem_ignore.label:	Ignore\n",
"*fileopts*pane.filem_ignore.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_use.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_use.wcCallback:    Register_action(use)\n",
"*fileopts*pane.filem_use.label:	        Don't ignore\n",
"*fileopts*pane.filem_use.select:	SetFileAction_menu()\n",
"*fileopts*pane.filem_DIR.wcClass: 	oblongButtonGadgetClass\n",
"*fileopts*pane.filem_DIR.wcCallback:    Register_action(dir)\n",
"*fileopts*pane.filem_DIR.label:	        Dir\n",
"*fileopts*pane.filem_DIR.select:	SetFileAction_menu()\n",
"*filesopts.wcCreate:  		menuButtonWidgetClass\n",
"*filesopts.wcCallback:          help_register \
	noop(get put dir connect action Sensitive ifsensitive) \
				Single_File_Actions \
   WcCreateChildren( this*pane,  \
	filesm_CLEAR, filesm_GET_ALL, filesm_PUT_ALL)\n",
"*filesopts.sensitive:           FALSE\n",
"*filesopts.label:      		Multi File Options \n",
"*filesopts.xRefName:		fileopts\n",
"*filesopts.xAddWidth:		TRUE\n",
"*filesopts.xOffset:		4\n",
"*filesopts*pane.filesm_CLEAR.wcClass: 	oblongButtonGadgetClass\n",
"*filesopts*pane.filesm_CLEAR.wcCallback:Register_action(clear_all)\n",
"*filesopts*pane.filesm_CLEAR.label:	Clear File Selections\n",
"*filesopts*pane.filesm_CLEAR.select:	SetFileAction_menu()\n",
"*filesopts*pane.filesm_GET_ALL.wcClass:	oblongButtonGadgetClass\n",
"*filesopts*pane.filesm_GET_ALL.wcCallback:Register_action(get_all)\n",
"*filesopts*pane.filesm_GET_ALL.label:	Get Selected Files\n",
"*filesopts*pane.filesm_GET_ALL.select:	SetFileAction_menu()\n",
"*filesopts*pane.filesm_PUT_ALL.wcClass:	oblongButtonGadgetClass\n",
"*filesopts*pane.filesm_PUT_ALL.wcCallback:Register_action(put_all)\n",
"*filesopts*pane.filesm_PUT_ALL.label:	Put Selected Files\n",
"*filesopts*pane.filesm_PUT_ALL.select:	SetFileAction_menu()\n",
"*helpm.wcCreate:              	menuButtonWidgetClass\n",
"*helpm.wcCallback: 		help_register \
       WcCreateChildren( this*pane, help_context, help_general) \n",
"*helpm.label:                 	Help\n",
"*helpm.xRefName:              	filesopts\n",
"*helpm.xAddWidth:            	TRUE\n",
"*helpm.xOffset:               	4\n",
"*helpm.select:                	Help()\n",
"*helpm*pane.help_context.wcClass: oblongButtonGadgetClass\n",
"*helpm*pane.help_context.Label:   Context Sensitive Help\n",
"*helpm*pane.help_context.select:  help(c)\n",
"*helpm*pane.help_general.wcClass: oblongButtonGadgetClass\n",
"*helpm*pane.help_general.Label:   General Help\n",
"*helpm*pane.help_general.select:  help()\n",
"*status.wcCreate:               Statictext\n",
"*status.wcCallback:		help_register set_width(1) \n",
"*status.gravity:		west\n",
"*status.yRefName:		quitm\n",
"*status.yAddHeight:		TRUE\n",
"*status.yOffset:		2\n",
"*status.xAttachRight:		TRUE\n",
"*status.xVaryOffset:		FALSE\n",
"*status.xResizable:		TRUE\n",
"*status.recomputeSize:		FALSE\n",
"*status.borderWidth:		2\n",
"*status.translations:     	#override \
                		<Key>Help:      help() \\n\
                		<Key>F1:        help() \\n\
                                <Btn1Down>: WcPopupACT(*Shellstatus) \
					    SetIcons(*Shellstatus) \n",
"*dir.wcCreate:         		Statictext\n",
"*dir.wcCallback:		help_register set_width(1)\n",
"*dir.gravity:			west\n",
"*dir.xRefName:		  	layout\n",
"*dir.borderWidth:		2\n",
"*dir.xAttachRight:		TRUE\n",
"*dir.xVaryOffset:		FALSE\n",
"*dir.xResizable:		TRUE\n",
"*dir.yRefName:			status\n",
"*dir.yAddHeight:		TRUE\n",
"*dir.yOffset:			2\n",
"*dir.recomputeSize:		FALSE\n",
"*host_name.wcCreate:        	Statictext\n",
"*host_name.wcCallback:		help_register set_width(3, -6) \n",
"*host_name.gravity:		west\n",
"*host_name.borderWidth:    	0	\n",
"*host_name.recomputeSize:	FALSE\n",
"*host_name.yRefName:		dir\n",
"*host_name.yAddHeight:		TRUE\n",
"*host_name.yOffset:		2\n",
"*system_name.wcCreate:     	Statictext\n",
"*system_name.wcCallback:	help_register set_width(3, -6)\n",
"*system_name.recomputeSize:	FALSE\n",
"*system_name.borderWidth:	0	\n",
"*system_name.xAttachRight:	TRUE\n",
"*system_name.xVaryOffset:	TRUE\n",
"*system_name.xResizable:	TRUE\n",
"*system_name.yRefName:		dir\n",
"*system_name.yAddHeight:	TRUE\n",
"*system_name.yOffset:		2\n",
"*system_name.xRefName:		host_name\n",
"*system_name.xAddWidth:		TRUE\n",
"*system_name.xOffset:		2\n",
"*default_mode.wcCreate:  	Statictext \n",
"*default_mode.wcCallback:	help_register set_width(3, -6)\n",
"*default_mode.recomputeSize:	FALSE\n",
"*default_mode.xAttachRight:	TRUE\n",
"*default_mode.xVaryOffset:	TRUE\n",
"*default_mode.borderWidth:	0\n",
"*default_mode.yRefName:    	dir\n",
"*default_mode.yAddHeight:       TRUE\n",
"*default_mode.yOffset:          2\n",
"*default_mode.xRefName:         system_name\n",
"*default_mode.xAddWidth:        TRUE\n",
"*default_mode.xOffset:          2\n",
"*default_mode.gravity:		east\n",
"*connect.wcCreate:		oblongButtonWidgetClass\n",
"*connect.wcCallback:		help_register \
			        help_register(system_list) \
                                help_register(dotxftp) \
				help_register(netrc) \
				help_register(Trademarks) \
			    	help_register(list_key_input) \
				help_register(oftp_fonts) \
                                help_register(Shellcommand) \
                                help_register(Shellview) \
                                help_register(Shelltran) \
                                help_register(Shellconnect) \
				help_register(Shellstatus) \
				help_register(Shellhelp) \
				help_register(Shellglobdialog) \
				help_register(Shellsearchdialog) \
				help_register(Shellsearchhostdialog) \
				help_register(op_listing) \
				help_register(op_sort)\n",
"*connect.label:           	Login\\ \\ \n",
"*connect.yRefName:		host_name\n",
"*connect.yAddHeight:		TRUE\n",
"*connect.yOffset:		4\n",
"*connect.select:		Set_noop(connect) connect_disconnect() \n",
"*dir_display.wcCreate:		oblongButtonWidgetClass\n",
"*dir_display.wcCallback:        noop(get put dir connect action Sensitive) \
                                help_register\n",
"*dir_display.label:		Remote\n",
"*dir_display.xRefName:		connect\n",
"*dir_display.xAddWidth:		TRUE\n",
"*dir_display.xOffset:		4\n",
"*dir_display.yRefName:		host_name\n",
"*dir_display.yAddHeight:	TRUE\n",
"*dir_display.yOffset:		4\n",
"*dir_display.select:		remote_local_toggle\n",
"*command.wcCreate:		oblongButtonWidgetClass\n",
"*command.wcCallback:		help_register\n",
"*command.label:           	Command Shell\n",
"*command.xRefName:		dir_display\n",
"*command.xAddWidth:		TRUE\n",
"*command.xOffset:		4\n",
"*command.yRefName:		host_name\n",
"*command.yAddHeight:		TRUE\n",
"*command.yOffset:		4\n",
"*command.select:	 	WcPopupCB(*Shellcommand) \
				raise_window(*Shellcommand) \
			        SetIcons(*Shellcommand)\n",
"*glob.wcCreate:        		oblongButtonWidgetClass\n",
"*glob.wcCallback:		CreateGlobDialog help_register\
	noop(get put dir connect notconnected action Sensitive ifsensitive)\n",
"*glob.label:        		Glob\n",
"*glob.xRefName:			command\n",
"*glob.xAddWidth:		TRUE\n",
"*glob.xOffset:			4\n",
"*glob.yRefName:			host_name\n",
"*glob.yAddHeight:		TRUE\n",
"*glob.yOffset:			4\n",
"*glob.select:			PositionDialog(*Shellglobdialog) \
				WcPopupCB(*Shellglobdialog) \
				raise_window(*Shellglobdialog) \n",
"*Shellglobdialog*translations:   #override \
   <Key>Help:                   help() \\n\
   <Key>F1:                     help() \\n\
\n",
"*Shellglobdialog*text*translations:   #override \
   <Key>Help:                   help() \\n\
   <Key>F1:                     help() \\n\
   <Key>Return:                 WcPopdownACT(*Shellglobdialog) \
                                set_glob_text(glob) \\n\
   Ctrl<Key>r:                  WcPopdownACT(*Shellglobdialog) \
                                set_glob_text(reg) \\n\
   Ctrl<Key>g:                  WcPopdownACT(*Shellglobdialog) \
                                set_glob_text(glob) \\n\
   Ctrl<Key>c:                  WcPopdownACT(*Shellglobdialog)\n",
"*search.wcCreate:      		oblongButtonWidgetClass\n",
"*search.wcCallback:		CreateSearchDialog  help_register\
	noop(get put dir connect notconnected action Sensitive ifsensitive)\n",
"*search.label:        		Search\n",
"*search.xRefName:		glob\n",
"*search.xAddWidth:		TRUE\n",
"*search.xOffset:		4\n",
"*search.yRefName:		host_name\n",
"*search.yAddHeight:		TRUE\n",
"*search.yOffset:		4\n",
"*search.select:			PositionDialog(*Shellsearchdialog) \
				WcPopupCB(*Shellsearchdialog) \
				raise_window(*Shellsearchdialog)\n",
"*next.wcCreate:      		oblongButtonWidgetClass\n",
"*next.wcCallback: help_register \
	noop(get put dir connect notconnected action Sensitive ifsensitive)\n",
"*next.label:        		Search Next\n",
"*next.xRefName:			search\n",
"*next.xAddWidth:		TRUE\n",
"*next.xOffset:			4\n",
"*next.yRefName:			host_name\n",
"*next.yAddHeight:		TRUE\n",
"*next.yOffset:			4\n",
"*next.sensitive:		FALSE\n",
"*next.select:	 		search_next()	\n",
"*reconnect.wcCreate:           	oblongButtonWidgetClass\n",
"*reconnect.wcCallback:		help_register\n",
"*reconnect.label:        	Reconnect\n",
"*reconnect.xRefName:		next\n",
"*reconnect.xAddWidth:		TRUE\n",
"*reconnect.xOffset:		4\n",
"*reconnect.yRefName:		host_name\n",
"*reconnect.yAddHeight:		TRUE\n",
"*reconnect.yOffset:		4\n",
"*reconnect.select:		Reconnect\n",
"*reconnect.sensitive:		False\n",
"*archie_command.wcCreate:	oblongButtonWidgetClass\n",
"*archie_command.wcCallback:	archie_noop() help_register\n",
"*archie_command.label:         	Archie\n",
"*archie_command.xRefName:	reconnect\n",
"*archie_command.xAddWidth:	TRUE\n",
"*archie_command.xOffset:	4\n",
"*archie_command.yRefName:	host_name\n",
"*archie_command.yAddHeight:	TRUE\n",
"*archie_command.yOffset:	4\n",
"*archie_command.select:	 	archie()\n",
"*rate.wcCreate:                 LableQUICKClass\n",
"*rate.label:                    \\ \\ \\ \\ \\ \\ \\ \\ \\ \\  \n",
"*rate.justify:                  right\n",
"*rate.xRefName:			archie_command\n",
"*rate.xAddWidth:		TRUE\n",
"*rate.borderWidth:		0\n",
"*rate.xOffset:			2\n",
"*rate.yRefName:			host_name\n",
"*rate.yAddHeight:		TRUE\n",
"*rate.yOffset:			4\n",
"*rate.traversalOn:	 	TRUE\n",
"*rate.topMargin:		0\n",
"*rate.bottomMargin:		0\n",
"*rate.rightMargin:		0\n",
"*rate.leftMargin:		0\n",
"*rate.xAttachRight:		TRUE\n",
"*rate.xVaryOffset:		FALSE\n",
"*rate.xResizable:		TRUE\n",
"*rate.gravity:			east\n",
"*rate.translations:          	\
                		<Key>Help:      help() \\n\
                		<Key>F1:        help() \n",
"*percent.wcCreate:          	Stub\n",
"*percent.wcCallback:		help_register set_width(1)\n",
"*percent.height:		3\n",
"*percent.borderWidth:		0\n",
"*percent.xAttachRight:		TRUE\n",
"*percent.xVaryOffset:		FALSE\n",
"*percent.xResizable:		TRUE\n",
"*percent.yRefName:		connect\n",
"*percent.yAddHeight:		TRUE\n",
"*percent.yOffset:		2\n",
"*percent.translations:          \
                		<Key>Help:      help() \\n\
                		<Key>F1:        help() \\n\
				<Expose>: resize_percent()\n",
"*listsw.wcCreate:		MyListSW\n",
"*listsw.wcCallback:             noop(get put dir action connect notconnected) \
                                help_register \
				set_width(1) \
				CreateContinueDialog() \
				WcCreatePopups(*list, fmenu)\n",
"*listsw.height:                 400\n",
"*listsw.yRefName:		percent\n",
"*listsw.yAddHeight:		TRUE\n",
"*listsw.yOffset:		2\n",
"*listsw.xOffset:		0\n",
"*listsw.xAttachRight:		TRUE\n",
"*listsw.xVaryOffset:		FALSE\n",
"*listsw.xResizable:		TRUE\n",
"*listsw.yAttachBottom:		TRUE\n",
"*listsw.yVaryOffset:           	FALSE\n",
"*listsw.yResizable:            	TRUE\n",
"*listsw.borderWidth:            1\n",
"*listsw.labelw:                 TRUE\n",
"*listsw.list.width:             1\n",
"*listsw.list.height:            1\n",
"*listsw.list.borderWidth:       1\n",
"*listsw.list.Callback:          list_notify\n",
"*listsw.list.traversalOn:	TRUE\n",
"*listsw.list.columnSpacing:     0\n",
"*listsw.list.defaultColumns:    1\n",
"*listsw.list.rowSpacing:        0\n",
"*listsw.list.translations:    	#override \
   <Key>Help:  		help() \\n\
   <Key>F1:    		help() \\n\
  ~Ctrl ~Shift <Key>h:	Listop(Left)\\n\
  ~Ctrl ~Shift <Key>k:  Listop(Up)\\n\
  ~Ctrl ~Shift <Key>l:  Listop(Right)\\n\
  ~Ctrl ~Shift <Key>j:	Listop(Down)\\n\
  ~Ctrl   <Key>dollar:  Listop(End)\\n\
  ~Ctrl ~Shift <Key>0:	Listop(Start)\\n\
  ~Ctrl  Shift <Key>m:	Listop(Down)\\n\
   Ctrl ~Shift <Key>f:  Listop(NextPage)\\n\
   Ctrl ~Shift <Key>b:  Listop(PrevPage)\\n\
   Ctrl ~Shift <Key>n:	Listop(Down)\\n\
   Ctrl ~Shift <Key>j:	Listop(Down)\\n\
   Ctrl ~Shift <Key>p:	Listop(Up)\\n\
  ~Shift  ~Meta ~Alt <Key>space: Listop(Select) \\n\
   Ctrl ~Shift <Key>t:	remote_local_toggle()\\n\
  ~Ctrl  Shift <Key>l:	Listing_type(LONG)\\n\
  ~Ctrl  Shift <Key>s: 	Listing_type(SHORT)\\n\
  ~Ctrl  Shift <Key>t:	Listing_type(TRANSLATIONS)\\n\
  ~Ctrl  <Key>greater:	Listop(NextPage)\\n\
  ~Ctrl  <Key>less:  	Listop(PrevPage)\\n\
   Ctrl  <Key>greater: 	Listop(Bottom)\\n\
   Ctrl  <Key>less:     Listop(Top)\\n\
  ~Ctrl ~Shift <Key>u:	SetFileAction(up)\\n\
  ~Ctrl ~Shift <Key>c:	SetFileAction(cd)\\n\
  ~Ctrl ~Shift <Key>g:	SetFileAction(get)\\n\
  ~Ctrl ~Shift <Key>p:	SetFileAction(put)\\n\
  ~Ctrl ~Shift <Key>a:	SetFileAction(ascii)\\n\
  ~Ctrl ~Shift <Key>b:	SetFileAction(binary)\\n\
  ~Ctrl ~Shift <Key>d:	SetFileAction(default)\\n\
  ~Ctrl ~Shift <Key>t:	SetFileAction(tenex)\\n\
   Ctrl        <Key>s:  search_next()\\n\
   Ctrl        <Key>g:  search_clear()\\n\
   Button1<Motion>:     Set(M)\\n\
   ~Ctrl <Btn1Down>:    Set()\\n\
   ~Ctrl <Btn1Up>:      Notify()\\n\
   Ctrl <Btn1Down>:     Set(x) Open_file() \\n\
   ~Ctrl <Btn3Down>:    Single_File_Actions() \
                        MyPopup(fmenu) \n",
"*Shellsearchdialog*translations:   #override \
   <Key>Help:                   help() \\n\
   <Key>F1:                     help() \\n\
\n",
"*Shellsearchdialog*text*translations:   #override \
   <Key>Help:                   help() \\n\
   <Key>F1:                     help() \\n\
   <Key>Return:                 WcPopdownACT(*Shellsearchdialog) \
                                set_search_text(glob) \\n\
   Ctrl<Key>r:                  WcPopdownACT(*Shellsearchdialog) \
                                set_search_text(reg) \\n\
   Ctrl<Key>g:                  WcPopdownACT(*Shellsearchdialog) \
                                set_search_text(glob) \\n\
   Ctrl<Key>c:                  WcPopdownACT(*Shellsearchdialog)\n",
"*fmenu.wcClass:		menuShellWidgetClass\n",
"*fmenu.title:		Signle File Options\n",
"*fmenu.pushpin:		none\n",
"*fmenu.wcCallback: WcCreateChildren( this*pane, \
                         fmenu_UP, fmenu_Cd, fmenu_GET, fmenu_PUT, fmenu_VIEW, \
                         fmenu_Ascii, fmenu_Binary, fmenu_Tenex, \
                         fmenu_Default,\
			 fmenu_ignore,\
			 fmenu_use,\
			 fmenu_dir, \
			 sep,\
			 fmenu_clear_all,\
			 fmenu_get_all, fmenu_put_all, fmenu_delete_all)\n",
"*fmenu.unmapCallback:	 Clear_List_Entry()\n",
"*fmenu*pane.fmenu_UP.wcClass:		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_UP.wcCallback:	Register_action(up)\n",
"*fmenu*pane.fmenu_UP.label:    		Up\n",
"*fmenu*pane.fmenu_UP.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_Cd.wcClass:  		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_Cd.wcCallback:	Register_action(cd)\n",
"*fmenu*pane.fmenu_Cd.label:    		Cd\n",
"*fmenu*pane.fmenu_Cd.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_GET.wcClass: 		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_GET.wcCallback:	Register_action(get)\n",
"*fmenu*pane.fmenu_GET.label:   		Get\n",
"*fmenu*pane.fmenu_GET.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_PUT.wcClass:		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_PUT.wcCallback:	Register_action(put)\n",
"*fmenu*pane.fmenu_PUT.label:   		Put\n",
"*fmenu*pane.fmenu_PUT.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_VIEW.wcClass:		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_VIEW.wcCallback:	Register_action(view)\n",
"*fmenu*pane.fmenu_VIEW.label:  		View\n",
"*fmenu*pane.fmenu_VIEW.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_Ascii.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_Ascii.wcCallback:	Register_action(ascii)\n",
"*fmenu*pane.fmenu_Ascii.label:		Ascii\n",
"*fmenu*pane.fmenu_Ascii.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_Binary.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_Binary.wcCallback:	Register_action(binary)\n",
"*fmenu*pane.fmenu_Binary.label:		Binary\n",
"*fmenu*pane.fmenu_Binary.select: 	SetFileAction()\n",
"*fmenu*pane.fmenu_Tenex.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_Tenex.wcCallback:	Register_action(tenex)\n",
"*fmenu*pane.fmenu_Tenex.label:		Tenex\n",
"*fmenu*pane.fmenu_Tenex.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_Default.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_Default.wcCallback:	Register_action(default)\n",
"*fmenu*pane.fmenu_Default.label: 	Default Mode\n",
"*fmenu*pane.fmenu_Default.select: 	SetFileAction()\n",
"*fmenu*pane.fmenu_ignore.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_ignore.wcCallback: 	Register_action(ignore)\n",
"*fmenu*pane.fmenu_ignore.label:		Ignore\n",
"*fmenu*pane.fmenu_ignore.select:    	SetFileAction()\n",
"*fmenu*pane.fmenu_use.wcClass: 		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_use.wcCallback:    	Register_action(use)\n",
"*fmenu*pane.fmenu_use.label:	        Don't ignore\n",
"*fmenu*pane.fmenu_use.select:		SetFileAction()\n",
"*fmenu*pane.fmenu_dir.wcClass: 		oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_dir.wcCallback:    	Register_action(dir)\n",
"*fmenu*pane.fmenu_dir.label:	        Dir\n",
"*fmenu*pane.fmenu_dir.select:		SetFileAction()\n",
"*fmenu*pane.sep.wcCreate: 		CreateSep\n",
"*fmenu*pane.fmenu_clear_all.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_clear_all.wcCallback:	Register_action(clear_all)\n",
"*fmenu*pane.fmenu_clear_all.label: 	Clear File Selections\n",
"*fmenu*pane.fmenu_clear_all.select: 	SetFileAction()\n",
"*fmenu*pane.fmenu_get_all.wcClass:  	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_get_all.wcCallback:   Register_action(get_all)\n",
"*fmenu*pane.fmenu_get_all.label: 	Get Selected Files\n",
"*fmenu*pane.fmenu_get_all.select: 	SetFileAction()\n",
"*fmenu*pane.fmenu_put_all.wcClass: 	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_put_all.wcCallback:   Register_action(put_all)\n",
"*fmenu*pane.fmenu_put_all.sensitive:   	FALSE\n",
"*fmenu*pane.fmenu_put_all.label: 	Put Selected Files\n",
"*fmenu*pane.fmenu_put_all.select: 	SetFileAction()\n",
"*fmenu*pane.fmenu_delete_all.wcClass:	oblongButtonGadgetClass\n",
"*fmenu*pane.fmenu_delete_all.label:  	Delete Selected Files\n",
"*fmenu*pane.fmenu_delete_all.sensitive: FALSE\n",
"!*fmenu*pane.fmenu_delete_all.select:	delete\n",
"*Shellconnect.wcCreate:         TopLevelShell\n",
"*Shellconnect.wcChildren:       connectlayout\n",
"*Shellconnect.title:            Connect...  \n",
"*connectlayout.wcCreate:        Form\n",
"*connectlayout.wcChildren:      anonymous, \
				DoBoxConnect, DoHide, DoArchie, \
				DoRetry, DoGateway,\
				hostsw, \
				hostlabel, hosttext,\
				logonlabel, logontext,\
				passwordlabel, passwordtext,\
				remotedirlabel, remotedirtext,\
				localdirlabel, localdirtext,\
				gatewaylabel, gatewaytext\n",
"*anonymous.wcClass:		menuButtonWidgetClass\n",
"*anonymous.wcCallback: \
   WcCreateChildren( this*pane, anonGuest, anonMail, anonUser)\n",
"*anonymous.xAddWidth:        	TRUE\n",
"*anonymous.xOffset:          	2\n",
"*anonymous*pane.anonGuest.wcClass:	oblongButtonGadgetClass\n",
"*anonymous*pane.anonGuest.label:	guest\n",
"*anonymous*pane.anonGuest.select:	Anonymous(guest)\n",
"*anonymous*pane.anonMail.wcClass:	oblongButtonGadgetClass\n",
"*anonymous*pane.anonMail.label:		Mail Address\n",
"*anonymous*pane.anonMail.select:	Anonymous(MAIL)\n",
"*anonymous*pane.anonUser.wcClass:	oblongButtonGadgetClass\n",
"*anonymous*pane.anonUser.label:		Name\n",
"*anonymous*pane.anonUser.select:	Anonymous(NAME)\n",
"*DoBoxConnect.wcCreate:		oblongButtonWidgetClass\n",
"*DoBoxConnect.label:     	Connect\n",
"*DoBoxConnect.select: 		Login()\n",
"*DoBoxConnect.yRefName:		anonymous\n",
"*DoBoxConnect.yAddHeight:	TRUE\n",
"*DoBoxConnect.yOffset:		2\n",
"*DoHide.wcCreate:		oblongButtonWidgetClass\n",
"*DoHide.label:    		Hide\n",
"*DoHide.yRefName:		anonymous\n",
"*DoHide.yAddHeight:		TRUE\n",
"*DoHide.yOffset:		2\n",
"*DoHide.xRefName:    		DoBoxConnect\n",
"*DoHide.xAddWidth:   		TRUE\n",
"*DoHide.xOffset:     		2\n",
"*DoHide.select: 		Clear_noop(connect) \
                                Set_noop(notconnected) \
                                WcSetSensitiveCB(\"*connect\") \
                                WcPopDownCB(~)\n",
"*DoArchie.wcCreate:		oblongButtonWidgetClass\n",
"*DoArchie.wcCallback:		archie_noop()\n",
"*DoArchie.label:    		Archie\n",
"*DoArchie.yRefName:		anonymous\n",
"*DoArchie.yAddHeight:		TRUE\n",
"*DoArchie.yOffset:		2\n",
"*DoArchie.xRefName:    		DoHide\n",
"*DoArchie.xAddWidth:   		TRUE\n",
"*DoArchie.xOffset:     		2\n",
"*DoArchie.select:		archie()\n",
"*DoRetry.wcCreate:		Checkbox\n",
"*DoRetry.traversalOn:		FALSE\n",
"*DoRetry.label:     		Retry\n",
"*DoRetry.select:      		Set_retry(1)\n",
"*DoRetry.unselect:     		Set_retry(0)\n",
"*DoRetry.yRefName:		DoBoxConnect\n",
"*DoRetry.yAddHeight:		TRUE\n",
"*DoRetry.xAttachRight:		TRUE\n",
"*DoRetry.xOffset:     		2\n",
"*DoRetry.xAddWidth:   		TRUE\n",
"*DoRetry.yOffset:		2\n",
"*DoGateway.wcCreate:		Checkbox\n",
"*DoGateway.traversalOn:		FALSE\n",
"*DoGateway.label:     		Use ftp gateway\n",
"*DoGateway.select:     		Set_gateway(1)\n",
"*DoGateway.unselect:   		Set_gateway(0)\n",
"*DoGateway.yRefName:		DoBoxConnect\n",
"*DoGateway.yAddHeight:		TRUE\n",
"*DoGateway.yOffset:		2\n",
"*DoGateway.xRefName:    	DoRetry\n",
"*DoGateway.xAddWidth:   	TRUE\n",
"*DoGateway.xOffset:     	2\n",
"*hostsw.wcCreate:               MyListSW\n",
"*hostsw.wcCallback:             help_register \
                                CreateHostSearchDialog()\n",
"*hostsw.height:                 100\n",
"*hostsw.width:			424\n",
"*hostsw.min:			100\n",
"*hostsw.labelw:			FALSE\n",
"*hostsw.yRefName:		DoRetry\n",
"*hostsw.yAddHeight:		TRUE\n",
"*hostsw.yOffset:		2\n",
"*hostsw.xOffset:		0\n",
"*hostsw.xAttachRight:		TRUE\n",
"*hostsw.xVaryOffset:		FALSE\n",
"*hostsw.xResizable:		TRUE\n",
"*hostsw.borderWidth:            1\n",
"*hostsw.list.Callback:          SelectHost\n",
"*hostsw.list.width:		10\n",
"*hostsw.list.height:		10\n",
"*hostsw.list.columnSpacing:     0\n",
"*hostsw.list.borderWidth: 	1\n",
"*hostsw.list.defaultColumns:    1\n",
"*hostsw.list.forceColumns:      FALSE\n",
"*hostsw.list.mulitselect:       FALSE\n",
"*hostsw.list.rowSpacing:        0\n",
"*hostsw.list.translations:      #override \
  Ctrl ~Shift <Key>n:           Listop(Down)\\n\
 ~Ctrl ~Shift <Key>j:           Listop(Down)\\n\
  Ctrl ~Shift <Key>p:           Listop(Up)\\n\
 ~Ctrl ~Shift <Key>k:           Listop(Up)\\n\
  Ctrl ~Shift <Key>f:           Listop(Right)\\n\
 ~Ctrl ~Shift <Key>l:           Listop(Right)\\n\
  Ctrl ~Shift <Key>b:           Listop(Left)\\n\
 ~Ctrl ~Shift <Key>h:           Listop(Left)\\n\
  Ctrl ~Shift <Key>a:           Listop(Start)\\n\
  Ctrl ~Shift <Key>e:           Listop(End)\\n\
 ~Ctrl  <Key>greater:           Listop(NextPage)\\n\
 ~Ctrl  <Key>less:              Listop(PrevPage)\\n\
  Ctrl  <Key>greater:           Listop(Bottom)\\n\
  Ctrl  <Key>less:              Listop(Top)\\n\
 ~Shift ~Meta ~Alt <Key>space:  Listop(Select)\\n\
  Ctrl  <Key>s:                 search_host()\\n\
  Ctrl  <Key>g:                 search_host(clear)\\n\
 <Btn1Down>(1):                 Set() Notify()\\n\
 <Btn1Down>(2):                 Login()\\n\
 <Btn1Up>:                      Noop()\\n\
 <Key>Help:                 	help()\n",
"*Shellsearchhostdialog*text*translations:   #override \
   <Key>Help:                   help() \\n\
   <Key>F1:                     help() \\n\
   <Key>Return:                 WcPopdownACT(*Shellsearchhostdialog) \
                                set_search_host(reg) \\n\
   Ctrl<Key>r:                  WcPopdownACT(*Shellsearchhostdialog) \
                                set_search_host(reg) \\n\
   Ctrl<Key>c:                  WcPopdownACT(*Shellsearchhostdialog) \\n\
\n",
"*hostlabel.wcCreate:     	Statictext\n",
"*hostlabel.wcCallback:		SetHostList\n",
"*hostlabel.String:     		Host:\n",
"*hostlabel.yRefName:		hostsw\n",
"*hostlabel.yAddHeight:		TRUE\n",
"*hostlabel.yOffset:		2\n",
"*hosttext.wcCreate:		textFieldWidgetClass\n",
"*hosttext.width:           	500\n",
"*hosttext.xAttachRight:         TRUE\n",
"*hosttext.xVaryOffset:          FALSE\n",
"*hosttext.xResizable:           TRUE\n",
"*hosttext.wcCallback:       	init_connect_info(host)\n",
"*hosttext.yRefName:		hostsw\n",
"*hosttext.yAddHeight:		TRUE\n",
"*hosttext.yOffset:		2\n",
"*hosttext.xRefName:        	hostlabel\n",
"*hosttext.xAddWidth:		TRUE\n",
"*hosttext.xOffset:		2\n",
"*hosttext.verification:		Login\n",
"*logonlabel.wcCreate:     	Statictext\n",
"*logonlabel.string:    		Login:\n",
"*logonlabel.yRefName:		hostlabel\n",
"*logonlabel.yAddHeight:		TRUE\n",
"*logonlabel.yOffset:		2\n",
"*logontext.wcCreate:		textFieldWidgetClass\n",
"*logontext.wcCallback:       	init_connect_info(login)\n",
"*logontext.yRefName:		hostlabel\n",
"*logontext.yAddHeight:		TRUE\n",
"*logontext.yOffset:		2\n",
"*logontext.xRefName:        	logonlabel\n",
"*logontext.xAddWidth:		TRUE\n",
"*logontext.xOffset:		2\n",
"*logontext.xAttachRight:        TRUE\n",
"*logontext.xVaryOffset:         FALSE\n",
"*logontext.xResizable:          TRUE\n",
"*logontext.verification:	Login\n",
"*passwordlabel.wcCreate:     	Statictext\n",
"*passwordlabel.string:     	Password:\n",
"*passwordlabel.yRefName:	logonlabel\n",
"*passwordlabel.yAddHeight:	TRUE\n",
"*passwordlabel.yOffset:		2\n",
"*passwordtext.wcCreate:		textFieldWidgetClass\n",
"*passwordtext.wcCallback:       init_connect_info(password) \
				take_over()\n",
"*passwordtext.borderWidth:	0\n",
"*passwordtext.yRefName:		logonlabel\n",
"*passwordtext.yAddHeight:	TRUE\n",
"*passwordtext.yOffset:		2\n",
"*passwordtext.xRefName:        	passwordlabel\n",
"*passwordtext.xAddWidth:	TRUE\n",
"*passwordtext.xOffset:		2\n",
"*passwordtext.xAttachRight:	TRUE\n",
"*passwordtext.xVaryOffset:   	FALSE\n",
"*passwordtext.xResizable:      	TRUE\n",
"*passwordtext.verification:	Login\n",
"*remotedirlabel.wcCreate:     	Statictext\n",
"*remotedirlabel.string:     	Remote Directory:\n",
"*remotedirlabel.yRefName:	passwordlabel\n",
"*remotedirlabel.yAddHeight:	TRUE\n",
"*remotedirlabel.yOffset:	2\n",
"*remotedirtext.wcCreate:	textFieldWidgetClass\n",
"*remotedirtext.wcCallback:      init_connect_info(remotedir)\n",
"*remotedirtext.yRefName:	passwordlabel\n",
"*remotedirtext.yAddHeight:	TRUE\n",
"*remotedirtext.yOffset:		2\n",
"*remotedirtext.xRefName:       	remotedirlabel\n",
"*remotedirtext.xAddWidth:	TRUE\n",
"*remotedirtext.xAttachRight:	TRUE\n",
"*remotedirtext.xVaryOffset:   	FALSE\n",
"*remotedirtext.xResizable:     	TRUE\n",
"*remotedirtext.xOffset:		2\n",
"*localdirlabel.wcCreate:  	Statictext\n",
"*localdirlabel.string:     	Local Directory:\n",
"*localdirlabel.yRefName:	remotedirlabel\n",
"*localdirlabel.yAddHeight:	TRUE\n",
"*localdirlabel.yOffset:		2\n",
"*localdirtext.wcCreate:		textFieldWidgetClass\n",
"*localdirtext.wcCallback:       init_connect_info(localdir)\n",
"*localdirtext.yRefName:		remotedirlabel\n",
"*localdirtext.yAddHeight:	TRUE\n",
"*localdirtext.yOffset:		2\n",
"*localdirtext.xRefName:        	localdirlabel\n",
"*localdirtext.xAddWidth:	TRUE\n",
"*localdirtext.xOffset:		2\n",
"*localdirtext.xAttachRight:	TRUE\n",
"*localdirtext.xVaryOffset:   	FALSE\n",
"*localdirtext.xResizable:     	TRUE\n",
"*gatewaylabel.wcCreate:  	Statictext\n",
"*gatewaylabel.string:     	Gateway:\n",
"*gatewaylabel.yRefName:		localdirlabel\n",
"*gatewaylabel.yAddHeight:	TRUE\n",
"*gatewaylabel.yOffset:		2\n",
"*gatewaytext.wcCreate:		textFieldWidgetClass\n",
"*gatewaytext.wcCallback:       	init_connect_info(gateway)\n",
"*gatewaytext.yRefName:		localdirlabel\n",
"*gatewaytext.yAddHeight:	TRUE\n",
"*gatewaytext.yOffset:		2\n",
"*gatewaytext.xRefName:        	gatewaylabel\n",
"*gatewaytext.xAddWidth:		TRUE\n",
"*gatewaytext.xOffset:		2\n",
"*gatewaytext.xAttachRight:	TRUE\n",
"*gatewaytext.xVaryOffset:   	FALSE\n",
"*gatewaytext.xResizable:     	TRUE\n",
"*Shellarchie.wcCreate:          XtCreateTopLevelShell\n",
"*Shellarchie.wcChildren:        archie_layout\n",
"*Shellarchie.title:             Archie Interface\n",
"*archie_layout.wcCreate:	Form\n",
"*archie_layout.wcChildren:	ArchieHits, ArchieHost, ArchieNice,\
				    ArchieSearch, ArchieSort,\
				archie_hide, archie_search, archie_abort,\
				    archie_label, archietext,\
				archie_lw\n",
"*ArchieHits.wcCreate:		menuButtonWidgetClass\n",
"*ArchieHits.wcCallback:	\
	WcCreateChildren( this*pane, archie_hit_95, archie_hit_200, \
				     archie_hit_400 archie_hit_800)\n",
"*ArchieHits.label:		Hits\n",
"*ArchieHits.xoffset:		4\n",
"*ArchieHits*pane.archie_hit_95.wcClass:	oblongButtonGadgetClass\n",
"*ArchieHits*pane.archie_hit_95.wcCallback: Register_archie(95)\n",
"*ArchieHits*pane.archie_hit_95.label:	95\n",
"*ArchieHits*pane.archie_hit_95.select:	do_archie\n",
"*ArchieHits*pane.archie_hit_200.wcClass:	oblongButtonGadgetClass\n",
"*ArchieHits*pane.archie_hit_200.wcCallback: Register_archie(200)\n",
"*ArchieHits*pane.archie_hit_200.label:	200\n",
"*ArchieHits*pane.archie_hit_200.select:	do_archie\n",
"*ArchieHits*pane.archie_hit_400.wcClass:	oblongButtonGadgetClass\n",
"*ArchieHits*pane.archie_hit_400.wcCallback: Register_archie(400)\n",
"*ArchieHits*pane.archie_hit_400.label:	400\n",
"*ArchieHits*pane.archie_hit_400.select:	do_archie\n",
"*ArchieHits*pane.archie_hit_800.wcClass: oblongButtonGadgetClass\n",
"*ArchieHits*pane.archie_hit_800.wcCallback: Register_archie(800)\n",
"*ArchieHits*pane.archie_hit_800.label:	800\n",
"*ArchieHits*pane.archie_hit_800.select:	do_archie\n",
"*ArchieHost.wcCreate:		menuButtonWidgetClass\n",
"*ArchieHost.wcCallback:         archie_hosts()\n",
"*ArchieHost.label:		Host\n",
"*ArchieHost.xoffset:		4\n",
"*ArchieHost.xRefName:           ArchieHits\n",
"*ArchieHost.xAddWidth:		TRUE\n",
"*ArchieHost*pane.archiehost.wcClass: oblongButtonGadgetClass\n",
"*ArchieHost*pane.archiehost.wcCallback: Register_archie(host)\n",
"*ArchieHost*pane.archiehost.label:	\n",
"*ArchieHost*pane.archiehost.select:do_archie\n",
"*ArchieNice.wcCreate:		menuButtonWidgetClass\n",
"*ArchieNice.wcCallback:	\
   WcCreateChildren( this*pane, archie_nice_default, archie_nice_nice,\
                                archie_nice_nicer,  archie_nice_very,\
                                archie_nice_exterm,  archie_nice_nicest)\n",
"*ArchieNice.label:		Nice\n",
"*ArchieNice.xoffset:		4\n",
"*ArchieNice.xRefName:            ArchieHost\n",
"*ArchieNice.xAddWidth:		TRUE\n",
"*ArchieNice*pane.archie_nice_default.wcClass:	oblongButtonGadgetClass\n",
"*ArchieNice*pane.archie_nice_default.wcCallback: Register_archie(nice_default)\n",
"*ArchieNice*pane.archie_nice_default.label:	Nice Default    (0)\n",
"*ArchieNice*pane.archie_nice_default.select:	do_archie\n",
"*ArchieNice*pane.archie_nice_nice.wcClass:	oblongButtonGadgetClass\n",
"*ArchieNice*pane.archie_nice_nice.wcCallback: Register_archie(nice_nice)\n",
"*ArchieNice*pane.archie_nice_nice.label:	Nice          (500)\n",
"*ArchieNice*pane.archie_nice_nice.select:	do_archie\n",
"*ArchieNice*pane.archie_nice_nicer.wcClass:	oblongButtonGadgetClass\n",
"*ArchieNice*pane.archie_nice_nicer.wcCallback: Register_archie(nice_nicer)\n",
"*ArchieNice*pane.archie_nice_nicer.label:	Nice         (1000)\n",
"*ArchieNice*pane.archie_nice_nicer.select:	do_archie\n",
"*ArchieNice*pane.archie_nice_very.wcClass:	oblongButtonGadgetClass\n",
"*ArchieNice*pane.archie_nice_very.label:	Nice         (5000)\n",
"*ArchieNice*pane.archie_nice_very.select:	do_archie\n",
"*ArchieNice*pane.archie_nice_exterm.wcClass:	oblongButtonGadgetClass\n",
"*ArchieNice*pane.archie_nice_exterm.label:	Nice        (10000) \n",
"*ArchieNice*pane.archie_nice_exterm.select:	do_archie\n",
"*ArchieNice*pane.archie_nice_nicest.wcClass:	oblongButtonGadgetClass\n",
"*ArchieNice*pane.archie_nice_nicest.label:	Nice        (32765) \n",
"*ArchieNice*pane.archie_nice_nicest.select:	do_archie\n",
"*ArchieSearch.wcCreate:		menuButtonWidgetClass\n",
"*ArchieSearch.wcCallback:	\
   WcCreateChildren( this*pane, archie_csss, archie_esm, archie_res,\
                                archie_ciss)\n",
"*ArchieSearch.label:		Search Types\n",
"*ArchieSearch.xoffset:		4\n",
"*ArchieSearch.xRefName:         ArchieNice\n",
"*ArchieSearch.xAddWidth:	TRUE\n",
"*ArchieSearch*pane.archie_csss.wcClass:	oblongButtonGadgetClass\n",
"*ArchieSearch*pane.archie_csss.wcCallback:  Register_archie(csss)\n",
"*ArchieSearch*pane.archie_csss.label:	Case Sensitive substring search\n",
"*ArchieSearch*pane.archie_csss.select:	do_archie\n",
"*ArchieSearch*pane.archie_esm.wcClass:	oblongButtonGadgetClass\n",
"*ArchieSearch*pane.archie_esm.wcCallback:  Register_archie(esm)\n",
"*ArchieSearch*pane.archie_esm.label:	Exact String Match\n",
"*ArchieSearch*pane.archie_esm.select:	do_archie\n",
"*ArchieSearch*pane.archie_res.wcClass:	oblongButtonGadgetClass\n",
"*ArchieSearch*pane.archie_res.wcCallback:  Register_archie(res)\n",
"*ArchieSearch*pane.archie_res.label:	Regular Expession Search\n",
"*ArchieSearch*pane.archie_res.select:	do_archie\n",
"*ArchieSearch*pane.archie_ciss.wcClass:	oblongButtonGadgetClass\n",
"*ArchieSearch*pane.archie_ciss.wcCallback:  Register_archie(ciss)\n",
"*ArchieSearch*pane.archie_ciss.label:	Case Insensitive Substring Search\n",
"*ArchieSearch*pane.archie_ciss.select:	do_archie\n",
"*ArchieSort.wcCreate:		menuButtonWidgetClass\n",
"*ArchieSort.wcCallback:	\
   WcCreateChildren( this*pane, archie_sort_age, archie_sort_name,\
                                archie_sort_size,\
                                archie_sort_sep,\
                                archie_sort_normal,\
                                archie_sort_reverse)\n",
"*ArchieSort.label:		Sort\n",
"*ArchieSort.xoffset:		4\n",
"*ArchieSort.xRefName:            ArchieSearch\n",
"*ArchieSort.xAddWidth:		TRUE\n",
"*ArchieSort*pane.archie_sort_age.wcClass: oblongButtonGadgetClass\n",
"*ArchieSort*pane.archie_sort_age.wcCallback:  Register_archie(sort_by_age)\n",
"*ArchieSort*pane.archie_sort_age.label: Age\n",
"*ArchieSort*pane.archie_sort_age.select:	do_archie\n",
"*ArchieSort*pane.archie_sort_name.wcClass: oblongButtonGadgetClass\n",
"*ArchieSort*pane.archie_sort_name.wcCallback:  Register_archie(sort_by_name)\n",
"*ArchieSort*pane.archie_sort_name.label: Name\n",
"*ArchieSort*pane.archie_sort_name.select:do_archie\n",
"*ArchieSort*pane.archie_sort_size.wcClass: oblongButtonGadgetClass\n",
"*ArchieSort*pane.archie_sort_size.wcCallback:  Register_archie(sort_by_size)\n",
"*ArchieSort*pane.archie_sort_size.label: Size\n",
"*ArchieSort*pane.archie_sort_size.select:do_archie\n",
"*ArchieSort*pane.archie_sort_sep.wcCreate:  CreateSep\n",
"*ArchieSort*pane.archie_sort_normal.wcClass: oblongButtonGadgetClass\n",
"*ArchieSort*pane.archie_sort_normal.wcCallback:  Register_archie(sort_normal)\n",
"*ArchieSort*pane.archie_sort_normal.label: Normal\n",
"*ArchieSort*pane.archie_sort_normal.select:do_archie\n",
"*ArchieSort*pane.archie_sort_reverse.wcClass: oblongButtonGadgetClass\n",
"*ArchieSort*pane.archie_sort_reverse.wcCallback:  Register_archie(sort_reverse)\n",
"*ArchieSort*pane.archie_sort_reverse.label: Reverse\n",
"*ArchieSort*pane.archie_sort_reverse.select:do_archie\n",
"*archie_hide.wcCreate:         	oblongButtonWidgetClass\n",
"*archie_hide.label:            	Hide\n",
"*archie_hide.yRefName:         	ArchieHits\n",
"*archie_hide.yAddHeight:       	TRUE\n",
"*archie_hide.yOffset:         	4\n",
"*archie_hide.select:     	WcPopDownCB(~)\n",
"*archie_search.wcCreate:       	oblongButtonWidgetClass\n",
"*archie_search.wcCallback:	Register_archie(do_search)\n",
"*archie_search.label:          	Search\n",
"*archie_search.yRefName:       	ArchieHits\n",
"*archie_search.yAddHeight:     	TRUE\n",
"*archie_search.yOffset:        	4\n",
"*archie_search.select:     	do_archie\n",
"*archie_search.xRefName:	archie_hide\n",
"*archie_search.xAddWidth:      	TRUE\n",
"*archie_search.xOffset:        	4\n",
"*archie_abort.wcCreate:       	oblongButtonWidgetClass\n",
"*archie_abort.label:          	Abort Search\n",
"*archie_abort.yRefName:       	ArchieHits\n",
"*archie_abort.yAddHeight:     	TRUE\n",
"*archie_abort.yOffset:        	4\n",
"*archie_abort.select:     	abort_archie\n",
"*archie_abort.xRefName:		archie_search\n",
"*archie_abort.xAddWidth:      	TRUE\n",
"*archie_abort.xOffset:        	4\n",
"*archie_label.wcCreate:   	Statictext\n",
"*archie_label.string:          	Archie Search Item:\n",
"*archie_label.yRefName:        	ArchieHits\n",
"*archie_label.yAddHeight:       TRUE\n",
"*archie_label.yOffset:          2\n",
"*archie_label.xRefName:		archie_abort\n",
"*archie_label.xAddWidth:      	TRUE\n",
"*archie_label.xOffset:        	4\n",
"*archietext.wcCreate:            textFieldWidgetClass\n",
"*archietext.wcCallback:          Register_archie(search_text)\n",
"*archietext.yRefName:            ArchieHits\n",
"*archietext.yAddHeight:          TRUE\n",
"*archietext.yOffset:             2\n",
"*archietext.xRefName:            archie_label\n",
"*archietext.xAddWidth:           TRUE\n",
"*archietext.xOffset:             2\n",
"*archietext.xAttachRight:        TRUE\n",
"*archietext.xVaryOffset:         FALSE\n",
"*archietext.xResizable:          TRUE\n",
"*archie_lw.wcCreate:            MyListSW\n",
"*archie_lw.height:              300\n",
"*archie_lw.width:               800\n",
"*archie_lw.yRefName:            archie_hide\n",
"*archie_lw.yAddHeight:          TRUE\n",
"*archie_lw.yOffset:		2\n",
"*archie_lw.XOffset:		0\n",
"*archie_lw.XAttachRight:      	TRUE\n",
"*archie_lw.xVaryOffset:    	FALSE\n",
"*archie_lw.xResizable:          TRUE\n",
"*archie_lw.yAttachBottom:       TRUE\n",
"*archie_lw.yVaryOffset:         FALSE\n",
"*archie_lw.yResizable:          TRUE\n",
"*archie_lw.labelw:              TRUE\n",
"*archie_lw.top:                 chaintop\n",
"*archie_lw.fromVert:            *archie_hide\n",
"*archie_lw.borderWidth:         1\n",
"*archie_lw.list.Callback:       archie_notify\n",
"*archie_lw.list.width:    	1\n",
"*archie_lw.list.height:    	1\n",
"*archie_lw.list.borderWidth:    0\n",
"*archie_lw.list.columnSpacing:  0\n",
"*archie_lw.list.defaultColumns: 1\n",
"*archie_lw.list.rowSpacing:     0\n",
"*archie_lw.translations:       #override \
                                <Key>Help: help() \\n\
                                <Key>F1:   help()\n",
"*Shellhelp.wcConstructor:       XtCreateTopLevelShell\n",
"*Shellhelp.wcChildren:        	helplayout\n",
"*Shellhelp.title:            	Help\n",
"*helplayout.wcClass:        	Form	\n",
"*helplayout.wcChildren:       	help_quit, helpmenu, help_title, \
				help_text\n",
"*help_quit.wcCreate:		oblongButtonWidgetClass\n",
"*help_quit.label:     		Hide\n",
"*help_quit.select: 		WcPopDownCB(~)\n",
"*helpmenu.wcClass:              menuButtonWidgetClass\n",
"*helpmenu.label:		Selections\n",
"*helpmenu.xRefName: 		help_quit\n",
"*helpmenu.xAddWidth:     	TRUE\n",
"*helpmenu.xOffset:		4\n",
"*helpmenu*pane.help_sub.wcClass:  menuButtonWidgetClass\n",
"*helpmenu*pane.helpmenuline.wcCreate:  CreateSep\n",
"*help_sub*pane.helpmenu1.wcClass:    oblongButtonWidgetClass \n",
"*help_sub*pane.helpmenu1.select:     help_by_title(), help_once \n",
"*help_title.wcCreate:       	Statictext\n",
"*help_title.borderWidth:	0\n",
"*help_title.text.width:        	400\n",
"*help_title.gravity:            west\n",
"*help_title.yOffset:		0\n",
"*help_title.xRefName:  		helpmenu\n",
"*help_title.xAddWidth:       	TRUE\n",
"*help_title.xOffset:           	2\n",
"*help_title.xAttachRight:       TRUE\n",
"*help_title.xVaryOffset:        FALSE\n",
"*help_title.xResizable:         TRUE\n",
"*help_title.recomputeSize:      FALSE\n",
"*help_text.wcCreate:		ScrolledText\n",
"*help_text.wcCallback:          set_help\n",
"*help_text.width:        	600\n",
"*help_text.height:        	400\n",
"*help_text.sourceType:		OL_STRING_SOURCE\n",
"*help_text.editType:      	textread\n",
"*help_textSW.yRefName:        	help_quit\n",
"*help_textSW.yAddHeight:      	TRUE\n",
"*help_textSW.yOffset:         	4\n",
"*help_textSW.xAttachRight:    	TRUE\n",
"*help_textSW.xVaryOffset:     	FALSE\n",
"*help_textSW.xResizable:      	TRUE\n",
"*help_textSW.yAttachBottom:    	TRUE\n",
"*help_textSW.yVaryOffset:      	FALSE\n",
"*help_textSW.yResizable:       	TRUE\n",
"*help_textSW.recomputeHeight:   FALSE\n",
"*help_textSW.recomputeWidth:    FALSE\n",
"*help_textSW.verticalSB:        	TRUE\n",
"*help_textSW.horizontalSB:      	TRUE\n",
"*Shelltran.wcConstructor:       XtCreateTopLevelShell\n",
"*Shelltran.wcChildren:          tran_layout\n",
"*Shelltran.title:               Remote File Translations Examples\n",
"*Shelltran*deleteResponse:   	unmap\n",
"*tran_layout.wcCreate:       	Form\n",
"*tran_layout.wcChildren:       	tran_quit, tran_text\n",
"*tran_quit.wcCreate:		oblongButtonWidgetClass\n",
"*tran_quit.label:     		Hide\n",
"*tran_quit.select: 		WcPopDownCB(~)\n",
"*tran_text.wcCreate:           	ScrolledText\n",
"*tran_text.wcCallback:          set_tran\n",
"*tran_text.width:             	600\n",
"*tran_text.hight:             	400\n",
"*tran_text.sourceType:		OL_STRING_SOURCE\n",
"*tran_text.editType:		textread\n",
"*tran_textSW.yRefName:         	tran_quit\n",
"*tran_textSW.yAddHeight:       	TRUE\n",
"*tran_textSW.yOffset:          	0\n",
"*tran_textSW.xAttachRight:     	TRUE\n",
"*tran_textSW.xVaryOffset:      	FALSE\n",
"*tran_textSW.xResizable:       	TRUE\n",
"*tran_textSW.yAttachBottom:    	TRUE\n",
"*tran_textSW.yVaryOffset:      	FALSE\n",
"*tran_textSW.yResizable:       	TRUE\n",
"*tran_textSW.recomputeSize:    	FALSE\n",
"*Shellstatus.wcCreate:		XtCreateTopLevelShell\n",
"*Shellstatus.wcCallback:        NoWindowGroup()\n",
"*Shellstatus.wcChildren:	status_layout\n",
"*Shellstatus.title:		Status Message Log\n",
"*status_layout.wcCreate:	Form\n",
"*status_layout.wcChildren:	status_quit, status_clear status_text\n",
"*status_quit.wcClass:		oblongButtonWidgetClass	\n",
"*status_quit.label:     	Hide\n",
"*status_quit.select: 		WcPopDownCB(~)\n",
"*status_clear.wcClass:		oblongButtonWidgetClass	\n",
"*status_clear.label:     	Clear Text\n",
"*status_clear.select: 		Clear_Text(*status_text)\n",
"*status_clear.xRefName:		status_quit\n",
"*status_clear.xAddWidth:	TRUE\n",
"*status_clear.xOffset:		4\n",
"*status_text.wcCreate:		ScrolledText\n",
"*status_text.width:          	600\n",
"*status_text.hight:          	400\n",
"*status_text.editType:		textread\n",
"*status_text.sourceType:	OL_STRING_SOURCE\n",
"*status_textSW.yRefName:       	status_quit\n",
"*status_textSW.yAddHeight:     	TRUE\n",
"*status_textSW.yOffset:        	0\n",
"*status_textSW.xAttachRight:   	TRUE\n",
"*status_textSW.xVaryOffset:    	FALSE\n",
"*status_textSW.xResizable:     	TRUE\n",
"*status_textSW.yAttachBottom:  	TRUE\n",
"*status_textSW.yVaryOffset:    	FALSE\n",
"*status_textSW.yResizable:     	TRUE\n",
"*status_textSW.recomputeSize:	FALSE\n",
"*Shellcommand.wcCreate:		XtCreateTopLevelShell\n",
"*Shellcommand.wcCallback:       NoWindowGroup()\n",
"*Shellcommand.wcChildren:	command_layout\n",
"*Shellcommand.title:		Ftp command\n",
"*command_layout.wcCreate:	Form\n",
"*command_layout.wcChildren:	command_quit, command_clear, ftp\n",
"*command_quit.wcClass:		oblongButtonWidgetClass	\n",
"*command_quit.label:     	Hide\n",
"*command_quit.select: 		WcPopDownCB(~)\n",
"*command_clear.wcClass:		oblongButtonWidgetClass	\n",
"*command_clear.label:     	Clear Text\n",
"*command_clear.select: 		Clear_Text(*ftp)\n",
"*command_clear.xRefName:	command_quit\n",
"*command_clear.xAddWidth:	TRUE\n",
"*command_clear.xOffset:		4\n",
"*ftp.wcCreate:			ScrolledText\n",
"*ftp.wcCallback:\
	noop(get put dir action connect notconnected Sensitive)\n",
"*ftp.width:         		800\n",
"*ftp.hight:         		400\n",
"*ftp.editType:                  textedit\n",
"*ftpSW.yRefName:       		command_quit\n",
"*ftpSW.yAddHeight:     		TRUE\n",
"*ftpSW.yOffset:        		0\n",
"*ftpSW.yAttachBottom:      	TRUE\n",
"*ftpSW.yVaryOffset:       	FALSE\n",
"*ftpSW.yResizable:        	TRUE\n",
"*ftpSW.xAttachRight:   		TRUE\n",
"*ftpSW.xVaryOffset:    		FALSE\n",
"*ftpSW.xResizable:     		TRUE\n",
"*ftpSW.recomputeSize:		TRUE\n",
"*ftp*translations:              #override \
        <Key>Help:      help() \\n\
        <Key>F1:        help() \\n\
        <Key>Return:    Dispatch() \\n\
        <Key>Delete:    delete_the_char() \\n\
        <Key>BackSpace: delete_the_char() \\n\
        Ctrl <Key>u:    clear_line() \\n\
        <Key>:          insert_the_char()\n",
"*Shellview.wcCreate:		XtCreateTopLevelShell\n",
"*Shellview.wcChildren:		view_layout\n",
"*Shellview.title:		View File\n",
"*view_layout.wcCreate:		Form\n",
"*view_layout.wcChildren:	view_quit, view_text\n",
"*view_quit.wcClass:		oblongButtonWidgetClass	\n",
"*view_quit.label:     		Dismiss\n",
"*view_quit.select: 		WcDestroyCB(~)\n",
"*view_text.wcCreate:		ScrolledText\n",
"*view_text.wcCallback:         	set_view_file\n",
"*view_text.width:          	600\n",
"*view_text.hight:          	400\n",
"*view_text.sourceType:		OL_STRING_SOURCE\n",
"*view_text.editType:      	textread\n",
"*view_textSW.yRefName:        	view_quit\n",
"*view_textSW.yAddHeight:      	TRUE\n",
"*view_textSW.yOffset:          	0\n",
"*view_textSW.xAttachRight:     	TRUE\n",
"*view_textSW.xVaryOffset:      	FALSE\n",
"*view_textSW.xResizable:       	TRUE\n",
"*view_textSW.yAttachBottom:    	TRUE\n",
"*view_textSW.yVaryOffset:      	FALSE\n",
"*view_textSW.yResizable:       	TRUE\n",
"*help_General.help_text:\
General Help\\n\
XXXX is a X front end to ftp.\\n\
\\n\
XXXX allows retrieval  or  transmission  of  selected  files  and\\n\
directory trees.\\n\
\\n\
The screen display for XXXX consists of 5 sections:  a  menu  bar\\n\
containing  a  quit  menu,   option menu, file option menu, mutli\\n\
file option menu, and help menu; a status window; a  remote/local\\n\
directory window; a series of buttons login,  remote/local direc-\\n\
tory, command, glob, search, next, reconnect and  archie;  and  a\\n\
scrolled list window.\\n\
\\n\
The status window display the current actions and error messages.\\n\
\\n\
The remote/local directory window display the  remote/local  name\\n\
of the displayed directory.\\n\
\\n\
The login button is used to initiate logins.\\n\
\\n\
The remote/local button toggles between remote and  local  direc-\\n\
tory display's.\\n\
\\n\
The command shell button is used to bring up a shell window  that\\n\
contains a direct interface to ftp.\\n\
\\n\
The glob button is used to select a set of files based  on  shell\\n\
glob syntax or regular expression syntax through a dialog.\\n\
\\n\
The search button is used to find a file or set of files .  based\\n\
on  shell glob syntax or regular expression syntax through a dia-\\n\
log.\\n\
\\n\
The next button will find the next file based on the glob a regu-\\n\
lar expression set by the search button.\\n\
\\n\
The reconnect button will  restart  the  ftp  session  after  the\\n\
foreign host has disconnected due to a inactivity disconnect.\\n\
\\n\
The archie command will bring up a dialog to run a archie command\\n\
if the archie command is in the users search path.\\n\
\\n\
All buttons and menu selections are done with the left mouse but-\\n\
ton.\\n\
\\n\
A file can be selected by clicking the left mouse button  on  the\\n\
file.  Multi file selection are accomplished by clicking the left\\n\
mouse button on the first file and then dragging the mouses  over\\n\
the  files  to  be  selected.   Selected  files  are displayed in\\n\
reverse video.  The current selection has a square border  around\\n\
it.\\n\
\\n\
The scrolled list window has a popup menu that can  be  activated\\n\
by holding down the right mouse button. You can also use the key-\\n\
board to select the listing options, local/remote  display,  sort\\n\
options,  files  or directories, and actions to apply to selected\\n\
files.\\n\
\\n\
You can click the left mouse button with the control key  pressed\\n\
on  a  directory to cd to it.  If you click the left mouse button\\n\
with the control key pressed on a file and it is  a  remote  file\\n\
then the file will be transferred to the local host or if it is a\\n\
local file then it is transferred to the remote host.\n",
"*netrc.help_text:\
moxftprc or netrc\\n\
XXXX will look for  ~/.moxftprc if not found then  it  will  look\\n\
for  ~/.netrc.   The format of of \".moxftprc\" is the same as that\\n\
of \".netrc\"  with  the  addition  of  three  new   tokens  called\\n\
\"remote_dir\", \"local_dir\", and \"note\".  \"note\" should be the last\\n\
token of a entry.\\n\
\\n\
It is not advisable to put  your  password  in  the  \".netrc\"  or\\n\
\".moxftprc\" files.\\n\
example:\\n\
machine ftp.chpc.utexas.edu\\n\
 login anonymous\\n\
 password jones@\\n\
 note Home of xmoftp\\n\
machine ftp.utexas.edu\\n\
 login anonymous\\n\
 password jones@\\n\
 remote_dir /packages/X\\n\
 note Lots of Networking Information\n",
"*xftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by xftp are defined by the following resources:\\n\
 Xftp*font:\\\n\
     -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Command.font:\\\n\
    -*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Text*font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*Label*font:\\\n\
    -*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*LabelQUICK*font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*listsw*list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Xftp*hostsw*list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*oftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by oftp are defined by the following resources:\\n\
 Oftp*font:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Oftp*listsw.*.list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Oftp*hostsw.*.list.font:\\\n\
    -*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*mftp_fonts.help_text:\
Default Fonts\\n\
The fonts used by mftp are defined by the following resources:\\n\
 Mftp*labelFontList:\\\n\
-*-helvetica-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*buttonFontList:\\\n\
-*-times-medium-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*textFontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar1*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*menuBar2*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*archie_menubar*fontList:\\\n\
-*-helvetica-bold-o-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp*rate.fontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\\n\
 Mftp.*.*.list.fontList:\\\n\
-*-courier-bold-r-*-*-14-*-*-*-*-*-ISO8859-1\n",
"*quit.help_text:\
Quit Button\\n\
Quit XXXX. Any pending actions will be terminated.\n",
"*listsw*help_text:\
Directory List Window\\n\
The current  local/remote directory  listing is  displayed  here.\\n\
There  are  four  types of listing formats sort, medium, long and\\n\
translations.  It is not always possible for there to be  a  long\\n\
or medium listing format for a remote system.   In such cases the\\n\
short listing format is used.\\n\
\\n\
A file or directory entry can be selected by  clicking  the  left\\n\
most mouse button on the entry.\\n\
\\n\
The right mouse button will select an entry and bring up  a  menu\\n\
with  a  list  of actions that can be applied to that entry.  The\\n\
actions are:\\n\
\\n\
UP      - Go up the directory tree\\n\
Cd      - Cd to selected directory\\n\
Get     - Get remote file\\n\
Put     - Put local file\\n\
View    - View remote file.\\n\
Ascii   - Set transfer mode of file to ascii\\n\
Binary  - Set transfer mode of file to binary\\n\
Tenex   - Set transfer mode to tenix\\n\
Default - Set transfer mode to default\\n\
Ignore  - Ignore file when retriving directory.\\n\
Don't Ignore - Do not ignore file retriving directory.\\n\
Dir     - Refresh directy listing.\\n\
\\n\
The menu also contains actions that can be taken on all  selected\\n\
files:\\n\
\\n\
Clear File Selections - Unselect all files and directories\\n\
Get Selected files    - Get all selected remote files and  direc-\\n\
tories.\\n\
Put Selected files    - Put all selected local files  and  direc-\\n\
tories.\\n\
\\n\
Also see the keyboard input help selection under general help.\n",
"*ftp*help_text:\
Ftp Window\\n\
This is the ftp window.  You can use the following  ftp  commands\\n\
in this window:\\n\
\\n\
 ascii\\n\
 binary\\n\
 delete\\n\
 dir\\n\
 cd <remote directory>\\n\
 help\\n\
 image\\n\
 get <remote file> [<local file>]\\n\
 reg <regular expression>\\n\
 regget\\n\
 regput\\n\
 lcd\\n\
 ls\\n\
 lmkdir\\n\
 lpwd\\n\
 mkdir\\n\
 put\\n\
 pwd\\n\
 quote\\n\
 remotehelp\\n\
 site\\n\
 tenex\n",
"*abort.help_text:\
Abort Button\\n\
Abort ftp. Since there is no reliable way to abort ftp  you  will\\n\
have to login again.\n",
"*op_sort.help_text:\
Option SubMenu - Sort\\n\
The sort option menu can be used to select the type of sort  that\\n\
is  done  on  directories.  Files  can be sorted by age, name, or\\n\
size.  The sort can be reverse or normal sort order.   File  also\\n\
can be sorted by type then age, name, or size.\n",
"*options.help_text:\
Options Menu\\n\
The options menu options, to turn on or of error ignoring  during\\n\
transfers  of  multiple  files,  to turn on or off auto directory\\n\
listing and  two submenus Listing and Sort to change listing for-\\n\
mats  or sort options.  See help on submenus Listing and Sort for\\n\
more information on Listing and Sort submenus.\n",
"*op_listing.help_text:\
Option SubMenu - Listing\\n\
Select the listing options.  There are four list options plus the\\n\
example translations table option:\\n\
\\n\
Short Listing\\n\
Medium Listing\\n\
Long Listing\\n\
Translation Listing\\n\
Translations\\n\
\\n\
The short listing format  displays  the  filename  only.  If  the\\n\
remote  file  system  is an UNIX system then a directory will end\\n\
with \"/\", a link with \"@\" and of offline file with \"%\".   If  the\\n\
remote  file system is not an unix file system then a d is placed\\n\
before the file name to indicate that it is a directory.\\n\
\\n\
The Medium  Listing  format  is  system  dependent.   It  usually\\n\
includes the file length.\\n\
\\n\
The Long Listing format is system dependent.  It usually includes\\n\
the file length, type and protections.\\n\
\\n\
The Translation Listing format will display the remote  to  local\\n\
or  the  local  to  remote  translation  for  the directory being\\n\
displayed.  It also shows the mode the file will  be  transferred\\n\
in.   If XXXX does not know how to translate the filename it will\\n\
leave the translation blank.\\n\
\\n\
The Translations menu option will produce a list of example local\\n\
and remote files and their translations.\n",
"*dir.help_text:\
Directory Window\\n\
The current selected local or remote directory name is  displayed\\n\
here.\n",
"*connect.help_text:\
Login/Close Button\\n\
Login to remote host or close the connection from a remote  host.\\n\
If the option is login, a menu will popup allowing you to set the\\n\
remote host name, the remote host login  name,  the  remote  host\\n\
password, the remote directory name, and the local directory name\\n\
to use at login time.\\n\
\\n\
The retry button informs XXXX to keep retrying connection every 5\\n\
minutes until it is able to log into the remote hosts.\\n\
\\n\
XXXX understands the ftp .netrc file format. It use this to  gen-\\n\
erate  a  menu  that  will  set the hostname, login name, and (if\\n\
specified) the password for the selected host.\\n\
\\n\
A comment for the specified host can be added to the  .xftp  file\\n\
found  in  the  login  directory  using the \"note\" directive; for\\n\
example:\\n\
\\n\
note dinosaur.cc.utexas.edu UTD\\n\
note ftp.uu.net Has most anything that any one would want.\\n\
\\n\
This will be displayed beside the host entry in the host menu.\n",
"*status.help_text:\
Status Window\\n\
Display status information.  Clicking the right mouse  button  on\\n\
the  status  window  will  popup the Status Message Log.  You can\\n\
then view all of the previous status messages.\n",
"*host_name.help_text:\
System Name Window\\n\
The host name of the connected  or  selected  host  is  displayed\\n\
here.\n",
"*system_name.help_text:\
System Type Window\\n\
The System type is displayed here.\n",
"*default_mode.help_text:\
Default Transfer Mode Window\\n\
The default transfer mode is displayed in this window.\n",
"*dir_display.help_text:\
Local/Remote Button\\n\
Toggle between current  local/remote  directories.   A  directory\\n\
listing  is  displayed  of the selected local/remote directory in\\n\
the directory list window.\n",
"*dotxftp.help_text:\
XXXX initialization file\\n\
XXXX reads the \".xftp\" initialization file in the home  directory\\n\
when  it  first starts up.  The \".xftp\" file can contain the fol-\\n\
lowing directives:\\n\
\\n\
trans        <machine type>\\n\
examples_r   <remote file>\\n\
examples_e   <local file>\\n\
unix         <regular expression>\\n\
             <source> [<conversion type>]\\n\
back         <regular expression>\\n\
             <source> [<conversion type>]\\n\
end\\n\
viewer       <audio|ps|picture|tar|text> <comand>\\n\
\\n\
The note directive allows you add a note that is displayed in the\\n\
host  list  menu  in the login window.  It is used in conjunction\\n\
with the \"~/.netrc\" file.\\n\
\\n\
The trans directive start a translation table block of  commands.\\n\
You  can  only  specify the examples_r, examples_e, unix and back\\n\
directive in a translation table block.  The end  directive  ends\\n\
the translation table block.\\n\
\\n\
The examples_r and examples_e directives are used to  generate  a\\n\
example of the translations specified by the unix and back direc-\\n\
tives.\\n\
\\n\
The unix and back directive are used to specific rewriting  rules\\n\
for translating file form the remote system file name to unix and\\n\
back.  You can specify \"ascii\", \"binary\" and \"tenex\" as  <conver-\\n\
sion type>\\n\
\\n\
The examples_r, examples_e,  unix  and  back  directives  can  be\\n\
repeated 50 times each.\\n\
\\n\
The following is example of a translation table  that  you  might\\n\
want for a Vax VMS system running MULTINET.\\n\
\\n\
trans        VMS MULTINET\\n\
examples_r   XFTP_TAR.Z;1\\n\
unix         ([a-z0-9_,]+)_TAR.Z;[0-9]+\\n\
             1.tar.Z binary\\n\
examples_e   xftp.tar.Z\\n\
back         ([A-Z0-9_,]+).tar.Z\\n\
             1_TAR.Z  binary\\n\
end\\n\
\\n\
The unix directive specifies a regular expression to apply to the\\n\
remote  file  name.   If  it matches then the string \"1.tar.Z\" is\\n\
used as the source  to  rewrite  the  file  name.  This  examples\\n\
translate  \"XFTP_TAR.Z;1\"  to the unix file name \"xftp.tar.Z\" and\\n\
specifies that the file is to be transferred in binary mode.\\n\
\\n\
The back directive specifies a regular expression to apply to the\\n\
local unix file.  If it matches then the string \"1_TAR.Z\" is used\\n\
as  the  source  to  rewrite  the  file  name.   The  unix   file\\n\
\"xftp.tar.Z\" should be rewritten as \"XFTP_TAR.Z\".  The file would\\n\
be transferred in binary mode.\\n\
\\n\
The viewer directive spicfies a program  to  execute  to  view  a\\n\
audio, postscript, tar, text and picture files.  XXXX regogonizes\\n\
the filename  extensions  .aiff  and  .au  as  audio  files;  the\\n\
filename   extensions .gif, .tiff, .rgp and .jpg as pictures; the\\n\
the filename extesions .ps as postscript; and the filname  exten-\\n\
sion\\n\
example:\\n\
viewer ps ghostview\\n\
viewer text xless\\n\
viewer pitcure xv\n",
"*list_key_input.help_text:\
Keyboard Input\\n\
The Directory List Window allows the following keyboard input.\\n\
\\n\
   <Key>Help:           Help Menu\\n\
   <Key>F1:             Help Menu\\n\
\\n\
  ~Ctrl ~Shift <Key>h:  Previous item\\n\
  ~Ctrl ~Shift <Key>j:  Down one item\\n\
  ~Ctrl ~Shift <Key>k:  Up one item\\n\
   Ctrl ~Shift <Key>l:  Next item\\n\
\\n\
  ~Ctrl ~Shift <Key>0:  Fisrt item in line\\n\
   Ctrl ~Shift <Key>$:  Last item in line\\n\
\\n\
   Ctrl ~Shift <Key>f:  Next page\\n\
   Ctrl ~Shift <Key>b:  Previous page\\n\
   Ctrl ~Shift <Key>n:  Down one item\\n\
   Ctrl ~Shift <Key>p:  Up one item\\n\
\\n\
   Ctrl ~Shift <Key>j:  Down one item\\n\
  ~Ctrl  Shift <Key>m:  Down one item\\n\
\\n\
           <Key>space:  Select item\\n\
\\n\
   Ctrl ~Shift <Key>t:  Toggle to remote/local directory\\n\
\\n\
  ~Ctrl  Shift <Key>l:  Set long listing format\\n\
  ~Ctrl  Shift <Key>s:  Set short listing format\\n\
  ~Ctrl  Shift <Key>t:  Set translation listing format\\n\
\\n\
  ~Ctrl  <Key>>:        Next page\\n\
  ~Ctrl  <Key><:  Previous page\\n\
   Ctrl  <Key>>:        Bottom\\n\
   Ctrl  <Key><:     Top\\n\
\\n\
  ~Ctrl ~Shift <Key>a:Set file transfer mode to type Ascii\\n\
  ~Ctrl ~Shift <Key>b:Set file transfer mode to type binary\\n\
  ~Ctrl ~Shift <Key>t:Set file transfer mod to tenex\\n\
  ~Ctrl ~Shift <Key>d:Use default transfer mode\\n\
\\n\
  ~Ctrl ~Shift <Key>u:Go to parent directory\\n\
  ~Ctrl ~Shift <Key>c:Change dir to directory\\n\
\\n\
  ~Ctrl ~Shift <Key>g:Get file\\n\
  ~Ctrl ~Shift <Key>p:Put file\\n\
\\n\
   Ctrl        <Key>s:  Search Next\\n\
   Ctrl        <Key>g:  Clear Search Pattern\n",
"*quitm.help_text:\
Quit Menu\\n\
The quit menu contains the abort and quit options.\\n\
\\n\
Since there is no reliable way to abort  ftp  you  will  have  to\\n\
login again after aborting a ftp connection.\n",
"*items.help_text:\
Display Items\\n\
The item display display the count of the following items,  block\\n\
devices,  char   devices,  links,  sockets, files, offline_files,\\n\
selected items, and the total number of items.\n",
"*command.help_text:\
Command Button\\n\
The command button brings up the command shell.  Commands can  be\\n\
given directly to ftp through this shell.\n",
"*hide.help_text:\
Hide Shell\\n\
Hide the current shell.\n",
"*help_quit.help_text:\
Hide Shell\\n\
Hide the help shell.\n",
"*tran_quit.help_text:\
Hide Shell\\n\
Hide the translation shell.\n",
"*status_quit.help_text:\
Hide Shell\\n\
Hide the status shell.\n",
"*command_quit.help_text:\
Hide Shell\\n\
Hide the command shell.\n",
"*Shellconnect.help_text:\
Connect Shell\\n\
Used to specify login  information,  remote  host,  user  number,\\n\
password, local directory and remote directory for XXXX.\n",
"*hosts.help_text:\
Host List Menu\\n\
List of host found in $HOME/.netrc.\n",
"*anonymous.help_text:\
Anonymous login menu\\n\
Can be used to set the login user anonymous and initial password.\\n\
The password can be set to guest, mail address, or user name.\n",
"*DoBoxConnect.help_text:\
Connect button\\n\
Initiate connection.\n",
"*DoHide.help_text:\
Hide Shell\\n\
Hide the connect shell.\n",
"*Shellhelp.help_text:\
Help Shell\\n\
Display text of help message.\n",
"*Shelltran.help_text:\
Translation Shell\\n\
Display translations used with non UNIX systems.\n",
"*Shellstatus.help_text:\
Status Shell\\n\
Display log of status messages.\n",
"*Shellcommand.help_text:\
Ftp Command Shell\\n\
The ftp command shell.\n",
"*Shellview.help_text:\
View Shell\\n\
Shell window brought up to view a text file. If the file ends  in\\n\
.Z  it  will  be uncompressed before viewing if uncompress is the\\n\
users path.  If the file ends in .gz it will unzip if  gunzip  is\\n\
in the users path.\n",
"*Trademarks.help_text:\
Trademarks\\n\
OPEN LOOK is a trademark of AT&T\\n\
UNIX is a registered trademark of AT&T\\n\
The X Window System is a trademark of the Massachusetts Institute\\n\
of Technology.\n",
"*helpm.help_text:\
Help Menu\\n\
The help menu provides a context sensitive help selection  and  a\\n\
general help selection.\\n\
\\n\
If you select the context sensitive  help  selection  the  cursor\\n\
will  change  to  a  cross bar.  You can then position the cursor\\n\
over the object that you want help on and click left  most  mouse\\n\
botton.   If  the  help  system  knows  about  the object it will\\n\
display the help text in the help shell. If it does not  it  will\\n\
display the general help message in the help shell.\n",
"*fileopts.help_text:\
Single File Options Menu\\n\
The single file options menu allows the following  operations  on\\n\
the high lighted file:\\n\
\\n\
 Up           - cd to parent directory\\n\
 Cd           - cd to high lighted directory\\n\
 Get          - get high lighted file or directory\\n\
 View         - view high lighted file\\n\
 Put          - put high lighted file or directory\\n\
 Ascii        - transfer high lighted file in ascii mode\\n\
 Binary       - transfer high lighted file in binary mode\\n\
 Tenex        - transfer high lighted file in tenex mode\\n\
 Default      - transfer high lighted using default transfer mode\\n\
 Ignore       - ignore  high lighted directory/file when\\n\
                transferring contents of a directory\\n\
 Don't ignore - don't ignore high lighted directory/file when\\n\
                transferring contents of a directory\n",
"*filesopts.help_text:\
Multi File Options Menu\\n\
The multi file options menu allows the  following  operations  on\\n\
the selected files:\\n\
\\n\
Clear File Selections - Clear all file selections in current\\n\
        directory\\n\
Get Selected Files    - Get selected file in current directory\\n\
Put Selected Files    - Put selected file in current directory\n",
"*archie_command.help_text:\
Archie Button\\n\
The archie button brings up arche interface shell.\n",
"*DoArchie.help_text:\
Archie Button\\n\
The archie button brings up arche interface shell.\n",
"*DoGateway.help_text:\
Gateway Button\\n\
Enable suns passthrough ftp gateway.\\n\
slag The Search Host List Dialog is activated  by  the  following\\n\
keys in the host list window:\\n\
   Ctrl        <Key>s:  Search Next\\n\
   Ctrl        <Key>g:  Clear Search Pattern\\n\
slag *Shellsearchhostdialog.help_text:\\n\
Search Host List Dialog\\n\
Set search string for regular expression search of the host  list\\n\
in the Connect Shell.\\n\
The Search Host List Dialog has the following keyboard input:\\n\
  <Key>Return:      Start search\\n\
   Ctrl<Key>r:      Start search\\n\
   Ctrl<Key>c:      Abort search\n",
"*Shellsearchdialog.help_text:\
Search Dialog\\n\
Set search string for regular expression  search  or  shell  glob\\n\
search of file.\\n\
The Search Dialog has the following keyboard input:\\n\
   <Key>Return:    Start glob search\\n\
   Ctrl<Key>r:     Start regualar expression search\\n\
   Ctrl<Key>g:     Start glob search\\n\
   Ctrl<Key>c:     Abort Search\n",
"*Shellglobdialog.help_text:\
Glob Dialog\\n\
Select files based on shell glob expression  or  regular  expres-\\n\
sions.\n",
"*reconnect.help_text:\
Recconect Button\\n\
The recconect button allows the continuation of ftp session after\\n\
the server has disconnected the seesion.\n",
"*glob.help_text:\
Glob Button\\n\
The Glob button will bring up a glob dialog which will allow  the\\n\
selection/deselection of files based on a regular expression or a\\n\
shell glob expression  search.\n",
"*search.help_text:\
Search Button\\n\
The Search button will bring up a search  dialog  to  search  the\\n\
current  directory   for the specified item.   The  search can be\\n\
based on regular expression  or shell globing.  The  Search  Next\\n\
button  will  search  for  the next item that matches the regular\\n\
expression or shell glob.\n",
"*next.help_text:\
Search Next Button\\n\
Search for the next item that matches the regular  expression  or\\n\
shell globing expression.\n",
NULL,
};
#endif
