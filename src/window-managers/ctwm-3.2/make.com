$! Make.com for building CTWM
$ SAVE_VERIFY='F$VERIFY(0)
$ if p1 .Eqs. "CLEAN" then goto clean
$ if p1 .Eqs. "CLOBBER" then goto clobber
$!
$!	Compile the CTWM Window Manager
$!
$ Set Symbol/Scope=NoGlobal
$!
$! Establish the Compiling Environment
$!
$ Cpu_Model = F$GetSYI("HW_MODEL")
$ If Cpu_Model .gt. 1024	! Cross compiling
$ Then
$	CC := CC/STAND=VAXC
$	OPT = "AXP_OPT"
$ Else
$	OPT = "VAX_OPT"
$ Endif
$!
$!  Define logicals pointing to the needed directories
$!
$! this is where the Xmu include files are held,
$!  usually in sys$sysroot:[decw$include.xmu]
$ define/nolog x11xmu MPHA7$USER1:[THEORY.PETERC.R5.XMU]
$! where the xpm object library is.
$ define/nolog xpmlib MPHA7$USER1:[THEORY.PETERC.XPM]
$!
$!  Get the compiler options via the logical name COPTS
$!
$ cc_options = f$trnlnm("COPTS") + "/define=(VMS,XPM)"
$!
$!  Get the linker options via the logical name LOPTS
$!
$ link_options = f$trnlnm("LOPTS")
$!
$!  Compile the "C" files
$!
$! procedure target	command 			depends upon
$! CALL MAKE FILE.OBJ	"CC ''cc_options' FILE.C"	FILE.C
$!
$ write sys$output "Compiling CTWM sources"
$ CALL MAKE ADD_WINDOW.OBJ  	"CC ''cc_options' ADD_WINDOW.C"	ADD_WINDOW.C
$ CALL MAKE CTWM.OBJ  		"CC ''cc_options' CTWM.C"	CTWM.C
$ CALL MAKE CURSOR.OBJ  	"CC ''cc_options' CURSOR.C"	CURSOR.C
$ CALL MAKE DEFTWMRC.OBJ  	"CC ''cc_options' DEFTWMRC.C"	DEFTWMRC.C
$ CALL MAKE EVENTS.OBJ  	"CC ''cc_options' EVENTS.C"	EVENTS.C
$ CALL MAKE GC.OBJ  		"CC ''cc_options' GC.C"	GC.C
$ CALL MAKE GRAM.OBJ  		"CC ''cc_options' GRAM.C"	GRAM.C
$ CALL MAKE ICONMGR.OBJ  	"CC ''cc_options' ICONMGR.C"	ICONMGR.C
$ CALL MAKE WORKMGR.OBJ  	"CC ''cc_options' WORKMGR.C"	WORKMGR.C
$ CALL MAKE ICONS.OBJ  		"CC ''cc_options' ICONS.C"	ICONS.C
$ CALL MAKE LEX.OBJ  		"CC ''cc_options' LEX.C"	LEX.C
$ CALL MAKE LIST.OBJ  		"CC ''cc_options' LIST.C"	LIST.C
$ CALL MAKE MENUS.OBJ  		"CC ''cc_options' MENUS.C"	MENUS.C
$ CALL MAKE PARSE.OBJ  		"CC ''cc_options' PARSE.C"	PARSE.C
$ CALL MAKE RESIZE.OBJ  	"CC ''cc_options' RESIZE.C"	RESIZE.C
$ CALL MAKE UTIL.OBJ  		"CC ''cc_options' UTIL.C"	UTIL.C
$ CALL MAKE VERSION.OBJ  	"CC ''cc_options' VERSION.C"	VERSION.C
$ CALL MAKE VMS_CMD_SERVICES.OBJ -
				"CC ''cc_options' VMS_CMD_SERVICES.C" -
				VMS_CMD_SERVICES.H
$ CALL MAKE LNM.OBJ  	"CC ''cc_options' LNM.C"	LNM.C
$!
$ write sys$output "Building CTWM image"
$ CALL MAKE CTWM.EXE	"LINK ''link_options'/EXE=CTWM.EXE CTWM.''OPT'/OPT"	*.OBJ
$!
$ deassign xpmlib
$ deassign x11xmu
$!
$ exit
$!
$ Clobber:	! Delete executables, Purge directory and clean up object files and listings
$ Delete/noconfirm/log *.exe;*
$!
$ Clean:	! Purge directory, clean up object files and listings
$ Purge
$ Delete/noconfirm/log *.lis;*
$ Delete/noconfirm/log *.obj;*
$!
$ exit
$!
$MAKE: SUBROUTINE   !SUBROUTINE TO CHECK DEPENDENCIES
$ V = 'F$Verify(0)
$! P1 = What we are trying to make
$! P2 = Command to make it
$! P3 - P8  What it depends on
$
$ If F$Search(P1) .Eqs. "" Then Goto Makeit
$ Time = F$CvTime(F$File(P1,"RDT"))
$arg=3
$Loop:
$	Argument = P'arg
$	If Argument .Eqs. "" Then Goto Exit
$	El=0
$Loop2:
$	File = F$Element(El," ",Argument)
$	If File .Eqs. " " Then Goto Endl
$	AFile = ""
$Loop3:
$	OFile = AFile
$	AFile = F$Search(File)
$	If AFile .Eqs. "" .Or. AFile .Eqs. OFile Then Goto NextEl
$	If F$CvTime(F$File(AFile,"RDT")) .Ges. Time Then Goto Makeit
$	Goto Loop3
$NextEL:
$	El = El + 1
$	Goto Loop2
$EndL:
$ arg=arg+1
$ If arg .Le. 8 Then Goto Loop
$ Goto Exit
$
$Makeit:
$ Set Verify
$ 'P2
$ VV='F$Verify(0)
$Exit:
$ If V Then Set Verify
$ENDSUBROUTINE
