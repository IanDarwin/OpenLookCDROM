/*
 * Globals from archie.c
 */
void Check_Archie P_(());
void Init_Archie P_(());
/*
 * Globals from callback.c
 */
void do_callbacks P_((int, struct _callback*));
struct _callback * link_callback P_((struct _callback*, void(*)(), DATA, struct _callback*));
/*
 * Globals from callftp.c
 */
void Abort_Ftp P_(());
void Close_Ftp P_(());
void Connect_Ftp P_((char*, void(*)(), void(*)(), char*, int, int, XtPointer));
void Do_Prompt P_((XtPointer, int));
int Get_Chars P_(());
void Init_Ftp P_(());
void Start_Listen P_((char*, void(*)(), void(*)(), int, int, int, CB*, char*, int));
void Update_Screen P_(());
char * concat P_((char*, char*));
char * concat_output P_((char*));
char * concatdir P_((char*, char*));
void write_ftp P_((char*, void(*)(), void(*)(), char*, int, int, XtPointer));
/*
 * Globals from cd.c
 */
void Start_Cd P_((int, char*, int, void(*)(), DATA, CB*));
/*
 * Globals from connect.c
 */
void Start_Connect P_((int, void(*)(), DATA, CB*));
/*
 * Globals from dialog.c
 */
void Continue_Dialog P_((char*, CB*));
void Dialog_Restart P_((int, CB*));
int Register_dialog_c_a_a P_(());
void Reset_Host_Search_String P_(());
void Reset_Search_String P_(());
void Search_Dialog P_(());
void Search_Host_Dialog P_(());
/*
 * Globals from disconnect.c
 */
void Set_Logged_Out P_(());
void Start_Discconect P_((int, void(*)(), DATA, CB*));
/*
 * Globals from dir_subs.c
 */
void Clean_Up P_((int, CB*));
void Clear_Dirs P_(());
void Clear_Files P_((struct _dirs*));
void Clear_List P_(());
void Count_Files P_((struct _dirs*, int));
void Display_Files P_((char*, char*, int));
void Do_File P_((void(*)(), int));
void Do_List P_((void(*)(), CB*));
void Draw_Files P_((struct _dirs*));
struct _dirs * Find_Dir P_((char*, char*, int));
void Free_Files P_((struct _dirs*));
void Pop_Files P_((struct _dirs*));
void Push_Files P_((struct _dirs*));
void Restart_List P_((CB*, int));
void Restore_Dir_Old P_((struct _dirs*));
void Set_Dir_Type P_((int));
void Set_List_Type P_((int));
void clean_up_finish P_(());
/*
 * Globals from do.c
 */
void add_func_do P_((void(*)(), int));
void do_all P_((int));
void remove_func_do P_((void(*)(), int));
/*
 * Globals from ftp.c
 */
void Set_Note P_((char*));
int  ftp_status P_((struct _output*));
void read_netrc P_((FILE*, char*, char*, int));
void response P_((char*));
/*
 * Globals from get.c
 */
void Start_Get P_((int, char*, char*, int, void(*)(), DATA, CB*));
void Start_View P_((int, char*, char*, int, void(*)(), DATA, CB*));
void get_peek_150 P_((char*));
/*
 * Globals from help.c
 */
void Create_Help_Menu P_(());
void Help P_((Widget, char*, int));
int Help_dead_cb P_((Widget, char*, caddr_t));
void Register_help_CallBacks_and_Actions P_(());
/*
 * Globals from icons.c
 */
void Animate P_((Widget));
void Set_Icon P_((Widget, char*));
void Set_Wn_Name P_((char*));
/*
 * Globals from initialize.c
 */
void Initialize P_(());
/*
 * Globals from lcd.c
 */
void Start_Lcd P_((int, char*, void(*)(), DATA, CB*));
/*
 * Globals from local_dir.c
 */
void Start_Local_Dir P_((int, int, void(*)(), DATA, CB*));
void Update_Local_Dir P_(());
void Update_Stat_Time P_((struct _dirs*));
/*
 * Globals from remote_dir.c
 */
void Start_Remote_Dir P_((int, int, void(*)(), DATA, CB*));
void Update_Remote_Time P_((struct _dirs*));
/*
 * Globals from login.c
 */
void Start_Login P_((int, void(*)(), DATA, CB*));
/*
 * Globals from mkdir.c
 */
void Start_Mkdir P_((int, char*, void(*)(), DATA, CB*));
/*
 * Globals from motif.c
 */
#if defined(MOTIF)
void AppendDialogText P_((Widget, char*));
void AppendStatusText P_((Widget, char*));
Widget CreateSimpleDialog P_((Widget, char*, char*, int, char*, int, char*, int, char*, int, char*, int, void(*)()));
char * GetDialogText P_((Widget));
char * GetLabel P_((Widget));
char * GetText P_((Widget));
int Has_Focus P_((Widget));
void Mark_Menu P_((Widget));
void RegisterWidgets P_((XtAppContext));
void Reset_D_String P_((Widget));
void Reset_Gateway_Button P_(());
void Reset_Retry_Button P_(());
void SetDialogLabel P_((Widget, char*));
void SetLabel P_((Widget, char*));
void SetText P_((Widget, char*));
void Unmark_Menu P_((Widget));
void View_The_File P_((char*, char*));
void WM_DELETE P_((Widget, void(*)(), caddr_t));
void clear_text_cb P_((Widget, XtPointer, XtPointer));
void init_motif_acctions P_(());
void warning_msg P_((String));
#endif /* MOTIF */
/*
 * Globals from noop.c
 */
void Clear_Noop P_((int));
int Is_Noop P_((Widget));
void Register_Noop_CallBacks_and_Actions P_(());
void Set_Noop P_((int));
/*
 * Globals from peek.c
 */
void Lower P_((char*));
char * Next_Token P_((char*, char*));
void Peek P_((char*));
/*
 * Globals from pwd.c
 */
void Start_Pwd P_((int, int, void(*)(), DATA, CB*));
/*
 * Globals from put.c
 */
void Start_Put P_((int, char*, char*, int, void(*)(), DATA, CB*));
/*
 * Globals from reconnect.c
 */
void Start_Reconnect P_((CB*));
/*
 * Globals from sort.c
 */
int Sort_Files P_((struct _dirs*, int, int, int));
/*
 * Globals from syst.c
 */
void Set_Other_Tables P_(());
void Set_Type_Name P_(());
void Start_Syst P_((int, void(*)(), DATA, CB*));
/*
 * Globals from translate.c
 */
char * Compute_Cd P_((char*));
char * Create_Translation_Examples P_(());
void Init_Dotxftp P_(());
void Init_Tranlation_List P_((Widget));
char * Tran_Remote_File P_((char*, int*));
int Tran_Scan P_((char*, struct _dirs*, int));
/*
 * Globals from type.c
 */
void Start_Type P_((int, char*, int, void(*)(), DATA, CB*));
/*
 * Globals from olt.c
 */
#if defined(OPENWINDOW)
void AppendDialogText P_((Widget, char*));
void AppendStatusText P_((Widget, char*));
void AppendTextEdit P_((Widget, char*));
Widget CreateSep P_((Widget, String, Arg*, Cardinal));
Widget CreateSimpleDialog P_((Widget, char*, char*, int, char*, int, char*, int, char*, int, char*, int, void(*)()));
char * GetDialogText P_((Widget));
char * GetText P_((Widget));
int Has_Focus P_((Widget));
void Mark_Menu P_((Widget));
void Position_Dialog_cb P_((Widget, XtPointer, XtPointer));
void RegisterWidgets P_((XtAppContext));
void Reset_D_String P_((Widget));
void Reset_Gateway_Button P_(());
void Reset_Retry_Button P_(());
void SetDialogLabel P_((Widget, char*));
void SetLabel P_((Widget, char*));
void SetStringSource P_((Widget, char*));
void SetText P_((Widget, char*));
void SetTextEdit P_((Widget, char*));
void Unmark_Menu P_((Widget));
void View_The_File P_((char*, char*));
void WM_DELETE P_((Widget, void(*)(), caddr_t));
void clear_text_cb P_((Widget, XtPointer, XtPointer));
void init_olt_acctions P_((XtAppContext));
#endif /* OPENWINDOW */
/*
 * Globals from xaw.c
 */
#if defined(XAW)
void AppendDialogText P_((Widget, char*));
void AppendStatusText P_((Widget, char*));
void Bind_Menu P_((Widget, char*, Widget));
Widget CreateHelpPopupChild P_((Widget, char*));
Widget CreateSimpleDialog P_((Widget, char*, char*, int, char*, int, char*, int, char*, int, char*, int, void(*)()));
char * GetDialogText P_((Widget));
char  * GetLabel P_((Widget));
char * GetText P_((Widget));
void Mark_Menu P_((Widget));
void Position_Dialog_cb P_((Widget, XtPointer, XtPointer));
void RegisterWidgets P_((XtAppContext));
void Reset_D_String P_((Widget));
void Reset_Gateway_Button P_(());
void Reset_Retry_Button P_(());
void SetDialogLabel P_((Widget, char*));
void SetLabel P_((Widget, char*));
void SetText P_((Widget, char*));
void SetTextRate P_((Widget, char*));
void Unmark_Menu P_((Widget));
void View_The_File P_((char*, char*));
void WM_DELETE P_((Widget, void(*)(), caddr_t));
void clear_text_cb P_((Widget, XtPointer, XtPointer));
void init_xaw_acctions P_(());
#endif /* XAW */
/*
 * Globals from wc_hooks.c
 */
void Add_File_Text P_((char*, int, int));
void Add_Status_Text P_((char*));
void Archie_Host P_((char*, char*));
void Changetext P_((char**, int*, int, int, int));
void Clear_Files_Marks P_(());
char * Compute_Up P_(());
char * Compute_Up_Apollo P_(());
void Finish_File_Text P_((int, int));
int Get_List_Position P_(());
void Glob_Files P_((char*, int));
void Highlight P_((int, int));
void Init_Close P_(());
void Init_Login P_(());
void Register P_(());
void Register_App_CallBacks_and_Actions P_(());
void Reset_Search P_(());
void Restore_Last_Status_Msg P_(());
void Search_Files P_((char*, int));
void Search_Host P_((char*));
void Set_Busy P_((   ));
void Set_Info P_(());
void Set_Item_Title P_((char*));
void Set_Local P_((char*));
void Set_Reconnect P_((int));
void Set_Remote P_((char*));
void Set_Remote_Local P_((int));
void Set_Status P_((char*));
void Set_Status_Error P_((char*, char*));
void Set_Status_No_Log P_((char*));
void Set_Status_Short P_((char*));
void Set_Xaw_List P_((int));
void Single_File_Actions P_(());
void Stop_Retry P_(());
void Unset_Busy P_(());
void Update_Files_Opts P_((int));
void Update_Mark P_((int));
void Update_Percent P_((int, int));
void View_File P_((char*, char*));
/*
 * Globals from view.c
 */
void View P_((char*, char*));
