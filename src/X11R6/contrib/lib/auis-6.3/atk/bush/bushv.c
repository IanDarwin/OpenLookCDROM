/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/bush/RCS/bushv.c,v 1.87 1993/11/12 20:16:23 gk5g Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Bush View-object

MODULE	bushv.c

VERSION	0.0

AUTHOR	TC Peters & GW Keim
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Bush View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  08/21/85	Created (TCP)
  01/15/89	Convert to ATK (GW Keim)

END-SPECIFICATION  ******************************************************/


#include <andrewos.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <errno.h>
#include <apt.h>
#include <rect.h>
#include <attribs.h>
#include <dataobj.ih>
#include <event.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <filetype.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <menulist.ih>
#include <cursor.ih>
#include <im.ih>
#include <view.ih>
#include <lpair.ih>
#include <suite.ih>
#include <textv.ih>
#include <text.ih>
#include <treev.ih>	/* tree.ih is included in bush.ih */
#include <complete.ih>
#include <apts.ih>
#include <bush.ih>
#include <bushv.eh>

#define	by_name					    0
#define	by_size					    1
#define	by_date					    2
#define	by_suffix				    3
#define	by_type					    4

#define	nodes_object				    1
#define	entries_object				    2
#define	entry_object				    4

#define	bushv_DefaultMenus			    (1<<6)
#define	bushv_EntryMenus			    (1<<7)
#define	bushv_RWEntryMenus			    (1<<8)

#define	DEFAULTCKPINTERVAL			    120
int CkpInterval;

static char	*sorts[] = { "name", 
			     "size", 
			     "date", 
			     "suffix", 
			     "type", 
			      NULL };


static char	*default_editor_choices[] = { "ez",
					      "emacs",
					      "gnu-emacs",
					      "zip",
					      "raster",
					      "table",
                                              "other",
					       NULL};

static char			    msg[MAXPATHLEN * 2];
static char			    cmd[MAXPATHLEN * 2];
static char			   *argv[10];
extern int			    errno, sys_nerr;
extern char			   *sys_errlist[];

static struct keymap		   *kmap;
static struct menulist		   *menulist = NULL;

static void			    Pop(),
				    PerformExit(),
				    PerformExec(),
				    PerformPrint(),
				    PerformSort(),
				    PerformPop(),
				    PerformDetail(),
				    PerformDestroy(),
				    PerformCreate(),
				    PerformRename(),
				    PerformRescan(),
				    PerformEdit(),
                                    PushToEntries(),
                                    PushToEntry(),
				    SetEditor(),
				    SwitchDirectory(),
				    Push(),
				    PassivateControls(),
				    IssueError(),
				    ToggleDebug(),
                                    HandleChangeDir(),
                                    Checkpoint(),
                                    UpdateDetailCaption(),
                                    EntriesPageUp(),
                                    EntriesPageDown();

static int			    PopToNodes(),
                                    PopToEntries();

static long			    ControlHitHandler(), 
				    TreeHitHandler(), 
				    EntriesHitHandler();

static int			    bushv_WriteFile(),
                                    bushv_SaveFile(),
                                    bushv_SetPrinter();

int				    SortByName(),
				    SortBySize(),
				    SortBySuffix(),
				    SortByType(),
				    SortByDate();

static DoPrint();
static DoExecute();
static int PerformSystemAction();
static int FinishDirMove();
static int DoDestroy();
static int HandleModifiedObject();
static int SortRequested();
static int ResetChildDirPaths();
static int bushv_WriteToFile();
static int bushv_SaveFile();
static int bushv_WriteFile();
static int bushv_SetPrinter();


#define Bush			    (self->bush)
#define ControlView		    (self->control_view)
#define	EntriesView		    (self->entries_view)
#define DirTreeView		    (self->dir_tree_view)
#define EntryView		    (self->entry_view)
#define	EntryViewAppLayer	    (self->entry_view_application_layer)
#define	EntryObject		    (self->entry)
#define	EntryObjectModified	    (self->entry_object_modified)
#define	EntryObjectLastCKP	    (self->entry_object_last_checkpoint)
#define	EntryFilep		    (self->entry_filep)
#define NumPrevSelected		    (self->num_prev_selected)
#define LP			    (self->lp)
#define SortMode		    (self->sortmode)
#define Object			    (self->object)
#define Kmap			    (self->kmap)
#define Kstate			    (self->keyState)
#define Menulist		    (self->menulist)
#define InitNode		    (self->initial_node)
#define CurrNode		    (self->current_node)
#define	MoveNode		    (self->move_node)
#define CurrEntry		    (self->current_entry)
#define Cursor			    (self->cursor)
#define	Detail			    (self->detail)
#define TopLevelInset		    (self->top_level_inset)
#define Debug			    (self->debug)
#define	EditorProgram		    (self->editor_program)
#define EditorChoices		    (self->editor_choices)
#define EditorIndex		    (self->editor_index)
#define	NumEditorChoices	    (self->num_editor_choices)

#define	Tree			    (bush_Tree(Bush))
#define	TreeRoot		    (bush_TreeRoot(Bush))
#define	DirMode(tn)		    (bush_DirMode(Bush,tn))
#define	DirPath(tn)		    (bush_DirPath(Bush,tn))
#define	DirName(tn)		    (bush_DirName(Bush,tn))
#define	DirTimeStamp(tn)	    (bush_DirTimeStamp(Bush,tn))
#define	ScanRequired(tn)	    (bush_ScanRequired(Bush,tn))
#define	RootDirPath		    (bush_RootDirPath(Bush))
#define	RootPathName		    (bush_RootPathName(Bush))
#define	DirEntries(tn)		    (bush_DirEntries(Bush,tn))
#define	DirEntriesCount(tn)	    (bush_DirEntriesCount(Bush,tn))
#define	DirEntryPtr(tn)		    (bush_DirEntryPtr(Bush,tn))
#define	DirEntry(tn,i)		    (bush_DirEntry(Bush,tn,i))
#define	DirEntryMode(tn,i)	    (bush_DirEntryMode(Bush,tn,i))
#define	DirEntryPos(tn,i)	    (bush_DirEntryPos(Bush,tn,i))
#define	DirEntryName(tn,i)	    (bush_DirEntryName(Bush,tn,i))
#define	DirEntryLinkName(tn,i)	    (bush_DirEntryLinkName(Bush,tn,i))
#define	DirEntryType(tn,i)	    (bush_DirEntryType(Bush,tn,i))
#define	DirEntryOwner(tn,i)	    (bush_DirEntryOwner(Bush,tn,i))
#define	DirEntryNLinks(tn,i)	    (bush_DirEntryNLinks(Bush,tn,i))
#define	DirEntryTimeStamp(tn,i)	    (bush_DirEntryTimeStamp(Bush,tn,i))
#define	DirEntrySize(tn,i)	    (bush_DirEntrySize(Bush,tn,i))
#define	DirEntryPerms(tn,i)	    (bush_DirEntryPerms(Bush,tn,i))

#define	Parent(tn)		    (tree_ParentNode(Tree,tn))
#define	Child(tn)		    (tree_ChildNode(Tree,tn))
#define	Left(tn)		    (tree_LeftNode(Tree,tn))
#define	Right(tn)		    (tree_RightNode(Tree,tn))

#define EntrySelected(entry)	    ((entry)->mode.selected)
#define EntryDirType(entry)	    ((entry)->type.dir)
#define EntryLinkType(entry)	    ((entry)->type.soft_link)
#define EntryPerms(entry)	    ((entry)->permissions)
#define	EntryLinkName(entry)	    ((entry)->link_name)

#define SetTreeNotificationCode(self,code) \
    tree_SetNotificationCode(Tree,code)
#define SetTreeNotificationNode(self,node) \
    tree_SetNotificationNode(Tree,node)
#define SetTreeNotificationData(self,node,code) \
    SetTreeNotificationNode(self,node);\
    SetTreeNotificationCode(self,code)
#define NotifyTreeObservers(self) \
    tree_NotifyObservers(Tree,NULL)

#define	AllocNameSpace(s,t)	    apts_CaptureString(s,t)
#define	Announce(msg_string)	    bushv_Announce(self,msg_string)
#define ClearMessageLine()	    if(!message_Asking(self)) Announce("")

#define	edit_code		    1
#define	exec_code		    2
#define	print_code		    3
#define	editor_code		    4
#define	sort_code		    5
#define	pop_code		    6
#define	detail_code		    7
#define	destroy_code		    8
#define	switch_code		    9
#define	rescan_code		    10
#define	create_code		    11
#define	rename_code		    12

struct item_data {  /* item data for the ControlView buttons */
    long code;	    /* item index */
    long activate;  /* mask that determines under what state the item is active */
};

static struct item_data	
  edit_data =	 {edit_code, entries_object | entry_object},
  exec_data =	 {exec_code, entries_object | entry_object},
  print_data =	 {print_code, nodes_object | entries_object | entry_object},
  editor_data =	 {editor_code, nodes_object | entries_object | entry_object},
  sort_data =	 {sort_code, nodes_object | entries_object},
  pop_data =	 {pop_code, entries_object | entry_object},
  detail_data =	 {detail_code, nodes_object | entries_object},
  destroy_data = {destroy_code, nodes_object | entries_object | entry_object},
  switch_data =	 {switch_code, nodes_object | entries_object | entry_object},
  rescan_data =	 {rescan_code, nodes_object | entries_object | entry_object},
  create_data =	 {create_code, nodes_object | entries_object},
  rename_data =	 {rename_code, nodes_object | entries_object | entry_object};

static suite_Specification edit[] = {
    suite_ItemCaption("Edit"),
    suite_ItemDatum(&edit_data), 
    NULL };
static suite_Specification exec[] = {
    suite_ItemCaption("Exec"), 
    suite_ItemDatum(&exec_data), 
    NULL };
static suite_Specification print[] = {
    suite_ItemCaption("Print"), 
    suite_ItemDatum(&print_data), 
    NULL };
static suite_Specification editor[] = {
    suite_ItemCaption("Editor"), 
    suite_ItemDatum(&editor_data), 
    NULL };
static suite_Specification sort[] = {
    suite_ItemCaption("Sort: name"), 
    suite_ItemDatum(&sort_data), 
    NULL };
static suite_Specification pop[] = {
    suite_ItemCaption("Pop"), 
    suite_ItemDatum(&pop_data), 
    NULL };
static suite_Specification detail[] = {
    suite_ItemCaption("Detail: off"), 
    suite_ItemDatum(&detail_data), 
    NULL };
static suite_Specification destroy[] = {
    suite_ItemCaption("Destroy"), 
    suite_ItemDatum(&destroy_data), 
    NULL };
static suite_Specification Switch[] = {
    suite_ItemCaption("Switch"), 
    suite_ItemDatum(&switch_data), 
    NULL };
static suite_Specification rescan[] = {
    suite_ItemCaption("ReScan"), 
    suite_ItemDatum(&rescan_data), 
    NULL };
static suite_Specification create[] = {
    suite_ItemCaption("Create"), 
    suite_ItemDatum(&create_data), 
    NULL };
static suite_Specification rename_it[] = {
    suite_ItemCaption("ReName"), 
    suite_ItemDatum(&rename_data), 
    NULL };

suite_Specification control_spec[] = {
    suite_TitleCaption( "Bush: A FileSystem Browser" ),
    suite_TitlePlacement( suite_Top ),
    suite_TitleCaptionFontName( "AndySans10" ),
    suite_TitleBorderStyle( suite_Rectangle ),
    suite_Item(edit),
    suite_Item(editor),
    suite_Item(exec),
    suite_Item(print),
    suite_Item(pop),
    suite_Item(Switch),
    suite_Item(sort),
    suite_Item(detail),
    suite_Item(create),
    suite_Item(destroy),
    suite_Item(rescan),
    suite_Item(rename_it),
    suite_ItemCaptionFontName( "AndySans10b" ),
    suite_TitleBorderSize( 2 ),
    suite_GutterSize( 1 ),
    suite_BorderSize( 2 ),
    suite_ItemBorderSize( 2 ),
    suite_ItemOrder( suite_ColumnMajor ),
    suite_Arrangement( suite_Matrix | suite_Fixed ),
    suite_Columns( 6 ),
    suite_Rows( 2 ),
    suite_SelectionMode( suite_Exclusive ),
    suite_HitHandler( ControlHitHandler ),
    NULL
};

suite_Specification entries_spec[] = {
    suite_TitleCaption( "Entries" ),
    suite_TitleBorderSize( 2 ),
    suite_TitleBorderStyle( suite_Rectangle ),
    suite_BorderStyle( suite_None ),
    suite_Arrangement( suite_List | suite_Matrix | suite_Unbalanced | suite_ColumnLine ),
    suite_HorizontalGutterSize( 3 ),
    suite_ItemOrder( suite_ColumnMajor ),
    suite_ItemCaptionFontName( "AndyType12f" ),
    suite_TitleFontName( "Andy10" ),
    suite_ItemCaptionAlignment( suite_Left | suite_Center ),
    suite_ItemBorderStyle( suite_Invisible ),
    suite_ItemBorderSize( 3 ),
    suite_VerticalGutterSize( 1 ), 
    suite_SelectionMode( suite_Inclusive ),
    suite_SortHandler( SortByName ),
    suite_Scroll( suite_Left ),
    suite_HitHandler( EntriesHitHandler ),
    suite_CursorFontName( "aptcsr20" ),
    suite_Cursor( 'c' ),
    NULL
};

treev_Specification tree_spec[] = {
    treev_NodeConnectorStyle( treev_Fold | treev_DogLeg ), 
    treev_NodeFiligree( treev_DropShadow ),
    treev_NodeHighlightStyle( treev_Invert ),
    treev_Scroll( treev_Left ),
    treev_BackgroundShade( 50 ),
    treev_Cursor( 'z' ),
    treev_CursorFontName( "aptcsr20" ),
    treev_HitHandler( TreeHitHandler ),
    NULL
};

static void
PostCursor( self, type )
  register struct bushv   *self;
  register int		   type;
{
  struct rectangle	   r;

  bushv_GetVisualBounds(self,&r);
  cursor_SetStandard(Cursor,type);
  bushv_PostCursor(self,&r,Cursor);
}

static long
ResetSelectedState( self, suite, item, datum )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item	*item;
  register unsigned		 datum;
/*  Called by static void EntriesHitHandler().
    Resets the Dir_Entry mode flag, associated with each suite_item, to 
    correspond with the items that are currently highlighted in the suite 
    (entries_object). We do this after the check for Pop conditions because we 
    need to determine the number of items previously highlighted for that check. */
{
  register struct Dir_Entry	*dirEntry = NULL;
  register long int		 status = 0;

  if(!suite || !item) return(status);
  dirEntry = (struct Dir_Entry*)
    suite_ItemAttribute(suite,item,suite_ItemDatum(0));
  if(!suite_ItemExposed(suite,item) || 
     !suite_ItemHighlighted(suite,item) || 
     !suite_ItemActivated(suite,item)) 
	    EntrySelected(dirEntry) = FALSE;
  else EntrySelected(dirEntry) = TRUE;
  return(status);
}

static long
EntriesHitHandler( self, suite, item, object, action, x, y, numClicks )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item	*item;
  register long			 object;
  register enum view_MouseAction action;
  register long			 x, y, numClicks;
{
  register struct Dir_Entry	*dirEntry = NULL;
  register int			 numSelected = 0, count = 0, i = 0;

  IN(EntriesHitHandler);
  ClearMessageLine();
  if((object == suite_ItemObject) && item && 
	((action == view_LeftUp) || (action == view_RightUp))) {
    if(action == view_LeftUp) {
	dirEntry = (struct Dir_Entry*) 
	suite_ItemAttribute(EntriesView,item,suite_ItemDatum(0));
	if(EntrySelected(dirEntry) && (NumPrevSelected == 1)) {
	  PostCursor(self,Cursor_Wait);
	  CurrEntry = dirEntry;
	  Push(self);
	  bushv_RetractCursor(self,Cursor);
	}
    }
    suite_Apply(suite,ResetSelectedState,self,NULL);
    if(DirEntries(CurrNode)) {
      count = DirEntriesCount(CurrNode);
      for(i = 0 ; i < count ; i++)
        if(DirEntryMode(CurrNode,i).selected) 
	  numSelected++;
    }
    NumPrevSelected = numSelected;
    if(Object == entries_object) { /* may have Pop'ed back to nodes */
      if((numSelected > 1) || (numSelected == 0)) 
	sprintf(msg,"%d entries currently selected",numSelected);
      else sprintf(msg,"1 entry selected");
	Announce(msg);
    }
  }
  OUT(EntriesHitHandler);
  return(0);
}

static void
StartDirMove( self, tn )
  register struct bushv    *self;
  register tree_type_node   tn;
{
  IN(StartDirMove);
  if(tn) {
    PostCursor(self,Cursor_Gunsight);
    sprintf(msg,"Moving directory '%s'...",DirName(tn));
    Announce(msg);
  }
  else Announce("No selected directory to move.");
  OUT(StartDirMove);
}

static int
FinishDirMove( self, tn )
  register struct bushv	    *self;
  register tree_type_node    tn;
{
  register int		     status = 0;
  char			     finalLocation[MAXPATHLEN];

  IN(FinishDirMove);
  PostCursor(self,Cursor_Wait);
  if(!MoveNode) status = -1;
  else if(!tn) {
    sprintf(msg,"No target selected. Move operation cancelled.");
    Announce(msg);
    status = -1;
  }
  else if(tn == MoveNode) {
    sprintf(msg,"Cancelled.");
    Announce(msg);
    status = -1;
  }
  else if(tree_NodeAncestor(Tree,MoveNode,tn)) {
    sprintf(msg,"You cannot move a directory to one of its children.");
    Announce(msg);
    status = -1;
  }
  else if(tn == Parent(MoveNode)) {
    sprintf(msg,"'%s' already lives under '%s'",
	     DirName(MoveNode),DirName(tn));
    Announce(msg);
    status = -1;
  }
  else {
    sprintf(msg,"Moving '%s' under '%s'",DirName(MoveNode),
	DirName(tn));
    Announce(msg);
    sprintf(finalLocation,"%s/%s",DirPath(tn),
	     DirName(MoveNode));
    if(status = rename(DirPath(MoveNode),finalLocation)) {
      IssueError(self,"Moving",DirName(MoveNode),TRUE);
      sprintf(msg,"Move failed.");
      Announce(msg);
    }
    else {
      if(tree_NodeLevel(Tree,CurrNode) >= tree_NodeLevel(Tree,MoveNode)) 
	  CurrNode = NULL;
      SetTreeNotificationData(self,MoveNode,tree_NodeDestroyed);
      NotifyTreeObservers(self);
      tree_DestroyNode(Tree,MoveNode);
      sprintf(msg,"Move succeeded.");
      Announce(msg);
    }
  }
  bushv_RetractCursor(self,Cursor);
  OUT(FinishDirMove);
  return(status);
}

static long
TreeHitHandler( self, tree_view, node, object, action, x, y, numClicks )
  register struct bushv		    *self;
  register struct treev		    *tree_view;
  register tree_type_node	     node;
  register long			     object;
  register enum view_MouseAction     action;
  register long			     x, y, numClicks;
{
  register tree_type_node	     old_CurrNode = NULL;
  register tree_type_node	     peer = NULL;
  struct stat			     stats;

  IN(TreeHitHandler);
  if(object == treev_NodeObject) {
    if(node) treev_HighlightNode(tree_view,node);
    if(action == view_RightDown) {
      MoveNode = node;
      StartDirMove(self,node);
      return 0;
    }
    else if(action == view_RightUp) {
      if(FinishDirMove(self,node)) {
        treev_HighlightNode(tree_view,CurrNode);
	MoveNode = NULL;
	return 0;
      }
      MoveNode = NULL;
      DirMode(node).do_rescan = TRUE;
    }
    else if(action != view_LeftUp || !node) return 0;
    ClearMessageLine();
    PostCursor(self,Cursor_Wait);
    if(stat(DirPath(node),&stats)) {
      DirMode(node).stat_failed = TRUE;
      IssueError(self,"Scanning",DirName(node),TRUE);
      bushv_RetractCursor(self,Cursor);
      return 0;
    }
    old_CurrNode = CurrNode;
    CurrNode = node;
    if(Parent(CurrNode)) peer = Child(Parent(CurrNode));
    else peer = CurrNode;
    if(!DirMode(CurrNode).selected || (action == view_RightUp)) {
      if(old_CurrNode) {
        DirMode(old_CurrNode).selected = FALSE;
	old_CurrNode = NULL;
      }
      while(peer) {
        if(Child(peer)) {
	  SetTreeNotificationData(self,peer,
				  tree_NodeChildrenDestroyed);
	  NotifyTreeObservers(self);
	  bush_DestroySubDirs(Bush,peer);
	  break;
	}
	peer = Right(peer);
      }
      DirMode(CurrNode).selected = TRUE;
      if(ScanRequired(CurrNode)) {
        sprintf(msg,"Scanning '%s' ...",DirName(CurrNode));
	Announce(msg);
	im_ForceUpdate();
	bush_ScanDir(Bush,CurrNode);
	ClearMessageLine();
      }
      bush_BuildSubDirs(Bush,CurrNode);
      SetTreeNotificationData(self,CurrNode,
			       tree_NodeChildrenCreated);
      NotifyTreeObservers(self);
      suite_ChangeSuiteAttribute(ControlView,
				  suite_TitleCaption(DirPath(CurrNode)));
    }
    else Push(self);
    bushv_RetractCursor(self,Cursor);
  }
  OUT(TreeHitHandler);
  return(0);
}

static long
ControlHitHandler( self, suite, item, object, action, x, y, numClicks )
  register struct bushv		    *self;
  register struct suite		    *suite;
  register struct suite_item	    *item;
  register long			     object;
  register enum view_MouseAction     action;
  register long			     x, y, numClicks;
{

  IN(ControlHitHandler);
  ClearMessageLine();
  if((action == view_LeftUp) || (action == view_RightUp)) {
    if(item && (object == suite_ItemObject)) {
      struct item_data	*itemData = NULL;

      if(itemData = (struct item_data*) suite_ItemAttribute(suite, item, suite_ItemData(0)))
        switch(itemData->code) {
	    case(edit_code):	PerformEdit(self);	break;
	    case(exec_code):	PerformExec(self);	break;
            case(print_code):	PerformPrint(self);	break;
            case(sort_code):	PerformSort(self);	break;
            case(destroy_code):	PerformDestroy(self);   break;
            case(rescan_code):	PerformRescan(self);    break;
            case(create_code):	PerformCreate(self);    break;
            case(pop_code):	PerformPop(self);	break;
            case(detail_code):	PerformDetail(self);    break;
            case(rename_code):	PerformRename(self);    break;
            case(editor_code):	SetEditor(self);	break;
            case(switch_code):	SwitchDirectory(self);  break;
	}
      PassivateControls(self);
    }
    else if(object == suite_TitleObject) {
      suite_HighlightTitle(suite);
      PostCursor(self,Cursor_Wait);
      HandleChangeDir(self,DirPath(CurrNode));
#ifndef hp9000s800 /* The SIGALRM causes trouble on the hp800. */
      sleep(1);
#endif /* hp9000s800 */
      bushv_RetractCursor(self,Cursor);
      sprintf(msg,"Working directory changed to '%s'",
	       DirPath(CurrNode));
      Announce(msg);
      suite_NormalizeTitle(suite);
    }
#if 0
    else if(object == suite_NoObject) {
      PassivateControls(self);
      if(suite_CurrentItem(suite)) 
        suite_PassivateItem(suite,suite_CurrentItem(suite));
    }	
#endif
  }
  OUT(ControlHitHandler);
  return(0);
}

static char *
FileSuffix( file_name )
  register char   *file_name;
{
  register char   *suffix;

  if(suffix = (char*)rindex(file_name,'.')) suffix++;
  else suffix = file_name + strlen(file_name);
  return(suffix);
}

static char *
FileType( file_name )
  register char	    *file_name;
{
  static char	    *suffixes[] = {"BAK","CKP",0};
  register char	    *suffix, **suffix_ptr;

  suffix = (char*)rindex(file_name,'.');
  if(!suffix) return(file_name + strlen(file_name));
  suffix++;
  suffix_ptr = suffixes;
  while(*suffix_ptr) {
    if(!strcmp(suffix,*suffix_ptr)) {
      *(suffix-1) = '\0';
      return(FileSuffix(file_name));
    }
    suffix_ptr++;
  }
  return(suffix);
}

int
SortByName( self, suite, e1, e2 )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item    *e1,*e2;
{
  register struct Dir_Entry	*a = NULL, *b = NULL;
  register long			 status = 0;

  if(!e1 || !e2) return(0);
  a = (struct Dir_Entry*)suite_ItemAttribute(suite,e1,suite_ItemDatum(0));
  b = (struct Dir_Entry*)suite_ItemAttribute(suite,e2,suite_ItemDatum(0));
  if(a && b) { 
    status = strcmp(a->name,b->name);
    if(status < 0) return(-1);
    else if(status > 0) return(1);
  }
  return(0);
}

int
SortBySuffix( self, suite, e1, e2 )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item    *e1,*e2;
{
  register struct Dir_Entry	*a = NULL, *b = NULL;
  register long			 rc;

  if(!e1|| !e2) return(0);
  a = (struct Dir_Entry*)suite_ItemAttribute(suite,e1,suite_ItemDatum(0));
  b = (struct Dir_Entry*)suite_ItemAttribute(suite,e2,suite_ItemDatum(0));
  if(a && b)
    if(!(rc = strcmp(FileSuffix(a->name),FileSuffix(b->name)))) {
      rc = strcmp(a->name,b->name);
      if(rc > 0) return(1);
      else if(rc < 0) return(-1);
    }
    else {
      if(rc > 0) return(1);
      else if(rc < 0) return(-1);
    }
  return(0);
}

int
SortBySize( self, suite, e1, e2 )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item    *e1,*e2;
{
  register struct Dir_Entry	*a = NULL, *b = NULL;

  if(!e1 || !e2) return(0);
  a = (struct Dir_Entry*)suite_ItemAttribute(suite,e1,suite_ItemDatum(0));
  b = (struct Dir_Entry*)suite_ItemAttribute(suite,e2,suite_ItemDatum(0));
  if(a && b) {
    if(a->size < b->size) return(1);
    else if(a->size > b->size) return(-1);
  }
  return(0);
}

int
SortByDate( self, suite, e1, e2 )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item    *e1,*e2;
{
  register struct Dir_Entry	*a = NULL, *b = NULL;

  if(!e1 || !e2) return(0);
  a = (struct Dir_Entry*)suite_ItemAttribute(suite,e1,suite_ItemDatum(0));
  b = (struct Dir_Entry*)suite_ItemAttribute(suite,e2,suite_ItemDatum(0));
  if(a && b) {
    if(a->time_stamp < b->time_stamp) return(1);
    if(a->time_stamp > b->time_stamp) return(-1);
  }
  return(0);
}

int
SortByType( self, suite, e1, e2 )
  register struct bushv		*self;
  register struct suite		*suite;
  register struct suite_item    *e1,*e2;
{
  register struct Dir_Entry	*a = NULL, *b = NULL;
  char				 n1[MAXPATHLEN+1], n2[MAXPATHLEN+1];
  register long			 rc;

  if(!e1 || !e2) return(0);
  a = (struct Dir_Entry*)suite_ItemAttribute(suite,e1,suite_ItemDatum(0));
  b = (struct Dir_Entry*)suite_ItemAttribute(suite,e2,suite_ItemDatum(0));
  if(a && b) {
    strcpy(n1,a->name);
    strcpy(n2,b->name);
    if(!(rc = strcmp(FileType(n1),FileType(n2)))) {
      rc = strcmp(a->name,b->name);
      if(rc < 0) return(-1);
      else if(rc > 0) return(1);
    }
    else {
      if(rc < 0) return(-1);
      else if(rc > 0) return(1);
    }
  }
  return(0);
}

static struct bind_Description bushvBindings[] = {
  {"bushv-exit","\030\003",0,"Quit~99",0,bushv_DefaultMenus, PerformExit, 
     "Exit.  If any file is modified ask for confirmation."},
  {"bushv-save-file","\030\023",0,"Save~20",0,bushv_RWEntryMenus, 
     (void(*)())bushv_SaveFile,"Saves buffer into its current file."},
  {"bushv-write-file","\030\027",0,"File~10,Save As~1",0,
     bushv_EntryMenus,(void(*)())bushv_WriteFile,"Prompt for a file to save."},
  {"bushv-set-printer",NULL,0,"File~10,Set Printer~20",0,
     bushv_EntryMenus, (void(*)())bushv_SetPrinter, "Set the default printer."},
  NULL
};

boolean
bushv__InitializeClass( ClassID )
  struct classheader	    *ClassID;
{
  struct classinfo	    *classInfo = NULL;
  struct proctable_Entry *pe;

  kmap = keymap_New();
  menulist = menulist_New();
  classInfo = class_Load("bushv");
  bind_BindList(bushvBindings, kmap, menulist, classInfo);
  proctable_DefineProc("bushv-DEBUG", ToggleDebug, 
    &bushv_classinfo, NULL, "Toggle Bush debug flag.");
  proctable_DefineProc("bushv-pop", PerformPop, 
    &bushv_classinfo, NULL, "Pop up a level.");
  proctable_DefineProc("bushv-switch", SwitchDirectory, 
    &bushv_classinfo, NULL, "Switch to a new directory.");
  proctable_DefineProc("bushv-rescan", PerformRescan, 
    &bushv_classinfo, NULL, "Rescan the current directory.");
  proctable_DefineProc("bushv-destroy", PerformDestroy, 
    &bushv_classinfo, NULL, "Destroy the current directory/files.");

  pe = proctable_DefineProc("bushv-entries-page-up", EntriesPageUp, &bushv_classinfo, NULL, "Page up file entries list");
  keymap_BindToKey(kmap, "\033v", pe, 0);
  keymap_BindToKey(kmap, "\033G", pe, 0);

  pe = proctable_DefineProc("bushv-entries-page-down", EntriesPageDown, &bushv_classinfo, NULL, "Page down file entries list");
  keymap_BindToKey(kmap, "\026", pe, 0);
  keymap_BindToKey(kmap, "\033E", pe, 0);
  return(TRUE);
}

static void
GetPreferredEditors( self )
  register struct bushv	 *self;
{
  register char		 *tmp = NULL, *colon = NULL;
  char			 *myCopy = NULL;
  register int		  i = 0;

  IN(GetPreferredEditors);
  environ_SetProgramName("bush");
  if(((tmp = environ_GetProfile("editors")) ||
      (tmp = environ_GetProfile("editor"))) && *tmp) {
    AllocNameSpace(tmp,&myCopy);
    if(colon = (char*)index(tmp = myCopy,':')) {
      while((colon = (char*)index(tmp,':')) && (i < MAXEDITORS)) {
        *colon = '\0';
	if(tmp && (*tmp != '\0')) {
	  EditorChoices[i] = NULL;
	  AllocNameSpace(tmp,&EditorChoices[i++]);
	}
	tmp = colon + 1;
      }
      if(tmp && (*tmp != '\0') && (i < MAXEDITORS)) {
        EditorChoices[i] = NULL;
	AllocNameSpace(tmp,&EditorChoices[i]);
	EditorChoices[i+1] = NULL;
      }
      else if(i >= MAXEDITORS)
	  fprintf(stderr,"bush: too many editors in preference.\n");
    }
    else if(tmp && (*tmp != '\0')) {
      EditorChoices[i] = NULL;
      AllocNameSpace(myCopy,&EditorChoices[i]);
      EditorChoices[i+1] = NULL;
    }
    i++;
    AllocNameSpace("other",&EditorChoices[i]);
    EditorChoices[i+1] = NULL;
    NumEditorChoices = i+1;
    if(myCopy) free(myCopy);
  }
  else if(!tmp || (i == 0)) {
    for( i = 0 ; default_editor_choices[i] && (i < MAXEDITORS); i++ ) {
      EditorChoices[i] = NULL;
      AllocNameSpace(default_editor_choices[i],&EditorChoices[i]);
    }
    EditorChoices[i] = NULL;
    NumEditorChoices = i;
  }
  EditorIndex = 0;
  strcpy(EditorProgram,EditorChoices[0]);
  if(EditorProgram && *EditorProgram != '\0') {
    struct suite_item	*editorItem = NULL;
    char		 editorCaption[64];

    if(editorItem = suite_ItemOfDatum(ControlView,&editor_data)) {
      sprintf(editorCaption,"Editor: %s",EditorProgram);
      suite_SetItemAttribute(ControlView, editorItem, suite_ItemCaption(editorCaption));
    }
  }
  OUT(GetPreferredEditors);
}

static void
GetPreferredFonts( self )
  register struct bushv	*self;
{
  char *control_font;
  char *tree_node_font;
  char *listing_font;
  int size = 0;

  IN(GetPreferredFont);
  control_font = environ_GetProfile("controlfont");
  tree_node_font = environ_GetProfile("treenodefont");
  listing_font = environ_GetProfile("listingfont");
  if(control_font && *control_font) {
      suite_SetSuiteAttribute(ControlView, suite_TitleFontName(control_font));
      suite_SetSuiteAttribute(ControlView, suite_ItemCaptionFontName(control_font));
  }
  if(listing_font && *listing_font) {
      suite_SetSuiteAttribute(EntriesView, suite_TitleFontName(listing_font));
      suite_SetSuiteAttribute(EntriesView, suite_ItemCaptionFontName(listing_font));
  }
  if(tree_node_font && *tree_node_font) {
      treev_SetTreeAttribute(DirTreeView, treev_NodeFontName(tree_node_font));
  }
  OUT(GetPreferredFont);
}

static void
GetPreferences( self )
  register struct bushv	*self;
{
  IN(GetPreferences);
  GetPreferredEditors(self);
  GetPreferredFonts(self);
  CkpInterval = environ_GetProfileInt("CheckpointInterval",DEFAULTCKPINTERVAL);
  OUT(GetPreferences);
}

boolean
bushv__InitializeObject( ClassID, self )
  register struct classheader	*ClassID;
  register struct bushv		*self;
{

  Kmap = kmap;
  if(!(Kstate = keystate_Create(self,Kmap))) {
    fprintf(stderr,"bushv: could not create keystate.\n");
    exit(1);
  }
  Cursor = cursor_Create(self);
  cursor_SetStandard(Cursor,Cursor_Wait);
  Bush = NULL;
  CurrNode = InitNode = MoveNode = NULL;
  CurrEntry = NULL;
  EntryView = EntryViewAppLayer = NULL;
  EntryObject = NULL;
  EntryObjectModified = EntryObjectLastCKP = 0;
  EntryFilep = NULL;
  ControlView = suite_Create(control_spec,self);
  EntriesView = suite_Create(entries_spec,self);
  DirTreeView = treev_Create(tree_spec,self);
  LP = lpair_New();
  lpair_VTFixed(LP,(struct view*)ControlView,(struct view*)DirTreeView,95,TRUE);
  lpair_LinkTree(LP,(struct view*)self);
  Object = nodes_object;
  SortMode = 0;
  Detail = 0;
  NumPrevSelected = 0;
  Debug = 1;
  Menulist = menulist_DuplicateML(menulist,self);
  GetPreferences(self);
  bushv_SetOptions(self,aptv_SuppressControl | aptv_SuppressBorder);
  TopLevelInset = TRUE;
  OUT(bushv_InitializeObject);
  return(TRUE);
}   

void
bushv__FinalizeObject( ClassID, self )
  register struct classheader	*ClassID;
  register struct bushv		*self;
{
  IN(bushv_FinalizeObject);
  if(LP) lpair_Destroy(LP);
  if(ControlView) suite_Destroy(ControlView);
  if(EntriesView) suite_Destroy(EntriesView);
  if(DirTreeView) treev_Destroy(DirTreeView);
  if(EntryView) view_Destroy(EntryView);
  if(EntryObject) dataobject_Destroy(EntryObject);
  if(Menulist) menulist_Destroy(Menulist);
  OUT(bushv_FinalizeObject);
}

struct bushv *
bushv__Create( ClassID, object )
  register struct classheader	*ClassID;
  register char			 object;
{
  struct bushv	*self = NULL;

  if(self = bushv_New()) Object = object;
  OUT(bushv_Create);
  return(self);
}

void
bushv__PostMenus( self, menulist )
  struct bushv		*self;
  struct menulist	*menulist;
{
  int			 mask = 0;

  IN(bushv_PostMenus);
  menulist_ClearChain(Menulist);
  mask = menulist_GetMask(Menulist) | bushv_DefaultMenus;
  if(Object == entry_object) 
    mask |= bushv_EntryMenus;
  else 
    mask &= (~bushv_EntryMenus & ~bushv_RWEntryMenus);
  menulist_SetMask(Menulist,mask);
  if(menulist) 
    menulist_ChainAfterML(Menulist,menulist,NULL);
  super_PostMenus(self,Menulist);
  OUT(bushv_PostMenus);
}

struct view *
bushv__Hit( self, action, x, y, numberOfClicks )
  struct bushv		    *self;
  enum view_MouseAction      action;
  long			     x, y, numberOfClicks;
{
  IN(bushv_Hit);
  return(lpair_Hit(LP,action,x,y,numberOfClicks));
}

void
bushv__FullUpdate( self, Type, left, top, width, height )
  struct bushv		    *self;
  enum view_UpdateType       Type;
  long			     left, top, width, height;
{
  struct rectangle	     r;
  char		     RootPathIfInset[MAXPATHLEN];
  char		     NewTitle[MAXPATHLEN];

  IN(bushv_FullUpdate);
  if(Type == view_LastPartialRedraw || Type == view_FullRedraw) {
    super_FullUpdate(self,Type,left,top,width,height);
    if(!RootPathName) {
      im_GetDirectory(RootPathIfInset);
      bush_InitTree(Bush,RootPathIfInset);
      CurrNode = InitNode = TreeRoot;
      bush_BuildSubDirs(Bush,TreeRoot);
      HandleChangeDir(self,RootPathName);
    }
    bushv_GetVisualBounds(self,&r);
    lpair_InsertView(LP,(struct view*)self,&r);
    bushv_SetTransferMode(self,graphic_BLACK);
    lpair_FullUpdate(LP,Type,0,0,r.width,r.height);
    if(CurrNode || (CurrNode = treev_CurrentNode(DirTreeView))) {
      sprintf(NewTitle,"%s%s%s",DirPath(CurrNode),
	       CurrEntry ? "/" : "", CurrEntry ? CurrEntry->name : "");
      suite_ChangeSuiteAttribute(ControlView,
				  suite_TitleCaption(NewTitle));
    }
    else 
	suite_ChangeSuiteAttribute(ControlView,"No Current Directory");
    PassivateControls(self);
    ClearMessageLine();
  }
  if(Object == nodes_object)
    bushv_WantInputFocus(self,DirTreeView);
  OUT(bushv_FullUpdate);
}

static void
DoEdit( self, path, name )
  struct bushv	*self;
  char		*path, *name; 
{
  char		 full_path[MAXPATHLEN * 2];

  IN(DoEdit);
  sprintf(full_path,"%s/%s",path,name);
  sprintf(msg,"%s '%s/%s'",EditorProgram,path,name);
  argv[0] = EditorProgram;
  argv[1] = full_path;
  argv[2] = NULL;
  PerformSystemAction(self,EditorProgram,argv,msg);
  OUT(DoEdit);
}

static void
PerformEdit( self )
  struct bushv	*self;
{
  register int	 i = 0;

  IN(PerformEdit);
  PostCursor(self,Cursor_Wait);
  switch(Object) {
    case nodes_object: 
      break;
    case entries_object:
      if(CurrNode && DirEntries(CurrNode) &&
	  (DirEntriesCount(CurrNode) >= 0))
        for(i=0;i<DirEntriesCount(CurrNode);i++)
	  if(DirEntryMode(CurrNode,i).selected &&
	     !DirEntryType(CurrNode,i).dir)
	    DoEdit(self,DirPath(CurrNode), 
		   DirEntryName(CurrNode,i));
      break;
    case entry_object:
      DoEdit(self,DirPath(CurrNode),CurrEntry->name);
      break;
  }
  bushv_RetractCursor(self,Cursor);
  OUT(PerformEdit);
}

static void
PerformPrint( self )
  struct bushv	*self;
{
  register int	 i = 0;
  register FILE	*file = NULL;

  IN(PerformPrint);
  PostCursor(self,Cursor_Wait);
  switch(Object) {
    case nodes_object: 
      if(file = fopen("/tmp/bush_print.PS","w")) {
        treev_Print(DirTreeView,file,"PostScript","PostScript",1);
	fclose(file);
	system("print -Tnative /tmp/bush_print.PS");
	sprintf(msg,"Printed '%s'",RootPathName);
	Announce(msg);
      }
      else Announce("Error Printing");
      break;
    case entries_object:
      for(i=0;i < DirEntriesCount(CurrNode);i++)
        if(DirEntryMode(CurrNode,i).selected && 
	    !DirEntryType(CurrNode,i).dir)
	  DoPrint(self,DirPath(CurrNode),
		  DirEntryName(CurrNode,i));
      break;
    case entry_object:
      DoPrint(self,DirPath(CurrNode),CurrEntry->name);
      break;
  }
  bushv_RetractCursor(self,Cursor);
  OUT(PerformPrint);
}

static
DoPrint( self, path, name )
  struct bushv *self;
  char *path, *name; 
{
  char full_path[MAXPATHLEN];
  int i = 0;

  IN(DoPrint);
  sprintf(full_path, "%s/%s", path, name);
  sprintf(msg, "Printing '%s'", full_path);
  argv[i++] = "ezprint";
  argv[i++] = full_path;
  argv[i] = NULL;
  PerformSystemAction(self, argv[0], argv, msg);
  OUT(DoPrint);
}

static void
PerformCreate( self )
  struct bushv *self;
{
  register int f = 0;
  char *response = NULL;

  IN(PerformCreate);
  if(!CurrNode) return;
  switch(Object) {
    case nodes_object:
      if(bushv_Query(self,"Directory Name: ","",&response)) return;
      sprintf(cmd,"%s/%s",DirPath(CurrNode),response);
      if(mkdir(cmd,0777)) {
        IssueError(self,"Creating",response,TRUE);
	return;
      }
      else PerformRescan(self);
      break;
    case entries_object:
      if(bushv_Query(self,"File Name: ","",&response)) return;
      sprintf(cmd,"%s/%s",DirPath(CurrNode),response);
      if((f = open(cmd,O_CREAT | O_EXCL, 0100 | 0200 | 0400)) < 0) {
        IssueError(self,"Creating",response,TRUE);
	return;
      }
      else {
        close(f);
	PerformRescan(self);
      }
      break;
    case entry_object:
      break;
  }
  OUT(PerformCreate);
}

static void
IssueError( self, what, where, overlay )
  register struct bushv *self;
  register char *what, *where;
  boolean overlay;
{
  int result = 0;
  static char *question[] = { "Continue", NULL };

  IN(IssueError);
  if(errno > 0 && errno <= sys_nerr) 
    sprintf(msg,"ERROR %s '%s': %s", what, where, sys_errlist[errno] );
  else if(errno != 0)
    sprintf(msg,"ERROR %s '%s': (Invalid System Error-code '%d')", what, where, errno );
  else
    sprintf(msg,"ERROR %s '%s'", what, where);
  if(overlay) message_MultipleChoiceQuestion(self,100,msg,0,&result,question,NULL);
  else Announce(msg);
  errno = 0;
  OUT(IssueError);
}

static int
DoDestroy( self, tn, Entry, overlay )
  register struct bushv *self;
  register tree_type_node tn;
  register struct Dir_Entry *Entry;
  boolean overlay;
{
  register int status = 0;

  IN(DoDestroy);
  if(self && tn && Entry) {
    if(status = bush_DestroyEntry(Bush, tn, Entry)) 
      IssueError(self, "Destroying", Entry->name, overlay);
  }
  OUT(DoDestroy);
  return(status);
}

static void
PerformDestroy( self )
  struct bushv *self;
{
    static char *question[] = {"Confirm","Cancel",0};
    register int i = 0;
    long result = 0;
    int count = 0;
    register struct Dir_Entry *Dir_Entry = NULL, *current_entry = CurrEntry;
    register tree_type_node current_node = CurrNode, tn = NULL;
    register struct suite_item **selected = NULL;

    IN(PerformDestroy);
    if(!current_node) return;
    switch(self->object) {
	case nodes_object:
	    if(tn = Parent(current_node)) {
		for(i = 0; i < DirEntriesCount(tn); i++)
		    if(DirEntryType(tn,i).dir  &&
		       strcmp(DirName(current_node), DirEntryName(tn, i)) == 0) {
			Dir_Entry = DirEntry(tn, i);
			break;
		    }
		if(Dir_Entry) {
		    sprintf(msg, "Confirm destroying '%s'",
			    DirName(current_node));
		    if(message_MultipleChoiceQuestion(self, 50, msg, 1, &result, question, NULL) == -1) 
			return;
		    else if((result == 0) && DirEntries(current_node) && 
			    (DirEntriesCount(current_node) > 0)){
			sprintf(msg, "'%s' has contents. Destroy Anyway?",
				DirName(current_node));
			if(message_MultipleChoiceQuestion(self, 100, msg, 1, &result, question, NULL) == -1)
			    return;
			if(result != 0) {
			    Announce("Cancelled");
			    return;
			}
		    }
		    if(result == 0) {
			PostCursor(self, Cursor_Wait);
			if(DoDestroy(self,Parent(current_node), Dir_Entry, TRUE)) {
			    bushv_RetractCursor(self,Cursor);
			    return;
			}
			CurrNode = Parent(current_node);
			treev_HighlightNode(DirTreeView, CurrNode);
			suite_ChangeSuiteAttribute(ControlView, suite_TitleCaption(DirPath(CurrNode)));
			bushv_RetractCursor(self, Cursor);
			PerformRescan(self);
			sprintf(msg, "Destroyed Node '%s'", Dir_Entry->name);
			Announce(msg);
		    }
		}
		else Announce("Node Not Found in Parent");
	    }
	    else Announce("May Not Destroy Root Directory");
	    break;
	case entries_object:
	    selected = suite_SelectedItems(EntriesView, &count);
	    if(count == 0) break;
	    if(count > 1)
		if(count == 2) sprintf(msg, "Destroy Both Items ?");
		else sprintf(msg, "Destroy All %d Items ?", count);
	    else {
		Dir_Entry = (struct Dir_Entry*)
		  suite_ItemAttribute(EntriesView, selected[0], suite_ItemDatum(0));
		sprintf(msg, "Destroy %s '%s' ?", EntryDirType(Dir_Entry)
			? "Node" : "", Dir_Entry->name);
	    }
	    if(message_MultipleChoiceQuestion(self, 100, msg, 1, &result, question, NULL) == -1)
		return;
	    if(result == 0) {
		PostCursor(self, Cursor_Wait);
		for( i = 0 ; i < count ; i++ ) {
		    Dir_Entry = (struct Dir_Entry*) 
		      suite_ItemAttribute(EntriesView, selected[i], suite_ItemDatum(0));
		    if(!DoDestroy(self, current_node, Dir_Entry, TRUE)) {
			suite_PassivateItem(EntriesView, selected[i]);
			EntrySelected(Dir_Entry) = FALSE;
		    }
		}
		bushv_RetractCursor(self, Cursor);
	    }
	    else Announce("Canceled");
	    break;
	case entry_object:
	    if(current_entry) {
		if(EntryDirType(current_entry))
		    sprintf(msg, "Destroy Directory '%s'?", current_entry->name);
		else
		    sprintf(msg, "Destroy '%s'?", current_entry->name);
		if(message_MultipleChoiceQuestion(self, 100, msg, 1, &result, question, NULL) == -1) return;
		if(result == 0) {
		    PostCursor(self, Cursor_Wait);
		    if(!DoDestroy(self, current_node, current_entry, TRUE)) {
			EntrySelected(current_entry) = FALSE;
			CurrEntry = NULL;
			PerformPop(self);
			suite_ChangeSuiteAttribute(ControlView,
						   suite_TitleCaption(DirPath(CurrNode)));
			suite_PassivateItem(EntriesView, suite_CurrentItem(EntriesView));
		    }
		    bushv_RetractCursor(self, Cursor);
		}
	    }
	    else Announce("Canceled");
	    break;
    }
    OUT(PerformDestroy);
}

static void
PerformExec( self )
  struct bushv *self;
{
  register int i = 0;

  IN(PerformExec);
  PostCursor(self,Cursor_Wait);
  switch(Object) {
    case nodes_object: break;
    case entries_object:
      if(CurrNode && DirEntries(CurrNode) && 
	  (DirEntriesCount(CurrNode) >= 0))
        for(i = 0;i < DirEntriesCount(CurrNode);i++)
	  if(DirEntryMode(CurrNode,i).selected &&
	     !DirEntryType(CurrNode,i).dir)
	      DoExecute(self,CurrNode,DirEntry(CurrNode,i));
      break;
    case entry_object:
      DoExecute(self,CurrNode,CurrEntry);
      break;
  }
  bushv_RetractCursor(self,Cursor);
  OUT(PerformExec);
}

static
DoExecute( self, tn, Entry )
  struct bushv		*self;
  tree_type_node	 tn; 
  struct Dir_Entry	*Entry; 
{
  char			 full_path[MAXPATHLEN * 2];

  IN(DoExecute);
  sprintf(full_path,"%s/%s",DirPath(tn),Entry->name);
  sprintf(msg,"Executing '%s'",full_path);
  argv[0] = full_path;
  argv[1] = NULL;
  PerformSystemAction(self,full_path,argv,msg);
  OUT(DoExecute);
}

static int
PerformSystemAction( self, name, argv, msg )
  struct bushv	*self;
  char		*name;
  char		*argv[];
  char		*msg;
{
  Announce(msg);
  return(bush_PerformSystemAction(Bush,name,argv));
}

static char*
Format_Tags( tag )
  u_short	    tag;
{
  static char	    tags[11];

  strcpy(tags,"----------");
  if((tag & S_IFMT) == S_IFREG) tags[0] = '-';
  else if((tag & S_IFMT) == S_IFDIR) tags[0] = 'd';
  else if((tag & S_IFMT) == S_IFCHR) tags[0] = 'c';
  else if((tag & S_IFMT) == S_IFBLK) tags[0] = 'b';
#ifdef S_IFLNK
  else if((tag & S_IFMT) == S_IFLNK) tags[0] = 'l';
#endif
#ifdef S_IFIFO
  else if((tag & S_IFMT) == S_IFIFO) tags[0] = '?';
#endif /* #ifdef S_IFIFO */
#ifdef S_IFSOCK
  else if((tag & S_IFMT) == S_IFSOCK) tags[0] = '?';
#endif /* #ifdef S_IFSOCK */
  if(tag & S_IREAD) tags[1] = 'r';
  if(tag & S_IWRITE) tags[2] = 'w';
  if(tag & S_IEXEC) tags[3] = 'x';
  if(tag & (S_IREAD >>3)) tags[4] = 'r';
  if(tag & (S_IWRITE>>3)) tags[5] = 'w';
  if(tag & (S_IEXEC >>3)) tags[6] = 'x';
  if(tag & (S_IREAD >>6)) tags[7] = 'r';
  if(tag & (S_IWRITE>>6)) tags[8] = 'w';
  if(tag & (S_IEXEC >>6)) tags[9] = 'x';
  tags[10] = '\0';
  return(tags);
}

static char*
FormatEntriesItem( self, tn, i, dirEntry )
  register struct bushv	*self;
  tree_type_node	 tn;
  int			 i;
  struct Dir_Entry	*dirEntry;
{
  static char		 entries_item[257], trailer[5];
  register char		*entries_ptr = NULL, *time_ptr = NULL;

  IN(FormatEntriesItem);
  if(tn) {
    if(!DirEntryType(tn,i).soft_link && DirEntryType(tn,i).dir) 
      strcpy(trailer,"/");
    else if(DirEntryType(tn,i).soft_link && Detail) 
	strcpy(trailer," -> ");
    else if(DirEntryPerms(tn,i) & S_IEXEC) strcpy(trailer,"*");
    else trailer[0] = '\0';
    if(Detail) {
      time_ptr = (char*) ctime(&DirEntryTimeStamp(tn,i));
      time_ptr[24] = '\0';
      sprintf(entries_item,"%s %2d %8s %8d %s %s%s%s",
	       Format_Tags(DirEntryPerms(tn,i)),
	       DirEntryNLinks(tn,i),DirEntryOwner(tn,i),
	       DirEntrySize(tn,i),time_ptr,DirEntryName(tn,i),
	       trailer,	DirEntryType(tn,i).soft_link ? DirEntryLinkName(tn,i) :	    "");
    }
    else sprintf(entries_item,"%s%s",DirEntryName(tn,i),trailer);
    entries_ptr = entries_item;
  }
  else if(dirEntry) {
    if(!EntryLinkType(dirEntry) && EntryDirType(dirEntry)) strcpy(trailer,"/");
    else if(EntryLinkType(dirEntry) && Detail) 
	strcpy(trailer," -> ");
    else if(EntryPerms(dirEntry) & S_IEXEC) strcpy(trailer,"*");
    else trailer[0] = '\0';
    if(Detail) {
      time_ptr = (char*) ctime(&dirEntry->time_stamp);
      time_ptr[24] = '\0';
      sprintf(entries_item,"%s %2d %8s %8d %s %s%s%s",
	       Format_Tags(dirEntry->permissions),
	       dirEntry->nlinks,dirEntry->owner,
	       dirEntry->size,time_ptr,dirEntry->name,
	       trailer, EntryLinkType(dirEntry) ? EntryLinkName(dirEntry) : "");
    }
    else sprintf(entries_item,"%s%s",dirEntry->name,trailer);
    entries_ptr = entries_item;
  }
  OUT(FormatEntriesItem);
  return(entries_ptr);
}

static void
ResetEntriesCaptions( self )
  struct bushv *self;
{
  register int i = 0, count = 0;
  struct suite_item *item = NULL;

  IN(ResetEntriesCaptions);
  suite_Reset(EntriesView, suite_ClearItems);
  if(DirEntries(CurrNode) && 
      (count = DirEntriesCount(CurrNode)) >= 0)
    for(i = 0 ; i < count ; i++) 
	if(DirEntryMode(CurrNode, i).destroyed == FALSE &&
	    (item = suite_CreateItem(EntriesView, DirEntryName(CurrNode,i), DirEntry(CurrNode,i))))
	    suite_SetItemAttribute(EntriesView, item,
				   suite_ItemCaption(FormatEntriesItem(self, CurrNode, i, NULL)));
  OUT(ResetEntriesCaptions);
}

static int
HandleModifiedObject( self )
  register struct bushv	    *self;
{
  int			     result = 0, return_value = 0;
  static char		    *answers[] = { "Save to file.",
                                           "Save As...",
					   "Don't save",
                                           "Cancel",
                                            NULL };

  IN(HandleModifiedObject);
  sprintf(msg, "File has been modified.  Save?");
  if(message_MultipleChoiceQuestion(self, 100, msg, 0, &result, answers, NULL) == -1)
    return_value = -1;
  else 
    switch(result) { 
      case(0):
	return_value = bushv_SaveFile(self);	    
	break;
      case(1):
	return_value = bushv_WriteFile(self);  
	break;
      case(2):
        EntryObjectModified = EntryObjectLastCKP = 0;
	break;
      case(3):
      default:		    
	return_value = -1;	    
	break;
    }
  OUT(HandleModifiedObject)
  return(return_value);
}

static void
PerformExit( self )
  struct bushv	*self;
{
  IN(PerformExit);
  if(Object == entry_object)
    if(dataobject_GetModified(EntryObject) > EntryObjectModified)
      if(HandleModifiedObject(self) < 0)
        return;
  exit(0);
  OUT(PerformExit);
}

static void
SwitchDirectory( self )
  struct bushv	    *self;
{
  static char	    *question[] = {"Continue",NULL};
  int		     msg_status = 0;
  long		     result = 0;
  char		    *response = NULL;
  struct stat	     stats;

  IN(SwitchDirectory);
  msg_status = bushv_QueryDirectoryName(self,"Switch To Directory: ",&response);
  if(msg_status) {
    return;
  }
  PostCursor(self,Cursor_Wait);
  if(stat(response,&stats)) {
    IssueError(self,"ReScanning",response,TRUE);
    bushv_RetractCursor(self,Cursor);
    return;
  }
  if((stats.st_mode & S_IFMT) != S_IFDIR) {
    sprintf(msg,"Must Change to a Directory");
    message_MultipleChoiceQuestion(self,100,msg,0,&result,question,NULL);
    sprintf(msg,"Failed to change to '%s'",response);
    Announce(msg);
    bushv_RetractCursor(self,Cursor);
    return;
  }
  if(Object != nodes_object) {
    PopToNodes(self);
    PostCursor(self,Cursor_Wait);
  }
  SetTreeNotificationData(self,TreeRoot,tree_NodeDestroyed);
  NotifyTreeObservers(self);
  tree_DestroyNode(Tree,TreeRoot);
  bush_InitTree(Bush,response);
  CurrNode = InitNode = TreeRoot;
  bush_BuildSubDirs(Bush,CurrNode);
  SetTreeNotificationData(self,CurrNode,tree_NodeCreated);
  NotifyTreeObservers(self);
  suite_ChangeSuiteAttribute(ControlView,
	suite_TitleCaption(DirPath(CurrNode)));
  if(TopLevelInset) 
    im_SetTitle(bushv_GetIM(self),DirName(CurrNode));
  bushv_RetractCursor(self,Cursor);
  HandleChangeDir(self,RootPathName);
  OUT(SwitchDirectory);
}

static void
SetEditor( self )
  struct bushv	*self;
{
  long		 result = 0;
  char		*response = NULL;

  IN(SetEditor);
  sprintf(msg,"Set editor to: ");
  if(message_MultipleChoiceQuestion(self,100,msg,EditorIndex,
	&result,EditorChoices,NULL) == -1) return;
  if(!strcmp(EditorChoices[result],"other")) {
    if(bushv_Query(self,"Set editor to: ","",&response)) return;
    AllocNameSpace(EditorChoices[NumEditorChoices-1],
	       &EditorChoices[NumEditorChoices]);
    AllocNameSpace(response,&EditorChoices[NumEditorChoices-1]);
    EditorChoices[++NumEditorChoices] = NULL;
  }
  EditorIndex = result;
  strcpy(EditorProgram,EditorChoices[result]);
  if(EditorProgram && *EditorProgram != '\0') {
    struct suite_item	*editorItem = NULL;
    char		 editorCaption[64];

    if(editorItem = suite_ItemOfDatum(ControlView,&editor_data)) {
      sprintf(editorCaption,"Editor: %s",EditorProgram);
      suite_ChangeItemAttribute(ControlView, editorItem, suite_ItemCaption(editorCaption));
    }
  }
  sprintf(msg,"Editor set to: '%s'",EditorProgram);
  Announce(msg);
  OUT(SetEditor);
}

void
bushv__PostKeyState( self, kstate )
  struct bushv		*self;
  struct keystate	*kstate;
{
  IN(bushv_KeyState);
  keystate_AddBefore(Kstate,kstate);
  super_PostKeyState(self,Kstate);
  OUT(bushv_KeyState);
}

char *
FormatEntriesInfo( self, tn )
  struct bushv		*self;
  tree_type_node	 tn;
{
  static char		 entries_info[257];
  register long		 i = 0, total_bytes = 0, count = 0;

  IN(FormatEntriesInfo);
  if(tn) {
    if(DirEntries(tn)) count = DirEntriesCount(tn);
      for( i = 0; i < count; i++ ) total_bytes += DirEntrySize(tn,i);
        sprintf(entries_info,"%d %s    %d %s    %s %s",count,
		 "Entries",total_bytes,"Bytes","Sorted by", sorts[SortMode]);
  }
  OUT(FormatEntriesInfo);
  return(entries_info);
}

static int
(*DetermineSortHandler( self, tn ))()
  struct bushv		*self;
  tree_type_node	 tn;
{
  int		       (*sorter)(), sMode = SortMode;

  IN(DetermineSortHandler);
  switch(sMode) {
    case by_name:    sorter = SortByName;    break;
    case by_size:    sorter = SortBySize;    break;
    case by_date:    sorter = SortByDate;    break;
    case by_suffix:  sorter = SortBySuffix;  break;
    case by_type:    sorter = SortByType;    break;
  }
  OUT(DetermineSortHandler);
  return(sorter);
}

static void
DoAutoRescan( self )
  register struct bushv		 *self;
{
  register struct suite_item	**selected = NULL, *item = NULL;
  register char			**names = NULL;
  int				  count = 0;
  register int			  i;

  IN(DoAutoRescan);
  if(environ_GetProfileSwitch("AutoRescan", FALSE)) {
      if(ScanRequired(CurrNode)) {
	  if(Object == entries_object) {
	      selected = suite_SelectedItems(EntriesView, &count);
	      names = (char**)calloc(count, sizeof(char*));
	      for(i = 0; i < count; i++)
		  AllocNameSpace( suite_ItemAttribute(EntriesView, selected[i], suite_ItemCaption(0)), &names[i] );
	  }
	  PerformRescan(self);
	  if(Object == entries_object)
	      for(i = 0; i < count; i++) {
		  if(item = suite_ItemOfName(EntriesView, names[i]))
		      suite_HighlightItem(EntriesView, item);
		  free(names[i]);
		  names[i] = NULL;
	      }
	  if(names) {
	      free(names);
	      names = NULL;
	  }
	  if(selected) {
	      free(selected);
	      selected = NULL;
	  }
      }
  }
  IN(DoAutoRescan);
}

static void
PushToEntries( self )
  struct bushv	      *self;
{
  register int       (*sorter)();

  IN(PushToEntries);
  if(EntriesView) {
    sorter = DetermineSortHandler(self,CurrNode);
    suite_SetSuiteAttribute(EntriesView,suite_SortHandler(sorter));
    Object = entries_object;
    ResetEntriesCaptions(self);
    suite_SetSuiteAttribute(EntriesView,
	    suite_TitleCaption(FormatEntriesInfo(self,CurrNode)));
    lpair_SetNth(LP,1,(struct view*)EntriesView);
    DoAutoRescan(self);
    bushv_WantInputFocus(self,EntriesView);
  }
  OUT(PushToEntries);
}

static void
PushToEntry( self )
  struct bushv *self;
{
  char file_name[MAXPATHLEN];
  char *objectName = NULL;
  long objectID = 0;
  struct attributes *attrs;

  IN(PushToEntry);
  PostCursor(self, Cursor_Wait);
  Object = entry_object;
  sprintf(file_name, "%s/%s", DirPath(CurrNode), CurrEntry->name);
  sprintf(msg, "reading '%s'", file_name);
  Announce(msg);
  if(EntryFilep = fopen(file_name, "r")) {
    if(!(objectName = filetype_Lookup(EntryFilep, file_name,
				       &objectID, &attrs)))
      objectName = "text";
    if(EntryView) {
      if(view_IsAncestor(EntryViewAppLayer, self))
        lpair_SetNth(LP, 1, NULL);
      view_DeleteApplicationLayer(EntryView, EntryViewAppLayer);
      view_Destroy(EntryView);
      EntryView = EntryViewAppLayer = NULL;
      dataobject_Destroy(EntryObject);
      EntryObject = NULL;
    }
    if(!(EntryObject = (struct dataobject *) class_NewObject(objectName)) || 
       !(EntryView = (struct view *)
	  class_NewObject(dataobject_ViewName(EntryObject)))) {
      IssueError(self, "Allocating Object", objectName, TRUE);
      Object = entries_object;
    }
    else { /*success*/
      struct attributes readWriteAttr;

      dataobject_Read(EntryObject, EntryFilep, objectID);
      view_SetDataObject(EntryView, EntryObject);
      readWriteAttr.next = attrs;
      readWriteAttr.key = "readonly";
      if((access(file_name, W_OK) == -1) && (errno == EACCES)) {
	readWriteAttr.value.integer = TRUE; /* Read Only */
	menulist_SetMask(Menulist,0);
      }
     else {
	readWriteAttr.value.integer = FALSE; /* Read Write */
	menulist_SetMask(Menulist, bushv_RWEntryMenus);
	if(CkpInterval != 0)
	  im_EnqueueEvent(Checkpoint, (long) self, event_SECtoTU(CkpInterval));
      }
      dataobject_SetAttributes(EntryObject, &readWriteAttr);
      lpair_SetNth(LP, 1,
	EntryViewAppLayer = view_GetApplicationLayer(EntryView));
      bushv_WantInputFocus(self, EntryView);
      suite_ChangeSuiteAttribute(ControlView, suite_TitleCaption(file_name));
      EntryObjectModified = EntryObjectLastCKP = dataobject_GetModified(EntryObject);
    }
  }
  else {
    IssueError(self, "Opening Entry", CurrEntry->name,TRUE);
    Object = entries_object;
  }
  ClearMessageLine();
  bushv_RetractCursor(self, Cursor);
  OUT(PushToEntry);
}

static int
PopToNodes( self )
  struct bushv	*self;
{
  int		 status = 0;

  IN(PopToNodes);
  if(DirTreeView) {
    Object = nodes_object;
    CurrEntry = NULL;
    suite_ChangeSuiteAttribute(ControlView,
	suite_TitleCaption(DirPath(CurrNode)));
    lpair_SetNth(LP,1,(struct view*)DirTreeView);
    DoAutoRescan(self);
    NumPrevSelected = 0;
    bushv_WantInputFocus(self,DirTreeView); 
  }
  OUT(PopToNodes);
  return(status);
}

static int
PopToEntries( self )
  struct bushv	*self;
{
  int		 status = 0;

  IN(PopToEntries);
  if(Object == entry_object)
    if(dataobject_GetModified(EntryObject) > EntryObjectModified)
      if((status = HandleModifiedObject(self)) < 0)
        return(status);
  if(EntryFilep) {
    fclose(EntryFilep);
    EntryFilep = NULL;
  }
  if(EntriesView) {
    Object = entries_object;
    lpair_SetNth(LP,1,(struct view*)EntriesView);
    suite_ChangeSuiteAttribute(ControlView,
	suite_TitleCaption(DirPath(CurrNode)));
    DoAutoRescan(self);
    bushv_WantInputFocus(self,EntriesView);
  }
  CurrEntry = NULL;
  OUT(PopToEntries);
  return(status);
}

static long
Passivator( self, suite, item, datum )
/*============================================================
I set the item_data attribute, active, of the ControlView items to be the OR'ed sum of the Object-codes for which the particular item is active. If a button is active during both the nodes_object(1) and entries_object(2) then it's data field is set to (nodes_object + entries_Object) = 1 + 2 = 3. This is how I implemented automatic button "Passivation" in bush.  It has been proposed that this feature be incorporated into suite (3/29/89).
=============================================================*/
  struct bushv	    *self;
  struct suite	    *suite;
  struct suite_item *item;
  unsigned	     datum;
{
  struct item_data  *itemData = NULL; 
  long int	     result = 0;

  if(suite && item) {
    itemData = (struct item_data*) suite_ItemAttribute(suite, item, suite_ItemData(0));
    if(itemData && (itemData->activate & Object))
      suite_ActivateItem(suite,item);
    else suite_PassivateItem(suite,item);
  }
  return(result);
}

static void
PassivateControls( self )
  struct bushv    *self;
{
  IN(PassivateControls);
  suite_Apply(ControlView,Passivator,self,NULL);
  suite_Reset(ControlView,suite_Normalize);
  OUT(PassivateControls);
}

static void
Push( self )
  register struct bushv	    *self;
{
  register tree_type_node    tn = NULL;
  char			    *name = NULL;

  IN(Push);
  switch(Object) {
    case nodes_object:
      PushToEntries(self);
      break;
    case entries_object:
      if(EntryDirType(CurrEntry)) {
	AllocNameSpace(CurrEntry->name,&name);
	PostCursor(self,Cursor_Wait);
	Pop(self);
	tn = Child(CurrNode);
	while(tn) 
	  if(!strcmp(DirName(tn),name)) 
	    break;
	  else 
            tn = Right(tn);
	if(tn) {
	  TreeHitHandler(self,DirTreeView,tn,
		      treev_NodeObject,view_LeftUp,0,0,0);
	  treev_HighlightNode(DirTreeView,tn);
	}
	else {
	  sprintf(msg,"Couldn't locate directory node '%s'.",CurrEntry->name);
	  Announce(msg);
	}
	bushv_RetractCursor(self,Cursor);
      }
      else PushToEntry(self);
      break;
  }
  if(name) free(name);
  PassivateControls(self);
  ClearMessageLine();
  OUT(Push);
}

static void
Pop( self )
  struct bushv    *self;
{
  int		   status = 0;

  IN(Pop);
  switch(Object) {
    case entries_object:
      status = PopToNodes(self);    break;
    case entry_object:
      status = PopToEntries(self);  break;
  }
  if(status == 0) {
    PassivateControls(self);
    ClearMessageLine();
  }
  OUT(Pop);
}

static void
PerformPop( self )
  struct bushv	*self;
{
  IN(PerformPop);
  PostCursor(self,Cursor_Wait);
  Pop(self);
  bushv_RetractCursor(self,Cursor);
  OUT(PerformPop);
}

static long
ToggleCaptionDetail( self, suite, item, datum )
  struct bushv		*self;
  struct suite		*suite;
  struct suite_item	*item;
  unsigned		 datum;
{
  long int		 result = 0;

  IN(ToggleCaptionDetail);
  if(suite && item )
    suite_SetItemAttribute(suite,item,
	suite_ItemCaption(FormatEntriesItem(self,NULL,0,(struct Dir_Entry*)
		suite_ItemAttribute(suite,item,suite_ItemDatum(0)))));
  OUT(ToggleCaptionDetail);
  return(result);
}

static void
SortDir( self, tn )
  struct bushv *self;
  tree_type_node tn;
{
  register int (*sorter)();

    IN(SortDir);
    sorter = DetermineSortHandler(self, tn);
    if(Object == entries_object)
	suite_Sort(EntriesView, 0, sorter);
    suite_ChangeSuiteAttribute(EntriesView, suite_TitleCaption(FormatEntriesInfo(self, CurrNode)));
    OUT(SortDir);
}


static void
PerformDetail( self )
  struct bushv *self;
{
    IN(PerformDetail);
    PostCursor(self, Cursor_Wait);
    Detail = !Detail;
    if(Object == nodes_object) {
	sprintf(msg, "Global detail '%s'", Detail ? "On" : "Off" );
	Announce(msg);
    }
    else if(Object == entries_object) {
	suite_Apply(EntriesView, ToggleCaptionDetail, self, 0);
	bushv_WantUpdate(self, EntriesView);
    }
    UpdateDetailCaption(self);
    bushv_RetractCursor(self, Cursor);
    OUT(PerformDetail);
}

void
bushv__SetDataObject( self, bush )
  struct bushv *self;
  struct bush *bush;
{
    IN(bushv_SetDataObject);
    Bush = bush;
    treev_SetDataObject(DirTreeView,Tree);
    super_SetDataObject(self,bush);
    CurrNode = InitNode = TreeRoot;
    HandleChangeDir(self,RootPathName);
    OUT(bushv_SetDataObject);
}

static int
SortRequested( self, tn )
  struct bushv *self;
  tree_type_node tn;
{
  int sort = -1, result = 0, current_mode = SortMode;

    IN(SortRequested);
    if(message_MultipleChoiceQuestion(self, 100, "Sort By: ", current_mode, &result, sorts, NULL) != -1)
	switch(result) {
	    case 0:  sort = by_name;   break;
	    case 1:  sort = by_size;   break;
	    case 2:  sort = by_date;   break;
	    case 3:  sort = by_suffix; break;
	    case 4:  sort = by_type;   break;
	    default: sort = -1;
	}
    OUT(SortRequested);
    return(sort);
}

static void
PerformSort( self )
  struct bushv *self;
{
  int (*sorter)(), sMode = 0;
  struct suite_item *sortItem = NULL;
  char sortCaption[16];

    IN(PerformSort);
    if((sMode = SortRequested(self, CurrNode)) != -1) {
	strcpy(sortCaption, "Sort: ");
	sprintf(msg, "Sorting by '%s' ...", sorts[SortMode = sMode]);
	Announce(msg);
	strcat(sortCaption, sorts[SortMode]);
	if(Object == nodes_object) {
	    sorter = DetermineSortHandler(self, CurrNode);
	    suite_SetSuiteAttribute(EntriesView, suite_SortHandler(sorter));
	}
	else if(Object == entries_object) {
	    PostCursor(self, Cursor_Wait);
	    SortDir(self, CurrNode);
	    bushv_RetractCursor(self, Cursor);
	}
	if(sortItem = suite_ItemOfDatum(ControlView, &sort_data))
	    suite_ChangeItemAttribute(ControlView, sortItem, suite_ItemCaption(sortCaption));
    }
    ClearMessageLine();
    OUT(PerformSort);
}

static void
PerformRescan( self )
  struct bushv *self;
{
  struct stat stats;

    IN(PerformRescan);
    if(!CurrNode)
	return;
    PostCursor(self, Cursor_Wait);
    if(stat(DirPath(CurrNode), &stats)) {
	DirMode(CurrNode).stat_failed = TRUE;
	IssueError(self, "ReScanning", DirPath(CurrNode), TRUE);
	bushv_RetractCursor(self, Cursor);
	return;
    }
    if(Object == nodes_object || Object == entries_object) {
	char firstVisibleName[MAXPATHLEN];
	int f;
	struct suite_item *first = suite_FirstVisible(EntriesView);

	sprintf(msg, "Scanning Directory '%s' ...", DirPath(CurrNode));
	Announce(msg);

	*firstVisibleName = (char)0;
	if(first) {
	    f = suite_ItemAttribute(EntriesView, first, suite_ItemPosition(0));
	    while(first && suite_ItemActivated(EntriesView, first) == FALSE)
		first = suite_ItemAtPosition(EntriesView, ++f);
	    if(first)
		strcpy(firstVisibleName, (char *) suite_ItemAttribute(EntriesView, first, suite_ItemName(0)));
	}
	if(Child(CurrNode)) {
	    SetTreeNotificationData(self, CurrNode, tree_NodeChildrenDestroyed);
	    NotifyTreeObservers(self);
	}
	bush_ScanDir(Bush, CurrNode);
	bush_BuildSubDirs(Bush, CurrNode);
	if(Child(CurrNode)) {
	    SetTreeNotificationData(self, CurrNode, tree_NodeChildrenCreated);
	    NotifyTreeObservers(self);
	}
	if(Object == entries_object) {
	    ResetEntriesCaptions(self); /* This call clears the current set and rebuilds the new set;  After this call, FirstVisible == NULL; set it to the desired first visible item here */
	    if(*firstVisibleName && (first = suite_ItemOfName(EntriesView, firstVisibleName)) != NULL)
		suite_SetFirstVisible(EntriesView, first);
	    SortDir(self, CurrNode);
	}

	sprintf(msg, "Finished scanning '%s'", DirPath(CurrNode));
	Announce(msg);
    }
    else if(Object == entry_object)
	PushToEntry(self);
    bushv_RetractCursor(self, Cursor);
    OUT(PerformRescan);
}

static int
ResetChildDirPaths( self, tree, tn, datum )
  struct bushv      *self;
  struct tree	    *tree;
  tree_type_node     tn;
  long		     datum;
{
  long int	     status = 0;
  char		     tmp_path[MAXPATHLEN];

  IN(ResetChildDirPaths);
  if(tn && tree_NodeLevel(Tree,tn) > tree_NodeLevel(Tree,CurrNode)) {
    sprintf(tmp_path,"%s/%s",(char*)datum,DirName(tn));
    AllocNameSpace(tmp_path,&DirPath(tn));
  }
  else if(tn && (CurrNode == tn)) status = 0;
  else status = -1;
  OUT(ResetChildDirPaths);
  return(status);
}


static void
PerformRename( self )
  struct bushv *self;
{
  int msg_status = 0, count = 0;
  char *response = NULL;
  char tmp_path[MAXPATHLEN],*tmp = NULL;
  register struct suite_item **selected = NULL;
  struct Dir_Entry *dirEntry = NULL;
  register int i = 0;

  IN(PerformRename);
  if(!CurrNode) return;
  switch(Object) {
    case nodes_object:
      msg_status = bushv_Query(self,"New Name: ",
				DirName(CurrNode),&response);
      if(msg_status || !strcmp(response,DirName(CurrNode))) return;
      sprintf(msg,"Directory '%s' renamed to '",DirName(CurrNode));
      strcpy(tmp_path,DirPath(CurrNode));
      if(tmp = rindex(tmp_path,'/')) *tmp = '\0';
      if(!bush_RenameDir(Bush,CurrNode,tmp_path,response)) {
        tree_SetNodeName(Tree,CurrNode,DirName(CurrNode));
	SetTreeNotificationData(self,CurrNode,tree_NodeNameChanged);
	NotifyTreeObservers(self);
	strcat(msg,DirName(CurrNode));
	strcat(msg,"'");
	Announce(msg);
	suite_ChangeSuiteAttribute(ControlView,
				   suite_TitleCaption(DirPath(CurrNode)));
	tree_Apply(Tree,CurrNode,ResetChildDirPaths,
		   self,strcpy(tmp_path,DirPath(CurrNode)));
      }
      else IssueError(self,"Renaming",DirName(CurrNode),TRUE);
      break;
    case entries_object:
      selected = suite_SelectedItems(EntriesView,&count);		
	for( i = 0 ; (i < count) && selected[i] ; i++ ) {
	  suite_PassivateItem(EntriesView,selected[i]);
	  dirEntry = (struct Dir_Entry *)
	    suite_ItemAttribute(EntriesView,selected[i],suite_ItemDatum(0));
	  sprintf(msg,"Renaming '%s' to '",dirEntry->name);
	  msg_status = bushv_Query(self,"New Name: ",
				   dirEntry->name,&response);
	  if(msg_status || !strcmp(response,dirEntry->name)) {
	    suite_ActivateItem(EntriesView,selected[i]);
	    suite_HighlightItem(EntriesView,selected[i]);
	    continue;
	  }
	  if(bush_MoveEntry(Bush,CurrNode,dirEntry,response) != -1) {
	    strcat(msg,response); strcat(msg,"'");
	    Announce(msg);
	    suite_ActivateItem(EntriesView,selected[i]);
	    suite_HighlightItem(EntriesView,selected[i]);
	    suite_ChangeItemAttribute(EntriesView,selected[i],
		suite_ItemCaption(FormatEntriesItem(self,NULL,0,dirEntry)));
	  }
	  else {
	    IssueError(self,"Renaming",dirEntry->name,TRUE);
	    suite_ActivateItem(EntriesView,selected[i]);
	    suite_HighlightItem(EntriesView,selected[i]);
	  }
	}   
	break;
    case entry_object:
      msg_status = bushv_Query(self,"New Name: ",CurrEntry->name,&response);
      if(msg_status || !strcmp(response,CurrEntry->name)) return;
      sprintf(msg,"'%s' renamed to '",CurrEntry->name);
      if(bush_MoveEntry(Bush,CurrNode,CurrEntry,response) != -1) {
	strcat(msg,response); strcat(msg,"'");
	Announce(msg);
	sprintf(msg,"%s/%s",DirPath(CurrNode),CurrEntry->name);
	suite_ChangeSuiteAttribute(ControlView,suite_TitleCaption(msg));
      }
      else IssueError(self,"Renaming",CurrEntry->name,TRUE);
      break;
  }	
  OUT(PerformRename);
}

static void
ToggleDebug( self )
  struct bushv    *self;
{
  IN(ToggleDebug);
  Debug = !Debug;
  suite_SetDebug(EntriesView,TRUE);
  OUT(ToggleDebug);
}

struct view *
bushv__GetApplicationLayer( self )
  struct bushv    *self;
{
  IN(bushv_GetApplicationLayer);
  TopLevelInset = FALSE;
  OUT(bushv_GetApplicationLayer);
  return((struct view *)self);
}

void
bushv__ReceiveInputFocus( self )
  struct bushv    *self;
{
  IN(bushv_ReceiveInputFocus);
  super_ReceiveInputFocus(self);
  OUT(bushv_ReceiveInputFocus);
}

static void
HandleChangeDir( self, dirName )
  register struct bushv   *self;
  register char		  *dirName;
{
  IN(HandleChangeDir);
  if(dirName && (*dirName != '\0'))
    im_ChangeDirectory(dirName);
  OUT(HandleChangeDir);
}

static int 
bushv_WriteToFile( self, filename )
  struct bushv  *self;
  char		*filename;
{
  char		 realName[MAXPATHLEN],tempFilename[MAXPATHLEN];
  char		*originalFilename = NULL, *endString, *basename;
  int		 closeCode, errorCode, originalMode, fd, counter = 1;
  FILE		*outFile;
  struct stat	 statBuf;

  IN(bushv_WriteToFile);
  errorCode = 0;
  filetype_CanonicalizeFilename(realName,filename,sizeof(realName) - 1);
  filename = realName;
  if((access(filename,W_OK) < 0) && (errno == EACCES))
    return(-1);
  if(stat(filename,&statBuf) >= 0)
    originalMode = statBuf.st_mode & (~S_IFMT);
  else originalMode = 0666;
#ifndef USESHORTFILENAMES
  strcpy(tempFilename,filename);
  strcat(tempFilename,".NEW");
  endString = tempFilename + strlen(tempFilename);
  while(access(tempFilename,F_OK) >= 0) /* While the file exists. */
    sprintf(endString,".%d",counter++);
#else /* USESHORTFILENAMES */
  strcpy(tempFilename,filename);
  basename = rindex(tempFilename,'/');
  if(!basename) basename = tempFilename;
  else basename++;
  if(strlen(basename) > 8) basename[8] = '\0';
  strcat(tempFilename,".NEW");
  endString = tempFilename + strlen(tempFilename);
  while(access(tempFilename,F_OK) >= 0 && counter < 10)
    sprintf(endString,".%d", counter++);
  if(counter == 10) return(-1);
#endif /* USESHORTFILENAMES */
  originalFilename = filename;
  filename = tempFilename;
  if((fd = open(filename, O_WRONLY | O_TRUNC | O_CREAT,originalMode)) < 0
      || (outFile = fdopen(fd,"w")) == NULL)
    return(-1);
  dataobject_Write(EntryObject,outFile,im_GetWriteID(),0);
  fflush(outFile);
  if(ferror(outFile)) {
    fclose(outFile);
    errorCode = 0;
    closeCode = -1;
  }
  else {
#ifdef AFS_ENV
    if((closeCode = vclose(fileno(outFile))) < 0) /* stdio can trash errno. */
      errorCode = errno; /* Protect it from the fclose below. */
    else if(originalFilename != NULL)
      if((closeCode = rename(filename, originalFilename)) < 0)
        errorCode = errno;
#else /* AFS_ENV */
    if((closeCode = close(fileno(outFile))) < 0) /* stdio can trash errno. */
      errorCode = errno; /* Protect it from the fclose below. */
    else if(originalFilename != NULL)
      if((closeCode = rename(filename, originalFilename)) < 0)
        errorCode = errno;
#endif /* AFS_ENV */
    fclose(outFile); /* Free stdio resources. */
    if(closeCode >= 0) { /* Reset readonly mode. */
      struct attributes attributes;

      attributes.next = NULL;
      attributes.key = "readonly";
      if(access(filename,W_OK) == -1 && errno == EACCES)
        attributes.value.integer = TRUE;
      else attributes.value.integer = FALSE;
      dataobject_SetAttributes(EntryObject,&attributes);
      EntryObjectModified = dataobject_GetModified(EntryObject);
    }
  }
  sprintf(tempFilename,"%s.CKP",filename);
  if(access(tempFilename,F_OK) >= 0)
    unlink(tempFilename);
  errno = errorCode;
  OUT(bushv_WriteToFile);
  return(closeCode);
}

#define DIR_MSG \
   "Write aborted: specified output file is a directory."
    
static int
bushv_SaveFile( self )
  struct bushv	*self;
{
  int		 result = 0, return_value = 0;
  char		 message[sizeof("Wrote file ''.") + sizeof("Could not save file") + MAXPATHLEN];
  char		 fName[MAXPATHLEN];
  char		*filename;
  struct stat    statbuf;

  IN(bushv_SaveFile);
  sprintf(fName,"%s/%s",DirPath(CurrNode), CurrEntry->name);
  filename = fName;
  if(stat(filename, &statbuf) == 0 && (statbuf.st_mode & S_IFDIR)) {
    Announce(DIR_MSG);
    return(-1);
  }
  PostCursor(self,Cursor_Wait);
  result = bushv_WriteToFile(self,filename);
  if(result >= 0) {
    sprintf(message, "Wrote file '%.*s'", MAXPATHLEN, filename);
    Announce(message);
  }
  else {
    switch (errno) {
      case EACCES:
        Announce("Could not save file; permission denied.");
        break;
#ifdef ETIMEDOUT
      case ETIMEDOUT:
        Announce("Could not save file; a server is down.");
        break;
#endif /* ETIMEDOUT */
#ifdef EFAULT
      case EFAULT:
        Announce("Could not save file; a server is down.");
        break;
#endif /* EFAULT */
#ifdef EDQUOT
      case EDQUOT:
        Announce("Could not save file; you are over your quota.");
        break;
#endif /* EDQUOT */
       case ENOSPC:
        Announce("Could not save file; no space left on partition.");
        break;
#ifdef EIO
       case EIO:
        Announce("Could not save file; an I/O error occurred on the disk.");
        break;
#endif /* EIO */
       case EISDIR:
        Announce("File not found; could not create. Attempt to write to a directory.");
        break;
       default:
        sprintf(message, "Could not save file: %s.",sys_errlist[errno]);
        Announce(message);
    }
    return_value = -1;
  }
  bushv_RetractCursor(self,Cursor);
  OUT(bushv_SaveFile);
  return(return_value);
}

static int
bushv_WriteFile( self )
  struct bushv	*self;
{
  char		 filename[MAXPATHLEN];
  char		 message[sizeof("Wrote file ''.") + sizeof("Could not save file") + MAXPATHLEN];
  int		 result = 0, return_value = 0;

  IN(bushv_WriteFile);
  sprintf(filename,"%s/%s",DirPath(CurrNode),CurrEntry->name);
  if(completion_GetFilename(self, "Write to file: ", filename, filename,
			     sizeof(filename), FALSE, FALSE) == -1)
    return(-1);
  PostCursor(self,Cursor_Wait);
  result = bushv_WriteToFile(self,filename);
  if(result >= 0) {
    sprintf(message, "Wrote file '%.*s'", MAXPATHLEN, filename);
    Announce(message);
  }
  else {
    switch (errno) {
      case EACCES:
        Announce("Could not save file; permission denied.");
        break;
#ifdef ETIMEDOUT
      case ETIMEDOUT:
        Announce("Could not save file; a server is down.");
        break;
#endif /* ETIMEDOUT */
#ifdef EFAULT
      case EFAULT:
        Announce("Could not save file; a server is down.");
        break;
#endif /* EFAULT */
#ifdef EDQUOT
      case EDQUOT:
        Announce("Could not save file; you are over your quota.");
        break;
#endif /* EDQUOT */
      case ENOSPC:
        Announce("Could not save file; no space left on partition.");
        break;
#ifdef EIO
      case EIO:
        Announce("Could not save file; an I/O error occurred on the disk.");
        break;
#endif /* EIO */
      case EISDIR:
        Announce("File not found; could not create. Attempt to write to a directory.");
        break;
      default:
        sprintf(message, "Could not save file: %s.",sys_errlist[errno]);
        Announce(message);
    }
    return_value = -1;
  }
  bushv_RetractCursor(self,Cursor);
  OUT(bushv_WriteFile);
  return(return_value);
}

static int
bushv_SetPrinter(self)
  struct bushv	*self;
{
  char		*currentPrinter, *defaultPrinter, answer[256];
  char		 prompt[sizeof("Current printer is . Set printer to []: ") + 128];
  int		 return_value = 0;

  currentPrinter = environ_Get("LPDEST");
  if (currentPrinter == NULL)
  	currentPrinter = environ_Get("PRINTER");
  defaultPrinter = environ_GetProfile("print.printer");
  if (!defaultPrinter) defaultPrinter = environ_GetProfile("print.spooldir");
  if((currentPrinter != NULL) && (defaultPrinter != NULL))
    sprintf(prompt,"Current printer is %.64s. Set printer to [%.64s]: ",    currentPrinter,defaultPrinter);
  else if(defaultPrinter != NULL)
    sprintf(prompt,"Set printer to [%.64s]: ",defaultPrinter);
  else strcpy(prompt,"Set printer to: ");
  if(message_AskForString(self,0,prompt,NULL,answer,sizeof(answer)) == -1)
    return;
  if(*answer != '\0') {
    environ_Put("LPDEST",answer);
    environ_Put("PRINTER",answer);
    defaultPrinter = answer;
  }
  else {
	environ_Delete("LPDEST");
	environ_Delete("PRINTER");
  }
  if(defaultPrinter != NULL) {
    sprintf(prompt,"Printer set to %.64s.",defaultPrinter);
    Announce(prompt);
  }
  else {
    Announce("Printer not set.");
    return_value = -1;
  }
  return(return_value);
}

static void 
Checkpoint( dummyData )
  long dummyData;
{
  struct bushv	*self = (struct bushv*)dummyData;
  char		 CkpFileName[MAXPATHLEN];
  int		 closeCode;

  if(Object == entry_object) {
    if(dataobject_GetModified(EntryObject) > EntryObjectLastCKP) {
      PostCursor(self,Cursor_Wait);
      Announce("Checkpointing ...");
      im_ForceUpdate();
      sprintf(CkpFileName,"%s/%s.CKP",DirPath(CurrNode),CurrEntry->name);
      closeCode = bushv_WriteToFile(self,CkpFileName);
      Announce(closeCode ? "Checkpoint Failed." : "Checkpointed.");
      bushv_RetractCursor(self,Cursor);
      EntryObjectLastCKP = dataobject_GetModified(EntryObject);
    }
    im_EnqueueEvent(Checkpoint,(long)self,event_SECtoTU(CkpInterval));
  }
}

static void
UpdateDetailCaption( self )
  struct bushv		    *self;
{
  struct suite_item	    *detailItem = NULL;
  char			     newCaption[16];

  if(detailItem = suite_ItemOfDatum(ControlView, &detail_data)) {
    strcpy(newCaption, "Detail: ");
    if(Detail) strcat(newCaption, "on");
    else strcat(newCaption, "off");
    suite_ChangeItemAttribute(ControlView, detailItem, suite_ItemCaption(newCaption));
  }
}

static void
EntriesPageUp( self )
    struct bushv *self;
{
    if(Object == entries_object) {
	struct suite_item *first, *last, *newFirst;

	if((first = suite_FirstVisible(EntriesView)) != NULL && (last = suite_LastVisible(EntriesView)) != NULL && first != last) {
	    int f = suite_ItemAttribute(EntriesView, first, suite_ItemPosition(0));
	    int l = suite_ItemAttribute(EntriesView, last, suite_ItemPosition(0));
	    int total = suite_ItemCount(EntriesView);
	    int span = l - f, n;

	    n = (f - span > 0 ? f - span : 1);
	    if((newFirst = suite_ItemAtPosition(EntriesView, n)) != NULL && newFirst != first) {
		suite_SetFirstVisible(EntriesView, newFirst);
		bushv_WantUpdate(self, EntriesView);
	    }
	    else if(newFirst == first) Announce("Top of list");
	}
    }
}

static void
EntriesPageDown( self )
    struct bushv *self;
{
    if(Object == entries_object) {
	struct suite_item *first, *last, *newFirst;

	if((first = suite_FirstVisible(EntriesView)) != NULL && (last = suite_LastVisible(EntriesView)) != NULL && first != last) {
	    int f = suite_ItemAttribute(EntriesView, first, suite_ItemPosition(0));
	    int l = suite_ItemAttribute(EntriesView, last, suite_ItemPosition(0));
	    int total = suite_ItemCount(EntriesView);

	    if(l != total) {
		int span = l - f, n;

		if(l + span > total)
		    n = total - span;
		else
		    n = (f + span < total ? f + span : total - 1);
		if((newFirst = suite_ItemAtPosition(EntriesView, n)) != NULL && newFirst != first) {
		    suite_SetFirstVisible(EntriesView, newFirst);
		    bushv_WantUpdate(self, EntriesView);
		}
	    }
	    else Announce("Last page");
	}
    }
}

void
bushv__LinkTree(self, parent)
    struct bushv *self;
    struct view *parent;
{
    super_LinkTree(self, parent);
    if(parent && bushv_GetIM(self)) 
	lpair_LinkTree(LP, (struct view *) self);
}
