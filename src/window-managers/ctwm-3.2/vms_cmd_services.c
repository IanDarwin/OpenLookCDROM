/*
 *	VMS_CMD_SERVICES.C
 *
 *	Author:		Patrick L. Mahan
 *	Location:	TGV, Inc
 *	Date:		01-Sep-1991
 *
 *	Purpose:	Provides utilities needed to create, send and
 *			receive messages to the process create by $CREPRC.
 *			This allows us to implement the user custom-
 *			izable root window normally associated with
 *			the most PD Window Managers.  This is needed
 *			because DECW$LOGINOUT.EXE starts the Window Manger
 *			without DCL.
 *
 *	Modification History
 *
 *	Date        | Who	| Version	| Reason
 *	------------+-----------+---------------+-------------------------------
 *	18-Mar-1991 | PLM	| 1.0		| First Write
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <starlet.h>
#include <lib$routines.h>
#include <descrip.h>
#include <dvidef.h>
#include <dcdef.h>
#include <prvdef.h>
#include <prcdef.h>
#include <psldef.h>
#include <jpidef.h>
#include <iodef.h>
#include <ssdef.h>
#include <accdef.h>
#include <msgdef.h>
#include <uaidef.h>
#include <decw$include/Xos.h>
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xatom.h>
#include <decw$include/Xresource.h>
#include "lnm.h"

#define MAXPROCESS	256
#define	DEFAULT_RUN_NAME	"X_"
#define TRM_NAME	"MBX_TRM"
#define STOP_CMD	"DUMMY:=="
#define STOP_CMD2	"STOP/ID=0"

#define	$INITDESCRIP(name)	struct dsc$descriptor_s name = \
	{ 0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0}

static char RunName[15];

typedef struct _vmsaccount {
	unsigned short int msgtype;	/* Type of message */
	unsigned short int unused0;	/* padding */
	unsigned long  int status;	/* final exit status */
	unsigned long  int pid;		/* Process ID */
	unsigned long  int timestamp[2];/* System time at process termination */
	unsigned char  account[8];	/* Account name */
	unsigned char  username[12];	/* User name */
	unsigned long  int cputime;	/* used cpu time in milliseconds */
	unsigned long  int pagefaults;	/* number of page faults incurred */
	unsigned long  int pgflpeak;	/* peak number of page faults */
	unsigned long  int wspeak;	/* peak working set size */
	unsigned long  int biocnt;	/* # of buffer I/O operations */
	unsigned long  int diocnt;	/* # of direct I/O operations */
	unsigned long  int volumes;	/* # of volumes mounted by process */
	unsigned long  int login[2];	/* time logged in */
	unsigned long  int owner;	/* Process ID of owner */
} VMSAccounting;

typedef struct _iosb {
	unsigned short int status;	/* I/O status */
	unsigned short int bytes;	/* number of bytes read/write */
	unsigned long  int pid;		/* Process ID of sender/receiver */
} MbxIOSB;

typedef struct _prcblk {
	struct _prcblk     *forward;	/* forward link */
	struct _prcblk     *backward;	/* backward link */
	unsigned long  int pid;		/* Process ID of the user command */
	unsigned short int chan;	/* mailbox channel for input */
	unsigned short int pad0;	/* padding to force longword boundary */
} PrcBLK;

static struct _trmmbxinfo {
	unsigned short int 	trmMbxChan;	/* termination mailbox channel number */
	unsigned short int 	trmMbxUnit;	/* termination mailbox unit number */
	MbxIOSB			trmIOSB;	/* termination mailbox I/O status block */
	struct dsc$descriptor_s trmMbxName;	/* termination mailbox logical name */
} TrmMbxInfo = { 0, 0, { 0, 0, 0 }, {sizeof(TRM_NAME), DSC$K_DTYPE_T, DSC$K_CLASS_S, TRM_NAME}};

typedef struct {
	unsigned short int length;
	unsigned short int code;
	unsigned long  int buffaddr;
	unsigned long  int retaddrlen;
} ItmLst3;

static PrcBLK *PrcHead = (PrcBLK *)NULL, *PrcCurrent = (PrcBLK *)NULL;

$DESCRIPTOR(BitBucket, "NLA0:");		/* null device */
$DESCRIPTOR(image, "SYS$SYSTEM:LOGINOUT.EXE");	/* image to be run by $CREPRC */

static struct dsc$descriptor_s wsaDevice; 	/* VMS string descriptor for the WorkStation Device */
static struct dsc$descriptor_s prcname;  	/* VMS string descriptor for run process name */
static struct dsc$descriptor_s asciitime;	/* VMS string descriptor for getting the returned time */
static unsigned char TrmMbxBuffer[ACC$K_TERMLEN];	/* Termination Mailbox's input buffer */
static unsigned char VMSAsciiTime[24];	/* VMS Ascii Time returned by $ASCTIM */
static unsigned char wsaName[20];	/* VMS null terminate name for WorkStation Device */
static unsigned short TrmMbxBufferLen = ACC$K_TERMLEN;	/* length of the termination's mailbox */
static unsigned short trmmbxchan = 0;	/* termination mailbox channel */

/*
 *	saveProcess - saves the pid and input mailbox channel of the created
 *	process for deallocation at process termination
 */

static void saveProcess (pid, chan)
unsigned long int  pid;
unsigned short int chan;
{
   PrcBLK *new;

   /* allocate the process slot storage */

   new = (PrcBLK *) calloc(1, sizeof(PrcBLK));

   /* initialize the internal process slot */

   new->forward = (PrcBLK *)NULL;
   new->backward = (PrcBLK *)NULL;
   new->pid = pid;
   new->chan = chan;
   new->pad0 = 0;

   /* insert it into the list */

   if (!(PrcHead))		/* first one */
   {
      PrcHead = new;
      PrcCurrent = new;
   }
   else			/* not the first one! */
   {
      PrcCurrent->forward = new;
      new->backward = PrcCurrent;
      PrcCurrent = new;
   }
}

/*
 *	TrmProcedure - AST routine called when a process is terminated
 */

void TrmProcedure ()
{
   int i, trmlen;
   unsigned long  int istatus;
   unsigned short int process_id;
   unsigned short int retlen;
   char logbuffer[256];
   char accountname[9];
   char username[13];
   VMSAccounting *accptr;
   PrcBLK *currptr;

   /* check the status of the IO */

   if (!(TrmMbxInfo.trmIOSB.status&1))
   {
      sys$exit(TrmMbxInfo.trmIOSB.status);
   }

   /* transfer the message */

   accptr = (VMSAccounting *)TrmMbxBuffer;

   if (accptr->msgtype != MSG$_DELPROC) goto ReQueueIt;

   /* loop through the processes and find matching pid */

   currptr = PrcHead;

   while (currptr)
   {
      if (currptr->pid == TrmMbxInfo.trmIOSB.pid)
      {
         sys$dassgn(currptr->chan);	/* deassign the channel to the Mailbox */
         if (currptr != PrcHead && currptr != PrcCurrent)
         {
            currptr->backward->forward = currptr->forward;
            currptr->forward->backward = currptr->backward;
         }
         else if (currptr == PrcHead && currptr == PrcCurrent)
         {
            PrcHead = (PrcBLK *)NULL;
            PrcCurrent = (PrcBLK *)NULL;
         }
         else if (currptr == PrcHead)
         {
            PrcHead = currptr->forward;
            if (PrcHead) PrcHead->backward = (PrcBLK *)NULL;
         }
         else
         {
            PrcCurrent = currptr->backward;
            PrcCurrent->forward = NULL;
         }

         free (currptr);

	 break;
      }

      currptr = currptr->forward;
   }

ReQueueIt:
   /* requeue the AST */

   istatus = sys$qio (0,			/* event flag (NONE)	*/
                      TrmMbxInfo.trmMbxChan,	/* channel 		*/
                      IO$_READVBLK,		/* function code	*/
                      &TrmMbxInfo.trmIOSB,	/* I/O Status Block	*/
                      TrmProcedure,		/* AST procedure	*/
                      0,			/* AST parameter (NONE)	*/
                      TrmMbxBuffer,		/* buffer		*/
                      TrmMbxBufferLen,		/* buffer length	*/
                      0, 0, 0, 0);		/* P3-P6 not used	*/

   if (!(istatus&1)) sys$exit (istatus);
}

/*
 *	initRun - intializes the internal datastructures required for creating
 *	user processes on the fly for handling the root menu background
 */

unsigned long int
initRun(ProcName)
char *ProcName;
{
   unsigned long int istatus;	/* vms returned error */
   unsigned long int i;
   unsigned long int retry_cnt;
   unsigned long int mbxunit;  /* mailbox unit number */
   static unsigned long int devclass;
   static unsigned long int retdevlen;
   unsigned long int ItemCode;
   char logname[15];
   char logbuffer[256];
   ItmLst3 ItemList[] = {
	{ 4, DVI$_DEVCLASS, (unsigned long int)&devclass, (unsigned long int)&retdevlen},
	{ 0, 0, 0, 0}
   };

   /* set the logical we want to look at */

   retry_cnt = 0;
   strcpy (logname, "SYS$OUTPUT");

   /* translate the logical SYS$OUTPUT, this will be the workstation device */
try_again:
   istatus = GetLogical (logname, wsaName);

   if (!(istatus&1)) return (istatus);

   wsaDevice.dsc$b_dtype	= DSC$K_DTYPE_T;
   wsaDevice.dsc$b_class	= DSC$K_CLASS_S;
   wsaDevice.dsc$w_length	= strlen((char *)wsaName);
   wsaDevice.dsc$a_pointer	= (char *)wsaName;

   /* make sure that the device is a workstation device */

   istatus = sys$getdviw (0,		/* event flag (NONE)		*/
                          0,		/* channel number (NONE)	*/
                          &wsaDevice,	/* device name 			*/
                          &ItemList,	/* item list			*/
                          0,		/* I/O status block (NONE)	*/
                          0,		/* AST routine (NONE)		*/
                          0,		/* AST parameter (NONE)		*/
                          0);		/* Reserved by DIGITAL		*/

   if (!(istatus&1)) return (istatus);

   if (devclass != DC$_WORKSTATION && retry_cnt++ == 0) 
   {
      strcpy (logname, "DECW$DISPLAY");
      goto try_again;
   }
   else if (devclass != DC$_WORKSTATION)
      return (SS$_IVDEVNAM);

   /* create the termination mailbox */

   istatus = sys$crembx (0,				/* create a temporary mailbox		*/
                         &TrmMbxInfo.trmMbxChan,	/* channel number for mailbox		*/
                         0,				/* maximum message size (DEFAULT)	*/
                         0,                     	/* buffer quota (BUFQUO) (DEFAULT)	*/
                         0,				/* protection mask (DEFAULT)		*/
                         0,				/* access mode (DEFAULT)		*/
			 &TrmMbxInfo.trmMbxName);	/* logical name for this mailbox	*/

   if (!(istatus&1)) return (istatus);

   /* set the item code */

   ItemCode = DVI$_UNIT;

   /* get the unit number of the termination mailbox */

   istatus = lib$getdvi(&ItemCode,		/* item code		*/
                        &TrmMbxInfo.trmMbxChan,	/* device channel	*/
                        0,			/* device name		*/
                        &mbxunit,		/* returned numeric val	*/
                        0,			/* return str descrip	*/
                        0);			/* len str return	*/

   if (!(istatus&1)) return (istatus);

   TrmMbxInfo.trmMbxUnit = (unsigned short int) mbxunit;	/* set the unit number */

   /* que a read AST on the termination mailbox */

   istatus = sys$qio (0,			/* event flag (NONE)	*/
                      TrmMbxInfo.trmMbxChan,	/* channel 		*/
                      IO$_READVBLK,		/* function code	*/
                      &TrmMbxInfo.trmIOSB,	/* I/O Status Block	*/
                      TrmProcedure,		/* AST procedure	*/
                      0,			/* AST parameter (NONE)	*/
                      TrmMbxBuffer,		/* buffer		*/
                      TrmMbxBufferLen,		/* buffer length	*/
                      0, 0, 0, 0);		/* P3-P6 not used	*/

   if (!(istatus&1)) return (istatus);

   /* the run name */

   if (ProcName != NULL && ProcName[0] != 0)
	sprintf(RunName, "%s_", ProcName);
   else
	strcpy(RunName, DEFAULT_RUN_NAME);

   /* return success */

   return (1);
}

/*
 *	createProcess - creates a process for the user's program to
 *	execute within.  This is via the LOGINOUT.EXE image which gives
 *	the process DCL.
 */

unsigned long int
createProcess (command)
char *command;
{
   unsigned short int inmbxchan;
   unsigned long int istatus;
   static unsigned long int i;
   unsigned long int pid;
   unsigned long int protection_mask = 0x0000FF00;
   static unsigned long int base_priority;
   static unsigned long int MbxUnit;
   static unsigned long int MbxNameLen = 0;
   static unsigned long int MbxUnitLen = 0;
   char *inMbxPtr;
   static char MbxName[64];
   char process_name[16];
   char logbuffer[256];
   $INITDESCRIP(InMbxDevice);
   MbxIOSB           iosb;
   ItmLst3 ItemList[] = {
	{ 64, DVI$_DEVNAM, (unsigned long int)MbxName, (unsigned long int)&MbxNameLen},
	{ 4,  DVI$_UNIT, (unsigned long int)&MbxUnit, (unsigned long int)&MbxUnitLen},
	{ 0, 0, 0, 0}
   };
   ItmLst3 JpiItemList[] = {
	{ 4, JPI$_PRIB, (unsigned long int)&base_priority, (unsigned long int)&i},
	{ 0, 0, 0, 0}
   };

   /* get the base priority of the process */

   istatus = sys$getjpiw (0,		/* event flag (NONE)	*/
                          0,		/* pid address		*/
                          0,		/* process name		*/
                          &JpiItemList,	/* item list		*/
                          0,		/* I/O Status Block (NONE)*/
                          0,		/* AST routine (NONE)	*/
                          0);		/* AST parameter (NONE)	*/

   if (!(istatus&1)) return (istatus);

   /* create the input mailbox */

   istatus = sys$crembx (0,				/* create a temporary mailbox */
                         &inmbxchan,			/* channel number assigned to this Mailbox */
                         1024,				/* maximum message size (DEFAULT) */
                         1024,				/* buffer quota (BUFQUO) (DEFAULT) */
                         protection_mask,		/* protection mask (DEFAULT) */
                         PSL$C_USER,			/* access mode (DEFAULT) */
                         0);				/* logical name for this Mailbox (NONE) */

   if (!(istatus & 1)) return (istatus);		/* return on error */

   /* get the device name and unit number of the MailBox */

   istatus = sys$getdviw (0,			/* event flag (NONE)		*/
                          inmbxchan,		/* channel number		*/
                          0,			/* device name (NONE)		*/
                          &ItemList,		/* what we want			*/
                          0,			/* I/O status block (NONE)	*/
                          0,			/* AST routine (NONE)		*/
                          0,			/* AST parameter (NONE) 	*/
                          0);			/* reserved by DIGITAL		*/

   if (!(istatus&1)) return (istatus);

   MbxName[MbxNameLen] = '\0';
   inMbxPtr = (char *) calloc(strlen(MbxName)+1, sizeof(char));
   strcpy (inMbxPtr, MbxName);
   InMbxDevice.dsc$w_length	= strlen(inMbxPtr);
   InMbxDevice.dsc$a_pointer	= inMbxPtr;

   /* que up the command to be read by the process */

#ifdef DEBUG
   fprintf (stdout, "Queueing user command (\"%s\") to input Mailbox\n", command);
#endif
   istatus = sys$qiow (0,					/* event flags (NONE)	*/
                       inmbxchan,				/* channel number	*/
                       IO$_WRITEVBLK|IO$M_NOW|IO$M_NORSWAIT,	/* function code	*/
                       &iosb,					/* I/O status block     */
                       0,					/* AST routine (NONE)	*/
                       0,					/* AST parameter (NONE)	*/
                       command,					/* buffer to be sent	*/
                       strlen(command),				/* length of buffer	*/
                       0,0,0,0);				/* P3-P6 not used	*/

   if (!(istatus&1)) return (istatus);

   if (!(iosb.status&1)) return (iosb.status);

   if(iosb.bytes != strlen(command)) return (SS$_MBFULL);

   /* que up a dummy command so that we know the process has ended */

#ifdef DEBUG
   fprintf (stdout, "Queueing dummy command (\"%s\") to input Mailbox\n", STOP_CMD);
#endif
   istatus = sys$qiow (0,					/* event flags (NONE)	*/
                       inmbxchan,				/* channel number	*/
                       IO$_WRITEVBLK|IO$M_NOW|IO$M_NORSWAIT,	/* function code	*/
                       &iosb,					/* I/O status block     */
                       0,					/* AST routine (NONE)	*/
                       0,					/* AST parameter (NONE)	*/
                       STOP_CMD,				/* buffer to be sent	*/
                       strlen(STOP_CMD),			/* length of buffer	*/
                       0,0,0,0);				/* P3-P6 not used	*/

   if (!(istatus&1)) return (istatus);

   if (!(iosb.status&1)) return (iosb.status);

   if(iosb.bytes != strlen(STOP_CMD)) return (SS$_MBFULL);

   /* que up the stop command to end the process */

#ifdef DEBUG
   fprintf (stdout, "Queueing stop command (\"%s\") to input Mailbox\n", STOP_CMD2);
#endif
   istatus = sys$qiow (0,					/* event flags (NONE)	*/
                       inmbxchan,				/* channel number	*/
                       IO$_WRITEVBLK|IO$M_NOW|IO$M_NORSWAIT,	/* function code	*/
                       &iosb,					/* I/O status block     */
                       0,					/* AST routine (NONE)	*/
                       0,					/* AST parameter (NONE)	*/
                       STOP_CMD2,				/* buffer to be sent	*/
                       strlen(STOP_CMD2),			/* length of buffer	*/
                       0,0,0,0);				/* P3-P6 not used	*/

   if (!(istatus&1)) return (istatus);

   if (!(iosb.status&1)) return (iosb.status);

   if(iosb.bytes != strlen(STOP_CMD2)) return (SS$_MBFULL);

   /* create the process name buffer */

   prcname.dsc$b_dtype		= DSC$K_DTYPE_T;
   prcname.dsc$b_class		= DSC$K_CLASS_S;
   prcname.dsc$a_pointer	= process_name;

   /* create the process and let it run */

   i = 1;
   do {
      sprintf (process_name, "%s%d", RunName, i);
      prcname.dsc$w_length	= strlen(process_name);

      istatus = sys$creprc (&pid,						/* created process ID		*/
                            &image,						/* image to run in process	*/
                            &InMbxDevice,					/* input			*/
                            &BitBucket,						/* output			*/
                            &wsaDevice,						/* error			*/
                            0,							/* privileges (DEFAULT)		*/
                            0,							/* quotas (DEFAULT)		*/
                            &prcname,						/* process name			*/
                            base_priority,					/* base priority		*/
                            0,							/* UIC				*/
                            TrmMbxInfo.trmMbxUnit,				/* termination mailbox		*/
                            (PRC$M_DETACH | PRC$M_INTER | PRC$M_NOPASSWORD));	/* process status flags		*/

   i++;	/* bump the count up */
   } while (istatus == SS$_DUPLNAM);

   if (!(istatus&1))
   {
      sys$dassgn(inmbxchan);
      return (istatus);
   }
   else
      saveProcess (pid, inmbxchan);

#ifdef DEBUG
   fprintf (stdout, "Created process \"%s\" identification 0x%x\n", process_name, pid);
#endif
   /* free allocated memory */

   free (inMbxPtr);

   return (SS$_NORMAL);
}
