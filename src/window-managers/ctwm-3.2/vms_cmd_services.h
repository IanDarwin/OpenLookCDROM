/*
 *	VMS_CMD_SERVICES.H
 *
 *	Author:		Patrick L. Mahan
 *	Location:	TGV, Incorporated
 *	Date:		01-Sep-1991
 *
 *	Purpose:	Defines the external references for the
 *			VMS Command Services.
 *
 *	Modification History
 *
 *	Date        | Who	| Version	| Reason
 *	------------+-----------+---------------+------------------------------
 *	18-Mar-1992 | PLM	| 1.0		| First Write
 *
 */

extern unsigned long int initRun ();		/* initializes the process, and the input/output Mailboxes */
extern unsigned long int createProcess ();	/* creates a user process to execute the command in */

