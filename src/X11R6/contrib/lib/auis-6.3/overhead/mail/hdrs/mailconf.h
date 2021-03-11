/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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

/*
	mailconf.h -- Mail system configuration parameters and data structures.
*/

#include <andyenv.h>
#include <svcconf.h>

#define MAILDAEMON_PORT	(2400)	    /* Port for queuemail daemon */

/* External entry points in mailconf.c */

extern char *LocalQueue;
extern char **lastDitchPrefixes;
extern int nlastditchprefixes;
extern char **bailoutPrefixes;
extern int nbailoutprefixes;
extern char *PostmasterMailbox;
extern char *vicemail, *switchmail, *trymail, *queuemail, *logpgm, *arpadatepgm, *nntpxmit;
extern char *DuplicateDB, *SentSuffix, *NNTPSuffix, *CaptSuffix;
extern char *MailboxName, *ForwardingName, *OutgoingName;
extern char *PostmasterName, *PostmasterTitle, *WPIUpdateReqAddr;
#ifdef CMU_ENV
extern char *IDChangeDate;
#endif /* CMU_ENV */

extern char **AMS_ValidDomainSuffixes, **AMS_ValidDomainRelays;
extern int numAMS_ValidDomainSuffixes, numAMS_ValidDomainRelays;
extern int AMS_ValidateDestHosts, AMS_HardHostValidationErrors, 
	AMS_DeliveryViaDomainMXAddress, AMS_DeliveryViaDomainAddress,
	AMS_DeliveryViaGethostbyname, AMS_DeliveryViaHostTable;

extern int AMS_WSRunsQueuemail;

/* A flag for testing whether the ``+'' hack works */
extern int AMS_UseridPlusWorks;

extern char *CellCommonMailQueueDirSuffix, *CellCommonPrintingDirSuffix,
	*CellCommonConfigDirSuffix, *CellConfigPostmaster, *CellConfigNameSeparator,
	*CellConfigMessageServer;
extern char *MailQueueNamePrefix, *SlowQueueNamePrefix;

/* Whether we're running on a limited system with 14 character file names */

extern int AMS_UseShortFileNames;

/* Whether to exercise strict locking on standalone mailbox files.  In the standalone system,
    strict locking is only possible if /usr/spool/mail is writable by EVERYONE */

extern int AMS_StrictStandaloneLocking;

/* Whether or not to interpret the pw_gecos field in the amazingly convoluted manner that has become tradional under BSD */

extern int AMS_GecosHacks;

/* An external mail collection hook command to run */

extern char *AMS_MailCollectionCommand;

/* Where the Master Update servers run (if anywhere) */
extern char **AMS_MasterUpdateHosts;
extern int numAMS_MasterUpdateHosts;

/* Configuration related to printer name validation */

extern char **AMS_PrintSpoolDirectories, **AMS_NamedValidPrinters;
extern int numAMS_PrintSpoolDirectories, numAMS_NamedValidPrinters;
extern int AMS_PrinterNamesInSpoolDirectories, AMS_PrinterNamesInPrintcap;

extern char *Organization, *NNTPhost, *NNTPuser, *DefaultSurfaceAddress;

extern int AMS_UUCPSupported;
extern int AMS_ViceQueueLifetime, AMS_ExtraViceQueueLifetime, AMS_ViceQueueLifetimeCap, AMS_CrossCellQueueLifetime;

/* Site configuration for message server.
*/

/* First come the templates for building an mspath */
/* In several cases, the "previous" value is also available, to help in major tree movements */
/* The following defines the mspath variable $local */

extern char *LOCALSEARCHPATHTEMPLATE, *OLDLOCALSEARCHPATHTEMPLATE;

/* The following defines the mspath variable $external */

extern char *EXTERNALSEARCHPATHTEMPLATE, *OLDEXTERNALSEARCHPATHTEMPLATE;

/* The following defines the mspath variable $official */

extern char *OFFICIALSEARCHPATHTEMPLATE, *OLDOFFICIALSEARCHPATHTEMPLATE;

/* The following defines the mspath variable $default, and also the mspath
    for people with no mspath preference.  Note that $mail cannot be redefined. */

extern char *DEFAULTSEARCHPATHTEMPLATE;

/* The following defines bboards everyone must subscribe to */

extern char *GlobalRequiredSubsFile;

/* The following defines bboards whose names have "recently" changed */

extern char *GlobalChangeSubsFile;

/* Automatic bug reports on messages bugs */

extern char *MessagesAutoBugAddress;

/* The following is used by IBM to inhibit the use of bboards on the system */

extern int AMS_OnlyMail;

/* The following specifies the location of the welcome mail given to new users */

extern char *WelcomeMailFile;

/* The following specifies the location of the ELI file loaded at startup. */

extern char *EliStartupFile;

/* The following is the initial value for the message server's DeathKnell value, expressed in seconds.  A message server will time out after this many seconds of inactivity. */
extern int AMS_InitialDeathKnell;

/* The following is the maximum value for MS_SetDeathKnell's time parameter. */
extern int AMS_MaximumDeathKnell;

#ifdef NOTDEF
/* Used to be a define, now configurable */
extern int postmaster_uid;
#endif /* NOTDEF */

/* Configuration for non-AMS delivery reading /usr/spool/mail/uid.
   That file has a grotesque format that varies at many sites; this can
   be configured somewhat using the variables below. */

extern char *AMS_MailBoxPrefix; /* Prefix for ``/userid'', giving file where mail is delivered. */

extern int AMS_DemandSeparatingCharacter; /* mail items separated by reserved char, e.g. ^A or ^C (yes/no value) */
extern int AMS_DeleteLinesWithSeparator; /* delete lines up to newline if they contain separating char */

extern int AMS_SeparatingCharacter; /* The aforementioned separator's ascii value */

/* If we are not demanding a separting character, we are parsing the messages
  (ugh!) looking for From lines that seem to start new messages!  How do
  we decide?  Well...  If both of the parameters below are turned off, 
  nearly any line starting with "From" will work, with a few obvious exceptions. */



extern int AMS_CheckAddressInSeparatingFrom; /* Demand a reasonable address after the from  (yes/no value) */


extern int AMS_CheckDateInSeparatingFrom; /* Demand a reasonable date after the address (yes/no value) */

extern int AMS_AllowColonInSeparatingFrom; /* Normally looks for "From ", but can configure to also look for "From:" in separating lines */

/* Location of old BSD sendmail program.  */

extern char *oldsendmail;

/* Where to link temporary lock files in old-style /usr/spool/mail/xxx locking.
  This needs to be configured because the user needs to be able to create files
  in the directory.  */

extern char *SpoolMailLockTemp;

/* Where to put .lock files for user foo. */
extern char *SpoolMailLockDir;

/* Whether to exercise strict locking on the above files.  In the standalone system,
    strict locking is only possible if /usr/spool/mail is writable by EVERYONE */

extern int AMS_StrictStandaloneLocking;

/* The default value of ANDREWDIR for the site -- used by console, but also by messageserver. */

extern char *DefaultAndrewDir;

/* Values to determine how local addresses are validated */
extern int AMS_WPValidation; /* Use white pages */
extern int AMS_PasswdValidation; /* Use passwd file */
extern int AMS_LocalDatabaseValidation; /* Use something else, defined locally in mailconf.c */
#ifdef USE_MMDF_ENV
extern int AMS_MMDFValidation;		/* Use MMDF validation */
#endif
extern int AMS_AliasesValidation;	/* Use alias file */
extern char *AMS_AliasFileName;		/* name of alias file */
extern int AMS_NameSeparator;	/* If non-zero, character to separate parts of names */

/* Value to override the enforcement of the reading of the external posting etiquette file */

extern int AMS_UsersAreGrownups;

/* Value to specify whether local users can understand formatted mail */

extern int AMS_UsersHandleFormatting;

/* Value to pacify people who call for your head if you write to /dev/console */

extern int AMS_DevConsoleIsSacred;


/* Value to not always append the local domain name to local mail */
 
extern int AMS_NoDomainPreferredOnLocalMail;
