/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        Copyright Carnegie Mellon University 1992 - All Rights Reserved *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/mailconf.c,v 2.68 1994/05/19 16:24:15 rr2b Exp $";
#endif

/*
	mailconf.c -- Mail system configuration parameters and data structures.
*/

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <andyenv.h>
#include <stdio.h>
#include <sys/param.h>
#include <system.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include <util.h>
#include <parseadd.h>
#include <mail.h>
#include <pwd.h>

#include <mailconf.h>

extern int errno;

/* MAIL SYSTEM CONFIGURATION */
/* The variables in here are configured in several different ways.  In modifying this file, you must take note of the possibilities and the applicable rules.

There are five categories of information in this file's set of variables.  These categories are:
    (1) information that is never changed (such as the paths used for cross-cell configuration),
    (2) information that is workstation-specific (such as the name of a mail delivery queue on the local disk) and thus configured only by reading AndrewSetup (via GetConfiguration),
    (3) information that is specific to the user's ``AMS home'' domain (such as the name of the required-subscriptions file), that is configured by reading AndrewSetup and then optionally by calling CkAMSCellConfig(``AMSHomeDomainForTheUser'') (which reads and loads an AMS-Server file),
    (4) information that the AMS wishes to know about for any domain, not just the local ThisDomain or for the user's AMS home domain, and
    (5) information that the AMS wishes to know about for any domain, but for which this module maintains the knowledge for the user's AMS home domain.

In general, an AndrewSetup file describes attributes of the domain that it names as ThisDomain, and also describes attributes of the workstation on which it resides.  The AMS-Server file for a given domain ``mydomain'' (found as the file ``/afs/mydomain/service/configuration/AMS-Server'' and read by the CkAMSCellConfig procedure) describes attributes of the domain mydomain.

Information in category (1) is not configurable; it is loaded into variables in this module and cannot be changed.

Information in category (2) pertains only to the local workstation or to the domain named as ThisDomain in AndrewSetup, and is configurable via GetConfiguration, which searches a path for an AndrewSetup file and reads it.  The flags field for the descriptors for such information is ``f_none'' so that CkAMSCellConfig does not overwrite the local copy of this information.  Thus, this module's copy of that information is believed as the truth about the domain named in the ThisDomain field, and as truth about the current workstation.

Information in category (3) is domain-dependent but is of interest for only the user's AMS home domain.  It is configured both via GetConfiguration (i.e. via AndrewSetup) and via CkAMSCellConfig.  The flags field for the descriptors for such information is ``f_cell'' so that CkAMSCellConfig will overwrite the local copy of this information with information as it pertains to the domain given as argument to the CkAMSCellConfig procedure.  Thus, CkAMSCellConfig should only be called with the name of the ``AMS home'' as a parameter, since it will overwrite state exported by this module for variables in category (3).  Thus, this module's copy of that state is to be interpreted as applying to the AMS home domain, which in general can differ from the domain named as ThisDomain.

Information in category (4) is domain-dependent and is potentially of interest for any domain.  The configuration processes in this module cannot capture these dependencies; instead, procedures in the ``ckamsdel.c'' module cache and deliver this information as it applies to a named domain.  This module's copy of the information is current only for the domain named as ThisDomain, a fact exploited by the procedures in ckamsdel.c to accelerate their functioning.  The truth about this information resides in the AMS-Server file for a given domain.  The flags field for the descriptors for this information is ``f_none'' so that the CkAMSCellConfig procedure will *not* overwrite the state exported by this module; instead, the information loaded by GetConfiguration from the AndrewSetup file will be preserved, so that it is always true that the state of category-(4) information is the truth about the domain ThisDomain.

Information in category (5) is domain-dependent and is potentially of interest for any domain.  Even though this module cannot capture all these dependencies, the copy of the information in this module will always be the information as it applies to the user's ``AMS home'' domain.  Thus, the flags field for the descriptors for such information is ``f_cell' so that CkAMSCellConfig will overwrite the local copy.  This renders the local copy of the information useless for purposes of procedures that read the information from another cell's AMS-Server file, because the local copy in general is not the one that applies to the domain named in ThisDomain, and the CkAMSCellConfig procedure does not at present record the name of the domain for which it is successfully able to read the AMS-Server file.
*/

/* All strings that use the %a or %l conventions must be enumerated in the ConfigStrings list. */

/* %%%% CATEGORY (1): INFORMATION THAT NEVER CHANGES */

/* Cellular pathnames.  The conventional prefix and suffixes for the cell name to arrive at its service directory and that directory's contents. */
char *CellCommonMailQueueDirSuffix = "mailqs";	/* /afs/CELLNAME/service/mailqs is the directory that holds the mail delivery queues */
char *CellCommonPrintingDirSuffix = "printing";	/* /afs/CELLNAME/service/printing is the directory for the print queues */
char *CellCommonConfigDirSuffix = "configuration";	/* /afs/CELLNAME/service/configuration 
is the dir for further configuration information */
char *CellConfigPostmaster = "Postmaster";	/* File with Postmaster address in cell */
char *CellConfigNameSeparator = "name-separator";	/* substitute for spaces in names */
char *CellConfigMessageServer = "AMS-Server";	/* config file for MS in another cell */
char *MailQueueNamePrefix = "q";	/* This starts standard queue directories under mailqs */
char *SlowQueueNamePrefix = "sq";	/* This starts background queue directories under mailqs */

/* %%%% CATEGORY (2): PERTAINING ONLY TO THE WORKSTATION OR TO ThisDomain */

/* Master Update servers */
static char *DefaultAMS_MasterUpdateHosts[] = {
#ifdef CMU_ENV
    "128.2.10.202",		/* bb2.andrew.cmu.edu */
#endif /* CMU_ENV */
    NULL
};
char **AMS_MasterUpdateHosts = DefaultAMS_MasterUpdateHosts;
int numAMS_MasterUpdateHosts = (sizeof(DefaultAMS_MasterUpdateHosts) / sizeof(DefaultAMS_MasterUpdateHosts[0])) - 1;

static char *defLastDitchPrefixes[] = {
#ifdef CMU_ENV
    	"/afs/andrew.cmu.edu/usr0/postman/errors",
#endif /* CMU_ENV */
	"/usr/spool/MailErrors"
};
char **lastDitchPrefixes = defLastDitchPrefixes;
int nlastditchprefixes = (sizeof(defLastDitchPrefixes) / sizeof(defLastDitchPrefixes[0]));

static char *defBailoutPrefixes[] = {
#ifdef CMU_ENV
	"/afs/andrew.cmu.edu/usr0/postman/errors",
#endif /* CMU_ENV */
	"/usr/spool/MailErrors",
	"/tmp",
	"/usr/tmp"
    };
char **bailoutPrefixes = defBailoutPrefixes;
int nbailoutprefixes = (sizeof(defBailoutPrefixes) / sizeof (defBailoutPrefixes[0]));

char *LocalQueue = "/usr/spool/ViceMsgQueue";

/* Location of various mail programs */
#if defined(CMU_ENV) && defined(WHY_USR_AMS)
char *vicemail		= "/usr/ams/vicemail",
     *switchmail	= "/usr/ams/switchmail",
     *trymail		= "/usr/ams/trymail",
     *queuemail		= "/usr/ams/queuemail",
     *logpgm		= "%l/bin/log",
     *arpadatepgm	= "%l/etc/arpadate",
     *nntpxmit		= "%l/etc/nntpxmit";
#else /* CMU_ENV */
char *vicemail		= "%a/etc/vicemail",
     *switchmail	= "%a/etc/switchmail",
     *trymail		= "%a/etc/trymail",
     *queuemail	= "/etc/queuemail",
     *logpgm		= "%a/bin/log",
     *arpadatepgm		= "%a/etc/arpadate",
     *nntpxmit = "%a/etc/nntpxmit";
#endif /* CMU_ENV */
#ifdef RUN_AMDS_ENV
char *oldsendmail = "/etc/oldsendmail";
#else /* RUN_AMDS_ENV */
/* paranoia check for bsdi since I don't know if BSD386 defines __386BSD__ as well */
/* Any NetBSD machine has the below, FreeBSD is a guess */
#if defined(__386BSD__) && !defined(bsdi) || defined(__NetBSD__) || defined(__FreeBSD__)
char *oldsendmail = "/usr/sbin/sendmail";
#else
char *oldsendmail = "/usr/lib/sendmail";
#endif /* defined(__386BSD__) */
#endif /* RUN_AMDS_ENV */

/* Location of duplicate-delivery database */
char *DuplicateDB = "/usr/spool/log";
char *SentSuffix = "SENT";
char *NNTPSuffix = "NNTP";
char *CaptSuffix = "Captured";

/* Whether we're running on a limited system with 14 character file names */

#ifdef M_UNIX
int AMS_UseShortFileNames = 1;
#else
int AMS_UseShortFileNames = 0;
#endif

/* Value to not always append the local domain name to local mail */
 
int AMS_NoDomainPreferredOnLocalMail = 0;

/* Value to pacify people who call for your head if you write to /dev/console */

int AMS_DevConsoleIsSacred = 0;

/* A run-time Boolean to tell us if the workstation is running a queuemail daemon, even if the workstation cell as a whole isn't running the AMS delivery system.  This variable isn't even looked at unless the workstation cell does *not* run AMS delivery. */
#ifdef RUN_AMDS_ENV
int AMS_WSRunsQueuemail = 1;
#else /* RUN_AMDS_ENV */
int AMS_WSRunsQueuemail = 0;
#endif /* RUN_AMDS_ENV */

/* Configuration for non-AMS delivery reading /usr/spool/mail/uid.
   That file has a grotesque format that varies at many sites; this can
   be configured somewhat using the variables below. */

#if ((SY_U5x != 0) || defined(hpux))	/* some kind of System V */
char *AMS_MailBoxPrefix = "/usr/mail";	/* Prefix for ``/userid'', giving file where mail is delivered. */
#else /* ((SY_U5x != 0) || defined(hpux))	 */
/* yes, below it really is __386BSD__ || bsdi, they both want /var/mail in place of /usr/spool/mail. */
#if defined(__386BSD__) || defined(bsdi) || defined(__NetBSD__) || defined(__FreeBSD__)
char *AMS_MailBoxPrefix = "/var/mail"; /* Prefix for ``/userid'', giving file where mail is delivered. */
#else
char *AMS_MailBoxPrefix = "/usr/spool/mail"; /* Prefix for ``/userid'', giving file where mail is delivered. */
#endif /* defined(__386BSD__) */
#endif /* ((SY_U5x != 0) || defined(hpux))	 */

#ifdef USE_MMDF_ENV
int AMS_DemandSeparatingCharacter = 1; /* mail items separated by reserved char, e.g. ^A or ^C */

int AMS_SeparatingCharacter = 1; /* The aforementioned separator's ascii value */
int AMS_DeleteLinesWithSeparator = 1; /* delete from separator to newline */
#else
int AMS_DemandSeparatingCharacter = 0; /* mail items separated by reserved char, e.g. ^A or ^C */

int AMS_SeparatingCharacter = 3; /* The aforementioned separator's ascii value */
int AMS_DeleteLinesWithSeparator = 0; /* delete from separator to newline */
#endif

/* If we are not demanding a separting character, we are parsing the messages
  (ugh!) looking for From lines that seem to start new messages!  How do
  we decide?  Well...  If both of the parameters below are turned off, 
  nearly any line starting with "From" will work, with a few obvious exceptions. */

int AMS_CheckAddressInSeparatingFrom = 1; /* Demand a reasonable address after the from */
int AMS_CheckDateInSeparatingFrom = 1; /* Demand a reasonable date after the address */
int AMS_AllowColonInSeparatingFrom = 0; /* Normally looks for "From ", but can configure to also look for "From:" in separating lines */

/* Where to link temporary lock files in old-style /usr/spool/mail/xxx locking.
  This needs to be configured because the user needs to be able to create files
  in the directory.  */

char *SpoolMailLockTemp = "/usr/tmp/AMS_XXXXXX";

/* Where the .lock file for user foo is created.  Might also reasonably be in /tmp.  The mail file for user foo is ${AMS_MailBoxPrefix}/foo, and that file is locked not only by flock but also by creating the file ${SpoolMailLockDir}/foo.lock .  ${SpoolMailLockTemp} should be on the same partition as the .lock file. */

#if SY_U53 || SY_U54
char *SpoolMailLockDir = "/usr/spool/locks";
#else /* SY_U53 */
char *SpoolMailLockDir = "/usr/spool/mail";
#endif /* SY_U53 */

/* Whether to exercise strict locking on the above files.  In the standalone system,
    strict locking is only possible if /usr/spool/mail is writable by EVERYONE */

int AMS_StrictStandaloneLocking = 0;

/* Whether or not to interpret the pw_gecos field in the amazingly convoluted manner that has become tradional under BSD */

#ifdef RUN_AMDS_ENV
int AMS_GecosHacks = 0;
#else /* RUN_AMDS_ENV */
int AMS_GecosHacks = 1;
#endif /* RUN_AMDS_ENV */

/* An external mail collection hook command to run */

char *AMS_MailCollectionCommand = NULL;

/* The following is the initial value for the message server's DeathKnell value, expressed in seconds.  A message server will time out after this many seconds of inactivity. */
int AMS_InitialDeathKnell = 30*60;   /* 30 minutes */

/* The following is the maximum limit for MS_SetDeathKnell's time parameter, expressed in seconds.  A value of zero means not to limit it. */
int AMS_MaximumDeathKnell = 1*60*60;   /* one hour */

/* Stuff for validation of printer names.  Currently assigned to category (2) (ThisDomain), since no cross-cell printing architecture is in place. */
/* Whether to have the messageserver validate printer names by looking
  for subdirectories of one or more spool directory */

int AMS_PrinterNamesInSpoolDirectories = 1;

/* The aforementioned set of spool directories */

static char *DefaultAMS_PrintSpoolDirectories[] = {
#ifdef CMU_ENV
    "/afs/andrew.cmu.edu/service/printing/spool",
#endif /* CMU_ENV */
    NULL
};
char **AMS_PrintSpoolDirectories = DefaultAMS_PrintSpoolDirectories;
int numAMS_PrintSpoolDirectories = (sizeof(DefaultAMS_PrintSpoolDirectories) / sizeof(DefaultAMS_PrintSpoolDirectories[0])) - 1;

/* Whether to have the messageserver validate printer names by looking
  in /etc/printcap */

int AMS_PrinterNamesInPrintcap = 1;

/* Printers explicitly deemed acceptable by the setup file */

static char *DefaultAMS_NamedValidPrinters[] = {
    NULL
};
char **AMS_NamedValidPrinters = DefaultAMS_NamedValidPrinters;
int numAMS_NamedValidPrinters = (sizeof(DefaultAMS_NamedValidPrinters) / sizeof(DefaultAMS_NamedValidPrinters[0])) - 1;

/* %%%% CATEGORY (3): DOMAIN-DEPENDENT, PERTAINING ONLY TO THE AMS-HOME DOMAIN */
/* As the introduction explains, this copy of this information is to be interpreted as applying to the AMS home domain, which in general can differ from the domain named as ThisDomain. */

/* Location of postmaster's mailbox */
#ifdef CMU_ENV
char *PostmasterMailbox = "/afs/andrew.cmu.edu/usr0/postman/Mailbox";
#else /* CMU_ENV */
char *PostmasterMailbox = "/usr/postman/Mailbox";
#endif /* CMU_ENV */

#ifdef CMU_ENV
char *IDChangeDate = "1 June 1987";
#endif /* CMU_ENV */

/* Netnews/Usenet stuff */

#ifdef CMU_ENV
char *Organization = "Carnegie Mellon, Pittsburgh, PA"; /* For netnews posts */
char *NNTPhost = "bb3.andrew.cmu.edu";	/* Host with NNTP service */
char *NNTPuser = "postman+";		/* Default poster */
char *DefaultSurfaceAddress = "Campus Mail, Carnegie Mellon, Pittsburgh, PA 15213";	/* Where paper mail could be sent */
#else /* CMU_ENV */
char *Organization = "Andrew Message System"; /* For netnews posts */
char *NNTPhost = "news.yoursite.edu";	/* Host with NNTP service */
char *NNTPuser = "postmaster";		/* Default poster */
char *DefaultSurfaceAddress = "Anytown, Anywhere";	/* Where paper mail could be sent */
#endif /* CMU_ENV */

/* Value to override the enforcement of the reading of the external posting etiquette file */

int AMS_UsersAreGrownups = 1;

/* Value to specify whether local users can understand formatted mail */

#ifdef RUN_AMDS_ENV
int AMS_UsersHandleFormatting = 1;
#else /* RUN_AMDS_ENV */
int AMS_UsersHandleFormatting = 0;
#endif /* RUN_AMDS_ENV */

/* Integers giving the length of time (in seconds) that a message will be kept in the AFS queues before being rejected. */
/* AMS_ViceQueueLifetime gives how long we'll retry a message waiting for the user to get his or her act together (e.g., to get under quota).  After this time, we'll reject the message back to the sender for such problems. */
int AMS_ViceQueueLifetime = 4 * 24 * 60 * 60;	/* Default is four days */

/* AMS_ExtraViceQueueLifetime gives the additional time we'll hang on to a message after AMS_ViceQueueLifetime has expired, before unqueueing it (on *any* temporary failure) and sending it to the postmaster.  This is thus the extra time that we'll wait for system problems to be cleared up. */
int AMS_ExtraViceQueueLifetime = 3 * 24 * 60 * 60;	/* Default is three days */

/* AMS_ViceQueueLifetimeCap is the maximum believable age of a message.  Messages older than this will not be considered to be expired; instead, the local clock will be considered suspect. */
int AMS_ViceQueueLifetimeCap = 2 * (4 + 3) * 24 * 60 * 60;	/* Default is 14 days */

/* AMS_CrossCellQueueLifetime gives how long we'll retry a message, testing whether a destination domain is an AMDS cell.  After this time, we'll send the mail via other channels (oldsendmail) anyway. */
int AMS_CrossCellQueueLifetime = 8 * 60 * 60;	/* Default is eight hours */

/* The following defines bboards everyone must subscribe to */

char *GlobalRequiredSubsFile = "%l/lib/RequiredSubscriptions";

/* The following defines bboards whose names have "recently" changed */
#ifdef CMU_ENV
char *GlobalChangeSubsFile = "/afs/andrew.cmu.edu/usr0/postman/ChangedSubscriptions";
#else /* CMU_ENV */
char *GlobalChangeSubsFile = "/usr/postman/ChangedSubscriptions";
#endif /* CMU_ENV */

/* Automatic bug reports on messages bugs */

#ifdef CMU_ENV
char *MessagesAutoBugAddress = "postman+auto-messages-bugs@andrew.cmu.edu";
#else /* CMU_ENV */
char *MessagesAutoBugAddress = "postmaster";
#endif /* CMU_ENV */

/* The following is used by IBM to inhibit the use of bboards on the system */

#ifdef RUN_AMDS_ENV
int AMS_OnlyMail = 0;
#else /* RUN_AMDS_ENV */
int AMS_OnlyMail = 1;
#endif /* RUN_AMDS_ENV */

/* The following specifies the location of the welcome mail given to new users */

char *WelcomeMailFile = "%l/lib/Hello";

/* The following specifies the location of the ELI file loaded at startup. */

char *EliStartupFile = "%l/lib/eli/ms_startup.eli";

/* Name of mail-forwarding information file in user's home directory */
char *ForwardingName = ".forward";

/* Name of outgoing directory in user's home directory */
char *OutgoingName = ".Outgoing";

/* A special user to the queuemail and vicemail programs */
char *PostmasterTitle = "Postmaster";

/* Stuff for destination domain name validation that is dynamically configurable. */
/* Currently assigned in category (3) (only for AMS home), though it might not always be appropriate. */

/* A run-time Boolean to tell us if we do any host name validation. */
int AMS_ValidateDestHosts = 1;

/* A run-time Boolean to tell us if any host name validation can produce a hard error. */
int AMS_HardHostValidationErrors = 1;

/* A run-time Boolean to tell us if we deliver mail via both MX and domain addresses and we get them from the domain system. */
#ifdef RESOLVER_ENV
int AMS_DeliveryViaDomainMXAddress = 1;
#else /* RESOLVER_ENV */
int AMS_DeliveryViaDomainMXAddress = 0;
#endif /* RESOLVER_ENV */

/* A run-time Boolean to tell us if we deliver mail only via IP addresses and we get them from the domain system. */
#ifdef RESOLVER_ENV
int AMS_DeliveryViaDomainAddress = 1;
#else /* RESOLVER_ENV */
int AMS_DeliveryViaDomainAddress = 0;
#endif /* RESOLVER_ENV */

/* A run-time Boolean to tell us if we deliver mail only via IP addresses and we get them from this system's gethostbyname(3) call. */
int AMS_DeliveryViaGethostbyname = 0;

/* A run-time Boolean to tell us if we deliver mail only via IP addresses and we get them from the host table. */
#ifdef CMU_ENV
int AMS_DeliveryViaHostTable = 0;
#else
int AMS_DeliveryViaHostTable = 1;
#endif

/* Suffixes for hosts; hosts with these suffixes are always considered valid. */
static char *DefaultAMS_ValidDomainSuffixes[] = {
#ifdef CMU_ENV
	".bitnet",
#endif /* CMU_ENV */
	NULL
};
char **AMS_ValidDomainSuffixes = DefaultAMS_ValidDomainSuffixes;
int numAMS_ValidDomainSuffixes = (sizeof(DefaultAMS_ValidDomainSuffixes) / sizeof(DefaultAMS_ValidDomainSuffixes[0])) - 1;
/* Relays for mail destined for hosts with the above suffixes: a parallel array */
static char *DefaultAMS_ValidDomainRelays[] = {
#ifdef CMU_ENV
	"bitnet.cc.cmu.edu",
#endif /* CMU_ENV */
	NULL
};
char **AMS_ValidDomainRelays = DefaultAMS_ValidDomainRelays;
int numAMS_ValidDomainRelays = (sizeof(DefaultAMS_ValidDomainRelays) / sizeof(DefaultAMS_ValidDomainRelays[0])) - 1;

/* %%%% CATEGORY (4): DOMAIN-DEPENDENT, KNOWABLE FOR MULTIPLE DOMAINS */
/* As the introduction explains, the local copy of this information always applies only to ThisDomain.  Thus, clients in general need to use procedures such as those in ckamsdel.c to obtain this information. */

/* Name of mailbox directory in user's home directory */
char *MailboxName = "Mailbox";	/* Read this via CheckAMSMBName(domain) */

/* Distinguished delivery username--trusted to preserve authentication */

char *PostmasterName = "postman";	/* Read this via CheckAMSPMName(domain) */

/* Values to determine how local addresses are validated */

#ifdef RUN_AMDS_ENV
int AMS_WPValidation = 1;   	/* Read these via CheckAMSValidationMask(domain) */
int AMS_PasswdValidation = 0;
int AMS_AliasesValidation = 0;
#else /* RUN_AMDS_ENV */
int AMS_WPValidation = 0;
int AMS_PasswdValidation = 1;
int AMS_AliasesValidation = 1;
#endif /* RUN_AMDS_ENV */
int AMS_LocalDatabaseValidation = 0;
#ifdef USE_MMDF_ENV
int AMS_MMDFValidation = 1;
#endif

#ifdef M_UNIX
char *AMS_AliasFileName = "/usr/lib/mail/aliases";
#else
char *AMS_AliasFileName = "/usr/lib/aliases";
#endif

/* A run-time Boolean to tell us if UUCP addresses are supposed to look like remote ones */
int AMS_UUCPSupported = 0;

int AMS_NameSeparator = -1;	/* Read this via CheckAMSNameSep(domain) */

/* Address to which white-pages update requests are to be mailed. */
#ifdef CMU_ENV
char *WPIUpdateReqAddr = "wd00+WPI-UPDATE-REQUEST@andrew.cmu.edu";
#else /* CMU_ENV */
char *WPIUpdateReqAddr = "postmaster";
#endif /* CMU_ENV */

/* A flag for testing whether the ``+'' hack works in a given domain */
#ifdef RUN_AMDS_ENV
int AMS_UseridPlusWorks = 1;	/* Read this via CheckAMSUseridPlusWorks(domain) */
#else /* RUN_AMDS_ENV */
int AMS_UseridPlusWorks = 0;	/* Read this via CheckAMSUseridPlusWorks(domain) */
#endif /* RUM_AMDS_ENV */

/* %%%% CATEGORY (5): DOMAIN-DEPENDENT, KNOWABLE FOR MULTIPLE DOMAINS, KEPT FOR AMS-HOME DOMAIN */

/* Templates for building an mspath. */  /* Read these via CheckAMSDfMSPath(domain) */
/* In several cases, the "previous" value is also available, to help in major tree movements */
/* The following defines the mspath variable $local */

char *LOCALSEARCHPATHTEMPLATE = "/afs/andrew.cmu.edu/usr0/bb/.MESSAGES";
char *OLDLOCALSEARCHPATHTEMPLATE = "/cmu/itc/bb/.MESSAGES";

/* The following defines the mspath variable $external */

char *EXTERNALSEARCHPATHTEMPLATE = "/afs/andrew.cmu.edu/usr0/netbb/.MESSAGES";
char *OLDEXTERNALSEARCHPATHTEMPLATE = "/cmu/itc/netbb/.MESSAGES";

/* The following defines the mspath variable $official */

char *OFFICIALSEARCHPATHTEMPLATE = "/afs/andrew.cmu.edu/usr0/bb/off/.MESSAGES";
char *OLDOFFICIALSEARCHPATHTEMPLATE = "/cmu/itc/bb/off/.MESSAGES";

/* The following defines the mspath variable $default, and also the mspath
    for people with no mspath preference.  Note that $mail cannot be redefined. */

#ifdef SITE_DEFAULT_MS_SEARCHPATH
char *DEFAULTSEARCHPATHTEMPLATE = SITE_DEFAULT_MS_SEARCHPATH ;
#else /* SITE_DEFAULT_MS_SEARCHPATH */
#ifdef RUN_AMDS_ENV
char *DEFAULTSEARCHPATHTEMPLATE = "$mail:$official:$local:$external";
#else /* RUN_AMDS_ENV */
char *DEFAULTSEARCHPATHTEMPLATE = "$mail";
#endif /* RUN_AMDS_ENV */
#endif /* SITE_DEFAULT_MS_SEARCHPATH */

/* Flags for each variable */
#define	f_none	0	/* Set only via GetConfiguration. */
#define	f_cell	1	/* Initialize this variable according to the AMS home cell identity. */
/* ...and others might be added later. */

static struct ConfigStrings {
    char *ConfigKey, **ConfigParm; int Flags;
} ConfigStrings[] = {
    {"AMS_AliasFileName", &AMS_AliasFileName, f_none}, /* (4) */
    {"AMS_MailBoxPrefix", &AMS_MailBoxPrefix, f_none}, /* (2) */
    {"SpoolMailLockTemp", &SpoolMailLockTemp, f_none}, /* (2) */
    {"SpoolMailLockDir", &SpoolMailLockDir, f_none}, /* (2) */
    {"AMS_MailCollectionCommand", &AMS_MailCollectionCommand, f_none}, /* (2) */
    {"LocalQueue", &LocalQueue, f_none}, /* (2) */
    {"vicemail", &vicemail, f_none}, /* (2) */
    {"trymail", &trymail, f_none}, /* (2) */
    {"switchmail", &switchmail, f_none}, /* (2) */
    {"queuemail", &queuemail, f_none}, /* (2) */
    {"logpgm", &logpgm, f_none}, /* (2) */
    {"arpadatepgm", &arpadatepgm, f_none}, /* (2) */
    {"nntpxmit", &nntpxmit, f_none}, /* (2) */
    {"OldSendmailProgram", &oldsendmail, f_none}, /* (2) */
    {"DuplicateDB", &DuplicateDB, f_none}, /* (2) */
    {"SentSuffix", &SentSuffix, f_none}, /* (2) */
    {"NNTPSuffix", &NNTPSuffix, f_none}, /* (2) */
    {"CaptSuffix", &CaptSuffix, f_none}, /* (2) */

    {"ForwardingName", &ForwardingName, f_none}, /* (3) */
    {"OutgoingName", &OutgoingName, f_cell}, /* (3) */
    {"PostmasterTitle", &PostmasterTitle, f_cell}, /* (3) */
    {"PostmasterMailbox", &PostmasterMailbox, f_cell}, /* (3) */
#ifdef CMU_ENV
    {"IDChangeDate", &IDChangeDate, f_cell}, /* (3) */
#endif /* CMU_ENV */
    {"Organization", &Organization, f_cell}, /* (3) */
    {"NNTPhost", &NNTPhost, f_cell}, /* (3) */
    {"NNTPuser", &NNTPuser, f_cell}, /* (3) */
    {"DefaultSurfaceAddress", &DefaultSurfaceAddress, f_cell}, /* (3) */
    {"RequiredSubsFile", &GlobalRequiredSubsFile, f_cell}, /* (3) */
    {"ChangedSubsFile", &GlobalChangeSubsFile, f_cell}, /* (3) */
    {"MessagesBugAddress", &MessagesAutoBugAddress, f_cell}, /* (3) */
    {"WelcomeMailFile", &WelcomeMailFile, f_cell}, /* (3) */
    {"EliStartupFile", &EliStartupFile, f_cell}, /* (3) */

    {"MailboxName", &MailboxName, f_none}, /* (4) */
    {"PostmasterName", &PostmasterName, f_none}, /* (4) */
    {"WPIUpdateReqAddr", &WPIUpdateReqAddr, f_none}, /* (4) */

    {"LocalBboardRoot", &LOCALSEARCHPATHTEMPLATE, f_cell}, /* (5) */
    {"ExternalBboardRoot", &EXTERNALSEARCHPATHTEMPLATE, f_cell}, /* (5) */
    {"OfficialBboardRoot", &OFFICIALSEARCHPATHTEMPLATE, f_cell}, /* (5) */
    {"OldLocalBboardRoot", &OLDLOCALSEARCHPATHTEMPLATE, f_cell}, /* (5) */
    {"OldExternalBboardRoot", &OLDEXTERNALSEARCHPATHTEMPLATE, f_cell}, /* (5) */
    {"OldOfficialBboardRoot", &OLDOFFICIALSEARCHPATHTEMPLATE, f_cell}, /* (5) */
    {"DefaultMSPath", &DEFAULTSEARCHPATHTEMPLATE, f_cell}, /* (5) */

    {NULL, NULL, f_none}
};

static struct ConfigBooleans {
    char *ConfigKey;
    int *ConfigParm;
    int Flags;
} ConfigBooleans[] = {
    {"AMS_NoDomainPreferredOnLocalMail", &AMS_NoDomainPreferredOnLocalMail, f_none}, /* (2) */
    {"AMS_WSRunsQueuemail", &AMS_WSRunsQueuemail, f_none}, /* (2) */
    {"AMS_DemandSeparatingCharacter", &AMS_DemandSeparatingCharacter, f_none}, /* (2) */
    {"AMS_CheckAddressInSeparatingFrom", &AMS_CheckAddressInSeparatingFrom, f_none}, /* (2) */
    {"AMS_CheckDateInSeparatingFrom", &AMS_CheckDateInSeparatingFrom, f_none}, /* (2) */
    {"AMS_AllowColonInSeparatingFrom", &AMS_AllowColonInSeparatingFrom, f_none}, /* (2) */
    {"AMS_StrictStandaloneLocking", &AMS_StrictStandaloneLocking, f_none}, /* (2) */
    {"AMS_GecosHacks", &AMS_GecosHacks, f_none}, /* (2) */
    {"AMS_UseShortFileNames", &AMS_UseShortFileNames, f_none}, /* (2) */
    {"AMS_DevConsoleIsSacred", &AMS_DevConsoleIsSacred, f_none}, /* (2) */

    {"AMS_PrinterNamesInSpoolDirectories", &AMS_PrinterNamesInSpoolDirectories, f_none}, /* (2p) */
    {"AMS_PrinterNamesInPrintcap", &AMS_PrinterNamesInPrintcap, f_none}, /* (2p) */

    {"AMS_OnlyMail", &AMS_OnlyMail, f_cell}, /* (3) */
    {"AMS_UsersAreGrownups", &AMS_UsersAreGrownups, f_cell}, /* (3) */

    {"AMS_ValidateDestHosts", &AMS_ValidateDestHosts, f_cell}, /* (3h) */
    {"AMS_HardHostValidationErrors", &AMS_HardHostValidationErrors, f_cell}, /* (3h) */
    {"AMS_DeliveryViaDomainMXAddress", &AMS_DeliveryViaDomainMXAddress, f_cell}, /* (3h) */
    {"AMS_DeliveryViaDomainAddress", &AMS_DeliveryViaDomainAddress, f_cell}, /* (3h) */
    {"AMS_DeliveryViaGethostbyname", &AMS_DeliveryViaGethostbyname, f_cell}, /* (3h) */
    {"AMS_DeliveryViaHostTable", &AMS_DeliveryViaHostTable, f_cell}, /* (3h) */

    {"AMS_UsersHandleFormatting", &AMS_UsersHandleFormatting, f_none}, /* (4) */
    {"AMS_UUCPSupported", &AMS_UUCPSupported, f_none}, /* (4) */
    {"AMS_AliasesValidation", &AMS_AliasesValidation, f_none}, /* (4) */
    {"AMS_UseridPlusWorks", &AMS_UseridPlusWorks, f_none}, /* (4) */

    {NULL, NULL, f_none},
};

static struct ConfigInts {
    char *ConfigKey;
    int *ConfigParm, Flags;
} ConfigInts[] = {
    {"AMS_InitialDeathKnell", &AMS_InitialDeathKnell, f_none}, /* (2) */
    {"AMS_MaximumDeathKnell", &AMS_MaximumDeathKnell, f_none}, /* (2) */
    {"AMS_SeparatingCharacter", &AMS_SeparatingCharacter, f_none}, /* (2) */
    {"AMS_DeleteLinesWithSeparator", &AMS_DeleteLinesWithSeparator, f_none}, /* (2) */

#ifdef NOTDEF
    {"Postmaster_uid", &postmaster_uid, f_cell}, /* (3) */
#endif /* NOTDEF */
    {"AMS_ViceQueueLifetime", &AMS_ViceQueueLifetime, f_cell}, /* (3) */
    {"AMS_ExtraViceQueueLifetime", &AMS_ExtraViceQueueLifetime, f_cell}, /* (3) */
    {"AMS_ViceQueueLifetimeCap", &AMS_ViceQueueLifetimeCap, f_cell}, /* (3) */
    {"AMS_CrossCellQueueLifetime", &AMS_CrossCellQueueLifetime, f_cell}, /* (3) */

/* The next four of these can't be f_cell, since LocalDatabaseValidation can't be. */
    {"AMS_WPValidation", &AMS_WPValidation, f_none}, /* (4) */
    {"AMS_PasswdValidation", &AMS_PasswdValidation, f_none}, /* (4) */
    {"AMS_LocalDatabaseValidation", &AMS_LocalDatabaseValidation, f_none}, /* (4) */
#ifdef USE_MMDF_ENV
    {"AMS_MMDFValidation", &AMS_MMDFValidation, f_none}, /* (4) */
#endif
    {"AMS_NameSeparator", &AMS_NameSeparator, f_none}, /* (4) */

    {NULL, NULL, f_none}
};

static struct ConfigStringArrays {
    char *ConfigKey, ***ConfigParm, **ConfigDefault;
    int *ArraySize, Flags;
} ConfigStringArrays[] = {
    {"AMS_MasterUpdateHosts", &AMS_MasterUpdateHosts, DefaultAMS_MasterUpdateHosts, &numAMS_MasterUpdateHosts, f_none}, /* (2) */
    {"lastDitchPrefixes", &lastDitchPrefixes, defLastDitchPrefixes, &nlastditchprefixes, f_none}, /* (2) */
    {"bailoutPrefixes", &bailoutPrefixes, defBailoutPrefixes, &nbailoutprefixes, f_none}, /* (2) */

    {"AMS_PrintSpoolDirectories", &AMS_PrintSpoolDirectories, DefaultAMS_PrintSpoolDirectories, &numAMS_PrintSpoolDirectories, f_none}, /* (2p) */
    {"AMS_NamedValidPrinters", &AMS_NamedValidPrinters, DefaultAMS_NamedValidPrinters, &numAMS_NamedValidPrinters, f_none}, /* (2p) */

    {"AMS_ValidDomainSuffixes", &AMS_ValidDomainSuffixes, DefaultAMS_ValidDomainSuffixes, &numAMS_ValidDomainSuffixes, f_cell}, /* (3h) */
    {"AMS_ValidDomainRelays", &AMS_ValidDomainRelays, DefaultAMS_ValidDomainRelays, &numAMS_ValidDomainRelays, f_cell}, /* (3h) */

    {NULL, NULL, NULL, f_none}
};

/* Now, provide a mechanism to dynamically configure these guys. */

/* First, a mechanism to get the cell-dependent stuff. */
static struct configurelist *openCellConfig(cellN)
char *cellN;
{
	int mylen;
	char *mybuf;
	struct configurelist *cc;

	errno = 0;
	mylen = strlen(CellCommonPrefix) + strlen(cellN);
	mylen += (strlen(CellCommonSuffix)
		+ strlen(CellCommonConfigDirSuffix)
		+ strlen(CellConfigMessageServer) + 4);
	mybuf = malloc(mylen);
	if (mybuf == NULL) return NULL;
	strcpy(mybuf, CellCommonPrefix);
	LCappend(mybuf, cellN);
	strcat(mybuf, CellCommonSuffix);
	strcat(mybuf, CellCommonConfigDirSuffix);
	strcat(mybuf, "/");
	strcat(mybuf, CellConfigMessageServer);
	cc = ReadConfigureFile(mybuf);
	free(mybuf);
	return (cc);
}

int CkAMSCellConfig(Cell) char *Cell; {
    int i, val, retVal;
    char *s, *olds;
    struct configurelist *CList = NULL;
    char *andyDir, *locDir, *homeDir;

/* Currently, there's nothing in serviceconfig that needs to be checked on a per-cell basis. */

    if (Cell != NULL) {
	CList = openCellConfig(Cell);
	if (CList == NULL) return (1);
    }
    retVal = 0;
    for (i=0; ConfigBooleans[i].ConfigKey; ++i) {
	if (Cell != NULL && (ConfigBooleans[i].Flags & f_cell) == 0) continue;
	s = NULL;
	if (Cell == NULL)  {
	    s = GetConfiguration(ConfigBooleans[i].ConfigKey);
	} else {
	    s = GetConfig(CList, ConfigBooleans[i].ConfigKey, 1);
	}
	if (s && *s) {
		while (isspace(*s) || *s == '-') ++s;
		switch(*s) {
		    case 'y': case 'Y': case '1': case 't': case 'T':
			*ConfigBooleans[i].ConfigParm = 1;
			break;
		    case 'n': case 'N': case '0': case 'f': case 'F':
			*ConfigBooleans[i].ConfigParm = 0;
			break;
		    default:
			fprintf(stderr, "Warning: Bad Boolean value '%s' for '%s' in configuration file; ignored\n", s, ConfigBooleans[i].ConfigKey);
			break;
		}	
	}
    }
    if (retVal == 0) for (i=0; ConfigStringArrays[i].ConfigKey; ++i) {
	if (Cell != NULL && (ConfigStringArrays[i].Flags & f_cell) == 0) continue;
	s = NULL;
	if (Cell == NULL)  {
	    s = GetConfiguration(ConfigStringArrays[i].ConfigKey);
	} else {
	    s = GetConfig(CList, ConfigStringArrays[i].ConfigKey, 1);
	}
	if (s) {
		int itemcount;
		char *mycopy;

		mycopy = malloc(1+strlen(s));
		if (!mycopy) {retVal = -1; break;}
		strcpy(mycopy, s);
		for(itemcount = 0, s = mycopy; s=strchr(s, ','); ++s, ++itemcount) {
		    ;
		}
		*(ConfigStringArrays[i].ConfigParm) = (char **) malloc((2+itemcount) * sizeof(char *));
		if (! *(ConfigStringArrays[i].ConfigParm)) {retVal = -1; break;}
		itemcount = 0;
		while(s=strchr(mycopy, ',')) {
		    if (s == mycopy) {
			++s;
			continue;
		    }
		    *s = '\0';
		    while (isspace(*mycopy)) ++mycopy;
		    (*(ConfigStringArrays[i].ConfigParm))[itemcount] = mycopy;
		    mycopy = ++s;
		    ++itemcount;
		}
		while (isspace(*mycopy)) ++mycopy;
		if (*mycopy != '\0') {
		    (*(ConfigStringArrays[i].ConfigParm))[itemcount++] = mycopy;
		}
		(*(ConfigStringArrays[i].ConfigParm))[itemcount] = NULL;
		if (ConfigStringArrays[i].ArraySize) {
		    *ConfigStringArrays[i].ArraySize = itemcount;
		}
	}
    }
    if (retVal == 0) for (i=0; ConfigInts[i].ConfigKey; ++i) {
	if (Cell != NULL && (ConfigInts[i].Flags & f_cell) == 0) continue;
	s = NULL;
	if (Cell == NULL)  {
	    s = GetConfiguration(ConfigInts[i].ConfigKey);
	} else {
	    s = GetConfig(CList, ConfigInts[i].ConfigKey, 1);
	}
	if (s && *s) {
		val = atoi(s);
		if (val == 0) {
		    olds = s;
		    while (*s != '\0' && (isspace(*s) || *s == '0')) ++s;
		    if (*s != '\0') fprintf(stderr, "Warning: Value for configuration integer %s was specified as %s (in %s), which will be interpreted as zero.\n", ConfigInts[i].ConfigKey, olds, (Cell == NULL ? "AndrewSetup" : CellConfigMessageServer));
		}
		*ConfigInts[i].ConfigParm = val;
	}
    }
    andyDir = locDir = homeDir = NULL;
    if (retVal == 0) for (i=0; ConfigStrings[i].ConfigKey; ++i) {
	if (Cell != NULL && (ConfigStrings[i].Flags & f_cell) == 0) continue;
	s = NULL;
	if (Cell == NULL)  {
	    s = GetConfiguration(ConfigStrings[i].ConfigKey);
	} else {
	    s = GetConfig(CList, ConfigStrings[i].ConfigKey, 1);
	}
	if (s) {while (isspace(*s)) ++s;}
	if (s == NULL && Cell == NULL && (*ConfigStrings[i].ConfigParm) && *(*ConfigStrings[i].ConfigParm) == '%')
		s = *ConfigStrings[i].ConfigParm;	/* edit %-escapes */
	if (s) {
		char *t; int newlen;

		newlen = strlen(s)+1;
		while (isspace(*s)) ++s;
		if (*s == '%') {
			if (s[1] == 'a' || s[1] == 'A') {	/* AndrewDir */
				if (andyDir == NULL) andyDir = (char *) AndrewDir(NULL);
				newlen += strlen(andyDir);
			} else if (s[1] == 'l' || s[1] == 'L') {	/* LocalDir */
				if (locDir == NULL) locDir = (char *) LocalDir(NULL);
				newlen += strlen(locDir);
			} else if (s[1] == 'h' || s[1] == 'H') {	/* user's home directory */
				if (homeDir == NULL) {
					struct passwd *PW = getpwuid(geteuid());
					homeDir = PW->pw_dir;
				}
				newlen += strlen(homeDir);
			}
		}
		t = malloc(newlen);
		if (!t) {retVal = -1; break;}
		if (*s == '%') {
			if (s[1] == 'a' || s[1] == 'A') {	/* AndrewDir */
				strcpy(t, andyDir);
				strcat(t, s+2);
			} else if (s[1] == 'l' || s[1] == 'L') {	/* LocalDir */
				strcpy(t, locDir);
				strcat(t, s+2);
			} else if (s[1] == 'h' || s[1] == 'H') {	/* user's home directory */
				strcpy(t, homeDir);
				strcat(t, s+2);
			} else if (s[1] == '%') {		/* quoted percent sign */
				strcpy(t, s+1);
			} else {
				fprintf(stderr, "Warning: No %%-escape ``%%%c'' defined (to interpret the value of %s in %s); passing ``%s'' through unchanged.\n",
					s[1], ConfigStrings[i].ConfigKey, 
					(Cell == NULL ?
					"AndrewSetup" : CellConfigMessageServer), s);
				strcpy(t, s);
			}
		} else strcpy(t, s);
		*ConfigStrings[i].ConfigParm = t;
	}
    }
    if (Cell != NULL) FreeConfigureList(CList);
    return (retVal);
}

int CheckAMSConfiguration() {
    int val;
    static int AlreadyChecked = 0;

    if (AlreadyChecked) return 0;

    val = CheckServiceConfiguration();
    if (val != 0) return val;

    val = CkAMSCellConfig(NULL);
    if (val != 0) return val;

    AlreadyChecked = 1;
    return(0);
}

#ifdef TESTINGONLYTESTING
main(argc, argv) char *argv[]; {
    int i;
    char *dom;

    CheckAMSConfiguration();
    printf("ThisDomain is %s len %d\n", ThisDomain, ThisDomainLen);
    printf("WorkstationCell is ``%s''.\n", WorkstationCell);
    printf("num valid suffixes is %d, to wit:\n", numAMS_ValidDomainSuffixes);
    for (i=0; i<numAMS_ValidDomainSuffixes; ++i) {
	printf("    [%d]: '%s'\n", i, AMS_ValidDomainSuffixes[i]);
    }
    printf("nbailoutprefixes is %d, to wit:\n", nbailoutprefixes);
    for (i=0; i<nbailoutprefixes; ++i) {
	printf("    [%d]: '%s'\n", i, bailoutPrefixes[i]);
    }
    printf("AMS_MailBoxPrefix is ``%s''.\n", AMS_MailBoxPrefix);
    printf("Trymail is %s; switchmail is %s\n", trymail, switchmail);
    printf("WelcomeMail file is %s.\n", WelcomeMailFile);
    dom = ThisDomain;
    if (argc >= 2) dom = argv[1];
    CkAMSCellConfig(dom);
    printf("In %s, LOCALSEARCHPATHTEMPLATE is %s.\n", dom, LOCALSEARCHPATHTEMPLATE);
    printf("In %s, numAMS_ValidDomainRelays is %d:", dom, numAMS_ValidDomainRelays);
    for (i = 0; i < numAMS_ValidDomainRelays; ++i) printf(" %s", AMS_ValidDomainRelays[i]);
    printf(".\n");
}
#endif /* TESTINGONLYTESTING */
