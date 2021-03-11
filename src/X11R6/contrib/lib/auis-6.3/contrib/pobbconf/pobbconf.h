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

/* ************************************************************ *\
	pobbconf.h
	Configuration declarations for the post office and bboard machines.
\* ************************************************************ */

/* Coarse-grain configuration: whether to include a facility or not. */
/* Unlike many C preprocessor definitions, this facility looks not at whether
   a given name is defined, but instead at its value.  0 is FALSE, non-0 is TRUE. */
#define	FALSE	0
#define	TRUE	1

/* pobb_WPUpdate: whether we configure the White Pages updater. */
#ifndef	pobb_WPUpdate
#define	pobb_WPUpdate	TRUE
#endif /* pobb_WPUpdate */

/* pobb_RunAMSDelivery: whether we service the queues etc. */
#ifndef	pobb_RunAMSDelivery
#define	pobb_RunAMSDelivery	TRUE
#endif /* pobb_RunAMSDelivery */

/* pobb_AutoPost: whether to turn mail messages into bboard posts at all. */
/* We only have to run BBoard machine tasks if this is true. */
#ifndef	pobb_AutoPost
#define	pobb_AutoPost	TRUE
#endif /* pobb_AutoPost */

/* pobb_MaybeExperimental: whether we can run an experimental configuration on machines. */
#ifndef	pobb_MaybeExperimental
#define	pobb_MaybeExperimental	TRUE
#endif /* pobb_MaybeExperimental */

/* Now derive whether we're going to run server machines. */
#if (pobb_RunAMSDelivery || pobb_AutoPost)
#define	pobb_RunMachines	TRUE
#else /* (pobb_RunAMSDelivery || pobb_AutoPost) */
#define	pobb_RunMachines	FALSE
#endif /* (pobb_RunAMSDelivery || pobb_AutoPost) */

/* pobb_NNTPIn: whether to get incoming Usenet messages from NNTP. */
#ifndef	pobb_NNTPIn
#define	pobb_NNTPIn	TRUE
#endif /* pobb_NNTPIn */

/* pobb_NNTPOut: whether to post outgoing Usenet messages via NNTP. */
#ifndef	pobb_NNTPOut
#define	pobb_NNTPOut	TRUE
#endif /* pobb_NNTPOut */

/* pobb_UnDigest: whether we split apart incoming mailed digests. */
#ifndef	pobb_UnDigest
#define	pobb_UnDigest	TRUE
#endif /* pobb_UnDigest */

/* pobb_GetHostTable: whether to retrieve an up-to-date Arpanet host table periodically */
#ifndef	pobb_GetHostTable
#define	pobb_GetHostTable	TRUE
#endif /* pobb_GetHostTable */

/* pobb_PublishAccounts: whether we make a listing of all our accounts. */
#ifndef	pobb_PublishAccounts
#define	pobb_PublishAccounts	FALSE
#endif /* pobb_PublishAccounts */

/* pobb_GetListOfLists: whether to retrieve the Arpanet List-of-lists periodically. */
#ifndef	pobb_GetListOfLists
#define	pobb_GetListOfLists	TRUE
#endif /* pobb_GetListOfLists */

/* pobb_CheckBBMailboxSizes: whether to check the sizes of important BB Mailboxes periodically */
#ifndef pobb_CheckBBMailboxSizes
#define	pobb_CheckBBMailboxSizes    TRUE
#endif /* pobb_CheckBBMailboxSizes */

/* pobb_TakeHelpStats: whether to probe for Help-system statistics. */
#ifndef	pobb_TakeHelpStats
#define	pobb_TakeHelpStats	TRUE
#endif /* pobb_TakeHelpStats */

/* pobb_CaptureAddresses: whether to capture From: addresses as they fly by */
#ifndef	pobb_CaptureAddresses
#define	pobb_CaptureAddresses		FALSE
#endif /* pobb_CaptureAddresses */

/* pobb_WPInteractive: whether WP updates can happen by a mail-based daemon */
#ifndef	pobb_WPInteractive
#define	pobb_WPInteractive		TRUE
#endif /* pobb_WPInteractive */

/* pobb_ButlerPool: whether telnet to the main host simply gives a list of Butler-able WS names */
#ifndef	pobb_ButlerPool
#define	pobb_ButlerPool		TRUE
#endif /* pobb_ButlerPool */

/* pobb_DowJonesIn: whether we process high-volume Dow-Jones mail from a special mailbox */
#ifndef	pobb_DowJonesIn
#define	pobb_DowJonesIn		TRUE
#endif /* pobb_DowJonesIn */

/* pobb_MUServer: whether we run a muserver */
#ifndef pobb_MUServer
#define pobb_MUServer		TRUE
#endif /* pobb_MUServer */

/* Some definitions override the values of others. */
#if (! pobb_WPUpdate)
#undef	pobb_PublishAccounts
#define	pobb_PublishAccounts	FALSE
#undef	pobb_WPInteractive
#define	pobb_WPInteractive	FALSE
#endif /* (! pobb_WPUpdate) */

#if (! pobb_AutoPost)
#undef	pobb_NNTPIn
#define	pobb_NNTPIn	FALSE
#undef	pobb_NNTPOut
#define	pobb_NNTPOut	FALSE
#undef	pobb_UnDigest
#define	pobb_UnDigest	FALSE
#undef	pobb_GetListOfLists
#define	pobb_GetListOfLists	FALSE
#undef	pobb_TakeHelpStats
#define	pobb_TakeHelpStats	FALSE
#undef	pobb_WPInteractive
#define	pobb_WPInteractive	FALSE
#undef	pobb_DowJonesIn
#define	pobb_DowJonesIn		FALSE
#undef  pobb_MUServer
#define pobb_MUServer		FALSE
#endif /* (! pobb_AutoPost) */

#if (! pobb_RunAMSDelivery)
#undef	pobb_GetHostTable
#define	pobb_GetHostTable	FALSE
#undef	pobb_ButlerPool
#define	pobb_ButlerPool	FALSE
#endif /* (! pobb_RunAMSDelivery) */

#if (! pobb_RunMachines)
#undef	pobb_CaptureAddresses
#define	pobb_CaptureAddresses	FALSE
#endif /* (! pobb_RunMachines) */

/* BBoard address to which daemons send logs and things */
extern char *DaemonBB;
/* BBoard address to which daemons send even more boring things */
extern char *LogBB;
/* Where some larger logs go */
extern char *DaemonLog;
/* Where some other logs go, for temp storage */
extern char *TempLog;
#if (pobb_AutoPost && pobb_RunAMSDelivery)
/* Distribution address for some occasionally-generated logs */
extern char *StatsDistList;
#endif /* (pobb_AutoPost && pobb_RunAMSDelivery) */
/* Shutdown signal */
extern char *ShutdownSignal;

#if (pobb_UnDigest)
/* Directories in which the undigest program works */
extern char *UnDigestUser;
extern char *UnDigestHome;		/* home base */
					/* Uses UnDigestHome/MailboxName, too */
extern int UnDigestSleepTime;		/* number of seconds for sleeping */
#endif /* (pobb_UnDigest) */

#if (pobb_NNTPIn)
/* Directories in which the nntppoll program works */
extern char *NNTPPollHome;	/* home base */
extern int NNTPPollSleepTime;		/* number of seconds to sleep between tries */
extern char **NNTPGroups;
extern int numNNTPGroups;
extern char **NNTPDists;
extern int numNNTPDists;
extern char **NNTPDelayedGroups;
extern int numNNTPDelayedGroups;
extern char *NNTPPeakHours;
#endif /* (pobb_NNTPIn) */

#if (pobb_WPUpdate)
/* Some white-pages stuff. */
extern char *wpbuildUser;		/* The username of the user that's supposed to be permitted to read all .forward files. */
extern char *wpbuildHome;		/* The home directory of wpbuildUser. */
extern char *wpbuildDir;	/* Directory in which WP is built and state about the previous build is preserved for next time */
extern char *WPBackupDir;	/* Directory in which backups for the WP db and the previous-build state are kept. */
extern char *WPPasswdFileName;	/* Directory out of which the workstation's  /etc/passwd file is updated */
extern char *WPErrorsAddress;		/* Address to which unanticipated WP build errors are mailed */
extern char *WPAffilSrc;		/* Where administrators put per-user affiliation information */
extern char *WPAffilMap;		/* Where administrators say what the abbreviations mean */
#endif /* (pobb_WPUpdate) */

#if (pobb_WPInteractive)
extern char *WPIInBox;	/* Mailbox where requests arrive */
extern char *WPIWorkDir;	/* Where daemon does its work */
extern char **WPIAdministrators;	/* Login-ids of WP admins in this cell */
extern int numWPIAdministrators;
#ifdef AFS30_ENV
extern char *WPIAdministratorGroup;
#endif /* AFS30_ENV */
#if (pobb_AutoPost)
extern char *WPIAdminFolderRoot;
#endif /* (pobb_AutoPost) */
#endif /* (pobb_WPInteractive) */

#if (pobb_MaybeExperimental)
extern char *TestExperimental;	/* sh(1) ``test'' argument to determine if we're experimental*/
extern char *SetExperimental;	/* sh(1) command to set the experimental environment; usually ANDREWDIR=something. */
#endif /* (pobb_MaybeExperimental) */

#if (pobb_GetHostTable)
extern char *GetHostTableHost;	/* What host to get our hosts table from */
extern char *GetHostTableDir;		/* What dir on that host has it */
#endif /* (pobb_GetHostTable) */

#if (pobb_PublishAccounts)
extern char *AndrewAcctDir;	/* Publish Andrew accts here, too */
#endif /* (pobb_PublishAccounts) */

#if (pobb_WPUpdate)
extern char *WPPasswordFile;	/* Where wpbuildUser's password is stored */
#endif /* (pobb_WPUpdate) */
#if (pobb_RunMachines)
extern char *PMPasswordFile;	/* Where PostmasterName's password is stored */
#endif /* (pobb_RunMachines) */

#if (pobb_RunAMSDelivery)
extern char *POQueues;	/* where /usr/andrew/bin/mailq gets its info, written by the post offices */
extern char **GlobalMailQueues;
extern int numGlobalMailQueues;
extern char **GlobalSlowMailQueues;
extern int numGlobalSlowMailQueues;
#endif /* (pobb_RunAMSDelivery) */

/* For the .cron files */
extern char *CronAuth;		/* AUTH=viceii */
extern char *CronPath;
#if (pobb_RunMachines)
extern char *PostmasterHome;
#endif /* (pobb_RunMachines) */

#if (pobb_RunAMSDelivery || pobb_NNTPIn)
/* For purging the duplicate DBs */
extern char *PurgeProgram;
#endif /* (pobb_RunAMSDelivery || pobb_NNTPIn) */
#if (pobb_RunAMSDelivery)
extern char *SentKeepLen;		/* time to keep SENT entries */
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_NNTPIn)
extern char *NNTPKeepLen;	/* time to keep NNTP entries */
#endif /* (pobb_NNTPIn) */

#if (pobb_RunMachines)
extern char *SomeScriptDir;		/* want this to become obsolete */
extern char *POConsoleDir;		/* where some console files live */
#endif /* (pobb_RunMachines) */

#if (pobb_AutoPost)
extern char *ViceCUILogsDir;	/* where the collected CUI logs go */
/* Database for bb-daemon statistics */
extern char *CUIDatabase;
extern char *NetBBUser;		/* ``user'' in whose acct network BBs are stored */
extern char *NetBBHome;		/* home dir of that user */
extern char *InternetUser;
#endif /* (pobb_AutoPost) */

#if (pobb_CaptureAddresses)
extern char *AFSCaptureProcess;	/* under which captured addresses are gathered and processed */
extern int CaptureLifetime;		/* how many seconds to keep addresses */
#endif /* (pobb_CaptureAddresses) */

#if (pobb_RunMachines)
/* Authentication string for incoming network mail */
extern char *NetworkAuthentication;
#endif /* (pobb_RunMachines) */

#if (pobb_WPUpdate)
/* Location of some programs. */
extern char *genacctspgm;
extern char *makebothpgm;
extern char *nickgenpgm;
extern char *wpbuildscript;
#endif /* (pobb_WPUpdate) */

#if (pobb_WPInteractive)
extern char *wpiupdat;
#endif /* (pobb_WPInteractive) */

#if (pobb_RunAMSDelivery)
/* Dir on every workstation for mail error reports */
extern char *LocalErrorDir;

#ifdef SNAP_ENV
/* Guardian-startup */
extern char *GuardianFlagFile, *GuardianOnLocalDisk, *MoveLogScriptDir;
#endif /* SNAP_ENV */

extern char *MultiUserFlagFile;

/* What oldsendmail expects and needs */
extern char *SendmailConfigDir;	/* for sendmail.cf, sendmail.fc in our version */
extern char *SendmailQueueDir;	/* where its queue files live */
extern char *SendmailQueueTime;	/* how often to run the sendmail queue */
/* Some abbreviations for the local hosts for use only by the sendmail.cf file: */
extern char **SendmailLocalAbbrevs;
extern int numSendmailLocalAbbrevs;

/* How to invoke a queuemail daemon: */
extern char *QueuemailCommonArgs;
extern int QueuemailNormalInterval;	/* on an ordinary first-dropoff queue */
extern int QueuemailSlowInterval;	/* on a hold-for-retry (``slow'') queue */
extern int QueuemailOutgoingInterval;	/* Arg to the -O switch for the outgoing daemon */
extern char *MailQueueNamePrefix;	/* This starts standard queue directories under mailqs */
extern char *SlowQueueNamePrefix;	/* This starts background queue directories under mailqs */

extern int POCleanInterval;	/* Sleep time for the po.clean script */
#endif /* (pobb_RunAMSDelivery) */

#ifdef AFS_ENV
#if (pobb_RunMachines)
extern int ReauthSleepInterval;	/* How long /usr/andrew/etc/reauth should sleep between successful /usr/andrew/bin/log runs. */
#endif /* (pobb_RunMachines) */
#endif /* AFS_ENV */

#if (pobb_RunMachines)
/* Now to the the local disk component of the delivery system installation. */
/* Two directories for bigger or smaller programs or scripts. */
extern char *BigLocalDir;
extern char *SmallLocalDir;
extern char *AMSLocalDir;

#if (pobb_NNTPIn)
/* Where aux local storage is. */
extern char *NetDatabaseRoot;
#endif /* (pobb_NNTPIn) */

#if (pobb_GetHostTable)
/* Where /etc/hosts, /etc/networks, /etc/gateways are updated */
extern char *IPFilePrefix;
extern char *IPFileSuffix;
#endif /* (pobb_GetHostTable) */
#endif /* (pobb_RunMachines) */

#ifdef AFS_ENV
/* Permissions for various things. */
extern char *generalReadingPublic, *generalEnqueueingPublic;
extern char **PostmanDirOwners;
extern int numPostmanDirOwners;
extern char **PackageDirOwners;
extern int numPackageDirOwners;
extern char *PackageAccess, *PostmasterAccess, *ConfigDirAccess;
#endif /* AFS_ENV */

/* Now the directories out of which package will download the workstation's local disk. */
extern char *PackageHome;
extern char *PackageBaseLib;	/* where the rest of the package source files are */
extern char *PackageBigDisk;	/* for big disks */
extern char *PackageSmallDisk;	/* for small disks */
extern char **PackagePredefines;
extern int numPackagePredefines;
extern char **PackagePrefixFiles;
extern int numPackagePrefixFiles;
extern char **PackageSuffixFiles;
extern int numPackageSuffixFiles;
extern char **PackageSupportedSysTypes;
extern int numPackageSupportedSysTypes;

#if (pobb_RunAMSDelivery)
extern char **PossiblePOs;
extern int numPossiblePOs;
extern char **PossiblePOAddrs;
extern int numPossiblePOAddrs;
extern char **PossiblePOCapas;
extern int numPossiblePOCapas;
extern char **PossiblePOHDSizes;
extern int numPossiblePOHDSizes;
extern char **DeployedPOs;
extern int numDeployedPOs;
extern char *PrimaryMXHost;
extern char **MXHosts;
extern int numMXHosts;
extern char **MXWeights;
extern int numMXWeights;
#endif /* (pobb_RunAMSDelivery) */

#if (pobb_AutoPost)
extern char **PossibleBBs;
extern int numPossibleBBs;
extern char **PossibleBBAddrs;
extern int numPossibleBBAddrs;
extern char **PossibleBBCapas;
extern int numPossibleBBCapas;
extern char **PossibleBBHDSizes;
extern int numPossibleBBHDSizes;
extern char **DeployedBBs;
extern int numDeployedBBs;
#endif /* (pobb_AutoPost) */

#if (pobb_NNTPOut)
extern char *OutnewsID, *OutnewsHome;
#endif /* (pobb_NNTPOut) */

#if (pobb_AutoPost)
/* Requirements for running the CUI daemons on the bboard machines. */
extern int CUIDaemonSleepTime, CUIHintsSleepTime;
#if (pobb_DowJonesIn)
extern int CUIDJSleepTime;
extern char *DJUserName;	/* Username for DJ runs */
extern char *DJLocalDir;	/* Dir local to workstation for DJ processing */
extern char *DJHomeDir;	/* AFS home dir */
extern char *DJPassword;   /* where local password is */
extern char *DJPgmPackageSource;	/* for line-monitor program */
#endif /* (pobb_DowJonesIn) */
#if (pobb_NNTPIn)
extern int CUINNSleepTime;
#endif /* (pobb_NNTPIn) */
extern char *CUIPrelude;	/* Prelude for msdaemon scripts */
extern char *CUIOncePrelude;	/* Prelude for msonce scripts */

#if (pobb_DowJonesIn)
extern char **CUIDJBoxes;	/* Where dow-jones stuff is posted */
extern int numCUIDJBoxes;
#endif /* (pobb_DowJonesIn) */

#if (pobb_NNTPIn)
extern char **CUINetnewsBoxes;	/* From where incoming netnews is posted */
extern int numCUINetnewsBoxes;

extern char **PostedNetnewsRoots;/* File sys roots of the posted netnews tree */
extern int numPostedNetnewsRoots;

extern char *NetnewsRootName;	/* Visible name of that tree */

extern char **PostedNetnewsVolRoots;	/* Volume roots for the posted netnews tree */
extern int numPostedNetnewsVolRoots;
#endif /* (pobb_NNTPIn) */

#if (pobb_MUServer)
extern char **MUServerBBs;
extern int numMUServerBBs;
#endif /* (pobb_MUServer) */

extern char *PostedInternetRoot;	/* File sys root of the mailing-list root tree */
extern char *InternetRootName;	/* Visible name of that root */

extern char **CUIExtBoxes;	/* From where stuff from external sources is posted */
extern int numCUIExtBoxes;

extern char **CUILocalHighBoxes;	/* From where stuff from local sources is posted */
extern int numCUILocalHighBoxes;

extern char **CUILocalLowBoxes;	/* From where stuff from local sources is posted */
extern int numCUILocalLowBoxes;

extern char **CUILocalBoxes;	/* From where stuff from local sources is posted */
extern int numCUILocalBoxes;

extern char **PostedVolumeRoots;
extern int numPostedVolumeRoots;

extern char **PurgingCommandsNightly;
extern int numPurgingCommandsNightly;
extern char **PurgingCommandsWeekly;
extern int numPurgingCommandsWeekly;
extern char **PurgingCommandsBiWeekly;
extern int numPurgingCommandsBiWeekly;
extern char **PurgingCommandsMonthly;
extern int numPurgingCommandsMonthly;
extern char **PurgingCommandsSemiAnnually;
extern int numPurgingCommandsSemiAnnually;

extern char **BBDMonitorVolumePaths;
extern int numBBDMonitorVolumePaths;

/* Bboard daemon processing directory */
extern char *BBDaemonDir;
#endif /* (pobb_AutoPost) */

/* BBD script configuration */
extern char *BBDFormatHeader;	/* a flag as to how contents should be interpreted */
extern char **BBDBeginBody;
extern int numBBDBeginBody;
extern char *BBDEndBodyLine;

#if (pobb_TakeHelpStats)
extern char *BBDHelpPrefix;
extern char *BBDHelpSuffix;
#endif /* (pobb_TakeHelpStats) */

#if (pobb_RunMachines)
/* Access control on dedicated machines. */
extern char **EquivalentHosts;
extern int numEquivalentHosts;
extern char **UsersToAllow;
extern int numUsersToAllow;
#endif /* (pobb_RunMachines) */
