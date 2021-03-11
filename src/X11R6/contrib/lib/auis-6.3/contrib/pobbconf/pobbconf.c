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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/pobbconf/RCS/pobbconf.c,v 1.5 1993/08/25 20:44:09 susan Exp $";
#endif

/* ************************************************************ *\
	pobbconf.c
	Configuration information for the post office and bboard machines.
\* ************************************************************ */

#include <andyenv.h>
#include <system.h>
#include <signal.h>
#include <pobbconf.h>
#define NULL 0

/* BBoard address to which daemons send logs and things */
char *DaemonBB = "bb+andrew.daemons";
/* BBoard address to which daemons send even more boring things */
char *LogBB = "postman+logs";
/* Where some larger logs go */
char *DaemonLog = "/usr/log";
/* Where some other logs go, for temp storage */
char *TempLog = "/usr/spool/log";
#if (pobb_AutoPost && pobb_RunAMSDelivery)
/* Distribution address for some occasionally-generated logs */
char *StatsDistList = "bb+andrew.ms.stats@?[ThisDomain]";
#endif /* (pobb_AutoPost && pobb_RunAMSDelivery) */
/* Shutdown signal */
#ifdef SIGXCPU
char *ShutdownSignal = "XCPU";
#else /* #ifdef SIGXCPU */
#ifdef SIGUSR1
char *ShutdownSignal = "USR1";
#else /* #ifdef SIGUSR1 */
char *ShutdownSignal = "FPE";
#endif /* #ifdef SIGUSR1 */
#endif /* #ifdef SIGXCPU */
/* Similarly, XFSZ is mapped to USR2 (and from there to HUP) if not defined. */

#if (pobb_UnDigest)
/* Directories in which the undigest program works */
char *UnDigestUser = "digestbb";	/* user name for mailbox */
char *UnDigestHome = "/afs/andrew.cmu.edu/usr0/digestbb";	/* home base */
int UnDigestSleepTime = 360;		/* number of seconds for sleeping */
#endif /* (pobb_UnDigest) */

#if (pobb_NNTPIn)
/* Directories in which the nntppoll program works */
char *NNTPPollHome = "/afs/andrew.cmu.edu/usr0/usenetbb";	/* home base */
int NNTPPollSleepTime = 1800;		/* number of seconds to sleep between tries */
static char *initNNTPGroups[] = {
	"comp", "news", "sci", "rec", "misc", "soc", "talk",
	"cmu", "pgh", "na",
	"alt", "gnu", "unix-pc", "bionet", "vmsnet",
	NULL
};
char **NNTPGroups = initNNTPGroups;
int numNNTPGroups = (sizeof(initNNTPGroups) / sizeof(initNNTPGroups[0])) - 1;
static char *initNNTPDists[] = {
	"cmu", "pgh", "pa", "na", "usa", "world", "net", "inet",
	NULL
};
char **NNTPDists = initNNTPDists;
int numNNTPDists = (sizeof(initNNTPDists) / sizeof(initNNTPDists[0])) - 1;
/* Groups to delay during peak hours */
static char *initNNTPDelayedGroups[] = {
	"rec", "misc", "soc", "talk", "alt",
	NULL
};
char **NNTPDelayedGroups = initNNTPDelayedGroups;
int numNNTPDelayedGroups = (sizeof(initNNTPDelayedGroups) / sizeof(initNNTPDelayedGroups[0])) - 1;
char *NNTPPeakHours = "11am-7pm M-F"; /* When to delay NNTPDelayedGroups */
#endif /* (pobb_NNTPIn) */

#if (pobb_WPUpdate)
/* Some white-pages stuff. */
char *wpbuildUser = "wd00";		/* The username of the user that's supposed to be permitted to read all .forward files. */
char *wpbuildHome = "/afs/andrew.cmu.edu/usr0/wd00";	/* The home directory of wpbuildUser. */
char *wpbuildDir = "/afs/andrew.cmu.edu/usr0/wd00/wpbuild";	/* Directory in which WP is built and state about the previous build is preserved for next time */
char *WPBackupDir = "/afs/andrew.cmu.edu/usr0/wd00/wpbackup";	/* Directory in which backups for the WP db and the previous-build state are kept. */
char *WPPasswdFileName = "/afs/.andrew.cmu.edu/common/etc/passwd";	/* File from which the workstation's  /etc/passwd file is updated */
char *WPErrorsAddress = "postman+wperrors";	/* Address to which unanticipated WP build errors are mailed */
char *WPAffilSrc = "/afs/andrew.cmu.edu/data/db/useradmin/affil/andrew.affil";	/* Where administrators put per-user affiliation information */
char *WPAffilMap = "";	/* Where administrators say what the affiliation codes mean */
#endif /* (pobb_WPUpdate) */

#if (pobb_WPInteractive)
char *WPIInBox = "/afs/andrew.cmu.edu/usr0/wd00/Mailbox";	/* Mailbox where requests arrive */
char *WPIWorkDir = "/afs/andrew.cmu.edu/usr0/wd00/wpi";	/* Where daemon does its work */
static char *initWPIAdministrators[] = {	/* IDs or groups of WP admins in this cell */
	"grm", "dr1p", "wally", "aw0g", "jm36",
	NULL
};
char **WPIAdministrators = initWPIAdministrators;
int numWPIAdministrators = (sizeof(initWPIAdministrators) / sizeof(initWPIAdministrators[0])) - 1;
#ifdef AFS30_ENV
/* Group that should contain the above people. */
char *WPIAdministratorGroup = "wd00:wpadmins";
#endif /* AFS30_ENV */
#if (pobb_AutoPost)
char *WPIAdminFolderRoot = "?[LOCALSEARCHPATHTEMPLATE]/org/wp";
#endif /* (pobb_AutoPost) */
#endif /* (pobb_WPInteractive) */

#if (pobb_MaybeExperimental)
char *TestExperimental="-d /usr/itc";	/* sh(1) ``test'' argument to determine if we're experimental*/
char *SetExperimental="ANDREWDIR=/usr/andy";	/* sh(1) command to set the experimental environment; usually ANDREWDIR=something. */
#endif /* (pobb_MaybeExperimental) */

#if (pobb_GetHostTable)
char *GetHostTableHost = "dist.fac.cs.cmu.edu";	/* What host to get our hosts table from */
char *GetHostTableDir = "/usr/htable/lib";		/* What dir on that host has it */
#endif /* (pobb_GetHostTable) */

#if (pobb_PublishAccounts)
char *AndrewAcctDir = "/afs/andrew.cmu.edu/usr0/wd00/accts";	/* Publish Andrew accts here, too */
#endif /* (pobb_PublishAccounts) */

#if (pobb_WPUpdate)
char *WPPasswordFile = "/.WPassword";	/* Where wpbuildUser's password is stored */
#endif /* (pobb_WPUpdate) */
#if (pobb_RunMachines)
char *PMPasswordFile = "/.Password";	/* Where PostmasterName's password is stored */
#endif /* (pobb_RunMachines) */

#if (pobb_RunAMSDelivery)
char *POQueues = "/afs/andrew.cmu.edu/usr0/postman/POQueues";	/* where DESTDIR/bin/mailq gets its info, written by the post offices */

static char *initGlobalMailQueues[] = {
	"q000", "q001", "q002", "q003", /* "q004", "q005", "q006", "q007", */
	"testq0",
	NULL
};
char **GlobalMailQueues = initGlobalMailQueues;
int numGlobalMailQueues = (sizeof(initGlobalMailQueues) / sizeof(initGlobalMailQueues[0])) - 1;

static char *initGlobalSlowMailQueues[] = {
	"sq1", "sq2",
	"testsq1",
	NULL
};
char **GlobalSlowMailQueues = initGlobalSlowMailQueues;
int numGlobalSlowMailQueues = (sizeof(initGlobalSlowMailQueues) / sizeof(initGlobalSlowMailQueues[0])) - 1;
#endif /* (pobb_RunAMSDelivery) */

/* For the .cron files */
#ifdef AFS_ENV
char *CronAuth = "viceii";		/* AUTH=viceii */
#else /* AFS_ENV */
char *CronAuth = "unix";		/* AUTH=unix */
#endif /* AFS_ENV */
char *CronPath = "?[POBBAndrewDir]/bin:/usr/ucb:/bin:/usr/bin:/usr/local/bin:?[POBBAndrewDir]/etc";
#if (pobb_RunMachines)
char *PostmasterHome = "/afs/andrew.cmu.edu/usr0/postman";
#endif /* (pobb_RunMachines) */

#if (pobb_RunAMSDelivery || pobb_NNTPIn)
/* For purging the duplicate DBs */
char *PurgeProgram = "?[POBBAndrewDir]/etc/purgesent";
#endif /* (pobb_RunAMSDelivery || pobb_NNTPIn) */
#if (pobb_RunAMSDelivery)
char *SentKeepLen = "3d";		/* time to keep SENT entries */
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_NNTPIn)
char *NNTPKeepLen = "20d";	/* time to keep NNTP entries */
#endif /* (pobb_NNTPIn) */

#if (pobb_RunMachines)
char *SomeScriptDir = "/afs/andrew.cmu.edu/usr0/postman/bin";	/* Want this to become obsolete--find a better home for these things. */
char *POConsoleDir = "/afs/andrew.cmu.edu/usr0/postman/Consoles";	/* Where some console lacc files live. */
#endif /* (pobb_RunMachines) */

#if (pobb_AutoPost)
char *ViceCUILogsDir = "/afs/andrew.cmu.edu/usr0/postman/logs";	/* Where the collected CUI logs go */
/* Database for bb-daemon statistics */
char *CUIDatabase = "/afs/andrew.cmu.edu/usr0/postman/bb-data";
char *NetBBUser = "netbb";		/* ``user'' in whose acct network BBs are stored */
char *NetBBHome = "/afs/andrew.cmu.edu/usr0/netbb";	/* home dir of that user */
char *InternetUser = "listbb";
#endif /* (pobb_AutoPost) */

#if (pobb_CaptureAddresses)
char *AFSCaptureProcess = "/afs/andrew.cmu.edu/usr0/postman";	/* under which captured addresses are gathered and processed */
int CaptureLifetime = 100 * 24 * 60 * 60;		/* how many seconds to keep addresses */
#endif /* (pobb_CaptureAddresses) */

#if (pobb_RunMachines)
/* Authentication string (no spaces or commas, please) for mail from the network. */
char *NetworkAuthentication = "0;?[WorkstationCell];Network-Mail";
#endif /* (pobb_RunMachines) */

#if (pobb_WPUpdate)
/* Location of some programs built out of the ``wputil'' directory. */
char *genacctspgm = "?[POBBAndrewDir]/etc/genaccts";
char *makebothpgm = "?[POBBAndrewDir]/etc/makeboth";
char *nickgenpgm = "?[POBBAndrewDir]/etc/nickgen";
char *wpbuildscript = "?[POBBAndrewDir]/etc/wpbuild";
#endif /* (pobb_WPUpdate) */

#if (pobb_WPInteractive)
char *wpiupdat = "?[POBBAndrewDir]/etc/wpiupdat";
#endif /* (pobb_WPInteractive) */

#if (pobb_RunAMSDelivery)
/* Dir on every workstation for mail error reports */
char *LocalErrorDir = "/usr/spool/MailErrors";

#ifdef SNAP_ENV
/* Guardian-startup flag file */
char *GuardianFlagFile = "/RemoteGuardianRequests";
char *GuardianOnLocalDisk = "/usr/snap/guardian";
char *MoveLogScriptDir = "/etc";
#endif /* SNAP_ENV */

/* Tells configurables that this is a multi-user system. */
char *MultiUserFlagFile = "/MultiUser";

/* What oldsendmail expects and needs */
char *SendmailConfigDir = "/etc";	/* for sendmail.cf, sendmail.fc in our version */
char *SendmailQueueDir = "/usr/spool/mqueue";	/* where its queue files live */
char *SendmailQueueTime = "30m";		/* how often to run the Sendmail queue */
/* Some abbreviations for the local hosts for use only by the sendmail.cf file: */
static char *initSendmailLocalAbbrevs[] = {
	"andrew",
	"po1", "po2", "po3", "po4", "po5",
	"po6", "po7", "po8",
	NULL
};
char **SendmailLocalAbbrevs = initSendmailLocalAbbrevs;
int numSendmailLocalAbbrevs = (sizeof(initSendmailLocalAbbrevs) / sizeof(initSendmailLocalAbbrevs[0])) - 1;

/* How to invoke a queuemail daemon on a post office machine: */
char *QueuemailCommonArgs = "-D 393216 -d -s -T";
int QueuemailNormalInterval = 120;	/* on an ordinary first-dropoff queue */
int QueuemailSlowInterval = 900;	/* on a hold-for-retry (``slow'') queue */
int QueuemailOutgoingInterval = 7200;	/* Arg to the -O switch for the outgoing daemon */

int POCleanInterval = 3600;		/* Sleep time for the po.clean script */
#endif /* (pobb_RunAMSDelivery) */

#ifdef AFS_ENV
#if (pobb_RunMachines)
int ReauthSleepInterval = 43200;	/* How long ?[POBBAndrewDir]/etc/reauth should sleep between successful ?[POBBAndrewDir]/bin/log runs. */
#endif /* (pobb_RunMachines) */
#endif /* AFS_ENV */

#if (pobb_RunMachines)
/* Now to the the local disk component of the delivery system installation. */
/* Two directories for bigger or smaller programs or scripts. */
char *BigLocalDir = "/usr/start";
char *SmallLocalDir = "/etc";
char *AMSLocalDir = "/usr/ams";

#if (pobb_NNTPIn)
/* Where aux local storage is. */
char *NetDatabaseRoot = "/usr/net";
#endif /* (pobb_NNTPIn) */

#if (pobb_GetHostTable)
/* Where /etc/hosts, /etc/networks, /etc/gateways are updated */
char *IPFilePrefix = "/afs/.?[WorkstationCell]/common/etc";
char *IPFileSuffix = ".arpa";	/* e.g., /etc/hosts comes from /afs/andrew.cmu.edu/common/etc/hosts.arpa */
#endif /* (pobb_GetHostTable) */
#endif /* (pobb_RunMachines) */

#ifdef AFS_ENV
/* Permissions for various things. */
char *generalReadingPublic = "System:AnyUser";
char *generalEnqueueingPublic = "System:AnyUser";
static char *initPostmanDirOwners[] = {
		"postman:MailMavens",
		"?[PostmasterName]",
		NULL
};
char **PostmanDirOwners = initPostmanDirOwners;
int numPostmanDirOwners = (sizeof(initPostmanDirOwners) / sizeof(initPostmanDirOwners[0])) - 1;
static char *initPackageDirOwners[] = {
		"?[arrlist(PostmanDirOwners)]",
		"System:Administrators",
		"sysmaint:ams.commanders",
		NULL
};
char **PackageDirOwners = initPackageDirOwners;
int numPackageDirOwners = (sizeof(initPackageDirOwners) / sizeof(initPackageDirOwners[0])) - 1;
char *PackageAccess = "?[arrlist(PackageDirOwners, \" all \")] all ?[generalReadingPublic] rl";
char *PostmasterAccess = "?[arrlist(PostmanDirOwners, \" all \")] all ?[generalReadingPublic] rl";
char *ConfigDirAccess = "sysmaint:treeadmins rlidwka System:AnyUser rl postman a";
#endif /* AFS_ENV */

/* Now the directories out of which package will download the workstation's local disk. */
char *PackageHome = "/afs/andrew.cmu.edu/wsadmin/postman"; 
/* where the rest of the package source files are */
char *PackageBaseLib = "/afs/.andrew.cmu.edu/wsadmin/lib"; 

char *PackageBigDisk = "%define specialcacheinfo 20000";  /* for big disks */
char *PackageSmallDisk = "%define specialcacheinfo 10000";/* for small disks */
static char *initPackagePredefines[] = {
#if 0
    "beta",
    "betalocal",
#endif
    "kmemmode\troot kmem 644",
    NULL
};
char **PackagePredefines = initPackagePredefines;
int numPackagePredefines = (sizeof(initPackagePredefines) / sizeof(initPackagePredefines[0])) - 1;
static char *initPackagePrefixFiles[] = {
		"?[PackageBaseLib]/base.generic",
		"?[PackageBaseLib]/cmu.readonly",
		NULL
};
char **PackagePrefixFiles = initPackagePrefixFiles;
int numPackagePrefixFiles = (sizeof(initPackagePrefixFiles) / sizeof(initPackagePrefixFiles[0])) - 1;
static char *initPackageSuffixFiles[] = {
		"?[PackageBaseLib]/tree.generic",
		"?[PackageBaseLib]/device.generic",
		NULL
};
char **PackageSuffixFiles = initPackageSuffixFiles;
int numPackageSuffixFiles = (sizeof(initPackageSuffixFiles) / sizeof(initPackageSuffixFiles[0])) - 1;
static char *initPackageSupportedSysTypes[] = {
		"pmax_ul4",
		NULL
};
char **PackageSupportedSysTypes = initPackageSupportedSysTypes;
int numPackageSupportedSysTypes = (sizeof(initPackageSupportedSysTypes) / sizeof(initPackageSupportedSysTypes[0])) - 1;

/* Possible post office and bulletin board server machines */
#if (pobb_RunAMSDelivery)
static char *initPossiblePOs[] = {
		"andrew.cmu.edu",
		"po2.andrew.cmu.edu",
		"po3.andrew.cmu.edu",
		"po4.andrew.cmu.edu",
		"po5.andrew.cmu.edu",
		"po6.andrew.cmu.edu",
		"po7.andrew.cmu.edu",
		"po8.andrew.cmu.edu",
		NULL
};
char **PossiblePOs = initPossiblePOs;
int numPossiblePOs = (sizeof(initPossiblePOs) / sizeof(initPossiblePOs[0])) - 1;
static char *initPossiblePOAddrs[] = {
		"128.2.11.131",
		"128.2.249.105",
		"128.2.12.31",
		"128.2.10.104",
		"128.2.10.105",
		"128.2.10.106",
		"128.2.10.107",
		"128.2.10.108",
		NULL
};
char **PossiblePOAddrs = initPossiblePOAddrs;
int numPossiblePOAddrs = (sizeof(initPossiblePOAddrs) / sizeof(initPossiblePOAddrs[0])) - 1;
static char *initPossiblePOCapas[] = {
		"160",
		"160",
		"160",
		"160",
		"160",
		"160",
		"160",
		"160",
		NULL
};
char **PossiblePOCapas = initPossiblePOCapas;
int numPossiblePOCapas = (sizeof(initPossiblePOCapas) / sizeof(initPossiblePOCapas[0])) - 1;
static char *initPossiblePOHDSizes[] = {
		"70",
		"70",
		"70",
		"40",
		"80",
		"40",
		"40",
		"40",
		NULL
};
char **PossiblePOHDSizes = initPossiblePOHDSizes;
int numPossiblePOHDSizes = (sizeof(initPossiblePOHDSizes) / sizeof(initPossiblePOHDSizes[0])) - 1;
static char *initDeployedPOs[] = {
		"andrew.cmu.edu",
		"po2.andrew.cmu.edu",
		"po3.andrew.cmu.edu",
/*		"po4.andrew.cmu.edu", */
		"po5.andrew.cmu.edu",
/*		"po6.andrew.cmu.edu", */
/*		"po7.andrew.cmu.edu", */
/*		"po8.andrew.cmu.edu", */
		NULL
};
char **DeployedPOs = initDeployedPOs;
int numDeployedPOs = (sizeof(initDeployedPOs) / sizeof(initDeployedPOs[0])) - 1;

char *PrimaryMXHost = "po3.andrew.cmu.edu";

static char *initMXHosts[] = {
                "po2.andrew.cmu.edu",
		"po3.andrew.cmu.edu",
		"po5.andrew.cmu.edu",
		"andrew.cmu.edu",
		NULL
};
char **MXHosts = initMXHosts;
int numMXHosts = (sizeof(initMXHosts) / sizeof(initMXHosts[0])) -1;

static char *initMXWeights[] = {
                "25",
		"25",
		"10",
		"5",
		NULL
};
char **MXWeights = initMXWeights;
int numMXWeights = (sizeof(initMXWeights) / sizeof(initMXWeights[0])) -1;

#endif /* (pobb_RunAMSDelivery) */

#if (pobb_AutoPost)
static char *initPossibleBBs[] = {
		"bb1.andrew.cmu.edu",
		"bb2.andrew.cmu.edu",
		"bb3.andrew.cmu.edu",
		"bb4.andrew.cmu.edu",
		NULL
};
char **PossibleBBs = initPossibleBBs;
int numPossibleBBs = (sizeof(initPossibleBBs) / sizeof(initPossibleBBs[0])) - 1;
static char *initPossibleBBAddrs[] = {
		"128.2.11.197",
		"128.2.12.40",
		"128.2.12.41",
		"128.2.10.204",
		NULL
};
char **PossibleBBAddrs = initPossibleBBAddrs;
int numPossibleBBAddrs = (sizeof(initPossibleBBAddrs) / sizeof(initPossibleBBAddrs[0])) - 1;
static char *initPossibleBBCapas[] = {
		"110",
		"160",
		"100",
		"100",
		NULL
};
char **PossibleBBCapas = initPossibleBBCapas;
int numPossibleBBCapas = (sizeof(initPossibleBBCapas) / sizeof(initPossibleBBCapas[0])) - 1;
static char *initPossibleBBHDSizes[] = {
		"40",
		"40",
		"40",
		"40",
		NULL
};
char **PossibleBBHDSizes = initPossibleBBHDSizes;
int numPossibleBBHDSizes = (sizeof(initPossibleBBHDSizes) / sizeof(initPossibleBBHDSizes[0])) - 1;
static char *initDeployedBBs[] = {
		"bb1.andrew.cmu.edu",
		"bb2.andrew.cmu.edu",
/*		"bb3.andrew.cmu.edu", */
/*		"bb4.andrew.cmu.edu",*/
		NULL
};
char **DeployedBBs = initDeployedBBs;
int numDeployedBBs = (sizeof(initDeployedBBs) / sizeof(initDeployedBBs[0])) - 1;
#endif /* (pobb_AutoPost) */

#if (pobb_NNTPOut)
char *OutnewsID = "outnews";
char *OutnewsHome = "/afs/andrew.cmu.edu/usr0/outnews";
#endif /* (pobb_NNTPOut) */

#if (pobb_AutoPost)
/* Requirements for running the CUI daemons on the bboard machines. */
int CUIDaemonSleepTime = 60;	/* sleep between loops */
#if (pobb_NNTPIn)
int CUINNSleepTime = 10;		/* sleep between loops for netnews */
#endif /* (pobb_NNTPIn) */
#if (pobb_DowJonesIn)
int CUIDJSleepTime = 30;		/* sleep between loops for Dow-Jones */
char *DJUserName = "dowjones";	/* Username for DJ runs */
char *DJLocalDir = "/usr/dowjones";	/* Dir local to workstation for DJ processing */
char *DJHomeDir	= "/afs/andrew.cmu.edu/usr0/dowjones";	/* AFS home dir */
char *DJPassword = "/.DPassword";   /* where local password is */
char *DJPgmPackageSource = "/afs/andrew.cmu.edu/netdev/bin/dj";	/* program to monitor incoming serial-line */
#endif /* (pobb_DowJonesIn) */
int CUIHintsSleepTime = 1020;	/* sleep between loops when just taking hints */

char *CUIPrelude = "set level wizard\nfork\nset terminal 0\nset whatmeworry on";
char *CUIOncePrelude = "set level wizard\nset terminal 0\nset whatmeworry on";

#if (pobb_DowJonesIn)
static char *initCUIDJBoxes[] = {"?[DJHomeDir]/?[MailboxName]", NULL};
char **CUIDJBoxes = initCUIDJBoxes;
int numCUIDJBoxes = (sizeof(initCUIDJBoxes) / sizeof(initCUIDJBoxes[0])) - 1;
#endif /* (pobb_DowJonesIn) */

#if (pobb_NNTPIn)
static char *initCUINetnewsBoxes[] = {"?[NetDatabaseRoot]/Failed", "?[NetDatabaseRoot]/Control", NULL};
char **CUINetnewsBoxes = initCUINetnewsBoxes;
int numCUINetnewsBoxes = (sizeof(initCUINetnewsBoxes) / sizeof(initCUINetnewsBoxes[0])) - 1;

/* Where NetNews is posted at this site. */
static char *initPostedNetnewsRoots[] = {
    /* default line must be first */
    "default ?[EXTERNALSEARCHPATHTEMPLATE]/netnews",
    "cmu.acs drop",
    "cmu.ext drop",
    "alt.sex.pictures ?[EXTERNALSEARCHPATHTEMPLATE]/netnews/alt/binaries/pictures/erotica",
    NULL
};
char **PostedNetnewsRoots = initPostedNetnewsRoots;
int numPostedNetnewsRoots = (sizeof(initPostedNetnewsRoots) / sizeof(initPostedNetnewsRoots[0])) - 1;

char *NetnewsRootName = "netnews";
#endif /* (pobb_NNTPIn) */

#if (pobb_MUServer)
static char *initMUServerBBs[] = {
    "bb2.andrew.cmu.edu",
    NULL
};
char **MUServerBBs = initMUServerBBs;
int numMUServerBBs = (sizeof(initMUServerBBs) / sizeof(initMUServerBBs[0])) - 1;
#endif /* (pobb_MUServer) */

/* Internet mailing list info--can be ignored if Internet lists are not retrieved */
char *PostedInternetRoot = "?[EXTERNALSEARCHPATHTEMPLATE]/internet";
char *InternetRootName = "internet";

static char *initCUIExtBoxes[] = {
#if (pobb_UnDigest)
		"?[UnDigestHome]/ReadyBox",
#endif /* (pobb_UnDigest) */
#if (pobb_NNTPOut)
		"?[OutnewsHome]/?[MailboxName]",
#endif /* (pobb_NNTPOut) */
		"~?[InternetUser]/?[MailboxName]",
		NULL
};
char **CUIExtBoxes = initCUIExtBoxes;
int numCUIExtBoxes = (sizeof(initCUIExtBoxes) / sizeof(initCUIExtBoxes[0])) - 1;

/* Local mailboxes needing high service rates */
static char *initCUILocalHighBoxes[] = {
			"~advisor/?[MailboxName]",
			"~bb/?[MailboxName]",
			"/afs/andrew.cmu.edu/data/spool/ams/acs/?[MailboxName]",
			"~us0s/?[MailboxName]",
                        "~ma55/?[MailboxName]",
#if (pobb_WPInteractive)
			"?[WPIInBox]",
#endif /* (pobb_WPInteractive) */
			NULL
};
char **CUILocalHighBoxes = initCUILocalHighBoxes; 
int numCUILocalHighBoxes = (sizeof(initCUILocalHighBoxes) / sizeof(initCUILocalHighBoxes[0])) - 1;

/* Local mailboxes not needing such a high service rate */
static char *initCUILocalLowBoxes[] = {
			"~ak99/?[MailboxName]",
			"~outbb/?[MailboxName]",
                        /* "~itcfax/?[MailboxName]", */
			"~deptbb/scsbb/?[MailboxName]",
			"~cdec/?[MailboxName]",
			"~dc0m/?[MailboxName]",
			"~itcbb/?[MailboxName]",
			"~deptbb/?[MailboxName]",
			"~deptbb/restrictbb/?[MailboxName]",
			"~deptbb/admbb/?[MailboxName]",
			"~deptbb/csdbb/?[MailboxName]",
			"~deptbb/cmubb/?[MailboxName]",
			"~deptbb/gsiabb/?[MailboxName]",
			"~deptbb/hssbb/?[MailboxName]",
			"~deptbb/unique-bb/?[MailboxName]",
			"~deptbb/libbb/?[MailboxName]",
			"?[PostmasterMailbox]",
			"~deptbb/helpbb/?[MailboxName]",
			"~pcs/?[MailboxName]",
			"~tcpip/?[MailboxName]",
			"/afs/andrew.cmu.edu/usr0/deptbb/andrew-svc/?[MailboxName]",
			NULL
};
char **CUILocalLowBoxes = initCUILocalLowBoxes; 
int numCUILocalLowBoxes = (sizeof(initCUILocalLowBoxes) / sizeof(initCUILocalLowBoxes[0])) - 1;

static char *initCUILocalBoxes[] = {
			"?[arrlist(CUILocalHighBoxes)]",
			"?[arrlist(CUILocalLowBoxes)]",
			NULL
};
char **CUILocalBoxes = initCUILocalBoxes; 
int numCUILocalBoxes = (sizeof(initCUILocalBoxes) / sizeof(initCUILocalBoxes[0])) - 1;

#if (pobb_NNTPIn)
static char *initPostedNetnewsVolRoots[] = {
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp/binaries",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp/lang",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp/os",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp/sources",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp/sys",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/comp/unix",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/misc",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/rec",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/rec/arts",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/rec/games",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/rec/music",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/rec/sport",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/sci",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/soc",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/talk",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/news",
        "?[EXTERNALSEARCHPATHTEMPLATE]/netnews/alt",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/alt/binaries",
	NULL
};
char **PostedNetnewsVolRoots = initPostedNetnewsVolRoots;
int numPostedNetnewsVolRoots = (sizeof(initPostedNetnewsVolRoots) / sizeof(initPostedNetnewsVolRoots[0])) - 1;
#endif /* (pobb_NNTPIn) */

static char *initPostedVolumeRoots[] = {
	"?[LOCALSEARCHPATHTEMPLATE]",
	"?[LOCALSEARCHPATHTEMPLATE]/andrew-demos",
	"?[LOCALSEARCHPATHTEMPLATE]/academic",
/*	"?[LOCALSEARCHPATHTEMPLATE]/andy",   */
	"?[LOCALSEARCHPATHTEMPLATE]/assocs",
	"?[LOCALSEARCHPATHTEMPLATE]/cmu",
	"?[LOCALSEARCHPATHTEMPLATE]/graffiti",
	"?[LOCALSEARCHPATHTEMPLATE]/hobbies",
	"?[LOCALSEARCHPATHTEMPLATE]/magazines",
	"?[OFFICIALSEARCHPATHTEMPLATE]/official",
	"?[LOCALSEARCHPATHTEMPLATE]/org/[A-z]*",
	"?[LOCALSEARCHPATHTEMPLATE]/org/special-projects/data",
	"?[LOCALSEARCHPATHTEMPLATE]/org/special-projects/internal",
	"?[LOCALSEARCHPATHTEMPLATE]/org/special-projects/project",
	"?[LOCALSEARCHPATHTEMPLATE]/org/advisor/trail",
	"?[LOCALSEARCHPATHTEMPLATE]/org/advisor/outbox",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/ASDiv",
        "?[LOCALSEARCHPATHTEMPLATE]/org/macadvisor",
	"?[EXTERNALSEARCHPATHTEMPLATE]/internet/hardware",
	"?[EXTERNALSEARCHPATHTEMPLATE]/internet/lang",
	"?[EXTERNALSEARCHPATHTEMPLATE]/internet/os",
	"?[EXTERNALSEARCHPATHTEMPLATE]/internet/other",
	"?[EXTERNALSEARCHPATHTEMPLATE]/internet/computing",
	"?[EXTERNALSEARCHPATHTEMPLATE]/internet/listserv",
	"/afs/andrew.cmu.edu/usr0/deptbb/.MSGS/ece",
	"?[EXTERNALSEARCHPATHTEMPLATE]",
#if (pobb_NNTPIn)
	"?[arrlist(PostedNetnewsVolRoots)]",
#endif /* (pobb_NNTPIn) */
#if (pobb_DowJonesIn)
	"?[EXTERNALSEARCHPATHTEMPLATE]/dow-jones",
#endif /* (pobb_DowJonesIn) */
	NULL
};
char **PostedVolumeRoots = initPostedVolumeRoots; 
int numPostedVolumeRoots = (sizeof(initPostedVolumeRoots)
					/ sizeof(initPostedVolumeRoots[0])) - 1;

static char *initPurgingCommandsNightly[] = {
#if (pobb_DowJonesIn)
	"?[EXTERNALSEARCHPATHTEMPLATE]/dow-jones/corp/misc 3 days ago",
#endif /* (pobb_DowJonesIn) */
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/alt/binaries/pictures 1 week ago",
	NULL
};
char **PurgingCommandsNightly = initPurgingCommandsNightly; 
int numPurgingCommandsNightly = (sizeof(initPurgingCommandsNightly)
					/ sizeof(initPurgingCommandsNightly[0])) - 1;

static char *initPurgingCommandsWeekly[] = {
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/logs 1 weeks ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/po-clean 2 weeks ago",
#if (pobb_RunAMSDelivery)
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/fwdcheck 2 weeks ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/pocheck 2 weeks ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/sqcheck 2 weeks ago",
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_WPUpdate)
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/pwcheck 2 weeks ago",
#endif /* pobb_WPUpdate */
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/oper-logs 1 month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/oper-stage 2 weeks ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/advisor/outbox 1 weeks ago",
	"?[LOCALSEARCHPATHTEMPLATE]/andrew/wash 1 weeks ago",
	"?[LOCALSEARCHPATHTEMPLATE]/andrew/daemons 2 weeks ago",
	"?[EXTERNALSEARCHPATHTEMPLATE]/dow-jones 1 week ago",
	NULL
};
char **PurgingCommandsWeekly = initPurgingCommandsWeekly; 
int numPurgingCommandsWeekly = (sizeof(initPurgingCommandsWeekly)
					/ sizeof(initPurgingCommandsWeekly[0])) - 1;

static char *initPurgingCommandsBiWeekly[] = {
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman/advisory 3 weeks ago",
#if (pobb_NNTPIn)
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews monday before 3 weeks ago",
        "?[EXTERNALSEARCHPATHTEMPLATE]/netnews/alt monday before 1 week ago",
	"?[EXTERNALSEARCHPATHTEMPLATE]/netnews/alt/sex/pictures monday before 2 week ago",
        "?[EXTERNALSEARCHPATHTEMPLATE]/netnews/soc monday before 2 week ago",
        "?[EXTERNALSEARCHPATHTEMPLATE]/netnews/rec monday before 2 week ago",
        "?[EXTERNALSEARCHPATHTEMPLATE]/netnews/talk monday before 2 week ago",
        "?[EXTERNALSEARCHPATHTEMPLATE]/netnews/news monday before 2 week ago",
#endif /* pobb_NNTPIn */
	NULL
};
char **PurgingCommandsBiWeekly = initPurgingCommandsBiWeekly; 
int numPurgingCommandsBiWeekly = (sizeof(initPurgingCommandsBiWeekly)
					/ sizeof(initPurgingCommandsBiWeekly[0])) - 1;

static char *initPurgingCommandsMonthly[] = {
	"?[PostedInternetRoot] first before one day after one month ago",
	"?[PostedInternetRoot]/info-andrew first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/cs first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/scs first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/psy first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/gsia first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/hss first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/itc first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/postman first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/advisor/helpbox first before one day after two months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/cmu first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/graffiti first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/andrew first before one day after three months ago",
/*	"?[LOCALSEARCHPATHTEMPLATE]/andy first before one day after three months ago",  */
	"?[LOCALSEARCHPATHTEMPLATE]/assocs first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/hobbies first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/magazines first before one day after three months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/cmu/market first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/students first before one day after six months ago",	
	"?[LOCALSEARCHPATHTEMPLATE]/org/ini first before one day after six months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/operations first before one day after six months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/oper-internal first before one day after six months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/oper-ser first before one day after two months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/operations-daily first before one day after six months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/ASDiv first before one day after eleven months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/CSNews first before one day after eleven months ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs first before one day after five months ago",
	"?[EXTERNALSEARCHPATHTEMPLATE]/dow-jones/corp first before one day after six months ago",
	NULL
};
char **PurgingCommandsMonthly = initPurgingCommandsMonthly; 
int numPurgingCommandsMonthly = (sizeof(initPurgingCommandsMonthly)
					/ sizeof(initPurgingCommandsMonthly[0])) - 1;

static char *initPurgingCommandsSemiAnnually[] = {
	"?[LOCALSEARCHPATHTEMPLATE]/academic first",
	"?[LOCALSEARCHPATHTEMPLATE]/org/advisor/discuss first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/students/official first before one day after one month ago",
	"?[LOCALSEARCHPATHTEMPLATE]/org/acs/oper-min first before one day after six months ago",
	"?[OFFICIALSEARCHPATHTEMPLATE] first before one day after six months ago",
        "?[LOCALSEARCHPATHTEMPLATE]/org/gsia/masters/game first before one day after five months ago",
	NULL
};
char **PurgingCommandsSemiAnnually = initPurgingCommandsSemiAnnually; 
int numPurgingCommandsSemiAnnually = (sizeof(initPurgingCommandsSemiAnnually)
					/ sizeof(initPurgingCommandsSemiAnnually[0])) - 1;

#endif /* (pobb_AutoPost) */

#if (pobb_RunMachines)
static char *initBBDMonitorVolumePaths[] = {
#if (pobb_AutoPost)
	"?[arrlist(PostedVolumeRoots)]",
#if (pobb_DowJonesIn)
	"?[arrlist(CUIDJBoxes)]",
#endif /* (pobb_DowJonesIn) */
	"?[arrlist(CUIExtBoxes)]",
	"?[arrlist(CUILocalBoxes)]",
	"?[ViceCUILogsDir]",
	"?[CUIDatabase]",
#endif /* (pobb_AutoPost) */
#if (pobb_WPInteractive)
	"?[WPIInBox]", "?[WPIWorkDir]",
#endif /* (pobb_WPInteractive) */
#if (pobb_WPUpdate)
	"?[wpbuildDir]", "?[WPBackupDir]",
#endif /* (pobb_WPUpdate) */
#if (pobb_UnDigest)
	"?[UnDigestHome]",
#endif /* (pobb_UnDigest) */
#if (pobb_NNTPIn)
	"?[NNTPPollHome]",
#endif /* (pobb_NNTPIn) */
#if (pobb_PublishAccounts)
	"?[AndrewAcctDir]",
#endif /* (pobb_PublishAccounts) */
#if (pobb_CaptureAddresses)
	"?[AFSCaptureProcess]/c-addr", "?[AFSCaptureProcess]/Addrs",
#endif /* (pobb_CaptureAddresses) */
#if (pobb_GetHostTable)
	"?[IPFilePrefix]",
#endif /* (pobb_GetHostTable) */
	"?[PackageHome]",
	"?[BBDaemonDir]",
#if (pobb_TakeHelpStats)
	"?[BBDHelpPrefix]/?[BBDHelpSuffix]",
#endif /* (pobb_TakeHelpStats) */
	"?[CellCommonPrefix]?[WorkstationCell]?[CellCommonSuffix]*",
#if (pobb_RunAMSDelivery)
	"?[CellCommonPrefix]?[WorkstationCell]?[CellCommonSuffix]?[CellCommonMailQueueDirSuffix]/*",
	"?[POQueues]",
#endif /* (pobb_RunAMSDelivery) */
	"?[PostmasterHome]",
	NULL
};
char **BBDMonitorVolumePaths = initBBDMonitorVolumePaths; 
int numBBDMonitorVolumePaths = (sizeof(initBBDMonitorVolumePaths)
					/ sizeof(initBBDMonitorVolumePaths[0])) - 1;

/* Bboard daemon processing directory */
char *BBDaemonDir = "/afs/andrew.cmu.edu/usr0/postman/bb-daemons";

/* BBD script configuration */
char *BBDFormatHeader = "X-Andrew-ScribeFormat: 10";
static char *initBBDBeginBody[] = {
	"\\begindata{text, 1446804}",
	"\\textdsversion{11}",
	"\\template{scribe}",
	NULL
};
char **BBDBeginBody = initBBDBeginBody; 
int numBBDBeginBody = (sizeof(initBBDBeginBody) / sizeof(initBBDBeginBody[0])) - 1;
char *BBDEndBodyLine = "\\enddata{text, 1446804}";

#if (pobb_TakeHelpStats)
char *BBDHelpPrefix = "/afs/andrew.cmu.edu/pmax_ul4/local";
char *BBDHelpSuffix = "depot/helpflaws/common";
#endif /* (pobb_TakeHelpStats) */

/* Stuff for access control on the dedicated machines. */
/* The set of users that will be permitted to authenticate on that machine. */
static char *initUsersToAllow[] = {
	"dc0m", "jm36",
	 "aw0g",
	"operator", "grm", 
#if (pobb_DowJonesIn)
	"?[DJUserName]", "sw0l",
#endif /* (pobb_DowJonesIn) */
	"root",
	"?[PostmasterName]",
#if (pobb_WPUpdate)
	"?[wpbuildUser]",
#endif /* (pobb_WPUpdate) */
	/* Beeper pool */
	"jm36",
	"cn0h",
	"sohan",
	"vanryzin",
	"ww0r",
	"dl2n",
	"wally",
	NULL
};
char **UsersToAllow = initUsersToAllow; 
int numUsersToAllow = (sizeof(initUsersToAllow) / sizeof(initUsersToAllow[0])) - 1;
#endif /* (pobb_RunMachines) */
