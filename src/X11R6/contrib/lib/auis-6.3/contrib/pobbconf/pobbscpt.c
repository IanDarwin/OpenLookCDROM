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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/pobbconf/RCS/pobbscpt.c,v 1.3 1992/12/15 21:56:26 rr2b R6tape $";
#endif

/* ************************************************************ *\
	pobbscpt.c
	Configure the scripts for the post office and bboard machines.
\* ************************************************************ */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <strings.h>
#include <andyenv.h>
#include <system.h>
#include <util.h>
#include <mailconf.h>
#include <mail.h>
#include <pobbconf.h>
extern int errno;
#define NULL 0

#define LINELENGTH 4000
static FILE *inF = NULL, *outF = NULL;
static char *inFName = NULL, *outFName = NULL;
static int inFLN, outFLN;	/* line numbers */

static int Debugging = 0, JustDumpTables = 0;

static int GlobalErrors;

static char *POBBAndrewDir, *POBBLocalDir;
static char *POBBSourceFile, *POBBDestFile, *POBBRunDate;
static char *POBBRunWarning, *POBBRunNotice;
static char *POBBQuotedPercent = "%";
static char *POBBLastArrlist;

static void SetPOBBValues()
{/* Set the POBBxxx strings. */
    static char DateBuff[100];
    char *X;

    POBBAndrewDir = (char *) AndrewDir(NULL);
    POBBLocalDir = (char *) LocalDir(NULL);

    strcpy(DateBuff, arpadate());
    if (X = index(DateBuff, '\n')) *X = '\0';
    POBBRunDate = DateBuff;

    POBBSourceFile = (inFName != NULL ? inFName : "**no source file**");
    POBBDestFile = (outFName != NULL ? outFName : "**no dest file**");

    POBBRunWarning = "DO NOT EDIT *** FILE AUTOMATICALLY GENERATED *** DO NOT EDIT";
    POBBRunNotice = "This file was generated as ?[POBBDestFile] from ?[POBBSourceFile] on ?[POBBRunDate].";
    POBBLastArrlist = NULL;
}

/* Values sucked in from mailconf and pobbconf */

static struct ConfStrings {
    char *Name;
    char **pValue;
} Strings[] = {
    {"CellCommonPrefix", &CellCommonPrefix},
    {"CellCommonRWSuffix", &CellCommonRWSuffix},
    {"CellCommonSuffix", &CellCommonSuffix},
    {"CellCommonWPDirSuffix", &CellCommonWPDirSuffix},
    {"CellCommonMailQueueDirSuffix", &CellCommonMailQueueDirSuffix},
    {"MailQueueNamePrefix", &MailQueueNamePrefix},
    {"SlowQueueNamePrefix", &SlowQueueNamePrefix},
    {"CellCommonPrintingDirSuffix", &CellCommonPrintingDirSuffix},
    {"CellCommonConfigDirSuffix", &CellCommonConfigDirSuffix},
    {"CellConfigPostmaster", &CellConfigPostmaster},
    {"CellConfigNameSeparator", &CellConfigNameSeparator},
    {"CellConfigMessageServer", &CellConfigMessageServer},
    {"DaemonBB", &DaemonBB},
    {"LogBB", &LogBB},
    {"DaemonLog", &DaemonLog},
    {"TempLog", &TempLog},
#if (pobb_AutoPost && pobb_RunAMSDelivery)
    {"StatsDistList", &StatsDistList},
#endif /* (pobb_AutoPost && pobb_RunAMSDelivery) */
    {"ShutdownSignal", &ShutdownSignal},
#if (pobb_UnDigest)
    {"UnDigestUser", &UnDigestUser},
    {"UnDigestHome", &UnDigestHome},
#endif /* (pobb_UnDigest) */
#if (pobb_NNTPIn)
    {"NNTPPollHome", &NNTPPollHome},
    {"NNTPPeakHours", &NNTPPeakHours},
#endif /* (pobb_NNTPIn) */
#if (pobb_DowJonesIn)
    {"DJUserName", &DJUserName},
    {"DJLocalDir", &DJLocalDir},
    {"DJHomeDir", &DJHomeDir},
    {"DJPassword", &DJPassword},
    {"DJPgmPackageSource", &DJPgmPackageSource},
#endif /* (pobb_DowJonesIn) */
#if (pobb_WPUpdate)
    {"wpbuildUser", &wpbuildUser},
    {"wpbuildHome", &wpbuildHome},
    {"wpbuildDir", &wpbuildDir},
    {"WPBackupDir", &WPBackupDir},
    {"WPPasswdFileName", &WPPasswdFileName},
    {"WPErrorsAddress", &WPErrorsAddress},
    {"WPPasswordFile", &WPPasswordFile},
    {"WPAffilSrc", &WPAffilSrc},
    {"WPAffilMap", &WPAffilMap},
#endif /* (pobb_WPUpdate) */
#if (pobb_WPInteractive)
    {"WPIInBox", &WPIInBox},
    {"WPIWorkDir", &WPIWorkDir},
#if (pobb_AutoPost)
    {"WPIAdminFolderRoot", &WPIAdminFolderRoot},
#endif /* (pobb_AutoPost) */
#endif /* (pobb_WPInteractive) */
#if (pobb_MaybeExperimental)
    {"TestExperimental", &TestExperimental},
    {"SetExperimental", &SetExperimental},
#endif /* (pobb_MaybeExperimental) */
#if (pobb_GetHostTable)
    {"GetHostTableHost", &GetHostTableHost},
    {"GetHostTableDir", &GetHostTableDir},
#endif /* (pobb_GetHostTable) */
#if (pobb_PublishAccounts)
    {"AndrewAcctDir", &AndrewAcctDir},
#endif /* (pobb_PublishAccounts) */
#if (pobb_RunMachines)
    {"PMPasswordFile", &PMPasswordFile},
#endif /* (pobb_RunMachines) */
#if (pobb_RunAMSDelivery)
    {"POQueues", &POQueues},
#endif /* (pobb_RunAMSDelivery) */
    {"CronAuth", &CronAuth},
    {"CronPath", &CronPath},
#if (pobb_RunAMSDelivery || pobb_NNTPIn)
    {"PurgeProgram", &PurgeProgram},
#endif /* (pobb_RunAMSDelivery || pobb_NNTPIn) */
#if (pobb_RunAMSDelivery)
    {"SentKeepLen", &SentKeepLen},
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_NNTPIn)
    {"NNTPKeepLen", &NNTPKeepLen},
#endif /* (pobb_NNTPIn) */

#if (pobb_RunMachines)
    {"SomeScriptDir", &SomeScriptDir},
    {"POConsoleDir", &POConsoleDir},
    {"NetworkAuthentication", &NetworkAuthentication},
#endif /* (pobb_RunMachines) */
#if (pobb_AutoPost)
    {"ViceCUILogsDir", &ViceCUILogsDir},
    {"CUIDatabase", &CUIDatabase},
    {"NetBBUser", &NetBBUser},
    {"NetBBHome", &NetBBHome},
    {"PostedInternetRoot", &PostedInternetRoot},
    {"InternetRootName", &InternetRootName},
    {"InternetUser", &InternetUser},
#endif /* (pobb_AutoPost) */
#if (pobb_CaptureAddresses)
    {"AFSCaptureProcess", &AFSCaptureProcess},
#endif /* (pobb_CaptureAddresses) */
#if (pobb_WPUpdate)
    {"genacctspgm", &genacctspgm},
    {"makebothpgm", &makebothpgm},
    {"nickgenpgm", &nickgenpgm},
    {"wpbuildscript", &wpbuildscript},
#endif /* (pobb_WPUpdate) */
#if (pobb_WPInteractive)
    {"wpiupdat", &wpiupdat},
#endif /* (pobb_WPInteractive) */
#if (pobb_RunAMSDelivery)
    {"LocalErrorDir", &LocalErrorDir},
#ifdef SNAP_ENV
    {"GuardianFlagFile", &GuardianFlagFile},
    {"GuardianOnLocalDisk", &GuardianOnLocalDisk},
    {"MoveLogScriptDir", &MoveLogScriptDir},
#endif /* SNAP_ENV */
    {"MultiUserFlagFile", &MultiUserFlagFile},
    {"SendmailConfigDir", &SendmailConfigDir},
    {"SendmailQueueDir", &SendmailQueueDir},
    {"SendmailQueueTime", &SendmailQueueTime},
    {"QueuemailCommonArgs", &QueuemailCommonArgs},
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_RunMachines)
    {"BigLocalDir", &BigLocalDir},
    {"SmallLocalDir", &SmallLocalDir},
    {"AMSLocalDir", &AMSLocalDir},
#if (pobb_GetHostTable)
    {"IPFilePrefix", &IPFilePrefix},
    {"IPFileSuffix", &IPFileSuffix},
#endif /* (pobb_GetHostTable) */
#if (pobb_NNTPIn)
    {"NetDatabaseRoot", &NetDatabaseRoot},
    {"NetnewsRootName", &NetnewsRootName},
#endif /* (pobb_NNTPIn) */
#endif /* (pobb_RunMachines) */
#ifdef AFS_ENV
    {"generalReadingPublic", &generalReadingPublic},
    {"generalEnqueueingPublic", &generalEnqueueingPublic},
    {"PackageAccess", &PackageAccess},
    {"PostmasterAccess", &PostmasterAccess},
    {"ConfigDirAccess", &ConfigDirAccess},
#endif /* AFS_ENV */
    {"PackageHome", &PackageHome},
    {"PackageBaseLib", &PackageBaseLib},
    {"PackageBigDisk", &PackageBigDisk},
    {"PackageSmallDisk", &PackageSmallDisk},
    {"LocalQueue", &LocalQueue},
    {"ThisDomain", &ThisDomain},
    {"WorkstationCell", &WorkstationCell},
    {"WorkstationName", &WorkstationName},
    {"ViceFile", &ViceFile},
    {"PostmasterMailbox", &PostmasterMailbox},
    {"vicemail", &vicemail},
    {"trymail", &trymail},
    {"switchmail", &switchmail},
    {"queuemail", &queuemail},
    {"logpgm", &logpgm},
    {"arpadatepgm", &arpadatepgm},
    {"nntpxmit", &nntpxmit},
    {"DuplicateDB", &DuplicateDB},
    {"SentSuffix", &SentSuffix},
    {"NNTPSuffix", &NNTPSuffix},
    {"CaptSuffix", &CaptSuffix},
    {"MailboxName", &MailboxName},
    {"ForwardingName", &ForwardingName},
    {"OutgoingName", &OutgoingName},
    {"PostmasterName", &PostmasterName},
    {"PostmasterTitle", &PostmasterTitle},
    {"WPIUpdateReqAddr", &WPIUpdateReqAddr},
    {"Organization", &Organization},
    {"NNTPhost", &NNTPhost},
    {"NNTPuser", &NNTPuser},
    {"DefaultSurfaceAddress", &DefaultSurfaceAddress},
    {"LOCALSEARCHPATHTEMPLATE", &LOCALSEARCHPATHTEMPLATE},
    {"OLDLOCALSEARCHPATHTEMPLATE", &OLDLOCALSEARCHPATHTEMPLATE},
    {"EXTERNALSEARCHPATHTEMPLATE", &EXTERNALSEARCHPATHTEMPLATE},
    {"OLDEXTERNALSEARCHPATHTEMPLATE", &OLDEXTERNALSEARCHPATHTEMPLATE},
    {"OFFICIALSEARCHPATHTEMPLATE", &OFFICIALSEARCHPATHTEMPLATE},
    {"OLDOFFICIALSEARCHPATHTEMPLATE", &OLDOFFICIALSEARCHPATHTEMPLATE},
    {"DEFAULTSEARCHPATHTEMPLATE", &DEFAULTSEARCHPATHTEMPLATE},
    {"GlobalRequiredSubsFile", &GlobalRequiredSubsFile},
    {"GlobalChangeSubsFile", &GlobalChangeSubsFile},
    {"MessagesAutoBugAddress", &MessagesAutoBugAddress},
    {"WelcomeMailFile", &WelcomeMailFile},
    {"EliStartupFile", &EliStartupFile},
    {"oldsendmail", &oldsendmail},
    {"AMS_MailBoxPrefix", &AMS_MailBoxPrefix},
    {"AMS_MailCollectionCommand", &AMS_MailCollectionCommand},
    {"SpoolMailLockTemp", &SpoolMailLockTemp},
    {"SpoolMailLockDir", &SpoolMailLockDir},
    {"POBBAndrewDir", &POBBAndrewDir},
    {"POBBLocalDir", &POBBLocalDir},
    {"POBBSourceFile", &POBBSourceFile},
    {"POBBDestFile", &POBBDestFile},
    {"POBBRunDate", &POBBRunDate},
    {"POBBRunWarning", &POBBRunWarning},
    {"POBBRunNotice", &POBBRunNotice},
    {"QuotedPercent", &POBBQuotedPercent},
    {"LastArrlist", &POBBLastArrlist},
#if (pobb_NNTPOut)
    {"OutnewsID", &OutnewsID},
    {"OutnewsHome", &OutnewsHome},
#endif /* (pobb_NNTPOut) */
#if (pobb_WPInteractive)
#ifdef AFS30_ENV
    {"WPIAdministratorGroup", &WPIAdministratorGroup},
#endif /* AFS30_ENV */
#endif /* (pobb_WPInteractive) */
#if (pobb_AutoPost)
    {"CUIPrelude", &CUIPrelude},
    {"CUIOncePrelude", &CUIOncePrelude},
    {"BBDaemonDir", &BBDaemonDir},
#endif /* (pobb_AutoPost) */
    {"PostmasterHome", &PostmasterHome},
    {"BBDFormatHeader", &BBDFormatHeader},
    {"BBDEndBodyLine", &BBDEndBodyLine},
#if (pobb_RunAMSDelivery)
    {"PrimaryMXHost", &PrimaryMXHost},
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_TakeHelpStats)
    {"BBDHelpPrefix", &BBDHelpPrefix},
    {"BBDHelpSuffix", &BBDHelpSuffix},
#endif /* (pobb_TakeHelpStats) */
};
static int numStrings = sizeof(Strings) / sizeof(Strings[0]);

static struct ConfStringArrays {
    char *Name;
    char ***pValues;
    int *pNumValues;
} StringArrays[] = {
#if (pobb_RunAMSDelivery)
    {"PossiblePOs", &PossiblePOs, &numPossiblePOs},
    {"PossiblePOAddrs", &PossiblePOAddrs, &numPossiblePOAddrs},
    {"PossiblePOCapas", &PossiblePOCapas, &numPossiblePOCapas},
    {"PossiblePOHDSizes", &PossiblePOHDSizes, &numPossiblePOHDSizes},
    {"DeployedPOs", &DeployedPOs, &numDeployedPOs},
    {"MXHosts", &MXHosts, &numMXHosts},
    {"MXWeights", &MXWeights, &numMXWeights},
#endif /* (pobb_RunAMSDelivery) */
#if (pobb_AutoPost)
    {"PossibleBBs", &PossibleBBs, &numPossibleBBs},
    {"PossibleBBAddrs", &PossibleBBAddrs, &numPossibleBBAddrs},
    {"PossibleBBCapas", &PossibleBBCapas, &numPossibleBBCapas},
    {"PossibleBBHDSizes", &PossibleBBHDSizes, &numPossibleBBHDSizes},
    {"DeployedBBs", &DeployedBBs, &numDeployedBBs},
#endif /* (pobb_AutoPost) */
#if (pobb_NNTPIn)
    {"NNTPGroups", &NNTPGroups, &numNNTPGroups},
    {"NNTPDists", &NNTPDists, &numNNTPDists},
    {"NNTPDelayedGroups", &NNTPDelayedGroups, &numNNTPDelayedGroups},
#endif /* (pobb_NNTPIn) */
#if (pobb_MUServer)
    {"MUServerBBs", &MUServerBBs, &numMUServerBBs},
#endif /* (pobb_MUServer) */
#if (pobb_WPInteractive)
    {"WPIAdministrators", &WPIAdministrators, &numWPIAdministrators},
#endif /* (pobb_WPInteractive) */
#ifdef AFS_ENV
    {"PostmanDirOwners", &PostmanDirOwners, &numPostmanDirOwners},
    {"PackageDirOwners", &PackageDirOwners, &numPackageDirOwners},
#endif /* AFS_ENV */
    {"PackagePredefines", &PackagePredefines, &numPackagePredefines},
    {"PackagePrefixFiles", &PackagePrefixFiles, &numPackagePrefixFiles},
    {"PackageSuffixFiles", &PackageSuffixFiles, &numPackageSuffixFiles},
    {"PackageSupportedSysTypes", &PackageSupportedSysTypes, &numPackageSupportedSysTypes},
#if (pobb_RunAMSDelivery)
    {"GlobalMailQueues", &GlobalMailQueues, &numGlobalMailQueues},
    {"GlobalSlowMailQueues", &GlobalSlowMailQueues, &numGlobalSlowMailQueues},
    {"SendmailLocalAbbrevs", &SendmailLocalAbbrevs, &numSendmailLocalAbbrevs},
#endif /* (pobb_RunAMSDelivery) */
    {"lastDitchPrefixes", &lastDitchPrefixes, &nlastditchprefixes},
    {"bailoutPrefixes", &bailoutPrefixes, &nbailoutprefixes},
    {"AMS_ValidDomainSuffixes", &AMS_ValidDomainSuffixes, &numAMS_ValidDomainSuffixes},
    {"AMS_ValidDomainRelays", &AMS_ValidDomainRelays, &numAMS_ValidDomainRelays},
    {"AMS_PrintSpoolDirectories", &AMS_PrintSpoolDirectories, &numAMS_PrintSpoolDirectories},
    {"AMS_NamedValidPrinters", &AMS_NamedValidPrinters, &numAMS_NamedValidPrinters},
#if (pobb_AutoPost)
#if (pobb_DowJonesIn)
    {"CUIDJBoxes", &CUIDJBoxes, &numCUIDJBoxes},
#endif /* (pobb_DowJonesIn) */
    {"CUIExtBoxes", &CUIExtBoxes, &numCUIExtBoxes},
    {"CUILocalHighBoxes", &CUILocalHighBoxes, &numCUILocalHighBoxes},
    {"CUILocalLowBoxes", &CUILocalLowBoxes, &numCUILocalLowBoxes},
    {"CUILocalBoxes", &CUILocalBoxes, &numCUILocalBoxes},
    {"BBDBeginBody", &BBDBeginBody, &numBBDBeginBody},
    {"BBDMonitorVolumePaths", &BBDMonitorVolumePaths, &numBBDMonitorVolumePaths},
    {"PostedVolumeRoots", &PostedVolumeRoots, &numPostedVolumeRoots},
    {"PurgingCommandsNightly", &PurgingCommandsNightly, &numPurgingCommandsNightly},
    {"PurgingCommandsWeekly", &PurgingCommandsWeekly, &numPurgingCommandsWeekly},
    {"PurgingCommandsBiWeekly", &PurgingCommandsBiWeekly, &numPurgingCommandsBiWeekly},
    {"PurgingCommandsMonthly", &PurgingCommandsMonthly, &numPurgingCommandsMonthly},
    {"PurgingCommandsSemiAnnually", &PurgingCommandsSemiAnnually, &numPurgingCommandsSemiAnnually},
#if (pobb_NNTPIn)
    {"CUINetnewsBoxes", &CUINetnewsBoxes, &numCUINetnewsBoxes},
    {"PostedNetnewsRoots", &PostedNetnewsRoots, &numPostedNetnewsRoots},
    {"PostedNetnewsVolRoots", &PostedNetnewsVolRoots, &numPostedNetnewsVolRoots},
#endif /* (pobb_NNTPIn) */
#endif /* (pobb_AutoPost) */
#if (pobb_RunMachines)
    {"UsersToAllow", &UsersToAllow, &numUsersToAllow},
#endif /* (pobb_RunMachines) */
};
static int numStringArrays = sizeof(StringArrays) / sizeof(StringArrays[0]);

/* Configured ints.  Also capture pobb_XXX definitions. */
static int st_val1 = 1;
static int st_val0 = 0;

static struct ConfInts {
    char *Name;
    int *pValue;
} Ints[] = {
    {"pobb_WPUpdate",
#if (pobb_WPUpdate)
    &st_val1
#else /* (pobb_WPUpdate) */
    &st_val0
#endif /* (pobb_WPUpdate) */
    }, {"pobb_RunAMSDelivery",
#if (pobb_RunAMSDelivery)
    &st_val1
#else /* (pobb_RunAMSDelivery) */
    &st_val0
#endif /* (pobb_RunAMSDelivery) */
    }, {"pobb_AutoPost",
#if (pobb_AutoPost)
    &st_val1
#else /* (pobb_AutoPost) */
    &st_val0
#endif /* (pobb_AutoPost) */
    }, {"pobb_RunMachines",
#if (pobb_RunMachines)
    &st_val1
#else /* (pobb_RunMachines) */
    &st_val0
#endif /* (pobb_RunMachines) */
    }, {"pobb_MaybeExperimental",
#if (pobb_MaybeExperimental)
    &st_val1
#else /* (pobb_MaybeExperimental) */
    &st_val0
#endif /* (pobb_MaybeExperimental) */
    }, {"pobb_NNTPIn",
#if (pobb_NNTPIn)
    &st_val1
#else /* (pobb_NNTPIn) */
    &st_val0
#endif /* (pobb_NNTPIn) */
    }, {"pobb_NNTPOut",
#if (pobb_NNTPOut)
    &st_val1
#else /* (pobb_NNTPOut) */
    &st_val0
#endif /* (pobb_NNTPOut) */
    }, {"pobb_UnDigest",
#if (pobb_UnDigest)
    &st_val1
#else /* (pobb_UnDigest) */
    &st_val0
#endif /* (pobb_UnDigest) */
    }, {"pobb_GetHostTable",
#if (pobb_GetHostTable)
    &st_val1
#else /* (pobb_GetHostTable) */
    &st_val0
#endif /* (pobb_GetHostTable) */
    }, {"pobb_PublishAccounts",
#if (pobb_PublishAccounts)
    &st_val1
#else /* (pobb_PublishAccounts) */
    &st_val0
#endif /* (pobb_PublishAccounts) */
    }, {"pobb_GetListOfLists",
#if (pobb_GetListOfLists)
    &st_val1
#else /* (pobb_GetListOfLists) */
    &st_val0
#endif /* (pobb_GetListOfLists) */
    }, {"pobb_CheckBBMailboxSizes",
#if (pobb_CheckBBMailboxSizes)
    &st_val1
#else /* (pobb_CheckBBMailboxSizes) */
    &st_val0
#endif /*(pobb_CheckBBMailboxSizes) */
    }, {"pobb_TakeHelpStats",
#if (pobb_TakeHelpStats)
    &st_val1
#else /* (pobb_TakeHelpStats) */
    &st_val0
#endif /* (pobb_TakeHelpStats) */
    }, {"pobb_CaptureAddresses",
#if (pobb_CaptureAddresses)
    &st_val1
#else /* (pobb_CaptureAddresses) */
    &st_val0
#endif /* (pobb_CaptureAddresses) */
    }, {"pobb_WPInteractive",
#if (pobb_WPInteractive)
    &st_val1
#else /* (pobb_WPInteractive) */
    &st_val0
#endif /* (pobb_WPInteractive) */
    }, {"pobb_ButlerPool",
#if (pobb_ButlerPool)
    &st_val1
#else /* (pobb_ButlerPool) */
    &st_val0
#endif /* (pobb_ButlerPool) */
    }, {"pobb_DowJonesIn",
#if (pobb_DowJonesIn)
    &st_val1
#else /* (pobb_DowJonesIn) */
    &st_val0
#endif /* (pobb_DowJonesIn) */
    }, {"pobb_MUServer",
#if (pobb_MUServer)
    &st_val1
#else /* (pobb_MUServer) */
    &st_val0
#endif /* (pobb_MUServer) */
    /* end of capturing of the pobb_XXX configuration definitions */
    /* Capture some significant andyenv.h definitions */
    }, {"pobbenv_AFS",
#ifdef AFS_ENV
    &st_val1
#else /* AFS_ENV */
    &st_val0
#endif /* AFS_ENV */
    }, {"pobbenv_AFS30",
#ifdef AFS30_ENV
    &st_val1
#else /* AFS30_ENV */
    &st_val0
#endif /* AFS30_ENV */
    }, {"pobbenv_AMS",
#ifdef AMS_ENV
    &st_val1
#else /* AMS_ENV */
    &st_val0
#endif /* AMS_ENV */
    }, {"pobbenv_AIX",
#ifdef AIX_ENV
    &st_val1
#else /* AIX_ENV */
    &st_val0
#endif /* AIX_ENV */
    }, {"pobbenv_AMS_DELIVERY",
#ifdef AMS_DELIVERY_ENV
    &st_val1
#else /* AMS_DELIVERY_ENV */
    &st_val0
#endif /* AMS_DELIVERY_ENV */
    }, {"pobbenv_RUN_AMDS",
#ifdef RUN_AMDS_ENV
    &st_val1
#else /* RUN_AMDS_ENV */
    &st_val0
#endif /* RUN_AMDS_ENV */
    }, {"pobbenv_WHITEPAGES",
#ifdef WHITEPAGES_ENV
    &st_val1
#else /* WHITEPAGES_ENV */
    &st_val0
#endif /* WHITEPAGES_ENV */
    }, {"pobbenv_RESOLVER",
#ifdef RESOLVER_ENV
    &st_val1
#else /* RESOLVER_ENV */
    &st_val0
#endif /* RESOLVER_ENV */
    }, {"pobbenv_SNAP",
#ifdef SNAP_ENV
    &st_val1
#else /* SNAP_ENV */
    &st_val0
#endif /* SNAP_ENV */
    }, {"pobbenv_WM",
#ifdef WM_ENV
    &st_val1
#else /* WM_ENV */
    &st_val0
#endif /* WM_ENV */
    }, {"pobbenv_X11",
#ifdef X11_ENV
    &st_val1
#else /* X11_ENV */
    &st_val0
#endif /* X11_ENV */
    }, {"pobbenv_ANDREW_MALLOC",
#ifdef ANDREW_MALLOC_ENV
    &st_val1
#else /* ANDREW_MALLOC_ENV */
    &st_val0
#endif /* ANDREW_MALLOC_ENV */
    }, {"pobbenv_DEBUG_MALLOC",
#ifdef DEBUG_MALLOC_ENV
    &st_val1
#else /* DEBUG_MALLOC_ENV */
    &st_val0
#endif /* DEBUG_MALLOC_ENV */
    }, {"pobbenv_CMU",
#ifdef CMU_ENV
    &st_val1
#else /* CMU_ENV */
    &st_val0
#endif /* CMU_ENV */
    }, {"pobbenv_ANDREW_PRINTING",
#ifdef ANDREW_PRINTING_ENV
    &st_val1
#else /* ANDREW_PRINTING_ENV */
    &st_val0
#endif /* ANDREW_PRINTING_ENV */
    }, {"pobbenv_DITROFF",
#ifdef DITROFF_ENV
    &st_val1
#else /* DITROFF_ENV */
    &st_val0
#endif /* DITROFF_ENV */
    }, 
    /* End of capturing of some significant andyenv.h definitions */

#if (pobb_UnDigest)
    {"UnDigestSleepTime", &UnDigestSleepTime},
#endif /* (pobb_UnDigest) */
#if (pobb_NNTPIn)
    {"NNTPPollSleepTime", &NNTPPollSleepTime},
#endif /* (pobb_NNTPIn) */
#if (pobb_CaptureAddresses)
    {"CaptureLifetime", &CaptureLifetime},
#endif /* (pobb_CaptureAddresses) */
#if (pobb_RunAMSDelivery)
    {"QueuemailNormalInterval", &QueuemailNormalInterval},
    {"QueuemailSlowInterval", &QueuemailSlowInterval},
    {"QueuemailOutgoingInterval", &QueuemailOutgoingInterval},
    {"POCleanInterval", &POCleanInterval},
#endif /* (pobb_RunAMSDelivery) */
#ifdef AFS_ENV
#if (pobb_RunMachines)
    {"ReauthSleepInterval", &ReauthSleepInterval},
#endif /* (pobb_RunMachines) */
#endif /* AFS_ENV */
    {"AMS_ValidateDestHosts", &AMS_ValidateDestHosts},
    {"AMS_HardHostValidationErrors", &AMS_HardHostValidationErrors},
    {"AMS_DeliveryViaDomainMXAddress", &AMS_DeliveryViaDomainMXAddress},
    {"AMS_DeliveryViaDomainAddress", &AMS_DeliveryViaDomainAddress},
    {"AMS_DeliveryViaGethostbyname", &AMS_DeliveryViaGethostbyname},
    {"AMS_DeliveryViaHostTable", &AMS_DeliveryViaHostTable},
    {"AMS_WSRunsQueuemail", &AMS_WSRunsQueuemail},
    {"AMS_ThisDomainAuthFromWS", &AMS_ThisDomainAuthFromWS},
    {"AMS_DefaultToAFSCellMail", &AMS_DefaultToAFSCellMail},
    {"ThisDomainLen", &ThisDomainLen},
    {"AMS_ViceIsRunning", &AMS_ViceIsRunning},
    {"AMS_DeliverySystem", &AMS_DeliverySystem},
    {"AMS_UseWP", &AMS_UseWP},
    {"AMS_LocalMailSystemExists", &AMS_LocalMailSystemExists},
    {"AMS_UUCPSupported", &AMS_UUCPSupported},
    {"AMS_OnlyMail", &AMS_OnlyMail},
#ifdef NOTDEF
    {"postmaster_uid", &postmaster_uid},
#endif /* NOTDEF */
    {"AMS_InitialDeathKnell", &AMS_InitialDeathKnell},
    {"AMS_MaximumDeathKnell", &AMS_MaximumDeathKnell},
    {"AMS_DemandSeparatingCharacter", &AMS_DemandSeparatingCharacter},
    {"AMS_SeparatingCharacter", &AMS_SeparatingCharacter},
    {"AMS_CheckAddressInSeparatingFrom", &AMS_CheckAddressInSeparatingFrom},
    {"AMS_CheckDateInSeparatingFrom", &AMS_CheckDateInSeparatingFrom},
    {"AMS_AllowColonInSeparatingFrom", &AMS_AllowColonInSeparatingFrom},
    {"AMS_StrictStandaloneLocking", &AMS_StrictStandaloneLocking},
    {"AMS_ViceQueueLifetime", &AMS_ViceQueueLifetime},
    {"AMS_ExtraViceQueueLifetime", &AMS_ExtraViceQueueLifetime},
    {"AMS_ViceQueueLifetimeCap", &AMS_ViceQueueLifetimeCap},
    {"AMS_CrossCellQueueLifetime", &AMS_CrossCellQueueLifetime},
    {"AMS_PrinterNamesInSpoolDirectories", &AMS_PrinterNamesInSpoolDirectories},
    {"AMS_PrinterNamesInPrintcap", &AMS_PrinterNamesInPrintcap},
    {"AMS_OnAIX", &AMS_OnAIX},
    {"AMS_WPValidation", &AMS_WPValidation},
    {"AMS_PasswdValidation", &AMS_PasswdValidation},
    {"AMS_LocalDatabaseValidation", &AMS_LocalDatabaseValidation},
    {"AMS_AliasesValidation", &AMS_AliasesValidation},
    {"AMS_NameSeparator", &AMS_NameSeparator},
    {"AMS_UsersAreGrownups", &AMS_UsersAreGrownups},
    {"AMS_UsersHandleFormatting", &AMS_UsersHandleFormatting},
    {"AMS_DevConsoleIsSacred", &AMS_DevConsoleIsSacred},
    {"AMS_UseShortFileNames", &AMS_UseShortFileNames},
    {"AMS_GecosHacks", &AMS_GecosHacks},
#if (pobb_AutoPost)
    {"CUIDaemonSleepTime", &CUIDaemonSleepTime},
#if (pobb_NNTPIn)
    {"CUINNSleepTime", &CUINNSleepTime},
#endif /* (pobb_NNTPIn) */
#if (pobb_DowJonesIn)
    {"CUIDJSleepTime", &CUIDJSleepTime},
#endif /* (pobb_DowJonesIn) */
    {"CUIHintsSleepTime", &CUIHintsSleepTime},
#endif /* (pobb_AutoPost) */
};
static int numInts = sizeof(Ints) / sizeof(Ints[0]);

/*VARARGS2*/
static void PrErr(fmt, p1, p2, p3, p4, p5)	/* No automatic newline added. */
char *fmt, *p1, *p2, *p3, *p4, *p5;
{
    fprintf(stderr, "%s, line %d: ", inFName, inFLN);
    fprintf(stderr, fmt, p1, p2, p3, p4, p5);
}

static void OpenGlobalFiles()
{
    struct stat inStat;

    inF = fopen(inFName, "r");
    if (inF == NULL) {
	fprintf(stderr, "Cannot open file ``%s'' for reading: %s.\n", inFName, UnixError(errno));
	exit(2);
    }
    if (fstat(fileno(inF), &inStat) < 0) {
	fprintf(stderr, "Cannot get status of input file ``%s'': %s.\n", inFName, UnixError(errno));
	exit(2);
    }
    if ((inStat.st_mode & S_IFMT) !=S_IFREG) {
	fprintf(stderr, "Input file ``%s'' is not a text file, but of type %#o instead.\n", inFName, inStat.st_mode & S_IFMT);
	exit(2);
    }
    inFLN = 1;

    outF = fopen(outFName, "w");
    if (outF == NULL) {
	fprintf(stderr, "Cannot open file ``%s'' for writing: %s.\n", outFName, UnixError(errno));
	exit(3);
    }
    outFLN = 1;
}

static void CloseGlobalFiles()
{
    if (ferror(inF)) {
	fprintf(stderr, "Error reading input file ``%s'': %s.\n", inFName, UnixError(errno));
	exit(2);
    }
    if (fclose(inF) != 0) {
	fprintf(stderr, "Error closing input file ``%s'': %s.\n", inFName, UnixError(errno));
	exit(2);
    }

    if (ferror(outF) || feof(outF)) {
	fprintf(stderr, "Error writing output file ``%s'': %s.\n", outFName, UnixError(errno));
	++GlobalErrors;
    }
    if (GlobalErrors) {
	if (unlink(outFName) == 0)
	    fprintf(stderr, "Output file %s deleted due to errors.\n", outFName);
	else
	    fprintf(stderr, "Attempted to unlink output file %s due to errors, but attempt failed: %s\n",
		    outFName, UnixError(errno));
    }
    if (vfclose(outF) != 0) {
	fprintf(stderr, "Error closing output file ``%s'': %s.\n", outFName, UnixError(errno));
	if (unlink(outFName) == 0)
	    fprintf(stderr, "Output file %s deleted due to errors.\n", outFName);
	else
	    fprintf(stderr, "Attempted to unlink output file %s due to errors, but attempt failed: %s\n",
		    outFName, UnixError(errno));
	exit(3);
    }
}

static void WasFuzzy(procname, key, keymatch)
char *procname, *key, *keymatch;
{
    PrErr("%s: ``%s'' is a mixed-case match for ``%s''.\n",
	   procname, key, keymatch);
}

static int GetIntValue(key, intP, Fuzzy)
char *key; int *intP; int Fuzzy;
{
    int Ix, Res;

    for (Ix = 0; Ix < numInts; ++Ix) {
	if (Fuzzy)
	    Res = ULstrcmp(key, Ints[Ix].Name);
	else
	    Res = strcmp(key, Ints[Ix].Name);
	if (Res == 0) {
	    if (Fuzzy) WasFuzzy("GetIntValue", key, Ints[Ix].Name);
	    *intP = *(Ints[Ix].pValue);
	    return 1;
	}
    }
    return 0;
}

static int GetStringValue(key, strP, Fuzzy)
char *key; char **strP; int Fuzzy;
{
    int Ix, Res;

    for (Ix = 0; Ix < numStrings; ++Ix) {
	if (Fuzzy)
	    Res = ULstrcmp(key, Strings[Ix].Name);
	else
	    Res = strcmp(key, Strings[Ix].Name);
	if (Res == 0) {
	    if (Fuzzy) WasFuzzy("GetStringValue", key, Strings[Ix].Name);
	    *strP = *(Strings[Ix].pValue);
	    return 1;
	}
    }
    return 0;
}

static int GetStringArrayValue(key, strarrP, arrsizeP, Fuzzy)
char *key; char ***strarrP; int *arrsizeP; int Fuzzy;
{
    int Ix, Res;

    for (Ix = 0; Ix < numStringArrays; ++Ix) {
	if (Fuzzy)
	    Res = ULstrcmp(key, StringArrays[Ix].Name);
	else
	    Res = strcmp(key, StringArrays[Ix].Name);
	if (Res == 0) {
	    if (Fuzzy) WasFuzzy("GetStringArrayValue", key, StringArrays[Ix].Name);
	    *strarrP = *(StringArrays[Ix].pValues);
	    *arrsizeP = *(StringArrays[Ix].pNumValues);
	    return 1;
	}
    }
    return 0;
}

static int PrintExpansion(keyB, sepDflt)
char *keyB, *sepDflt;
{/* Recursively expand the string-name in keyB and write the expansion to outF. */
    auto char NumBuff[50];
    auto char KeywordBuf[LINELENGTH];
    auto int Chr, KeyCount;
    auto char *keyP;
    auto enum {wantValue, wantDefined, wantCount, wantStrlen, wantSubscr, wantArrList}
    Wanted = wantValue;
    auto char *key, *keyEnd, *sub, *subEnd, *StringVal, *sepS, *sepD;
    auto char ListSep[500], LSep2[500];
    auto int IntVal, IntSub, IntRes, ConcatLen, addThis, Dum;
    auto char **StringArrayVal;
    auto char *Concat = NULL;
    auto char *OldLastArrlist = POBBLastArrlist;

    key = keyB;
    IntSub = -1;
    addThis = 0;
    while (isspace(*key)) ++key;
    if (strncmp(key, "defined", 7) == 0) {key += 7; Wanted = wantDefined;}
    else if (strncmp(key, "count", 5) == 0) {key += 5; Wanted = wantCount;}
    else if (strncmp(key, "strlen", 6) == 0) {key += 6; Wanted = wantStrlen;}
    else if (strncmp(key, "subscr", 6) == 0) {key += 6; Wanted = wantSubscr;}
    else if (strncmp(key, "arrlist", 7) == 0) {key += 7; Wanted = wantArrList;}
    if (Wanted != wantValue) {	/* see if it's really what's wanted */
	while (isspace(*key)) ++key;
	keyEnd = &key[strlen(key)-1];
	while (isspace(*keyEnd) && keyEnd > key) --keyEnd;
	if (*key != '(' || *keyEnd != ')') {	/* Nope. */
	    Wanted = wantValue;
	} else {			/* skip the parentheses */
	    ++key;
	    while(isspace(*key)) ++key;
	    --keyEnd;
	    while (isspace(*keyEnd) && keyEnd > key) --keyEnd;
	    *++keyEnd = '\0';
	    if (Wanted == wantSubscr) {	/* extract the subscript */
		sub = index(key, ',');
		if (sub != NULL) {
		    keyEnd = sub - 1;
		    while(isspace(*keyEnd) && keyEnd > key) --keyEnd;
		    *++keyEnd = '\0';
		    ++sub;
		    while (isspace(*sub)) ++sub;
		    subEnd = &sub[strlen(sub)-1];
		    while (isspace(*subEnd) && subEnd > sub) --subEnd;
		    *++subEnd = '\0';
		    IntSub = 0;
		    for (subEnd = sub; *subEnd != '\0'; ++subEnd)
			if (!isdigit(*subEnd)) IntSub = 1;
		    if (IntSub) {
			PrErr("subscr: non-digit in subscript ``%s''\n", sub);
			++GlobalErrors;
			Wanted = wantValue;
		    } else {
			IntSub = atoi(sub);
		    }
		} else {
		    PrErr("subscr: No subscript in string ``%s''\n", key);
		    ++GlobalErrors;
		    Wanted = wantValue;
		}
	    } else if (Wanted == wantArrList) {	/* get the separator */
		LSep2[0] = LSep2[1] = '\0';
		sub = index(key, ',');
		if (sub != NULL) {
		    keyEnd = sub - 1;
		    while (isspace(*keyEnd) && keyEnd > key) --keyEnd;
		    *++keyEnd = '\0';
		    ++sub;
		    while (isspace(*sub)) ++sub;
		    subEnd = &sub[strlen(sub)-1];
		    while (isspace(*subEnd) && subEnd > sub) --subEnd;
		    if (*sub == '"' && *subEnd == '"') {
			*subEnd = '\0';
			++sub;
			if ((subEnd - sub) >= sizeof(ListSep)) {
			    PrErr("arrlist: separator ``%s'' too long; using first %d characters\n", sub, sizeof(ListSep)-1);
			    ++GlobalErrors;
			    strncpy(ListSep, sub, sizeof(ListSep)-1);
			} else {
			    strcpy(ListSep, sub);
			}
			sepS = sepD = ListSep;
			for (;*sepS != '\0'; ++sepS) { /* copy over, handling backslashes */
			    if (*sepS == '\\') {
				++sepS;
				switch (*sepS) {
				    case '\\':
					*sepD++ = '\\'; break;
				    case 'b':
					*sepD++ = '\b'; break;
				    case 't':
					*sepD++ = '\t'; break;
				    case 'n':
					*sepD++ = '\n'; break;
				    case 'v':
					*sepD++ = '\v'; break;
				    case 'f':
					*sepD++ = '\f'; break;
				    case 'r':
					*sepD++ = '\r'; break;
				    case 'o':
					*sepD++ = '('; break;
				    case 'c':
					*sepD++ = ')'; break;
				    case 'O':
					*sepD++ = '['; break;
				    case 'C':
					*sepD++ = ']'; break;
				    case 'q':
					*sepD++ = '"'; break;
				    case '2':
					*sepD = '\0';
					sepD = LSep2;
					addThis = 1;
					break;
				    case '3':
					*sepD = '\0';
					sepD = LSep2;
					addThis = 2;
					break;
				    default:
					PrErr("arrlist: Malformed escape in separating string after name ``%s''--unrecoverable.\n", key);
					++GlobalErrors;
					Wanted = wantValue;
					ListSep[0] = ListSep[1] = '\0';
					LSep2[0] = LSep2[1] = '\0';
					addThis = 0;
					sepD = sepS = ListSep;
					break;
				}
			    } else *sepD++ = *sepS;
			}
			*sepD = '\0';
			if (Wanted == wantArrList) sepDflt = ListSep;
		    } else {
			PrErr("arrlist: Badly-formatted separating string after name ``%s''--unrecoverable.\n", key);
			++GlobalErrors;
			Wanted = wantValue;
			ListSep[0] = '\0';
			addThis = 0;
		    }
		} else {
		    strcpy(ListSep, sepDflt);
		}
	    }
	}
	if (Wanted == wantValue) {	/* Recovery from errors */
	    key = keyB;
	    while (isspace(*key)) ++key;
	}
    }
    keyEnd = &key[strlen(key)-1];
    while (isspace(*keyEnd) && keyEnd > key) --keyEnd;
    *++keyEnd = '\0';		/* flush trailing spaces */
    switch (Wanted) {
	case wantValue:
	    if (GetIntValue(key, &IntVal, 0)) {
		sprintf(NumBuff, "%d", IntVal);
		StringVal = NumBuff;
	    } else if (GetStringValue(key, &StringVal, 0)) {
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 0)) {
		PrErr("String ``%s'' is a string array, not a string.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 1)) {
		sprintf(NumBuff, "%d", IntVal);
		StringVal = NumBuff;
	    } else if (GetStringValue(key, &StringVal, 1)) {
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 1)) {
		PrErr("String ``%s'' names a string array, not a string.\n", key);
		++GlobalErrors;
		return 1;
	    } else {
		PrErr("Unrecognized string: ``%s''.\n", key);
		++GlobalErrors;
	    }
	    break;
	case wantDefined:
	    StringVal = NULL;
	    if (GetIntValue(key, &IntVal, 0)) {
		StringVal = NumBuff;
	    } else if (GetStringValue(key, &StringVal, 0)) {
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 0)) {
		StringVal = NumBuff;
	    } else if (GetIntValue(key, &IntVal, 1)) {
		StringVal = NumBuff;
	    } else if (GetStringValue(key, &StringVal, 1)) {
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 1)) {
		StringVal = NumBuff;
	    }
	    if (StringVal == NULL)
		StringVal = "0";
	    else
		StringVal = "1";
	    break;
	case wantCount:
	    if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 0)) {
		IntRes = IntVal;
	    } else if (GetStringValue(key, &StringVal, 0)) {
		PrErr("count: ``%s'' names a string, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 0)) {
		PrErr("count: ``%s'' names an integer, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 1)) {
		IntRes = IntVal;
	    } else if (GetStringValue(key, &StringVal, 1)) {
		PrErr("count: ``%s'' names a string, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 1)) {
		PrErr("count: ``%s'' names an integer, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else {
		PrErr("count: Unrecognized string: ``%s''.\n", key);
		++GlobalErrors;
	    }
	    sprintf(NumBuff, "%d", IntRes);
	    StringVal = NumBuff;
	    break;
	case wantSubscr:
	    if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 0)) {
		if (IntSub >= 0 && IntSub < IntVal) {
		    StringVal = StringArrayVal[IntSub];
		} else {
		    PrErr("subscr: value %d is out of range [0,%d) for array %s.\n",
			  IntSub, IntVal, key);
		    ++GlobalErrors;
		    return 1;
		}
	    } else if (GetStringValue(key, &StringVal, 0)) {
		PrErr("subscr: ``%s'' names a string, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 0)) {
		PrErr("subscr: ``%s'' names an integer, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 1)) {
		if (IntSub >= 0 && IntSub < IntVal) {
		    StringVal = StringArrayVal[IntSub];
		} else {
		    PrErr("subscr: value %d is out of range [0,%d) for array %s.\n",
			  IntSub, IntVal, key);
		    ++GlobalErrors;
		    return 1;
		}
	    } else if (GetStringValue(key, &StringVal, 1)) {
		PrErr("subscr: ``%s'' names a string, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 1)) {
		PrErr("subscr: ``%s'' names an integer, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else {
		PrErr("subscr: Unrecognized string: ``%s''.\n", key);
		++GlobalErrors;
	    }
	    break;
	case wantStrlen:
	    if (GetStringValue(key, &StringVal, 0)) {
		IntRes = strlen(StringVal);
	    } else if (GetIntValue(key, &IntVal, 0)) {
		PrErr("strlen: ``%s'' is an integer, not a string.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 0)) {
		PrErr("strlen: ``%s'' is a string array, not a string.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetStringValue(key, &StringVal, 1)) {
		IntRes = strlen(StringVal);
	    } else if (GetIntValue(key, &IntVal, 1)) {
		PrErr("strlen: ``%s'' is an integer, not a string.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 1)) {
		PrErr("strlen: ``%s'' is a string array, not a string.\n", key);
		++GlobalErrors;
		return 1;
	    } else {
		PrErr("strlen: Unrecognized string: ``%s''.\n", key);
		++GlobalErrors;
	    }
	    sprintf(NumBuff, "%d", IntRes);
	    StringVal = NumBuff;
	    break;
	case wantArrList:
	    if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 0)) {
		/* fall through */
	    } else if (GetStringValue(key, &StringVal, 0)) {
		PrErr("arrlist: ``%s'' names a string, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 0)) {
		PrErr("arrlist: ``%s'' names an integer, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetStringArrayValue(key, &StringArrayVal, &IntVal, 1)) {
		/* fall through */
	    } else if (GetStringValue(key, &StringVal, 1)) {
		PrErr("arrlist: ``%s'' names a string, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else if (GetIntValue(key, &IntVal, 1)) {
		PrErr("arrlist: ``%s'' names an integer, not a string array.\n", key);
		++GlobalErrors;
		return 1;
	    } else {
		PrErr("arrlist: Unrecognized string: ``%s''.\n", key);
		++GlobalErrors;
		return 1;
	    }
	    ConcatLen = 1;
	    Dum = strlen(ListSep);
	    if (addThis) Dum += strlen(LSep2);
	    for (IntSub = 0; IntSub < IntVal; ++IntSub)
		ConcatLen += (strlen(StringArrayVal[IntSub]));
	    if (addThis == 1) ConcatLen *= 2;
	    else if (addThis == 2) ConcatLen += (IntVal * strlen(OldLastArrlist));
	    ConcatLen += (IntVal * Dum);
	    Concat = (char *) malloc(ConcatLen);
	    if (Concat == NULL) {
		PrErr("arrlist: Out of memory; need %d bytes.\n", ConcatLen);
		++GlobalErrors;
		return 1;
	    }
	    Concat[0] = '\0';
	    POBBLastArrlist = "";
	    if (IntVal > 0) {
		POBBLastArrlist = StringArrayVal[0];
		strcpy(Concat, POBBLastArrlist);
	    }
	    for (IntSub = 1; IntSub < IntVal; ++IntSub) {
		strcat(Concat, ListSep);
		if (addThis) {
		    strcat(Concat, (addThis == 1 ? POBBLastArrlist : OldLastArrlist));
		    strcat(Concat, LSep2);
		}
		POBBLastArrlist = StringArrayVal[IntSub];
		strcat(Concat, POBBLastArrlist);
	    }
	    StringVal = Concat;
	    break;
    }

    while ((Chr = *StringVal++) != '\0') {
	switch (Chr) {
	    case '?':
		Chr = *StringVal++;
		if (Chr == '?') goto NormalChar;
		else if (Chr != '[') {
		    PrErr("Improper character following question-mark: ");
		    fprintf(stderr, "``%c'' (%#o); passed through verbatim.\n", Chr, Chr);
		    if (Chr == '\n') {++outFLN;}
		    goto NormalChar;
		}
		keyP = KeywordBuf;
		KeyCount = sizeof(KeywordBuf) - 1;
		for (;;) {		/* Read the string name, expand, print, and continue */
		    Chr = *StringVal++;
		    if (Chr == '\0') {
			PrErr("Unnatural termination in string name\n");
			++GlobalErrors;
			if (Concat != NULL) free(Concat);
			return 2;
		    } else if (Chr == '\n') {
			PrErr("Newline in string name\n");
		    } else if (Chr == ']') {
			auto char *oldLAL = POBBLastArrlist;
			*keyP = '\0';
			if (PrintExpansion(KeywordBuf, sepDflt)) {
			    if (Concat != NULL) free(Concat);
			    return 2;
			}
			POBBLastArrlist = oldLAL;
			break;	/* Return to ordinary processing */
		    } else {
			if (KeyCount <= 0) {
			    PrErr("String name too long\n");
			    ++GlobalErrors;
			    if (Concat != NULL) free(Concat);
			    return 2;
			}
			*keyP++ = Chr;
		    }
		}
		break;
	    case '\n':
		++outFLN;
		/* fall through */
	    default:
		NormalChar:
		  if (putc(Chr, outF) == EOF) {
		      fprintf(stderr, "Error writing to output file %s: %s\n",
			      outFName, UnixError(errno));
		      unlink(outFName);
		      if (Concat != NULL) free(Concat);
		      return 3;
		  }
		break;
	}
    }
    if (Concat != NULL) free(Concat);
    return 0;
}

static void ProcessInputFile()
{/* Read the file from inF, writing the macro-expanded version to outF. */
    auto char KeywordBuf[LINELENGTH];
    int Chr, KeyCount;
    char *keyP;

    while ((Chr = getc(inF)) != EOF) {
	switch (Chr) {
	    case '?':
		Chr = getc(inF);
		if (Chr == '?') goto NormalChar;
		else if (Chr != '[') {
		    PrErr("Improper character following question-mark: ");
		    if (isprint(Chr)) fprintf(stderr, "``%c'' (%#o)", Chr, Chr);
		    else fprintf(stderr, "%#o", Chr);
		    fprintf(stderr, "; passed through verbatim.\n");
		    if (Chr == '\n') {++inFLN; ++outFLN;}
		    goto NormalChar;
		}
		keyP = KeywordBuf;
		KeyCount = sizeof(KeywordBuf) - 1;
		for (;;) {		/* Read the string name, expand, print, and continue */
		    Chr = getc(inF);
		    if (Chr == EOF) {
			PrErr("EOF in string name\n");
			++GlobalErrors;
			return;
		    } else if (Chr == '\n') {
			PrErr("Newline in string name\n");
			++inFLN;
		    } else if (Chr == ']') {
			*keyP = '\0';
			if (PrintExpansion(KeywordBuf, " ")) return;
			break;	/* Return to ordinary processing */
		    } else {
			if (KeyCount <= 0) {
			    PrErr("String name too long\n");
			    ++GlobalErrors;
			    return;
			}
			*keyP++ = Chr;
		    }
		}
		break;
	    case '\n':
		++inFLN;  ++outFLN;
		/* fall through */
	    default:
		NormalChar:
		  if (putc(Chr, outF) == EOF) {
		      fprintf(stderr, "Error writing to output file %s: %s\n", outFName, UnixError(errno));
		      unlink(outFName);
		      return;
		  }
		break;
	}
    }
}

static void DumpTables()
{
    int Ix, subIx;

    if (numStrings > 0) {
	printf("%d configured strings:\n", numStrings);
	for (Ix = 0; Ix < numStrings; ++Ix) {
	    printf("%s:	``%s''\n", Strings[Ix].Name, *(Strings[Ix].pValue));
	}
	printf("\n");
    }

    if (numInts > 0) {
	printf("%d configured ints:\n", numInts);
	for (Ix = 0; Ix < numInts; ++Ix) {
	    printf("%s:	%d\n", Ints[Ix].Name, *(Ints[Ix].pValue));
	}
	printf("\n");
    }

    if (numStringArrays > 0) {
	printf("%d configured string arrays:\n", numStringArrays);
	for (Ix = 0; Ix < numStringArrays; ++Ix) {
	    printf("%s, size %d:\n", StringArrays[Ix].Name, *(StringArrays[Ix].pNumValues));
	    for (subIx = 0; subIx < *(StringArrays[Ix].pNumValues); ++subIx) {
		printf("    [%d]: ``%s''\n", subIx, (*(StringArrays[Ix].pValues))[subIx] );
	    }
	}
	printf("\n");
    }
}

static void ParseArguments(argc,argv)
int argc; char **argv;
{
    static char UsageArgs[] = " [-dD] [-M[val]] src object";
    int thisarg, argval;
    char *Swch;

    inFName = outFName = NULL;

    for (thisarg = 1; thisarg < argc && argv[thisarg][0] == '-'; ++thisarg) {
	Swch = &argv[thisarg][1];
	if (strcmp(Swch, "d") == 0) Debugging = 1;
	else if (strcmp(Swch, "D") == 0) Debugging = 2;
	else if (strcmp(Swch, "T") == 0) JustDumpTables = 1;
	else if (*Swch == 'M') {
#ifdef ANDREW_MALLOC_ENV
	    argval = 4;	/* default value */
	    ++Swch;
	    if (*Swch >= '0' && *Swch <= '9') argval = atoi(Swch);
	    else if (*Swch != '\0') {
		fprintf(stderr, "Bad argument to -M: ``%s''\nusage: %s %s\n", Swch, argv[0], UsageArgs);
		exit(1);
	    }
	    (void) SetMallocCheckLevel(argval);
#else /* #ifdef ANDREW_MALLOC_ENV */
	    fprintf(stderr, "-M is UNUSABLE without ANDREW_MALLOC_ENV defined \n");
#endif /* #ifdef ANDREW_MALLOC_ENV */
	}
	else {
	    fprintf(stdout, "Unrecognized option: ``%s''\nusage: %s%s\n", argv[thisarg], argv[0], UsageArgs);
	    exit(1);
	}
    }

    if (thisarg >= argc) {
	fprintf(stderr, "Missing input file name.\nusage: %s %s\n", argv[0], UsageArgs);
	exit(1);
    }
    inFName = argv[thisarg];

    thisarg++;
    if (thisarg >= argc) {
	fprintf(stderr, "Missing output file name.\nusage: %s %s\n", argv[0], UsageArgs);
	exit(1);
    }
    outFName = argv[thisarg];

    thisarg++;
    if (thisarg < argc) {
	fprintf(stderr, "Extra arguments beginning with ``%s''.\nusage: %s %s\n", argv[thisarg], argv[0], UsageArgs);
	exit(1);
    }
}

main (argc, argv)
int argc; char **argv;
{
    ParseArguments(argc,argv);

    GlobalErrors = 0;

    if (CheckAMSConfiguration() != 0) {
	fprintf(stderr, "The AMS Configuration check failed for some reason.\n");
	++GlobalErrors;
    }

    SetPOBBValues();

    if (JustDumpTables) {DumpTables(); exit(GlobalErrors == 0 ? 0 : 1);}

    OpenGlobalFiles();

    ProcessInputFile();

    CloseGlobalFiles();

#ifdef PLUMBFDLEAKS
    fdplumb_SpillGuts();
#endif /* PLUMBFDLEAKS */

    exit(GlobalErrors == 0 ? 0 : 1);	/* exit(1) if any big-scale errors */
}
