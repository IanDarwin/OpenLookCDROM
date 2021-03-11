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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/cmd/RCS/viceop.c,v 2.20 1992/12/15 21:30:21 rr2b R6tape $";
#endif



 

/* **********************************************************************
viceop.c  - Causes console to plot server activity			 

Function
- This program connects to the System Control Machine	 
- retrieves statistics once a minute and plots them	 
- on a console display for each server.			 

**********************************************************************
*/

#include <andrewos.h>
#include <class.h>
#include <conclass.ih>
#include <event.ih>
#include <im.ih>
#include <sitevars.h>
#include <console.h>
#include <errno.h>
#include <signal.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <lwp.h>
#include <rx/xdr.h>
#include <afs/afsint.h>
#endif /* AFS_ENV  */
#include <sys/stat.h>
#include <netdb.h>
#include <netinet/in.h>

void	InitHosts();
void	ReConnect();

extern char *Nullity;

/* the following char* values get used in:
  1.) ../lib/input.c
  2.) ../lib/setup.c
*/
char *RealProgramName = "Vopcon";
char EXTENSION[] = "vop";
char EXTENSION1[] = "vopcon";
char EXTENSION2[] = "Vopcon";

int         activeClusters = 0;

#define MAXCLUSTERS 25

static struct RegionLog RemoteLogs[MAXCLUSTERS];
static struct datum RegionLogDatums[MAXCLUSTERS];

ViceStatistics * stat1[MAXCLUSTERS];
ViceStatistics * stat2[MAXCLUSTERS];

/* these are the instruments that can be monitored */
struct MonitorStats {
    struct datum	ClusterName;
    struct datum	BootTime;
    struct datum	StartTime;
    struct datum	CurrentTime;
    struct datum	Connections;
    struct datum	ViceCalls;
    struct datum	Fetchs;
    struct datum	FetchDatas;
    struct datum	FetchedData;
    struct datum	FetchDataRate;
    struct datum	Stores;
    struct datum	StoreDatas;
    struct datum	StoredData;
    struct datum	StoreDataRate;
    struct datum	RPCBytesSent;
    struct datum	RPCPacketsSent;
    struct datum	RPCPacketsLost;
    struct datum	RPCBytesRcvd;
    struct datum	RPCPacketsRcvd;
    struct datum	RPCBogusPackets;
    struct datum	TotalIO;
    struct datum	TotalCPU;
    struct datum	SystemCPU;
    struct datum	UserCPU;
    struct datum	IdleCPU;
    struct datum	VMInUse;
    struct datum	EtherWrites;
    struct datum	EtherErrors;
    struct datum	EtherInterupts;
    struct datum	EtherGoodReads;
    struct datum	EtherBytesRcvd;
    struct datum	EtherBytesSent;
    struct datum	ProcessSize;
    struct datum	WorkStations;
    struct datum	ActiveWorkStations;
    struct datum	Spare1;
    struct datum	Spare2;
    struct datum	Spare3;
    struct datum	Spare4;
    struct datum	Spare5;
    struct datum	Spare6;
    struct datum	Spare7;
    struct datum	Spare8;
    struct datum	Disk1InUse;
    struct datum	AvailDisk1;
    struct datum	Disk2InUse;
    struct datum	AvailDisk2;
    struct datum	Disk3InUse;
    struct datum	AvailDisk3;
    struct datum	Disk4InUse;
    struct datum	AvailDisk4;
    struct datum	Disk5InUse;
    struct datum	AvailDisk5;
    struct datum	Disk6InUse;
    struct datum	AvailDisk6;
    struct datum	Disk7InUse;
    struct datum	AvailDisk7;
    struct datum	Disk8InUse;
    struct datum	AvailDisk8;
    struct datum	Disk9InUse;
    struct datum	AvailDisk9;
    struct datum	Disk10InUse;
    struct datum	AvailDisk10;
    struct datum	ServerMessages;
};
struct	MonitorStats	dials[MAXCLUSTERS];

struct datum	CentralMachine;  /* central machine datum */

char	host[64][MAXCLUSTERS];
int	bindrc;
struct r_connection * con;
int	NumberOfColumns = 1;

ConfigureMachines(self, Rows, Columns, Machines, Initialize)
struct consoleClass *self;
int *Rows, *Columns, *Machines;
boolean Initialize;
{
    mydbg(("entering: ConfigureMachines\n"));
    if (Initialize){
	ReConnect(self);
	InitHosts(self);
    }
    *Machines = activeClusters;
    *Columns = NumberOfColumns;
    *Rows = (*Machines + (*Columns - 1)) / *Columns;
}

#define MarkAndReturn(x)  { (x).IsDisplaying = TRUE; return (&(x));}

struct datum *BuildDatum(keyword, machine)
char *keyword;
int machine;
{
    mydbg(("entering: BuildDatum\n"));
    if(machine >= activeClusters)
	return((struct datum *)-1);
    if(*keyword == 'a') {
	if(!strcmp(keyword,"availdisk1")) MarkAndReturn((dials[machine].AvailDisk1))
	else if(!strcmp(keyword,"availdisk2")) MarkAndReturn((dials[machine].AvailDisk2))
	else if(!strcmp(keyword,"availdisk3")) MarkAndReturn((dials[machine].AvailDisk3))
	else if(!strcmp(keyword,"availdisk4")) MarkAndReturn((dials[machine].AvailDisk4))
	else if(!strcmp(keyword,"availdisk5")) MarkAndReturn((dials[machine].AvailDisk5))
	else if(!strcmp(keyword,"availdisk6")) MarkAndReturn((dials[machine].AvailDisk6))
	else if(!strcmp(keyword,"availdisk7")) MarkAndReturn((dials[machine].AvailDisk7))
	else if(!strcmp(keyword,"availdisk8")) MarkAndReturn((dials[machine].AvailDisk8))
	else if(!strcmp(keyword,"availdisk9")) MarkAndReturn((dials[machine].AvailDisk9))
	else if(!strcmp(keyword,"availdisk10")) MarkAndReturn((dials[machine].AvailDisk10))
	else if(!strcmp(keyword,"activeworkstations")) MarkAndReturn((dials[machine].ActiveWorkStations))
    }
    if(*keyword == 'b') {
	if(!strcmp(keyword, "boottime")) MarkAndReturn((dials[machine].BootTime))
    }
    if(*keyword == 'c') {
	if(!strcmp(keyword, "clustername")) MarkAndReturn((dials[machine].ClusterName))
	else if(!strcmp(keyword, "centralmachine")) MarkAndReturn(CentralMachine)
	else if(!strcmp(keyword, "connections")) MarkAndReturn((dials[machine].Connections))
	else if(!strcmp(keyword, "currenttime")) MarkAndReturn((dials[machine].CurrentTime))
    }
    if(*keyword == 'd') {
	if(!strcmp(keyword, "disk1inuse")) MarkAndReturn((dials[machine].Disk1InUse))
	else if(!strcmp(keyword, "disk2inuse")) MarkAndReturn((dials[machine].Disk2InUse))
	else if(!strcmp(keyword, "disk3inuse")) MarkAndReturn((dials[machine].Disk3InUse))
	else if(!strcmp(keyword, "disk4inuse")) MarkAndReturn((dials[machine].Disk4InUse))
	else if(!strcmp(keyword, "disk5inuse")) MarkAndReturn((dials[machine].Disk5InUse))
	else if(!strcmp(keyword, "disk6inuse")) MarkAndReturn((dials[machine].Disk6InUse))
	else if(!strcmp(keyword, "disk7inuse")) MarkAndReturn((dials[machine].Disk7InUse))
	else if(!strcmp(keyword, "disk8inuse")) MarkAndReturn((dials[machine].Disk8InUse))
	else if(!strcmp(keyword, "disk9inuse")) MarkAndReturn((dials[machine].Disk9InUse))
	else if(!strcmp(keyword, "disk10inuse")) MarkAndReturn((dials[machine].Disk10InUse))
    }
    if(*keyword == 'e') {
	if(!strcmp(keyword, "etherwrites")) MarkAndReturn((dials[machine].EtherWrites))
	else if(!strcmp(keyword, "ethererrors")) MarkAndReturn((dials[machine].EtherErrors))
	else if(!strcmp(keyword, "etherinterupts")) MarkAndReturn((dials[machine].EtherInterupts))
	else if(!strcmp(keyword, "ethergoodreads")) MarkAndReturn((dials[machine].EtherGoodReads))
	else if(!strcmp(keyword, "etherbytesrcvd")) MarkAndReturn((dials[machine].EtherBytesRcvd))
	else if(!strcmp(keyword, "etherbytessent")) MarkAndReturn((dials[machine].EtherBytesSent))
    }
    if(*keyword == 'f') {
	if(!strcmp(keyword, "fetchs")) MarkAndReturn((dials[machine].Fetchs))
	else if(!strcmp(keyword, "fetchdatas")) MarkAndReturn((dials[machine].FetchDatas))
	else if(!strcmp(keyword, "fetchdatarate")) MarkAndReturn((dials[machine].FetchDataRate))
	else if(!strcmp(keyword, "fetcheddata")) MarkAndReturn((dials[machine].FetchedData))
    }
    if(*keyword == 'i') {
	if(!strcmp(keyword, "idlecpu")) MarkAndReturn((dials[machine].IdleCPU))
    }
    if(*keyword == 'p') {
	if(!strcmp(keyword, "processsize")) MarkAndReturn((dials[machine].ProcessSize))
    }
    if(*keyword == 'r') {
	if(!strcmp(keyword, "rpcbytessent")) MarkAndReturn((dials[machine].RPCBytesSent))
	else if(!strcmp(keyword, "rpcbytesrcvd")) MarkAndReturn((dials[machine].RPCBytesRcvd))
	else if(!strcmp(keyword, "rpcpacketssent")) MarkAndReturn((dials[machine].RPCPacketsSent))
	else if(!strcmp(keyword, "rpcpacketsrcvd")) MarkAndReturn((dials[machine].RPCPacketsRcvd))
	else if(!strcmp(keyword, "rpcpacketslost")) MarkAndReturn((dials[machine].RPCPacketsLost))
	else if(!strcmp(keyword, "rpcboguspackets")) MarkAndReturn((dials[machine].RPCBogusPackets))
	else if(!strcmp(keyword, "remotelog")) {
	    RemoteLogs[machine].WhichDatum = &RegionLogDatums[machine];
	    RegionLogDatums[machine].IsDisplaying = TRUE;
	    return (&RegionLogDatums[machine]);
	}
    }
    if(*keyword == 's') {
	if(!strcmp(keyword, "starttime")) MarkAndReturn((dials[machine].StartTime))
	else if(!strcmp(keyword, "stores")) MarkAndReturn((dials[machine].Stores))
	else if(!strcmp(keyword, "storedatas")) MarkAndReturn((dials[machine].StoreDatas))
	else if(!strcmp(keyword, "storedatarate")) MarkAndReturn((dials[machine].StoreDataRate))
	else if(!strcmp(keyword, "storeddata")) MarkAndReturn((dials[machine].StoredData))
	else if(!strcmp(keyword, "systemcpu")) MarkAndReturn((dials[machine].SystemCPU))
	else if(!strcmp(keyword, "servermessages")) MarkAndReturn((dials[machine].ServerMessages))
	else if(!strcmp(keyword, "spare1")) MarkAndReturn((dials[machine].Spare1))
	else if(!strcmp(keyword, "spare2")) MarkAndReturn((dials[machine].Spare2))
	else if(!strcmp(keyword, "spare3")) MarkAndReturn((dials[machine].Spare3))
	else if(!strcmp(keyword, "spare4")) MarkAndReturn((dials[machine].Spare4))
	else if(!strcmp(keyword, "spare5")) MarkAndReturn((dials[machine].Spare5))
	else if(!strcmp(keyword, "spare6")) MarkAndReturn((dials[machine].Spare6))
	else if(!strcmp(keyword, "spare7")) MarkAndReturn((dials[machine].Spare7))
	else if(!strcmp(keyword, "spare8")) MarkAndReturn((dials[machine].Spare8))
    }
    if(*keyword == 't') {
	if(!strcmp(keyword, "totalio")) MarkAndReturn((dials[machine].TotalIO))
	else if(!strcmp(keyword, "totalcpu")) MarkAndReturn((dials[machine].TotalCPU))
    }
    if(*keyword == 'u') {
	if(!strcmp(keyword, "usercpu")) MarkAndReturn((dials[machine].UserCPU))
    }
    if(*keyword == 'v') {
	if(!strcmp(keyword, "vicecalls")) MarkAndReturn((dials[machine].ViceCalls))
	else if(!strcmp(keyword, "vminuse")) MarkAndReturn((dials[machine].VMInUse))
    }
    if(*keyword == 'w') {
	if(!strcmp(keyword, "workstations")) MarkAndReturn((dials[machine].WorkStations))
    }
    return((struct datum *)-1);
}

OneTimeRemoteInit(self)
    struct consoleClass *self;
{
    PROCESS parentPid;
    int		rc;

    mydbg(("entering: OneTimeRemoteInit\n"));
    InitDisplayBuffers();
    if (rc = LWP_InitializeProcessSupport(LWP_MAX_PRIORITY-2, &parentPid)) {
	char buf[100];
	sprintf(buf, "LWP Init failed %d\n", rc);
	LogIt(0, buf);
	exit(-1);
    }

    IOMGR_Initialize();
    /* initialize rpc package */
    r_nPackets = 10;
    r_nRetries = 30;
    r_Init(0);
}

InitializeInstruments(self)
    struct consoleClass *self;
{ /* not needed for file server console */
    mydbg(("entering: InitializeInstruments\n"));
    return;
}

WakeUp(self) 
    struct consoleClass *self; 
{
    ViceStatistics * current = 0;
    ViceStatistics * previous = 0;
    int     loop;
    int     rc;
    static	int	odd = 1;

    mydbg(("entering: WakeUp\n"));
    if (odd) {
	odd = 0;
    }
    else {
	odd = 1;
    }

    if ((long) con <= 0) {
	ReConnect(self);
	if ((long) con <= 0)
	    return;
    }

    for (loop = 0; loop < activeClusters; loop++) {

	if (odd) {
	    current = stat1[loop];
	    previous = stat2[loop];
	}
	else {
	    current = stat2[loop];
	    previous = stat1[loop];
	}

	if(rc = RControlGetStatistics(con, host[loop], current)) {
	char buf[100];
	    NewValue(self, &CentralMachine, 0, NULL, FALSE);
	    sprintf(buf, "ControlGetStats failed with a %d\n", rc);
	    LogIt(0, buf);
	}
	else {
	    NewValue(self, &CentralMachine, 1, NULL, FALSE);
	}

	if (previous->CurrentTime > 0) {
	    Calculate(self, current, previous, &dials[loop], loop);
	}
    }
    im_EnqueueEvent(WakeUp, self, event_SECtoTU(Period));
}


Calculate(self, current, previous, dial, index)
struct consoleClass *self;
ViceStatistics	* current;
ViceStatistics	* previous;
struct MonitorStats * dial;
int		  index;
{
    int		interval;
    int		total;
    int		subtotal;
    int		fudge;
    int		wval;

    mydbg(("entering: Calculate\n"));
    interval = ((current->CurrentTime - previous->CurrentTime) + 30) / 60;
    dial->ClusterName.RawText = host[index];
    if(interval < 1) {
	NewValue(self, &dial->ClusterName, dial->ClusterName.Value+1, NULL, FALSE);
	return(-1);
    } else {
	NewValue(self, &dial->ClusterName, 0, NULL, FALSE);
    }
    if(dial->ClusterName.Value > 10) {
	consoleClass_WantUpdate(self, self);
    }
    if(dial->BootTime.Value != current->BootTime) {
	if (dial->BootTime.RawText == Nullity) dial->BootTime.RawText = (char *)malloc(50);
	NewValue(self, &dial->BootTime, current->BootTime, ctime(&current->BootTime), FALSE);
    }
    if(dial->StartTime.Value != current->StartTime) {
	if (dial->StartTime.RawText == Nullity) dial->StartTime.RawText = (char *)malloc(50);
	NewValue(self, &dial->StartTime, current->StartTime, ctime(&current->StartTime), FALSE);
    }
    if (dial->CurrentTime.RawText == Nullity) dial->CurrentTime.RawText = (char *)malloc(50);
    NewValue(self, &dial->CurrentTime, current->CurrentTime, ctime(&current->CurrentTime), FALSE);
    NewValue(self, &dial->Connections, current->CurrentConnections, NULL, FALSE);
    NewValue(self, &dial->ViceCalls, (current->TotalViceCalls - previous->TotalViceCalls) / interval, NULL, FALSE);
    NewValue(self, &dial->Fetchs, (current->TotalFetchs - previous->TotalFetchs) / interval, NULL, FALSE);
    NewValue(self, &dial->FetchDatas, (current->FetchDatas - previous->FetchDatas) / interval, NULL, FALSE);
    NewValue(self, &dial->FetchedData, (current->FetchedBytes - previous->FetchedBytes) / interval, NULL, FALSE);
    NewValue(self, &dial->FetchDataRate, current->FetchDataRate, NULL, FALSE);
    NewValue(self, &dial->Stores, (current->TotalStores - previous->TotalStores) /interval, NULL, FALSE);
    NewValue(self, &dial->StoreDatas, (current->StoreDatas - previous->StoreDatas) / interval, NULL, FALSE);
    NewValue(self, &dial->StoredData, (current->StoredBytes - previous->StoredBytes) / interval, NULL, FALSE);
    NewValue(self, &dial->StoreDataRate, current->StoreDataRate, NULL, FALSE);
    NewValue(self, &dial->RPCBytesSent, (current->TotalRPCBytesSent - previous->TotalRPCBytesSent) / interval, NULL, FALSE);
    NewValue(self, &dial->RPCPacketsSent, (current->TotalRPCPacketsSent - previous->TotalRPCPacketsSent) / interval, NULL, FALSE);
    NewValue(self, &dial->RPCPacketsLost, (current->TotalRPCPacketsLost - previous->TotalRPCPacketsLost) / interval, NULL, FALSE);
    NewValue(self, &dial->RPCBytesRcvd, (current->TotalRPCBytesReceived - previous->TotalRPCBytesReceived) / interval, NULL, FALSE);
    NewValue(self, &dial->RPCPacketsRcvd, (current->TotalRPCPacketsReceived - previous->TotalRPCPacketsReceived) /  interval, NULL, FALSE);
    NewValue(self, &dial->RPCBogusPackets, (current->TotalRPCBogusPackets - previous->TotalRPCBogusPackets) / interval, NULL, FALSE);
    total = (current->SystemCPU + current->UserCPU + current->NiceCPU + current->IdleCPU) -
      (previous->SystemCPU + previous->UserCPU + previous->NiceCPU + previous->IdleCPU);
    fudge = (total -1) / 2;
    if(total < 1) total = 1;
    NewValue(self, &dial->SystemCPU, ((100 * (current->SystemCPU - previous->SystemCPU)) + fudge) / total, NULL, FALSE);
    NewValue(self, &dial->UserCPU, ((100 * (current->UserCPU - previous->UserCPU + current->NiceCPU - previous->NiceCPU)) + fudge) / total, NULL, FALSE);
    NewValue(self, &dial->IdleCPU, ((100 * (current->IdleCPU - previous->IdleCPU)) + fudge) / total, NULL, FALSE);
    subtotal = dial->SystemCPU.Value + dial->UserCPU.Value + dial->IdleCPU.Value;
    if(subtotal != 100) {
	if(subtotal > 100) {
	    dial->SystemCPU.Value--;
	}
	else {
	    dial->SystemCPU.Value++;
	}
    }
    NewValue(self, &dial->TotalCPU, dial->UserCPU.Value + dial->SystemCPU.Value, NULL, FALSE);
    NewValue(self, &dial->TotalIO, (current->TotalIO - previous->TotalIO) / interval, NULL, FALSE);
    wval = current->TotalVM;
    if(wval < 1) wval = 1;
    NewValue(self, &dial->VMInUse, (100 * current->ActiveVM) / wval, NULL, FALSE);
    NewValue(self, &dial->EtherWrites, (current->EtherNetTotalWrites - previous->EtherNetTotalWrites) / interval, NULL, FALSE);
    NewValue(self, &dial->EtherErrors, (current->EtherNetTotalErrors - previous->EtherNetTotalErrors) / interval, NULL, FALSE);
    NewValue(self, &dial->EtherInterupts, (current->EtherNetTotalInterupts - previous->EtherNetTotalInterupts) / interval, NULL, FALSE);
    NewValue(self, &dial->EtherGoodReads, (current->EtherNetGoodReads - previous->EtherNetGoodReads) / interval, NULL, FALSE);
    NewValue(self, &dial->EtherBytesRcvd, (current->EtherNetTotalBytesRead - previous->EtherNetTotalBytesRead) / interval, NULL, FALSE);
    NewValue(self, &dial->EtherBytesSent, (current->EtherNetTotalBytesWritten - previous->EtherNetTotalBytesWritten) / interval, NULL, FALSE);
    NewValue(self, &dial->ProcessSize, current->ProcessSize, NULL, FALSE);
    NewValue(self, &dial->WorkStations, current->WorkStations, NULL, FALSE);
    NewValue(self, &dial->ActiveWorkStations, current->ActiveWorkStations, NULL, FALSE);
    NewValue(self, &dial->Spare1, current->Spare1, NULL, FALSE);
    NewValue(self, &dial->Spare2, current->Spare2, NULL, FALSE);
    NewValue(self, &dial->Spare3, current->Spare3, NULL, FALSE);
    NewValue(self, &dial->Spare4, current->Spare4, NULL, FALSE);
    NewValue(self, &dial->Spare5, current->Spare5, NULL, FALSE);
    NewValue(self, &dial->Spare6, current->Spare6, NULL, FALSE);
    NewValue(self, &dial->Spare7, current->Spare7, NULL, FALSE);
    NewValue(self, &dial->Spare8, current->Spare8, NULL, FALSE);

    if(current->Disk1.TotalBlocks > 0) {
	if(strcmp(dial->Disk1InUse.RawText,current->Disk1.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk1InUse.RawText == Nullity) dial->Disk1InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk1InUse, 100 - ((100 * current->Disk1.BlocksAvailable) / current->Disk1.TotalBlocks), current->Disk1.Name, FALSE);
	NewValue(self, &dial->AvailDisk1, current->Disk1.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk1InUse, -1, NULL, FALSE);
    }
    if(current->Disk2.TotalBlocks > 0) {
	if(strcmp(dial->Disk2InUse.RawText,current->Disk2.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk2InUse.RawText == Nullity) dial->Disk2InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk2InUse, 100 - ((100 * current->Disk2.BlocksAvailable) / current->Disk2.TotalBlocks), current->Disk2.Name, FALSE);
	NewValue(self, &dial->AvailDisk2, current->Disk2.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk2InUse, -1, NULL, FALSE);
    }
    if(current->Disk3.TotalBlocks > 0) {
	if(strcmp(dial->Disk3InUse.RawText,current->Disk3.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk3InUse.RawText == Nullity) dial->Disk3InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk3InUse, 100 - ((100 * current->Disk3.BlocksAvailable) / current->Disk3.TotalBlocks), current->Disk3.Name, FALSE);
	NewValue(self, &dial->AvailDisk3, current->Disk3.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk3InUse, -1, NULL, FALSE);
    }
    if(current->Disk4.TotalBlocks > 0) {
	if(strcmp(dial->Disk4InUse.RawText,current->Disk4.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk4InUse.RawText == Nullity) dial->Disk4InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk4InUse, 100 - ((100 * current->Disk4.BlocksAvailable) / current->Disk4.TotalBlocks), current->Disk4.Name, FALSE);
	NewValue(self, &dial->AvailDisk4, current->Disk4.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk4InUse, -1, NULL, FALSE);
    }
    if(current->Disk5.TotalBlocks > 0) {
	if(strcmp(dial->Disk5InUse.RawText,current->Disk5.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk5InUse.RawText == Nullity) dial->Disk5InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk5InUse, 100 - ((100 * current->Disk5.BlocksAvailable) / current->Disk5.TotalBlocks), current->Disk5.Name, FALSE);
	NewValue(self, &dial->AvailDisk5, current->Disk5.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk5InUse, -1, NULL, FALSE);
    }
    if(current->Disk6.TotalBlocks > 0) {
	if(strcmp(dial->Disk6InUse.RawText,current->Disk6.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk6InUse.RawText == Nullity) dial->Disk6InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk6InUse, 100 - ((100 * current->Disk6.BlocksAvailable) / current->Disk6.TotalBlocks), current->Disk6.Name, FALSE);
	NewValue(self, &dial->AvailDisk6, current->Disk6.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk6InUse, -1, NULL, FALSE);
    }
    if(current->Disk7.TotalBlocks > 0) {
	if(strcmp(dial->Disk7InUse.RawText,current->Disk7.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk7InUse.RawText == Nullity) dial->Disk7InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk7InUse, 100 - ((100 * current->Disk7.BlocksAvailable) / current->Disk7.TotalBlocks), current->Disk7.Name, FALSE);
	NewValue(self, &dial->AvailDisk7, current->Disk7.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk7InUse, -1, NULL, FALSE);
    }
    if(current->Disk8.TotalBlocks > 0) {
	if(strcmp(dial->Disk8InUse.RawText,current->Disk8.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk8InUse.RawText == Nullity) dial->Disk8InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk8InUse, 100 - ((100 * current->Disk8.BlocksAvailable) / current->Disk8.TotalBlocks), current->Disk8.Name, FALSE);
	NewValue(self, &dial->AvailDisk8, current->Disk8.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk8InUse, -1, NULL, FALSE);
    }
    if(current->Disk9.TotalBlocks > 0) {
	if(strcmp(dial->Disk9InUse.RawText,current->Disk9.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk9InUse.RawText == Nullity) dial->Disk9InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk9InUse, 100 - ((100 * current->Disk9.BlocksAvailable) / current->Disk9.TotalBlocks), current->Disk9.Name, FALSE);
	NewValue(self, &dial->AvailDisk9, current->Disk9.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk9InUse, -1, NULL, FALSE);
    }
    if(current->Disk10.TotalBlocks > 0) {
	if(strcmp(dial->Disk10InUse.RawText,current->Disk10.Name))
	    consoleClass_WantUpdate(self, self);
	if (dial->Disk10InUse.RawText == Nullity) dial->Disk10InUse.RawText = (char *)malloc(50);
	NewValue(self, &dial->Disk10InUse, 100 - ((100 * current->Disk10.BlocksAvailable) / current->Disk10.TotalBlocks), current->Disk10.Name, FALSE);
	NewValue(self, &dial->AvailDisk10, current->Disk10.BlocksAvailable, NULL, FALSE);
    }
    else {
	NewValue(self, &dial->Disk10InUse, -1, NULL, FALSE);
    }
    return(0);
}

LogIt (level, str)
    int	    level;
    char    * str;
{
    mydbg(("entering: LogIt\n"));
    printf("%s", str);
    fflush(stdout);
}

void	ReConnect(self)
    struct consoleClass *self;
{
    struct stat buff;
    struct hostent *hoste;
    struct servent *porte;
    char	host[65]; /* keep the name for the control machine here */
    long hostn;
    long tportn;	/* temporary for converting port number */
    short portn;
    int fd, i, bytes;

    mydbg(("entering: ReConnect\n"));
    if (con != 0) {
	return;
    }
    NewValue(self, &CentralMachine, 1, NULL, FALSE); /* assume connection will work */
    if(StatusServer != NULL){
	strcpy(host, StatusServer);
    }
    else{
	if((stat("/.scm", &buff)) || ((fd = open("/.scm",O_RDONLY,0)) <= 0))
	    strcpy(host,_SITE_SCM);
	else {
	    if(bytes = read(fd,host,sizeof(host)) <0){
		arrgh(("Vopcon: read(fd, %s, %d) failed - exiting\n", host, sizeof(host)));
	    }
	    close(fd);
	    if(bytes == 0) {
		bytes = sizeof(host) - 1;
            }
	    host[bytes] = '\0';
	    for(i=0;i < bytes;i++) { /* make it a string ending at any white space */
		if(host[i] == '\0' || host[i] == '\n' || host[i] == '\t' || host[i] == ' ') {
		    host[i] = '\0';
		    break;
		}
	    }
	    if(host[0] == '\0')
		strcpy(host,_SITE_SCM);
	}
    }
    if((hoste = gethostbyname(host)) == NULL){
	arrgh(("Vopcon: gethostbyname(%s) failed - exiting\n", host));
	exit(-1);
    }
    if((porte = getservbyname("ropcons", 0)) == NULL){
	arrgh(("Vopcon: getservbyname(\"ropcons\", 0) failed - exiting"));
	exit(-1);
    }
    
    bcopy(hoste->h_addr,&hostn,sizeof(hostn));
    bcopy(&porte->s_port,&tportn,sizeof(tportn));
    /* move the port number */
    portn = ntohl(tportn);
    portn = htons(portn);

    /* connect to the host and save con id */
    con = r_NewConn(hostn,portn);
}

void InitHosts(self)
    struct consoleClass *self;
{
    long    count;
    register int    i;
    char   *ptr;
    BBS list;
    char    data[512];

    mydbg(("entering: InitHosts\n"));
    if (activeClusters == 0) {
	list.MaxSeqLen = 512;
	list.SeqLen = 0;
	list.SeqBody = data;

	while (!con) {
	    ReConnect(self);
	    if (!con)
		sleep(30);
	}
	if (RControlGetServers(con, &count, &list) != 0) {
	    LogIt(0, "GetServers failed - exiting\n");
	    exit(-1);
	}

	activeClusters = count;
	ptr = data;

	for (i = 0; i < activeClusters; i++) {
	    strcpy(host[i], ptr);
	    stat1[i] = (ViceStatistics *) malloc(sizeof(ViceStatistics));
	    stat2[i] = (ViceStatistics *) malloc(sizeof(ViceStatistics));
	    bzero(stat1[i], sizeof(ViceStatistics));
	    bzero(stat2[i], sizeof(ViceStatistics));
	    NewValue(self, &dials[i].ClusterName, 0, host[i], FALSE);
	    ptr += strlen(host[i]) + 1;
	}
    }
}

InitDisplayBuffers()
{
    int     i;
    struct datum    datumdum,
                   *datumptr,
                   *lastdatum;;

    mydbg(("entering: InitDisplayBuffers\n"))
    bzero(&datumdum, sizeof(struct datum));
    datumdum.ValueCtr = DATAMIN;
    datumdum.RawText = Nullity;
    datumptr = (struct datum   *) & dials[0];
    lastdatum = (struct datum  *) & dials[0] + (sizeof (dials) / sizeof (struct datum));
    while (datumptr < lastdatum) {
	bcopy(&datumdum, datumptr++, sizeof(struct datum));
    }
    bzero(&RemoteLogs[0], MAXCLUSTERS * sizeof(struct RegionLog));
    bzero(&RegionLogDatums[0], MAXCLUSTERS * sizeof(struct datum));
    for (i = 0; i <= MAXCLUSTERS; ++i) {
	RemoteLogs[i].WhichDatum = &RegionLogDatums[i];
	RegionLogDatums[i].ValueCtr = DATAMIN;
    }
    mydbg(("Done with Init display buffers\n"));
}
	
struct RegionLog *
WhichErrorLog(machine)
    int machine;
{
    mydbg(("entering: WhichErrorLog\n"));
    return((machine >= MAXCLUSTERS) ? (struct RegionLog *) -1 : &RemoteLogs[machine]);
}


/* The next few functions are defined here as noops to eliminate the need for a half
	dozen of consoles .o files without reorganizing a whole lot of junk. */

CheckClock(self)struct consoleClass *self;{}

CheckPrint(self)struct consoleClass *self;{}

LogMarinerFetchInfo(){}

InitPrint(self) struct consoleClass *self;{}

extern boolean LogErrorsExternally;
extern FILE *ExternalLogFP;

ReportInternalError(self, string)
    struct consoleClass *self;
    char *string;
{
    mydbg(("entering: ReportInternalError\n"));
    if (LogErrorsExternally) {
	fprintf(ExternalLogFP, "vopcon: %s\n", string);
	fflush(ExternalLogFP);
    } else {
	fprintf(stderr, "vopcon: %s\n", string);
	fflush(stderr);
    }
}


/* The following need to be filled in .  -- nsb */
#define MAXCOLUMNS 10

ChooseColumns(numcol)
    int numcol;
{
    mydbg(("entering: ChooseColumns\n"));
    NumberOfColumns = numcol;
    if(NumberOfColumns < 2) NumberOfColumns = 1;
    if(NumberOfColumns > MAXCOLUMNS) NumberOfColumns = MAXCOLUMNS;
}

ChooseMachines(self, machinelist)
    struct consoleClass *self;
    char *machinelist;
{
    char	* prev;
    char	* curr;
    int		  i;

    mydbg(("entering: ChooseMachines\n"));
    if(activeClusters) {
	for (i = 0; i < activeClusters; i++) {
	    strcpy(host[i], "");
	    if(stat1[i]) {
		free(stat1[i]);
		stat1[i] = 0;
	    }
	    if(stat2[i]) {
		free(stat2[i]);
		stat2[i] = 0;
	    }
	}
	activeClusters = 0;
    }
    for(curr = prev = machinelist;curr;prev = curr + 1)
    {
	curr = (char *)index(prev,' ');
	if(curr) *curr = '\0';
	strcpy(host[activeClusters],prev);
	stat1[activeClusters] = (ViceStatistics *)malloc(sizeof(ViceStatistics));
	stat2[activeClusters] = (ViceStatistics *)malloc(sizeof(ViceStatistics));
	bzero(stat1[activeClusters],sizeof(ViceStatistics));
	bzero(stat1[activeClusters],sizeof(ViceStatistics));
	NewValue(self, &dials[activeClusters].ClusterName, 0, host[activeClusters], FALSE);
	activeClusters++;
    }
}

/* More stuff to satisfy the linker */

int OutgoingAge;
char OtherVenusStr[10], FetchVenusStr[10], FinishedVenusStr[10], PrimaryErrorBuffer[10];

VenusNovelty(self)
    struct consoleClass *self;
{
    mydbg(("entering: VenusNovelty\n"));
    ReportInternalError(self, "vopcon:  Vopcon does not use VenusNovelty");
}


InitErrorMonitoring() {}

InitClock() {}

InitMail(self)struct consoleClass *self; {}

CheckMail(self, requested)
struct consoleClass *self;
int requested;
{}

ReInitializeRemoteInstruments() {
    struct datum *datumptr, *lastdatum;;

    mydbg(("entering: ReInitializeRemoteInstruments\n"));
    datumptr = (struct datum *) &dials[0];
    lastdatum = (struct datum *) &dials[0] + (sizeof(dials)/ sizeof(struct datum));
    while (datumptr < lastdatum) {
	datumptr->FirstDisplay = NULL;
	datumptr++;
    }
}
