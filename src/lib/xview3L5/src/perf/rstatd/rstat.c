#include <rpc/rpc.h>
#include <rpcsvc/rstat.h>

int
rstat(host, statp)
  char *host;
  struct statstime *statp;
{
  return callrpc(host, RSTATPROG, RSTATVERS_TIME, RSTATPROC_STATS,
                        xdr_void, (char *)NULL, xdr_statstime, (char *)statp);
}

