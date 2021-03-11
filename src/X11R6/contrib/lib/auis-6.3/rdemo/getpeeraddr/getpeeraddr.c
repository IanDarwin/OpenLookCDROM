#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

main()
{
    struct sockaddr_in name;
    int namelen = (sizeof (struct sockaddr_in));

    if (getpeername(0, &name, &namelen)) {
	exit(1);
    }
    printf("%d.%d.%d.%d\n",
	   ((name.sin_addr.s_addr & (0xff << 24)) >> 24),
	   ((name.sin_addr.s_addr & (0xff << 16)) >> 16),
	   ((name.sin_addr.s_addr & (0xff << 8)) >> 8),
	   name.sin_addr.s_addr & 0xff);
    exit(0);
}
