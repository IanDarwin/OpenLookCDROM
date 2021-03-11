#include <string.h>

typedef	unsigned char Byte, *PtrByte ;

	/* reverse baHzCiBuffer:
	 */
void	str_ReverseBuffer( baHzCiBuffer )
	PtrByte	baHzCiBuffer ;
{
	PtrByte	bp ;
	PtrByte	bpEnd ;
	Byte	bHeadByte1 ;
	Byte	bHeadByte2 ;

	bp = (PtrByte) baHzCiBuffer ;
	bpEnd = (PtrByte) strchr( baHzCiBuffer, '\0' ) - 1 ;

	while ( bp < bpEnd ) {
		bHeadByte1 = *bp ;
		bHeadByte2 = *(bp+1) ;
		*bp++ = *(bpEnd-1) ;
		*bp++ = *bpEnd ;
		*bpEnd-- = bHeadByte2 ;
		*bpEnd-- = bHeadByte1 ;
	}
	/* debug:
	printf( "%s\n", baHzCiBuffer );
	 */
}

