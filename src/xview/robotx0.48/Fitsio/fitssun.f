C------------------------------------------------------------------------------
C       The following routines are part of the FITSIO library
C       and are specific to SUN SPARC computers
C------------------------------------------------------------------------------
C   This software was prepared by High Energy Astrophysic Science Archive
C   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
C   shall not, without prior written permission of the U.S. Government,
C   establish a claim to statutory copyright.  The Government and others acting
C   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
C   worldwide license for Government purposes to publish, distribute,
C   translate, copy, exhibit, and perform such material. 
C------------------------------------------------------------------------------
        subroutine ftopnx(funit,fname,oldnew,rwmode,block,status)

C       low-level, machine-dependent routine to create and open a new file 
C       This is the SUN SPARC version.
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       oldnew  i  file status: 0 = open old/existing file; else open new file
C       rwmode  i  file access mode: 0 = readonly; else = read/write
C       block   i  FITS record blocking factor 
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer funit,oldnew,rwmode,block,status
        integer i,ibuff
        character*(*) fname
        character fstat*3
        logical found

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 12)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

C       the following statement forces the BLOCK DATA FTBKDT module to be
C       loaded by the linker if it is not already loaded.
        external ftbkdt

        if (status .gt. 0)return

C       check for valid unit number
        if (funit .lt. 1 .or. funit .gt. 199)then
                status=101
                return
        end if

C       find available buffer slot for this file
        do 10 i=1,nb
                if (bufpnt(i) .eq. 0)then
                        ibuff=i
                        go to 20
                end if
10      continue

C       error: no vacant buffer slots left
        status=102
        return

20      continue
        if (oldnew .eq. 0)then
                fstat='OLD'
C               test if file exists
                inquire(file=fname,exist=found)
                if (.not. found)then
C                       error: file doesn't exist??
                        status=103
                        return
                end if
C               The blocking factor is irrelevant for files on a this machine, 
C               therefore, simply return the default block size.
                block=1
        else
                fstat='NEW'
        end if
        
C       files do not have an intrinsic record length, so just use
C       the standard 2880-byte block length for convenience
        reclen(funit)=2880

        if (rwmode .eq. 0)then
                wrmode(ibuff)=.false.
        else
C               open file with read and write access
                wrmode(ibuff)=.true.
        end if

        open(unit=funit,file=fname,status=fstat,err=900,
     &       recl=2880,form='UNFORMATTED',access='DIRECT')

C       initialize various parameters about the CHDU
        recnum(ibuff)=0
        bytnum(ibuff)=0
        modify(ibuff)=.false.
        chdu(ibuff)=1
        maxhdu(ibuff)=1
        hdstrt(ibuff,1)=0
        hdend(ibuff)=0
        nxthdr(ibuff)=0
C       data start location is undefined
        dtstrt(ibuff)=-1000000000
C       store internal buffer number to use for this file
        bufnum(funit)=ibuff
C       store inverse pointer: tells which unit is attached to this buffer
        bufpnt(ibuff)=funit
        return

C       error opening file:
900     continue
        if (fstat .eq. 'OLD')then
                status=104
        else
                status=105
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgsdt(dd,mm,yy,status)

C       get the current date from the system

C       dd      i  day of the month (1-31)
C       mm      i  month of the year (1-12)
C       yy      i  last 2 digits of the year (1992 = 92, 2001 = 01)

        integer dd,mm,yy,status
        integer iarray(3)

        if (status .gt. 0)return

C       the following is the Sun Fortran routine to get the system date:
        call idate(iarray)
        
        dd=iarray(1)
        mm=iarray(2)
        yy=iarray(3)-(iarray(3)/100)*100

        end
C----------------------------------------------------------------------
        subroutine ftpi2b(ounit,nvals,incre,i2vals,status)

C       Write an array of Integer*2 bytes to the output FITS file.
C       Does any required translation from internal machine format to FITS.

        integer nvals,incre,ounit,status,i,offset
        integer*2 i2vals(nvals)

C       ounit   i  fortran unit number
C       nvals   i  number of pixels in the i2vals array
C       incre   i  byte increment between values
C       i2vals  i*2 array of input integer*2 values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftpbyt(ounit,nvals*2,i2vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-2
                do 10 i=1,nvals
                        call ftpbyt(ounit,2,i2vals(i),status)
                        call ftmoff(ounit,offset,.true.,status)                
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpi4b(ounit,nvals,incre,i4vals,status)

C       Write an array of Integer*4 bytes to the output FITS file.
C       Does any required translation from internal machine format to FITS.

        integer nvals,incre,ounit,status,i,offset
        integer i4vals(nvals)

C       ounit   i  fortran unit number
C       nvals   i  number of pixels in the i4vals array
C       incre   i  byte increment between values
C       i4vals  i  array of input integer*4 values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftpbyt(ounit,nvals*4,i4vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-4
                do 10 i=1,nvals
                        call ftpbyt(ounit,4,i4vals(i),status)
                        call ftmoff(ounit,offset,.true.,status)                
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpr4b(ounit,nvals,incre,r4vals,status)

C       Write an array of Real*4 bytes to the output FITS file.
C       Does any required translation from internal machine format to FITS.

        integer nvals,incre,ounit,status,i,offset
        real r4vals(nvals)

C       ounit   i  fortran unit number
C       nvals   i  number of pixels in the r4vals array
C       incre   i  byte increment between values
C       r4vals  r  array of input real*4 values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftpbyt(ounit,nvals*4,r4vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-4
                do 10 i=1,nvals
                        call ftpbyt(ounit,4,r4vals(i),status)
                        call ftmoff(ounit,offset,.true.,status)                
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpr8b(ounit,nvals,incre,r8vals,status)

C       Write an array of Real*8 bytes to the output FITS file.
C       Does any required translation from internal machine format to FITS.

        integer nvals,incre,ounit,status,i,offset
        double precision r8vals(nvals)

C       r8vals  d  array of input real*8 values
C       nvals   i  number of pixels in the r4vals array
C       ounit   i  fortran unit number
C       incre   i  byte increment between values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftpbyt(ounit,nvals*8,r8vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-8
                do 10 i=1,nvals
                        call ftpbyt(ounit,8,r8vals(i),status)
                        call ftmoff(ounit,offset,.true.,status)                
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgi2b(iunit,nvals,incre,i2vals,status)

C       Read an array of Integer*2 bytes from the input FITS file.
C       Does any required translation from FITS to internal machine format 

        integer nvals,iunit,incre,status,i,offset
        integer*2 i2vals(nvals)

C       iunit   i  fortran unit number
C       nvals   i  number of pixels to read
C       incre   i  byte increment between values
C       i2vals  i*2 output array of integer*2 values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftgbyt(iunit,nvals*2,i2vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-2
                do 10 i=1,nvals
                        call ftgbyt(iunit,2,i2vals(i),status)
                        call ftmoff(iunit,offset,.false.,status)
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgi4b(iunit,nvals,incre,i4vals,status)

C       Read an array of Integer*4 bytes from the input FITS file.
C       Does any required translation from FITS to internal machine format 

        integer nvals,iunit,incre,status,i,offset
        integer i4vals(nvals)

C       iunit   i  fortran unit number
C       nvals   i  number of pixels to read
C       incre   i  byte increment between values
C       i4vals  i  output array of integer values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftgbyt(iunit,nvals*4,i4vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-4
                do 10 i=1,nvals
                        call ftgbyt(iunit,4,i4vals(i),status)
                        call ftmoff(iunit,offset,.false.,status)                
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgr4b(iunit,nvals,incre,r4vals,status)

C       Read an array of Real*4 bytes from the input FITS file.
C       Does any required translation from FITS to internal machine format.

        integer nvals,iunit,incre,status,i,offset
        real r4vals(nvals)

C       iunit   i  fortran unit number
C       nvals   i  number of pixels to read
C       incre   i  byte increment between values
C       r4vals  r  output array of real*4 values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftgbyt(iunit,nvals*4,r4vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-4
                do 10 i=1,nvals
                        call ftgbyt(iunit,4,r4vals(i),status)
                        call ftmoff(iunit,offset,.false.,status)                
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgr8b(iunit,nvals,incre,r8vals,status)

C       Read an array of Real*8 bytes from the input FITS file.
C       Does any required translation from FITS to internal machine format.

        integer nvals,iunit,incre,status,i,offset
        double precision r8vals(nvals)

C       nvals   i  number of pixels to read
C       iunit   i  fortran unit number
C       incre   i  byte increment between values
C       r8vals  d  output array of real*8 values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftgbyt(iunit,nvals*8,r8vals,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-8
                do 10 i=1,nvals
                        call ftgbyt(iunit,8,r8vals(i),status)
                        call ftmoff(iunit,offset,.false.,status)                
10              continue
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftupch(string)

C       convert input string to upper case
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        character*(*) string
        integer i,length

        length=len(string)
        do 10 i=1,length
                if   (string(i:i) .ge. 'a' 
     1          .and. string(i:i) .le. 'z')then
                        string(i:i)=char(ichar(string(i:i))-32)
                end if
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftas2c(array,nchar)

C       convert characters in the array from the ASCII codes
C       to the machine's native character coding sequence 

C       array   c  array of characters to be converted (in place)
C       nchar   i  number of characters to convert

        character*(*) array
        integer nchar

C       This machine uses the ASCII character codes, so no conversion required.
        end
C--------------------------------------------------------------------------
        subroutine ftc2as(nbuff,fchar,lchar)

C       convert characters in the common block buffer from the machines
C       native character coding sequence in to ASCII codes

C       nbuff   i  number of the common block buffer to operate on
C       fchar   i  first character in the buffer to convert
C       lchar   i  last character in the buffer to convert

        integer nbuff,fchar,lchar

C       This machine uses the ASCII character codes, so no conversion required.
        end
C----------------------------------------------------------------------
        subroutine ftpbyt(ounit,nbytes,array,status)

C       write string of data bytes to output buffer.  If buffer fills up,
C       then dump it to the output disk file.

C       ounit   i  fortran unit number
C       nbytes  i  number of bytes
C       array   i  integer array
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer array(*)
        integer nbytes,ounit,status

C       simply call character writing routine:

        call ftpcbf(ounit,0,nbytes,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftgbyt(iunit,nbytes,array,status)

C       read string of data bytes from input buffer.  If buffer becomes
C       empty, then read in another block from the disk file.

C       iunit   i  fortran unit number
C       nbytes  i  number of bytes
C       array   i  integer array
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer nbytes,iunit,status
        integer array(*)

C       simply call character reading routine:

        call ftgcbf(iunit,0,nbytes,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpbyx(ounit,recnum,nbytes,nbuff,status)

C       low-level routine to write bytes to an output file

C       ounit   i  fortran unit number
C       recnum  i  direct access file record number
C       nbytes  i  number of bytes to write
C       nbuff   i  number of the buffer to be written
C       status  i  output error status

        integer ounit,recnum,nbytes,nbuff,status
        character*2880 buff1,buff2,buff3,buff4,buff5,buff6
        character*2880 buff7,buff8,buff9,buff10,buff11,buff12
        common/ft00b1/buff1
        common/ft00b2/buff2
        common/ft00b3/buff3
        common/ft00b4/buff4
        common/ft00b5/buff5
        common/ft00b6/buff6
        common/ft00b7/buff7
        common/ft00b8/buff8
        common/ft00b9/buff9
        common/ft00b10/buff10
        common/ft00b11/buff11
        common/ft00b12/buff12
        
C       Note: we have to use separate buffers, rather than one big array
C       of buffers, because some compilers have a limit on the length of
C       a character array (e.g., 32K).

        if (status .gt. 0)return

        if (nbuff .eq. 1)then
                write(ounit,rec=recnum,err=900)buff1(1:nbytes)
        else if (nbuff .eq. 2)then
                write(ounit,rec=recnum,err=900)buff2(1:nbytes)
        else if (nbuff .eq. 3)then
                write(ounit,rec=recnum,err=900)buff3(1:nbytes)
        else if (nbuff .eq. 4)then
                write(ounit,rec=recnum,err=900)buff4(1:nbytes)
        else if (nbuff .eq. 5)then
                write(ounit,rec=recnum,err=900)buff5(1:nbytes)
        else if (nbuff .eq. 6)then
                write(ounit,rec=recnum,err=900)buff6(1:nbytes)
        else if (nbuff .eq. 7)then
                write(ounit,rec=recnum,err=900)buff7(1:nbytes)
        else if (nbuff .eq. 8)then
                write(ounit,rec=recnum,err=900)buff8(1:nbytes)
        else if (nbuff .eq. 9)then
                write(ounit,rec=recnum,err=900)buff9(1:nbytes)
        else if (nbuff .eq. 10)then
                write(ounit,rec=recnum,err=900)buff10(1:nbytes)
        else if (nbuff .eq. 11)then
                write(ounit,rec=recnum,err=900)buff11(1:nbytes)
        else if (nbuff .eq. 12)then
                write(ounit,rec=recnum,err=900)buff12(1:nbytes)
        end if
        return

900     status=106
        end
C----------------------------------------------------------------------
        subroutine ftgbyx(iunit,recnum,nbytes,nbuff,status)

C       low-level routine to read bytes from a file

C       iunit   i  fortran unit number
C       recnum  i  direct access file record number
C       nbytes  i  number of bytes to read
C       nbuff   i  number of the buffer to read
C       status  i  output error status

        integer iunit,recnum,nbytes,nbuff,status,ios
        character*2880 buff1,buff2,buff3,buff4,buff5,buff6
        character*2880 buff7,buff8,buff9,buff10,buff11,buff12
        common/ft00b1/buff1
        common/ft00b2/buff2
        common/ft00b3/buff3
        common/ft00b4/buff4
        common/ft00b5/buff5
        common/ft00b6/buff6
        common/ft00b7/buff7
        common/ft00b8/buff8
        common/ft00b9/buff9
        common/ft00b10/buff10
        common/ft00b11/buff11
        common/ft00b12/buff12
        
C       Note: we have to use separate buffers, rather than one big array
C       of buffers, because some compilers have a limit on the length of
C       a character array (e.g., 32K).

        if (status .gt. 0)return

C       read the record; if the read fails then initialize the buffer with zeros
        if (nbuff .eq. 1)then
                read(iunit,rec=recnum,iostat=ios)buff1(1:nbytes)
        else if (nbuff .eq. 2)then
                read(iunit,rec=recnum,iostat=ios)buff2(1:nbytes)
        else if (nbuff .eq. 3)then
                read(iunit,rec=recnum,iostat=ios)buff3(1:nbytes)
        else if (nbuff .eq. 4)then
                read(iunit,rec=recnum,iostat=ios)buff4(1:nbytes)
        else if (nbuff .eq. 5)then
                read(iunit,rec=recnum,iostat=ios)buff5(1:nbytes)
        else if (nbuff .eq. 6)then
                read(iunit,rec=recnum,iostat=ios)buff6(1:nbytes)
        else if (nbuff .eq. 7)then
                read(iunit,rec=recnum,iostat=ios)buff7(1:nbytes)
        else if (nbuff .eq. 8)then
                read(iunit,rec=recnum,iostat=ios)buff8(1:nbytes)
        else if (nbuff .eq. 9)then
                read(iunit,rec=recnum,iostat=ios)buff9(1:nbytes)
        else if (nbuff .eq. 10)then
                read(iunit,rec=recnum,iostat=ios)buff10(1:nbytes)
        else if (nbuff .eq. 11)then
                read(iunit,rec=recnum,iostat=ios)buff11(1:nbytes)
        else if (nbuff .eq. 12)then
                read(iunit,rec=recnum,iostat=ios)buff12(1:nbytes)
        end if

C       if we failed to read the record, then just fill the
C       read buffer with all zeros
        if (ios .ne. 0)then
                call ftzero(nbuff,(nbytes+3)/4)
C               assume that this error indicates an end of file condition:
                status=107
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftpcbf(ounit,convrt,nchar,cbuff,status)

C       "Put Character BuFfer"
C       copy input buffer of characters to the output character buffer.
C       If output buffer fills up, then write it out to the disk file.
C       If nchar=0, then simply flush the current buffer to the disk file.
C
C       ounit   i  Fortran output unit number
C       convrt  i  whether (=1) or not (=0) to convert to ASCII
C               (this only applies to machines that do not use the
C                ASCII sequence for their native character representation)
C       nchar   i  number of characters in the string
C       cbuff   c  input character string
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character cbuff*(*)
        integer convrt,ounit,nchar,status

        integer nleft,nbyt,in1,nbuff,buflen,lastb

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 12)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid

        character*2880 buff1,buff2
        character*2880 buff3,buff4,buff5,buff6
        character*2880 buff7,buff8,buff9,buff10,buff11,buff12
        common/ft00b1/buff1
        common/ft00b2/buff2
        common/ft00b3/buff3
        common/ft00b4/buff4
        common/ft00b5/buff5
        common/ft00b6/buff6
        common/ft00b7/buff7
        common/ft00b8/buff8
        common/ft00b9/buff9
        common/ft00b10/buff10
        common/ft00b11/buff11
        common/ft00b12/buff12
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        if (status .gt. 0)return

        nbuff=bufnum(ounit)
        buflen=reclen(ounit)

        if (nchar .gt. 0)then

C       lastb   = position of last byte read from input buffer
C       nleft   = number of bytes left in the input buffer
C       in1     = position of first byte remaining in the input buffer
C       nbyt    = number of bytes to transfer from input to output

        lastb=bytnum(nbuff)
        nleft=nchar
        in1=1

C       find the number of bytes that will fit in output buffer
20      nbyt=min(nleft,buflen-lastb)
        if (nbyt .gt. 0)then
C           append the input buffer to the output buffer
            if (nbuff .eq. 1)then
                buff1(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 2)then
                buff2(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 3)then
                buff3(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 4)then
                buff4(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 5)then
                buff5(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 6)then
                buff6(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 7)then
                buff7(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 8)then
                buff8(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 9)then
                buff9(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 10)then
                buff10(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 11)then
                buff11(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            else if (nbuff .eq. 12)then
                buff12(lastb+1:lastb+nbyt)=cbuff(in1:in1+nbyt-1)
            end if

C           convert the characters to ASCII, if necessary
            if (convrt .ne. 0)call ftc2as(nbuff,lastb+1,lastb+nbyt)

            modify(nbuff)=.true.
            bytnum(nbuff)=bytnum(nbuff)+nbyt
            lastb=lastb+nbyt
            in1=in1+nbyt
            nleft=nleft-nbyt
        end if

C       process more bytes, if any
        if (nleft .gt. 0)then
            if (lastb .eq. buflen)then
                if (modify(nbuff))then
C                   write out full buffer to disk, then reinitialize
                  call ftpbyx(ounit,recnum(nbuff),buflen,nbuff,status)
                    if (status .gt. 0)return
                    modify(nbuff)=.false.
                end if

C               attempt to read the next record into buffer (there may
C               not be a next record, so a read error is not serious)
                recnum(nbuff)=recnum(nbuff)+1
                call ftgbyx(ounit,recnum(nbuff),buflen,nbuff,status)
                if (status .gt. 0)then
                        status =0
                        modify(nbuff)=.true.
                end if
                lastb=0
                go to 20
            end if
        end if
C       store current buffer location
        bytnum(nbuff)=lastb

        else if (nchar .eq. 0)then
C               simply dump the partially full buffer to disk, and reinitialize
                if (modify(nbuff))then
                  call ftpbyx(ounit,recnum(nbuff),buflen,nbuff,status)
                  modify(nbuff)=.false.
                end if
                recnum(nbuff)=0
                bytnum(nbuff)=0
        else 
C               error: negative number of bytes to write
                status=306
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftgcbf(iunit,convrt,nchar,array,status)

C       "Get Character BuFfer"
C       read NCHAR characters from the character buffer.
C       If buffer is empty, then read in another block of data from the
C       disk file.
C       If nchar=0, then simply flush out any remaining characters in the
C           the current block of data.
C
C       iunit   i  Fortran unit number for reading from disk
C       convrt  i  whether (=1) or not (=0) to convert from ASCII
C               (this only applies to machines that do not use the
C                ASCII sequence for their native character representation)
C       nchar   i  number of characters to read
C       array   c  output character string
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,convrt,nchar,status
        character*(*) array
        integer nleft,nbyt,lastb,in1,nbuff,buflen

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 12)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid

        character*2880 buff1,buff2
        character*2880 buff3,buff4,buff5,buff6
        character*2880 buff7,buff8,buff9,buff10,buff11,buff12
        common/ft00b1/buff1
        common/ft00b2/buff2
        common/ft00b3/buff3
        common/ft00b4/buff4
        common/ft00b5/buff5
        common/ft00b6/buff6
        common/ft00b7/buff7
        common/ft00b8/buff8
        common/ft00b9/buff9
        common/ft00b10/buff10
        common/ft00b11/buff11
        common/ft00b12/buff12
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        if (status .gt. 0)return

        if (nchar .lt. 0)then
C               error: negative number of bytes to read
                status=306
                return
        end if

        nbuff=bufnum(iunit)
        buflen=reclen(iunit)

C       lastb   = position of last byte read from input buffer
C       nleft   = number of bytes left in the input buffer
C       in1     = position of first byte remaining in the input buffer
C       nbyt    = number of bytes to transfer from input to output

        lastb=bytnum(nbuff)
        nleft=nchar
        in1=1

C       find the number of remaining bytes that can be read from buffer
10      nbyt=min(nleft,buflen-lastb)
C       append characters from the buffer to the output string
        if (nbyt .gt. 0)then
            if (nbuff .eq. 1)then
                array(in1:in1+nbyt-1)=buff1(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 2)then
                array(in1:in1+nbyt-1)=buff2(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 3)then
                array(in1:in1+nbyt-1)=buff3(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 4)then
                array(in1:in1+nbyt-1)=buff4(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 5)then
                array(in1:in1+nbyt-1)=buff5(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 6)then
                array(in1:in1+nbyt-1)=buff6(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 7)then
                array(in1:in1+nbyt-1)=buff7(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 8)then
                array(in1:in1+nbyt-1)=buff8(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 9)then
                array(in1:in1+nbyt-1)=buff9(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 10)then
                array(in1:in1+nbyt-1)=buff10(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 11)then
                array(in1:in1+nbyt-1)=buff11(lastb+1:lastb+nbyt)
            else if (nbuff .eq. 12)then
                array(in1:in1+nbyt-1)=buff12(lastb+1:lastb+nbyt)
            end if

C           store the total number of bytes read:
            lastb=lastb+nbyt
            in1=in1+nbyt
            nleft=nleft-nbyt
        end if
        if (nleft .gt. 0)then
C               read in the next record, but first, check if the current
C               record has been modified.  If so, write it to disk.
                if (modify(nbuff))then
                 call ftpbyx(iunit,recnum(nbuff),buflen,nbuff,status)
                 if (status .gt. 0)return
                 modify(nbuff)=.false.
                end if

C               now read new record from disk
                recnum(nbuff)=recnum(nbuff)+1
                call ftgbyx(iunit,recnum(nbuff),buflen,nbuff,status)
                if (status .gt. 0)return

                lastb=0
C               go back for more bytes
                go to 10
        end if

C       convert the array of characters to ASCII, if required
        if (convrt .ne. 0)call ftas2c(array,nchar)

C       save the current position in the read buffer
        bytnum(nbuff)=lastb
        end
C------------------------------------------------------------------------
        subroutine ftzero(nbuff,nwords)

C       fill the common block buffer with zeros, as efficiently as possible

        integer nbuff,nwords,i,nw2

        double precision buff1,buff2
        double precision buff3,buff4,buff5,buff6
        double precision buff7,buff8
        double precision buff9,buff10,buff11,buff12
        common /ft00b1/buff1(360)
        common /ft00b2/buff2(360)
        common /ft00b3/buff3(360)
        common /ft00b4/buff4(360)
        common /ft00b5/buff5(360)
        common /ft00b6/buff6(360)
        common /ft00b7/buff7(360)
        common /ft00b8/buff8(360)
        common /ft00b9/buff9(360)
        common /ft00b10/buff10(360)
        common /ft00b11/buff11(360)
        common /ft00b12/buff12(360)

        nw2=(nwords+1)/2
        if (nbuff .eq. 1)then
                do 10 i=1,nw2
10              buff1(i)=0
        else if (nbuff .eq. 2)then
                do 20 i=1,nw2
20              buff2(i)=0
        else if (nbuff .eq. 3)then
                do 30 i=1,nw2
30              buff3(i)=0
        else if (nbuff .eq. 4)then
                do 40 i=1,nw2
40              buff4(i)=0
        else if (nbuff .eq. 5)then
                do 50 i=1,nw2
50              buff5(i)=0
        else if (nbuff .eq. 6)then
                do 60 i=1,nw2
60              buff6(i)=0
        else if (nbuff .eq. 7)then
                do 70 i=1,nw2
70              buff7(i)=0
        else if (nbuff .eq. 8)then
                do 80 i=1,nw2
80              buff8(i)=0
        else if (nbuff .eq. 9)then
                do 90 i=1,nw2
90              buff9(i)=0
        else if (nbuff .eq. 10)then
                do 100 i=1,nw2
100              buff10(i)=0
        else if (nbuff .eq. 11)then
                do 110 i=1,nw2
110              buff11(i)=0
        else if (nbuff .eq. 12)then
                do 120 i=1,nw2
120              buff12(i)=0
        end if
        end
