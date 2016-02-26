C------------------------------------------------------------------------------
C   This software was prepared by High Energy Astrophysic Science Archive
C   Research Center (HEASARC) at the NASA Goddard Space Flight Center. Users
C   shall not, without prior written permission of the U.S. Government,
C   establish a claim to statutory copyright.  The Government and others acting
C   on its behalf, shall have a royalty-free, non-exclusive, irrevocable,
C   worldwide license for Government purposes to publish, distribute,
C   translate, copy, exhibit, and perform such material. 
C------------------------------------------------------------------------------
        subroutine ftvers(vernum)

C       Returns the current revision number of the FITSIO package.
C       The revision number will be incremented whenever any modifications,
C       bug fixes, or enhancements are made to the package

        real vernum

C       version 3.20 - 30 Mar   1992
C       version 3.10 -  4 Nov   1991
C       version 3.01 - 27 Sept  1991
C       version 3.00 - 12 Sept  1991
C       version 2.99 - 24 July  1991
C       version 2.0  -  1 May   1991
C       version 1.3  -  2 April 1991
C       version 1.22 - 22 March 1991
C       version 1.21 - 20 March 1991

        vernum=3.203
        end
C------------------------------------------------------------------------------
        subroutine ftgerr(errnum,text)

C       Return a descriptive error message corresponding to the error number

C       errnum i  input symbolic error code presumably returned by another
C                 FITSIO subroutine
C       text   C*30  Descriptive error message

        integer errnum
        character*(*) text

C       nerror specifies the maxinum number of different error messages
        integer nerror
        parameter (nerror=80)
        character*30 errors(nerror)
        character*30 er1(10),er2(10),er3(10),er4(10),er5(10),er6(10)
        character*30 er7(10),er8(10)
        integer i,ncode,errcod(nerror)
        save errors

C       we equivalence the big array to several smaller ones, so that
C       the DATA statements will not have too many continuation lines.
        equivalence (errors(1), er1(1))
        equivalence (errors(11),er2(1))
        equivalence (errors(21),er3(1))
        equivalence (errors(31),er4(1))
        equivalence (errors(41),er5(1))
        equivalence (errors(51),er6(1))
        equivalence (errors(61),er7(1))
        equivalence (errors(71),er8(1))
 
        data errcod/0,101,102,103,104,105,106,107,108,109,110,111,
     &  201,202,203,204,205,206,207,208,209,211,212,213,214,215,216,
     &  217,218,221,222,223,224,225,226,227,228,229,230,231,232,
     &  241,251,252,261,262,
     &  302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,
     &  317,318,319,    401,402,403,404,405,406,407,408,409,411,
     &  112,210,233,-1,0,0/

        data er1/
     & 'OK, no error',
     & 'Bad logical unit no. (1-99)',
     & 'Too many FITS files opened',
     & 'File not found; not opened',
     & 'Error opening existing file',
     & 'Error creating new FITS file',
     & 'Error writing to FITS file',
     & 'EOF while reading FITS file',
     & 'Error reading FITS file',
     & 'Bad blocking factor (1-28800)'/

        data er2/
     & 'Error closing FITS file',
     & 'Too many columns in table',
     & 'No room in header for keyword',
     & 'Specified keyword not found',  
     & 'Bad keyword record number', 
     & 'Keyword value field is blank',
     & 'Missing quote in string value',
     & 'Could not construct NAMEnnn',
     & 'Bad character in keyword name',
     & 'Keywords out of order?'/

        data er3/
     & 'Bad nnn value in NAMEnnn',
     & 'Illegal BITPIX keyword value',
     & 'Illegal NAXIS keyword value',
     & 'Illegal NAXISnnn keyword value',
     & 'Illegal PCOUNT keyword value',
     & 'Illegal GCOUNT keyword value',
     & 'Illegal TFIELDS keyword value',
     & 'Illegal NAXIS1 keyword value',
     & 'Illegal NAXIS2 keyword value',
     & 'SIMPLE keyword not found'/

        data er4/
     & 'BITPIX keyword not found',
     & 'NAXIS  keyword not found',
     & 'NAXISnnn keywords not found',
     & 'XTENSION keyword not found',
     & 'CHDU is not an ASCII table',
     & 'CHDU is not a binary table', 
     & 'PCOUNT keyword not found',
     & 'GCOUNT keyword not found',
     & 'TFIELDS keyword not found',
     & 'TBCOLnnn keywords not found'/

        data er5/
     & 'TFORMnnn keywords not found',
     & 'Row width not = field widths',
     & 'Unknown extension type',
     & 'Unknown FITS record type',
     & 'Cannot parse TFORM keyword',
     & 'Unknown TFORM datatype code',
     & 'Primary array must be 1st HDU',
     & 'Data structure not defined',
     & 'Negative file record number',
     & 'HDU start location is unknown'/

        data er6/
     & 'Requested no. of bytes < 0',
     & 'Illegal first row number',
     & 'Illegal first element number',
     & 'Bad TFORM for Character I/O',
     & 'Bad TFORM for Logical I/O',
     & 'Invalid ASCII table TFORM code',
     & 'Invalid BINTABLE TFORM code',
     & 'Error making formated string',
     & 'Null value is undefined',
     & 'Internal read error of string'/

        data er7/
     & 'Illegal logical column value',
     & 'Bad TFORM for descriptor I/O',
     & 'Variable array has 0 length',
     & 'End-of-rec in var. len. array',
     & 'Int to Char conversion error',
     & 'Real to Char conversion error',
     & 'Illegal Char to Int conversion',
     & 'Illegal Logical keyword value',
     & 'Illegal Char to R*4 conversion',
     & 'Illegal Char to R*8 conversion'/

        data er8/
     & 'Char to Int conversion error',
     & 'Char to Real conversion error',
     & 'Char to R*8 conversion error',
     & 'Illegal no. of decimal places',
     & 'Cannot modify a READONLY file',
     & 'Missing "= " in cols 9-10',
     & 'CHDU is not an IMAGE extension',
     & 'end of template header file',
     & ' ',
     & 'Unknown error status code'/

C       find the matching error code number
        do 10 i=1,nerror
                ncode=i
                if (errnum .eq. errcod(i))go to 20 
10      continue
20      continue

        text=errors(ncode)
        end
C--------------------------------------------------------------------------
        subroutine ftpkys(ounit,keywrd,strval,comm,status)

C       write a character string value to a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm,strval
        integer ounit,status,lenval,ncomm,nvalue
        character value*70,keynam*8,cmnt*48
        
        if (status .gt. 0)return

        keynam=keywrd
        cmnt=comm

C       convert string to quoted character string (max length = 70 characters)
        call fts2c(strval,value,lenval,status)

C       find amount of space left for comment string
C       (assume 10 char. for 'keyword = ', and 3 between value and comment)
C       which leaves 67 spaces for the value string + comment string
        nvalue=max(20,lenval)
        ncomm=67-nvalue

C       write the keyword record
        if (ncomm .gt. 0)then
C         there is space for a comment
          call ftprec(ounit,
     &    keynam//'= '//value(1:nvalue)//' / '//cmnt(1:ncomm),status)
        else
C         no room for a comment
          call ftprec(ounit,
     &    keynam//'= '//value(1:nvalue)//'   ',status)
        end if  
        end
C--------------------------------------------------------------------------
        subroutine ftpkyl(ounit,keywrd,logval,comm,status)

C       write a logical value to a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       logval  l  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer ounit,status
        logical logval
        character value*20

C       convert logical to character string
        call ftl2c(logval,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpkyj(ounit,keywrd,intval,comm,status)

C       write an integer value to a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       intval  i  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer ounit,status,intval
        character value*20

C       convert integer to character string
        call fti2c(intval,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpkyf(ounit,keywrd,rval,decim,comm,status)

C       write a real*4 value to a header record in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       rval    r  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        real rval
        integer ounit,status,decim
        character value*20

C       convert real to F format character string
        call ftr2f(rval,decim,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpkye(ounit,keywrd,rval,decim,comm,status)

C       write a real*4 value to a header record in E format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       rval    r  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        real rval
        integer ounit,status,decim
        character value*20

C       convert real to E format character string
        call ftr2e(rval,decim,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpkyg(ounit,keywrd,dval,decim,comm,status)

C       write a double precision value to a header record in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim
        character value*20

C       convert double precision to F format character string
        call ftd2f(dval,decim,value,status)

C       write the keyword record
        call ftpkey(ounit,keywrd,value,comm,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpkyd(ounit,keywrd,dval,decim,comm,status)

C       write a double precision value to a header record in E format
C       If it will fit, the value field will be 20 characters wide;
C       otherwise it will be expanded to up to 35 characters, left
C       justified.
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (max. 47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim,vlen
        character value*35,key*8,cmnt*48

        key=keywrd
        cmnt=comm

C       convert double precision to E format character string
        call ftd2e(dval,decim,value,vlen,status)

C       write the keyword record
        call ftprec(ounit,key//'= '//value(1:vlen)//' / '//cmnt,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpkns(ounit,keywrd,nstart,nkey,strval,comm,
     &                    status)

C       write an array of character string values to header records
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       strval  c  array of keyword values
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,strval(*),comm(*)
        integer nstart,nkey,ounit,status,i,j
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                   call ftpkys(ounit,keynam,strval(i),comm1,status)
                else
                   call ftpkys(ounit,keynam,strval(i),comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpknl(ounit,keywrd,nstart,nkey,logval,comm,
     &                    status)

C       write an array of logical values to header records
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       logval  l  array of keyword values
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,ounit,status,i,j
        logical logval(*)
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                  call ftpkyl(ounit,keynam,logval(i),comm1,status)
                else
                  call ftpkyl(ounit,keynam,logval(i),comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpknj(ounit,keywrd,nstart,nkey,intval,comm,
     &                    status)

C       write an array of integer values to header records
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       intval  i  array of keyword values
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,ounit,status,intval(*),i,j
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                   call ftpkyj(ounit,keynam,intval(i),comm1,status)
                else
                   call ftpkyj(ounit,keynam,intval(i),comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpknf(ounit,keywrd,nstart,nkey,rval,decim,comm,
     &                    status)

C       write an array of real*4 values to header records in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       rval    r  array of keyword values
C       decim   i  number of decimal places to display in the value field
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,decim,ounit,status,i,j
        real rval(*)
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                  call ftpkyf(ounit,keynam,rval(i),decim,comm1,status)
                else
                  call ftpkyf(ounit,keynam,rval(i),decim,comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpkne(ounit,keywrd,nstart,nkey,rval,decim,comm,
     &                    status)

C       write an array of real*4 values to header records in E format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       rval    r  array of keyword values
C       decim   i  number of decimal places to display in the value field
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,decim,ounit,status,i,j
        real rval(*)
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                  call ftpkye(ounit,keynam,rval(i),decim,comm1,status)
                else
                  call ftpkye(ounit,keynam,rval(i),decim,comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpkng(ounit,keywrd,nstart,nkey,dval,decim,comm,
     &                    status)

C       write an array of real*8 values to header records in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       dval    d  array of keyword values
C       decim   i  number of decimal places to display in the value field
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,decim,ounit,status,i,j
        double precision dval(*)
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                  call ftpkyg(ounit,keynam,dval(i),decim,comm1,status)
                else
                  call ftpkyg(ounit,keynam,dval(i),decim,comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpknd(ounit,keywrd,nstart,nkey,dval,decim,comm,
     &                    status)

C       write an array of real*8 values to header records in E format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       dval    d  array of keyword values
C       decim   i  number of decimal places to display in the value field
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,decim,ounit,status,i,j
        double precision dval(*)
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                  call ftpkyd(ounit,keynam,dval(i),decim,comm1,status)
                else
                  call ftpkyd(ounit,keynam,dval(i),decim,comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftmrec(ounit,nkey,record,status)

C       modify the nth keyword in the CHU, by replacing it with the
C       input 80 character string.
C
C       ounit   i  fortran output unit number
C       nkey    i  sequence number (starting with 1) of the keyword to read
C       record  c  80-character string to replace the record with
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nkey,status
        character*(*) record
        character rec*80

C       find the old keyword; just use REC as a temporary variable
        call ftgkyn(ounit,nkey,rec,rec,rec,status)
        
        rec=record
C       overwrite the keyword with the new record
        call ftmodr(ounit,rec,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmcrd(ounit,keywrd,card,status)

C       modify (overwrite) a given header record specified by keyword name.
C       This can be used to overwrite the name of the keyword as well as
C       the value and comment fields.
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       card    c  new 80-character card image to be written
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) keywrd,card
        integer ounit,status
        character value*80
        
        if (status .gt. 0)return

C       find the old keyword string
        call ftgcrd(ounit,keywrd,value,status)
        
        value=card

C       make sure new keyword name is in upper case
        call ftupch(value(1:8))

C       test that keyword name contains only legal characters
        call fttkey(value(1:8),status)

C       write the new keyword record
        call ftmodr(ounit,value,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmcom(ounit,keywrd,comm,status)

C       modify a the comment string in a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       comm    c  new keyword comment (max of 72 characters long)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) keywrd,comm
        integer ounit,status,lenval,ncomm
        character value*80,keynam*8,cmnt*72
        
        if (status .gt. 0)return

        keynam=keywrd

C       find the old keyword + value string
        call ftgknv(ounit,keynam,value,lenval,status)
        
        cmnt=comm

C       find amount of space left for comment string (3 spaces needed for ' / ')
        ncomm=77-lenval

C       write the keyword record
        if (ncomm .gt. 0)then
C         there is space for a comment
          call ftmodr(ounit,
     &    value(1:lenval)//' / '//cmnt(1:ncomm),status)
        else
C         no room for a comment
          call ftmodr(ounit,
     &    value(1:lenval)//'   ',status)
        end if  
        end
C--------------------------------------------------------------------------
        subroutine ftmkys(ounit,keywrd,strval,comm,status)

C       modify a character string value header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm,strval
        integer ounit,status,lenval,ncomm,nvalue
        character value*70,keynam*8,cmnt*48
        
        if (status .gt. 0)return

        keynam=keywrd

C       find the old keyword
        call ftgkey(ounit,keynam,value,cmnt,status)
        
C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert string to quoted character string (max length = 70 characters)
        call fts2c(strval,value,lenval,status)
        if (status .gt. 0)return

C       find amount of space left for comment string
C       (assume 10 char. for 'keyword = ', and 3 between value and comment)
C       which leaves 67 spaces for the value string + comment string
        nvalue=max(20,lenval)
        ncomm=67-nvalue

C       write the keyword record
        if (ncomm .gt. 0)then
C         there is space for a comment
          call ftmodr(ounit,
     &    keynam//'= '//value(1:nvalue)//' / '//cmnt(1:ncomm),status)
        else
C         no room for a comment
          call ftmodr(ounit,
     &    keynam//'= '//value(1:nvalue)//'   ',status)
        end if  
        end
C--------------------------------------------------------------------------
        subroutine ftmkyl(ounit,keywrd,logval,comm,status)

C       modify a logical value header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       logval  l  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer ounit,status
        logical logval
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)
        
C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert logical to character string
        call ftl2c(logval,value,status)

C       modify the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmkyj(ounit,keywrd,intval,comm,status)

C       modify an integer value header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       intval  i  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer ounit,status,intval
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert integer to character string
        call fti2c(intval,value,status)

C       modify the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmkyf(ounit,keywrd,rval,decim,comm,status)

C       modify a real*4 value header record in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       rval    r  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        real rval
        integer ounit,status,decim
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert real to F format character string
        call ftr2f(rval,decim,value,status)

C       write the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmkye(ounit,keywrd,rval,decim,comm,status)

C       modify a real*4 value header record in E format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       rval    r  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        real rval
        integer ounit,status,decim
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert real to E format character string
        call ftr2e(rval,decim,value,status)

C       modify the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmkyg(ounit,keywrd,dval,decim,comm,status)

C       modify a double precision value header record in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim
        character value*20,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert double precision to F format character string
        call ftd2f(dval,decim,value,status)

C       modify the keyword record
        call ftmkey(ounit,keywrd,value,cmnt,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmkyd(ounit,keywrd,dval,decim,comm,status)

C       modify a double precision value header record in E format
C       If it will fit, the value field will be 20 characters wide;
C       otherwise it will be expanded to up to 35 characters, left
C       justified.
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (max. 47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim,vlen
        character value*35,key*8,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

        key=keywrd
C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert double precision to E format character string
        call ftd2e(dval,decim,value,vlen,status)

C       write the keyword record
        call ftmodr(ounit,key//'= '//value(1:vlen)//' / '//cmnt,status)
        end
C--------------------------------------------------------------------------
        block data ftbkdt

C       initialize the BUFPNT pointers to zero, in case the compiler does
C       not do it automatically

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        data bufpnt/nb*0/
        data compid/1.1111111111/
C       compid is used by FITSIO to determine at run time what type of
C       machine it is running on.  Two I*2 integers are equivalenced to
C       the R*4 value of 1.111111111.  The resulting value of the integers
C       is machine specific, depending on whether R*4 values are stored in
C       IEEE format or not, and whether the bytes are swapped or not.
        end
C--------------------------------------------------------------------------
        subroutine ftdkey(iunit,keynam,status)

C       delete a header keyword
C
C       iunit   i  fortran output unit number
C       keynam  c  keyword name    ( 8 characters, cols.  1- 8)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) keynam
        integer iunit,status,i,j,ibuff,maxkey,nshift
        character kname*8
        character*80 keybuf,keytmp

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

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       make sure keyword name is in uppercase
        kname=keynam
        call ftupch(kname)

C       find the old keyword:
C       Start by searching for keyword from current pointer position to the end.
C       Calculate the maximum number of keywords to be searched:
        maxkey=(hdend(ibuff)-nxthdr(ibuff))/80
        if (maxkey .gt. 0)then
C               position I/O pointer to the next header keyword
                call ftmbyt(iunit,nxthdr(ibuff),.false.,status)
                if (status .gt. 0)go to 100
        end if
        do 20 j=1,2
            do 10 i=1,maxkey
                call ftgcbf(iunit,1,80,keybuf,status)
                if (status .gt. 0)go to 100
                if (keybuf(1:8) .eq. kname)go to 50
10          continue

C           didn't find keyword yet, so now search from top down to starting pt.
C           move I/O pointer to beginning of header
            call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.false.,status)
            if (status .gt. 0)go to 100

C           calculate the maximum number of keywords to be searched
            maxkey=(nxthdr(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
20      continue                        

C       keyword was not found
        status=202
        go to 100

C       update header pointer position (set equal to the current position)
50      nxthdr(ibuff)=(recnum(ibuff)-1)*reclen(iunit)+bytnum(ibuff)-80
        if (status .gt. 0)go to 100

C       calculate number of header records following the deleted record
        nshift=(hdend(ibuff)-nxthdr(ibuff))/80

C       go through header shifting each 80 byte record up one place to
C       fill in the gap created by the deleted keyword
        j=hdend(ibuff)
        keybuf=' '
        do 60 i=1,nshift
                j=j-80
C               read current record contents
                call ftmbyt(iunit,j,.false.,status)
                call ftgcbf(iunit,0,80,keytmp,status)
C               overwrite with new contents
                call ftmbyt(iunit,j,.false.,status)
                call ftpcbf(iunit,0,80,keybuf,status)
                keybuf=keytmp
60      continue

C       update end-of-header pointer
        hdend(ibuff)=hdend(ibuff)-80
100     continue
        end
C--------------------------------------------------------------------------
        subroutine ftcrep(comm,comm1,repeat)
        
C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)

C       comm    c  input comment string
C       OUTPUT PARAMETERS:
C       comm1   c  output comment string, = COMM minus the last '&' character
C       repeat  l  true if the last character of COMM was the '&" character
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) comm,comm1
        logical repeat
        integer i,j

        repeat=.false.
        j=len(comm)
        do 10 i=j,1,-1
                if (comm(i:i) .ne. ' ')then
                        if (comm(i:i) .eq. '&')then
                                comm1=comm(1:i-1)
                                repeat=.true.
                        end if
                        return
                end if
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpkey(ounit,keywrd,value,comm,status)

C       write a simple FITS keyword record with format:
C            "KEYWORD = VALUE / COMMENT"
C               VALUE is assumed to be 20 characters long
C               COMMENT is assumed to be 47 characters long
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       value   c  keyword value   (20 characters, cols. 11-30)
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,value,comm
        integer ounit,status
        character key*8, val*20, com*47

        key=keywrd
        val=value
        com=comm

C       append the 80 characters to the output buffer:
        call ftprec(ounit,key//'= '//val//' / '//com,status)
        end
C--------------------------------------------------------------------------
        subroutine ftmkey(ounit,keywrd,value,comm,status)

C       modify an existing simple FITS keyword record with format:
C            "KEYWORD = VALUE / COMMENT"
C               VALUE is assumed to be 20 characters long
C               COMMENT is assumed to be 47 characters long
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       value   c  keyword value   (20 characters, cols. 11-30)
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,value,comm
        integer ounit,status
        character key*8, val*20, com*47

        key=keywrd
        val=value
        com=comm

C       overwrite the preceeding 80 characters to the output buffer:
        call ftmodr(ounit,key//'= '//val//' / '//com,status)
        end
C--------------------------------------------------------------------------
        subroutine ftprec(ounit,record,status)

C       write a 80 character record to the FITS header
C
C       ounit   i  fortran output unit number
C       record  c  input 80 character header record
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) record
        character*80  rec
        integer ounit,status,ibuff

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
C       nb = number of file buffers = max. number of FITS file opened at once
C       nf = maximum number of fields allowed in a table
C       ne = maximum number of extensions allowed in a FITS file
        integer nb,ne
        parameter (nb = 12)
        parameter (ne = 128)

C       bufnum = stores which file buffer is used for this unit
C       bufpnt = pointer showing which unit is associated with the file buffer
C       reclen = the record length in bytes of the FITS file
C       recnum = the sequence number of the current FITS file record
C       bytnum = the sequence number of the current byte in RECNUM
C       wrmode = indicates whether the FITS file was opened with write access
C       modify = indicates whether the current record has been modified
C       chdu = sequence number of the Current Header-Data Unit (starting at 1)
C       maxhdu = the highest numbered HDU in the FITS file so far encountered
C       hdstrt = the zero-index byte number of the start of the extension
C       hdend = the zero-index byte number of the start of the END record
C       nxthdr = the zero-index byte no. of the current position in the CHU
C       dtstrt = the zero-index byte no. of the start of the current data unit
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
C-------END OF COMMON BLOCK DEFINITIONS:------- -----------------------------

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .gt. 0 
     &    .and.(dtstrt(ibuff)-hdend(ibuff)) .le. 80)then
C               not enough room in the header for another keyword

C               try getting more header space
                call ftiblk(ounit,status)
                if (status .gt. 0)then
                        go to 900
                end if
        end if
                
        rec=record

C       make sure keyword name is in upper case
        call ftupch(rec(1:8))

C       test that keyword name contains only legal characters
        call fttkey(rec(1:8),status)

C       position the I/O pointer to the end of the header
        call ftmbyt(ounit,hdend(ibuff),.true.,status)

C       append the 80 characters to the output buffer:
        call ftpcbf(ounit,1,80,rec,status)
        if (status .gt. 0)go to 900

C       increment the pointer to the last header record
        hdend(ibuff)=hdend(ibuff)+80

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftpcom(ounit,commnt,status)

C       write a COMMENT record to the FITS header
C
C       ounit   i  fortran output unit number
C       commnt c  input comment string 
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,status
        character*(*) commnt
        character*80  rec

        if (status .gt. 0)return
                
        rec='COMMENT   '//commnt
        call ftprec(ounit,rec,status)
        end
C--------------------------------------------------------------------------
        subroutine ftphis(ounit,histry,status)

C       write a HISTORY record to the FITS header
C
C       ounit   i  fortran output unit number
C       histry  c  input history string
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,status
        character*(*) histry
        character*80  rec

        if (status .gt. 0)return
                
        rec='HISTORY   '//histry
        call ftprec(ounit,rec,status)
        end
C--------------------------------------------------------------------------
        subroutine ftpdat(ounit,status)

C       write the current date to the DATE keyword in the ounit CHU
C
C       ounit   i  fortran output unit number
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Jan 1992

        integer ounit,status,dd,mm,yy
        character datstr*8

C       call the system dependent routine to get the current date
        call ftgsdt(dd,mm,yy,status)
        if (status .gt. 0)return

        datstr='  /  /  '
        write(datstr(1:2),1001)dd
        write(datstr(4:5),1001)mm
        write(datstr(7:8),1001)yy
1001    format(i2)

C       replace blank with leading 0 in each field if required
        if (datstr(1:1) .eq. ' ')datstr(1:1)='0'
        if (datstr(4:4) .eq. ' ')datstr(4:4)='0'
        if (datstr(7:7) .eq. ' ')datstr(7:7)='0'
        
C       first, try updating the DATE keyword, if it exists
        call ftmkys(ounit,'DATE',datstr,
     &             'FITS file creation date (dd/mm/yy)',status)

        if (status .gt. 0)then
C             keyword doesn't exist so write the new keyword to the header
              status=0
              call ftpkys(ounit,'DATE',datstr,
     &             'FITS file creation date (dd/mm/yy)',status)
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftmodr(ounit,record,status)

C       modify the preceeding 80 character record in the FITS header
C
C       ounit   i  fortran output unit number
C       record  c  input 80 character header record
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) record
        character*80  rec
        integer ounit,status,ibuff

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
C-------END OF COMMON BLOCK DEFINITIONS:------- -----------------------------

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

        rec=record

C       make sure keyword name is in upper case
        call ftupch(rec(1:8))

C       test that keyword name contains only legal characters
        call fttkey(rec(1:8),status)

C       move the I/O pointer back to the beginning of the preceeding keyword
        call ftmbyt(ounit,nxthdr(ibuff)-80,.false.,status)

C       overwrite the 80 characters to the output buffer:
        call ftpcbf(ounit,1,80,rec,status)
        end
C--------------------------------------------------------------------------
        subroutine ftiblk(ounit,status)

C       attempt to insert a blank-filled 2880-byte block at the end
C       of the CHU, to create space for more header records.

C       ounit   i  fortran output unit number
C       status  i  returned error status (0=ok)

        integer ounit,status

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
        character*2880 buff(2)
        common/ftheap/buff
C       END OF COMMON BLOCK DEFINITIONS:------------------------------------

        integer ibuff,jpoint,in,out,i

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(ounit)

C       initialize the first buffer
        buff(1)=' '

        jpoint=dtstrt(ibuff)
        in=2
        out=1

C       move to the read start position
10      call ftmbyt(ounit,jpoint,.false.,status)

C       check for End-Of-File
        if (status .eq. 107)go to 20

C       read on 2880-byte FITS logical record in to the input buffer
        call ftgcbf(ounit,0,2880,buff(in),status)

C       move back to the write start postion
        call ftmbyt(ounit,jpoint,.false.,status)

C       write the 2880-byte FITS logical record stored in the output buffer
        call ftpcbf(ounit,0,2880,buff(out),status)

C       check for error during write (the file may not have write access)
        if (status .gt. 0)return

C       swap the input and output buffer pointers and move to next block
        if (in .eq. 1)then
                in=2
                out=1
        else
                in=1
                out=2
        end if
        jpoint=jpoint+2880

C       now repeat the process until we reach the End-Of-File
        go to 10

20      continue
C       we have reached the end of file; now append the last block to the file
        status=0

C       move back to the write start postion
        call ftmbyt(ounit,jpoint,.true.,status)

C       write the 2880-byte FITS logical record stored in the output buffer
        call ftpcbf(ounit,0,2880,buff(out),status)

        if (status .le. 0)then
C               recalculate the starting location of the current data unit
                dtstrt(ibuff)=dtstrt(ibuff)+2880

C               recalculate the starting location of all subsequent HDUs
                do 30 i=chdu(ibuff)+1,maxhdu(ibuff)
                        hdstrt(ibuff,i)=hdstrt(ibuff,i)+2880
30              continue
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftgkys(iunit,keywrd,strval,comm,status)

C       read a character string value and comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       strval  c  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm,strval
        integer status,iunit
        character value*70

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to unquoted string
        call ftc2s(value,strval,status)
        end
C--------------------------------------------------------------------------
        subroutine ftgkyl(iunit,keywrd,logval,comm,status)

C       read a logical value and the comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       logval  l  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer iunit,status
        character value*20
        logical logval

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to logical
        call ftc2l(value,logval,status)
        end
C--------------------------------------------------------------------------
        subroutine ftgkyj(iunit,keywrd,intval,comm,status)

C       read an integer value and the comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       intval  i  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer iunit,intval,status
        character value*35

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to integer
C       datatype conversion will be performed if necessary and if possible
        call ftc2i(value,intval,status)
        end
C--------------------------------------------------------------------------
        subroutine ftgkye(iunit,keywrd,rval,comm,status)

C       read a real*4 value and the comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       rval    r  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer iunit,status
        character value*35
        real rval

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to real
C       datatype conversion will be performed if necessary and if possible
        call ftc2r(value,rval,status)
        end
C--------------------------------------------------------------------------
        subroutine ftgkyd(iunit,keywrd,dval,comm,status)

C       read a double precision value and comment string from a header record 
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name   
C       OUTPUT PARAMETERS:
C       dval    i  output keyword value 
C       comm    c  output keyword comment
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        integer iunit,status
        character value*35
        double precision dval

C       find the keyword and return value and comment as character strings
        call ftgkey(iunit,keywrd,value,comm,status)

C       convert character string to double precision
C       datatype conversion will be performed if necessary and if possible
        call ftc2d(value,dval,status)
        end
C--------------------------------------------------------------------------
        subroutine ftgkns(iunit,keywrd,nstart,nmax,strval,nfound,
     &                    status)

C       read an array of character string values from  header records
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       nstart  i  starting sequence number (usually 1)
C       nmax    i  number of keywords to read
C       OUTPUT PARAMETERS:
C       strval  i  array of output keyword values
C       nfound  i  number of keywords found
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,strval(*)
        integer iunit,nstart,nmax,nfound,status,i,j
        character keynam*8
        character*80 comm

        nfound=0
        j=nstart
C       search for each possible keyword:
        do 10 i=1,nmax
C               construct keyword name
                call ftkeyn(keywrd,j,keynam,status)
                if (status .gt. 0)go to 900

C               search for keyword value
                call ftgkys(iunit,keynam,strval(i),comm,status)

                if (status .le. 0)then
                        nfound=i
                else if (status .eq. 202)then
C                       ignore 'keyword not found' error
                        status =0
                end if
                j=j+1
10      continue

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgknl(iunit,keywrd,nstart,nmax,logval,
     &                    nfound,status)

C       read an array of logical values from  header records
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       nstart  i  starting sequence number (usually 1)
C       nmax    i  number of keywords to read
C       OUTPUT PARAMETERS:
C       logval  l  array of output keyword values
C       nfound  i  number of keywords found
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd
        integer iunit,nstart,nmax,nfound,status,i,j
        logical logval(*)
        character keynam*8
        character*80 comm

        nfound=0
        j=nstart
C       search for each possible keyword:
        do 10 i=1,nmax
C               construct keyword name
                call ftkeyn(keywrd,j,keynam,status)
                if (status .gt. 0)go to 900

C               search for keyword value
                call ftgkyl(iunit,keynam,logval(i),comm,status)

                if (status .le. 0)then
                        nfound=i
                else if (status .eq. 202)then
C                       ignore 'keyword not found' error
                        status =0
                end if
                j=j+1
10      continue

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgknj(iunit,keywrd,nstart,nmax,intval,
     &                    nfound,status)

C       read an array of integer values from  header records
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       nstart  i  starting sequence number (usually 1)
C       nmax    i  number of keywords to read
C       OUTPUT PARAMETERS:
C       intval  i  array of output keyword values
C       nfound  i  number of keywords found
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd
        integer iunit,nstart,nmax,nfound,status,intval(*),i,j
        character keynam*8
        character*80 comm

        nfound=0
        j=nstart
C       search for each possible keyword:
        do 10 i=1,nmax
C               construct keyword name
                call ftkeyn(keywrd,j,keynam,status)
                if (status .gt. 0)go to 900

C               search for keyword value
                call ftgkyj(iunit,keynam,intval(i),comm,status)

                if (status .le. 0)then
                        nfound=i
                else if (status .eq. 202)then
C                       ignore 'keyword not found' error
                        status =0
                end if
                j=j+1
10      continue

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgkne(iunit,keywrd,nstart,nmax,
     &                    rval,nfound,status)

C       read an array of real*4 values from  header records
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       nstart  i  starting sequence number (usually 1)
C       nmax    i  number of keywords to read
C       OUTPUT PARAMETERS:
C       rval    r  array of output keyword values
C       nfound  i  number of keywords found
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd
        integer iunit,nstart,nmax,nfound,status,i,j
        real rval(*)
        character keynam*8
        character*80 comm

        nfound=0
        j=nstart
C       search for each possible keyword:
        do 10 i=1,nmax
C               construct keyword name
                call ftkeyn(keywrd,j,keynam,status)
                if (status .gt. 0)go to 900

C               search for keyword value
                call ftgkye(iunit,keynam,rval(i),comm,status)

                if (status .le. 0)then
                        nfound=i
                else if (status .eq. 202)then
C                       ignore 'keyword not found' error
                        status =0
                end if
                j=j+1
10      continue

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgknd(iunit,keywrd,nstart,nmax,
     &                    dval,nfound,status)

C       read an array of real*8 values from  header records
C
C       iunit   i  fortran input unit number
C       keywrd  c  keyword name
C       nstart  i  starting sequence number (usually 1)
C       nmax    i  number of keywords to read
C       OUTPUT PARAMETERS:
C       dval    d  array of output keyword values
C       nfound  i  number of keywords found
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd
        integer iunit,nstart,nmax,nfound,status,i,j
        double precision dval(*)
        character keynam*8
        character*80 comm

        nfound=0
        j=nstart
C       search for each possible keyword:
        do 10 i=1,nmax
C               construct keyword name
                call ftkeyn(keywrd,j,keynam,status)
                if (status .gt. 0)go to 900

C               search for keyword value
                call ftgkyd(iunit,keynam,dval(i),comm,status)

                if (status .le. 0)then
                        nfound=i
                else if (status .eq. 202)then
C                       ignore 'keyword not found' error
                        status =0
                end if
                j=j+1
10      continue

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgknv(iunit,keynam,keyval,lenval,status)

C       return the keyword name + value string of a given keyword record

C       iunit   i  Fortran I/O unit number
C       keynam  c  name of keyword to be read
C       OUTPUT PARAMETERS:
C       keyval  c  output keyword name + value string of the keyword
C       lenval  i  length of the keyval string
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb, 1992

        character*(*) keynam
        character*80 keyval
        integer iunit,lenval,status,i,j,ibuff,maxkey
        character kname*8

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       make sure keyword name is in uppercase
        kname=keynam
        call ftupch(kname)

C       Start by searching for keyword from current pointer position to the end.
C       Calculate the maximum number of keywords to be searched:
        maxkey=(hdend(ibuff)-nxthdr(ibuff))/80
        if (maxkey .gt. 0)then
C               position I/O pointer to the next header keyword
                call ftmbyt(iunit,nxthdr(ibuff),.false.,status)
                if (status .gt. 0)go to 100
        end if
        do 20 j=1,2
            do 10 i=1,maxkey
                call ftgcbf(iunit,1,80,keyval,status)
                if (status .gt. 0)go to 100
                if (keyval(1:8) .eq. kname)go to 50
10          continue

C           didn't find keyword yet, so now search from top down to starting pt.
C           move I/O pointer to beginning of header
            call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.false.,status)
            if (status .gt. 0)go to 100

C           calculate the maximum number of keywords to be searched
            maxkey=(nxthdr(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
20      continue                        

C       keyword was not found
        status=202
        go to 100

C       parse the record to find the end of the value string
50      call ftprsv(keyval,lenval,status)
        if (status .gt. 0)go to 100

C       update header pointer position (set equal to the current position)
        nxthdr(ibuff)=(recnum(ibuff)-1)*reclen(iunit)+bytnum(ibuff)

100     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgcrd(iunit,keynam,card,status)

C       Read the 80 character card image of a specified header keyword record

C       iunit   i  Fortran I/O unit number
C       keynam  c  name of keyword to be read
C       OUTPUT PARAMETERS:
C       card    c  80 character card image that was read
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        character*(*) keynam
        integer iunit,status,i,j,ibuff,maxkey
        character*(*) card
        character kname*8
        character*80 keybuf

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        card=' '
        if (status .gt. 0)go to 100

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       make sure keyword name is in uppercase
        kname=keynam
        call ftupch(kname)

C       Start by searching for keyword from current pointer position to the end.
C       Calculate the maximum number of keywords to be searched:
        maxkey=(hdend(ibuff)-nxthdr(ibuff))/80
        if (maxkey .gt. 0)then
C               position I/O pointer to the next header keyword
                call ftmbyt(iunit,nxthdr(ibuff),.false.,status)
                if (status .gt. 0)go to 100
        end if
        do 20 j=1,2
            do 10 i=1,maxkey
                call ftgcbf(iunit,1,80,keybuf,status)
                if (status .gt. 0)go to 100
                if (keybuf(1:8) .eq. kname)go to 50
10          continue

C           didn't find keyword yet, so now search from top down to starting pt.
C           move I/O pointer to beginning of header
            call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.false.,status)
            if (status .gt. 0)go to 100

C           calculate the maximum number of keywords to be searched
            maxkey=(nxthdr(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
20      continue                        

C       keyword was not found
        status=202
        go to 100

C       set the value of the returned card image parameter
50      card=keybuf

C       update header pointer position (set equal to the current position)
        nxthdr(ibuff)=(recnum(ibuff)-1)*reclen(iunit)+bytnum(ibuff)

100     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgkey(iunit,keynam,value,comm,status)

C       Read value and comment of a header keyword from the keyword buffer

C       iunit   i  Fortran I/O unit number
C       keynam  c  name of keyword to be read
C       OUTPUT PARAMETERS:
C       value   c  output value of the keyword, if any
C       comm    c  output comment string, if any, of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        character*(*) keynam
        integer iunit,status,i,j,ibuff,maxkey
        character*(*) value,comm
        character kname*8
        character*80 keybuf

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       make sure keyword name is in uppercase
        kname=keynam
        call ftupch(kname)

C       Start by searching for keyword from current pointer position to the end.
C       Calculate the maximum number of keywords to be searched:
        maxkey=(hdend(ibuff)-nxthdr(ibuff))/80
        if (maxkey .gt. 0)then
C               position I/O pointer to the next header keyword
                call ftmbyt(iunit,nxthdr(ibuff),.false.,status)
                if (status .gt. 0)go to 100
        end if
        do 20 j=1,2
            do 10 i=1,maxkey
                call ftgcbf(iunit,1,80,keybuf,status)
                if (status .gt. 0)go to 100
                if (keybuf(1:8) .eq. kname)go to 50
10          continue

C           didn't find keyword yet, so now search from top down to starting pt.
C           move I/O pointer to beginning of header
            call ftmbyt(iunit,hdstrt(ibuff,chdu(ibuff)),.false.,status)
            if (status .gt. 0)go to 100

C           calculate the maximum number of keywords to be searched
            maxkey=(nxthdr(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
20      continue                        

C       keyword was not found
        status=202
        go to 100

C       parse the record to find value and comment strings
50      call ftpsvc(keybuf,value,comm,status)
        if (status .gt. 0)go to 100

C       update header pointer position (set equal to the current position)
        nxthdr(ibuff)=(recnum(ibuff)-1)*reclen(iunit)+bytnum(ibuff)

100     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgrec(iunit,nrec,record,status)

C       Read the Nth 80-byte header record 
C       This routine is useful for reading the entire header, one
C       record at a time.

C       iunit   i  Fortran I/O unit number
C       nrec    i  sequence number (starting with 1) of the record to read
C       OUTPUT PARAMETERS:
C       record  c  output 80-byte record
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,nrec,status
        character*80 record

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

        integer ibuff,nbyte,endhd

        if (status .gt. 0)return

C       get the number of the data buffer used for this unit
        ibuff=bufnum(iunit)

C       calculate byte location of the record, and check if it is legal
        nbyte=hdstrt(ibuff,chdu(ibuff))+(nrec-1)*80

        endhd=(hdend(ibuff)/2880+1)*2880

        if (nbyte .ge. endhd .or. nrec .le. 0)then
C               header record number is out of bounds
                status=203
                go to 100
        end if 

C       position the I/O pointer to the appropriate header keyword
        call ftmbyt(iunit,nbyte,.false.,status)

C       read the 80 byte record
        call ftgcbf(iunit,1,80,record,status)

C       update the keyword pointer position
        nxthdr(ibuff)=nbyte+80

100     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgkyn(iunit,nkey,keynam,value,comm,status)

C       Read value and comment of the NKEYth header record 
C       This routine is useful for reading the entire header, one
C       record at a time.

C       iunit   i  Fortran I/O unit number
C       nkey    i  sequence number (starting with 1) of the keyword to read
C       OUTPUT PARAMETERS:
C       keynam  c  output name of the keyword
C       value   c  output value of the keyword, if any
C       comm    c  output comment string, if any, of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,nkey,status
        character*(*) keynam
        character*(*) value,comm
        character*80 keybuf

        if (status .gt. 0)return

        call ftgrec(iunit,nkey,keybuf,status)

        keynam=keybuf(1:8)
C       parse the value and comment fields from the record
        call ftpsvc(keybuf,value,comm,status)

        end
C--------------------------------------------------------------------------
        subroutine ftprsv(keyrec,lenval,status)

C       find the total length of the header+value string in a keyword record

C       keyrec  c  80 column header record
C       OUTPUT PARAMETERS:
C       lenval  i  output length of keyword+value string
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*80 keyrec
        integer lenval,status,j,c1

        if (status .gt. 0)return

        if (keyrec(1:8) .eq.'COMMENT ' .or. keyrec(1:8).eq.'HISTORY '
     &  .or. keyrec(1:8).eq.'END     ' .or. keyrec(1:8).eq.'        ')
     &  then
C           this is a COMMENT or HISTORY record, with no value
             lenval=8
        else if (keyrec(9:10) .eq. '= ')then
C           this keyword has a value field; now find the first character:
            do 10 j=10,80
                if (keyrec(j:j) .ne. ' ')then
                        c1=j
                        go to 15
                end if
10          continue
C           error: value is blank
            status=204
            return

15          if (keyrec(c1:c1) .eq. '''')then
C               This is a string value.
C               Work forward to find a single quote.  Two single quotes
C               in succession is to be interpreted as a literal single
C               quote character as part of the character string, not as
C               the end of the character string.  Everything to the right 
C               of the closing quote is assumed to be the comment.
                do 20 j=c1+1,80
                    if (keyrec(j:j) .eq. '''')then
                        if (j.lt.80 .and. keyrec(j+1:j+1).eq.'''')then
C                               found 2 successive quote characters; this is 
C                               interpreted as a literal quote character
                        else
                                lenval=max(30,j)
                                go to 30
                        end if
                    end if
20              continue
C               error: no closing quote character
                status=205
                return
            else
C               This is either an integer, floating point, or logical value.
C               Extract the first token as the value; remainder = comment
                do 25 j=c1,80
                    if (keyrec(j:j) .eq. ' ')then
                        lenval=j-1
                        go to 30
                    end if
25              continue
C               the first token went all the way to column 80:
                lenval=80
            end if
        else
C               illegal keyword record format; must have '= ' in columns 9-10
                status=210
        end if
30      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpsvc(keyrec,value,comm,status)

C       parse the header record to find value and comment strings

C       keyrec  c  80 column header record
C       OUTPUT PARAMETERS:
C       value   c  output keyword value string
C       comm    c  output keyword comment string
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*80 keyrec,keytmp
        character*(*) value,comm
        character*80 ctemp
        integer status,j,c1

        if (status .gt. 0)return

        if (keyrec(1:8) .eq.'COMMENT ' .or. keyrec(1:8).eq.'HISTORY '
     &  .or. keyrec(1:8).eq.'END     ' .or. keyrec(1:8).eq.'        ')
     &  then
C           this is a COMMENT or HISTORY record, with no value
            value=' '
            comm=keyrec(9:80)
        else if (keyrec(9:10) .eq. '= ')then
C           this keyword has a value field; now find the first character:
            do 10 j=10,80
                if (keyrec(j:j) .ne. ' ')then
                        c1=j
                        go to 15
                end if
10          continue
C           error: value is blank
            status=204
            return

15          if (keyrec(c1:c1) .eq. '''')then
C               This is a string value.
C               Work forward to find a single quote.  Two single quotes
C               in succession is to be interpreted as a literal single
C               quote character as part of the character string, not as
C               the end of the character string.  Everything to the right 
C               of the closing quote is assumed to be the comment.
C               First, copy input to temporary string variable
                keytmp=keyrec
                do 20 j=c1+1,80
                    if (keytmp(j:j) .eq. '''')then
                        if (j.lt.80 .and. keytmp(j+1:j+1).eq.'''')then
C                               found 2 successive quote characters; this is 
C                               interpreted as a literal quote character; remove
C                               one of the quotes from the string, and continue
C                               searching for the closing quote character:
                                keytmp(j+1:80)=keytmp(j+2:80)
                        else
                                value=keytmp(c1:j)
                                if (j .lt. 80)then
                                        ctemp=keytmp(j+1:80)
                                else
                                        ctemp=' '
                                end if
                                go to 30
                        end if
                    end if
20              continue
C               error: no closing quote character
                status=205
                return
            else
C               This is either an integer, floating point, or logical value.
C               Extract the first token as the value; remainder = comment
                do 25 j=c1,80
                    if (keyrec(j:j) .eq. ' ')then
                        value=keyrec(c1:j-1)
                        ctemp=keyrec(j+1:80)
                        go to 30
                    end if
25              continue
C               the first token went all the way to column 80:
                value=keyrec(c1:80)
                ctemp=' '
            end if

30          comm=' '
C           look for first character in the comment string
            do 40 j=1,78
                if (ctemp(j:j).ne.' ')then
                        if (ctemp(j:j).eq.'/')then
C                            ignore first space, if it exists
                             if (ctemp(j+1:j+1) .eq. ' ')then
                                comm=ctemp(j+2:80)
                             else
                                comm=ctemp(j+1:80)
                             end if
                        else
                                comm=ctemp(j:80)
                        end if
                        go to 50
                end if
40          continue
        else
C               illegal keyword record format; must have '= ' in columns 9-10
                status=210
        end if
50      continue
        end
C--------------------------------------------------------------------------
        subroutine ftkeyn(keywrd,nseq,keyout,status)

C       Make a keyword name by concatinating the root name and a 
C       sequence number

C       keywrd  c  root keyword name
C       nseq    i  sequence number 
C       OUTPUT PARAMETERS:
C       keyout  c  output concatinated keyword name
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        character*(*) keywrd
        integer nseq,status,nspace,i
        character keyout*8

        if (status .gt. 0)return

        keyout=keywrd

C       find end of keyword string
        nspace=1
        do 10 i=1,8
                if (keyout(i:i) .eq. ' ')go to 15
                nspace=nspace+1
10      continue
15      continue

C       append sequence number to keyword root only if there is room
        if (nseq .lt. 0)then
C               illegal value
                go to 900
        else if (nseq .lt. 10 .and. nspace .le. 8)then
                write(keyout(nspace:nspace),1001,err=900)nseq
        else if (nseq .lt. 100 .and. nspace .le. 7)then
                write(keyout(nspace:nspace+1),1002,err=900)nseq
        else if (nseq .lt. 1000 .and. nspace .le. 6)then
                write(keyout(nspace:nspace+2),1003,err=900)nseq
        else if (nseq .lt. 10000 .and. nspace .le. 5)then
                write(keyout(nspace:nspace+3),1004,err=900)nseq
        else if (nseq .lt. 100000 .and. nspace .le. 4)then
                write(keyout(nspace:nspace+4),1005,err=900)nseq
        else if (nseq .lt. 1000000 .and. nspace .le. 3)then
                write(keyout(nspace:nspace+5),1006,err=900)nseq
        else if (nseq .lt. 10000000 .and. nspace .le. 2)then
                write(keyout(nspace:nspace+6),1007,err=900)nseq
        else
C               number too big to fit in keyword
                go to 900
        end if

1001    format(i1)
1002    format(i2)
1003    format(i3)
1004    format(i4)
1005    format(i5)
1006    format(i6)
1007    format(i7)

        return
C       come here if error concatinating the seq. no. to the root string
900     status=206
        end
C----------------------------------------------------------------------
        subroutine fttkey(keynam,status)

C       test that keyword name contains only legal characters:
C         uppercase letters, numbers, hyphen, underscore, or space

C       keywrd  c*8  keyword name
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)

        character keynam*(*)
        integer status,i
        character*1 c1

        if (status .gt. 0)return

        do 20 i=1,8
            c1=keynam(i:i)
            if (c1 .ge. 'A' .and. c1 .le. 'Z')then
            else if (c1 .ge. '0' .and. c1 .le. '9')then
            else if (c1 .eq. ' ' .or. c1 .eq. '-' .or. c1 .eq. '_')then
            else 
C                       illegal character found
                        status=207
                        return
            end if
20      continue
        end
C----------------------------------------------------------------------
        subroutine ftpprh(ounit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       write required primary header keywords
C       THE PREFERRED NAME OF THIS SUBROUTINE HAS BEEN CHANGED TO FTPHPR
C
C       ounit   i  fortran output unit number
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters
C       gcount  i  number of random groups
C       extend  l  may extensions be present in the FITS file?
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)

        integer ounit,bitpix,naxis,naxes(*),pcount,gcount,status
        logical simple,extend

        call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)
        end
C----------------------------------------------------------------------
        subroutine ftphpr(ounit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       write required primary header keywords
C
C       ounit   i  fortran output unit number
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters
C       gcount  i  number of random groups
C       extend  l  may extensions be present in the FITS file?
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,bitpix,naxis,naxes(*),pcount,gcount,status,i,ibuff
        character comm*50
        logical simple,extend

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (chdu(ibuff) .eq. 1)then
            if (simple)then
                comm='file does conform to FITS standard'
            else
                comm='file does not conform to FITS standard'
            end if
            call ftpkyl(ounit,'SIMPLE',simple,comm,status)
        else  
            comm='IMAGE extension'
            call ftpkys(ounit,'XTENSION','IMAGE',comm,status)
        end if

C       test for legal value of bitpix
        call fttbit(bitpix,status)
        comm='number of bits per data pixel'
        call ftpkyj(ounit,'BITPIX',bitpix,comm,status)
        if (status .gt. 0)go to 900

        if (naxis .ge. 0 .and. naxis .le. 999)then
                comm='number of data axes'
                call ftpkyj(ounit,'NAXIS',naxis,comm,status)
        else
C               illegal value of naxis
                status=212
                go to 900
        end if

        comm='length of data axis'
        do 10 i=1,naxis
                if (naxes(i) .ge. 0)then
                        write(comm(21:23),1000)i
1000                    format(i3)      
                        call ftpknj(ounit,'NAXIS',i,1,naxes(i),comm,
     &                              status)
                else
C                       illegal NAXISnnn keyword value
                        status=213
                        go to 900
                end if
10      continue

        if (chdu(ibuff) .eq. 1)then
C               only write the EXTEND keyword to primary header if true
                if (extend)then
                        comm='FITS dataset may contain extensions'
                        call ftpkyl(ounit,'EXTEND',extend,comm,status)
                end if

C               write the PCOUNT and GCOUNT values if nonstandard
                if (pcount .gt. 0 .or. gcount .gt. 1)then
                    comm='random group records are present'
                    call ftpkyl(ounit,'GROUPS',.true.,comm,status)
                    comm='number of random group parameters'
                    call ftpkyj(ounit,'PCOUNT',pcount,comm,status)  
                    comm='number of random groups'
                    call ftpkyj(ounit,'GCOUNT',gcount,comm,status)
                end if
        else 
                comm='number of random group parameters'
                call ftpkyj(ounit,'PCOUNT',pcount,comm,status)                
                comm='number of random groups'
                call ftpkyj(ounit,'GCOUNT',gcount,comm,status) 
        end if

900     continue
        end
C----------------------------------------------------------------------
        subroutine ftgprh(iunit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       get the required primary header or image extension keywords
C       THE PREFERRED NAME OF THIS SUBROUTINE HAS BEEN CHANGED TO FTGHPR
C
C       iunit   i  fortran unit number to use for reading
C       OUTPUT PARAMETERS:
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters (usually 0)
C       gcount  i  number of random groups (usually 1 or 0)
C       extend  l  may extensions be present in the FITS file?
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,bitpix,naxis,naxes(*),pcount,gcount,blank,status
        integer nblank
        logical simple,extend
        double precision fill
        
        call ftgphx(iunit,0,simple,bitpix,naxis,naxes,
     &        pcount,gcount,extend,fill,fill,blank,nblank,status)
        end
C----------------------------------------------------------------------
        subroutine ftghpr(iunit,maxdim,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       get the required primary header or image extension keywords
C
C       iunit   i  fortran unit number to use for reading
C       maxdim  i  maximum no. of dimensions to read; dimension of naxes
C       OUTPUT PARAMETERS:
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters (usually 0)
C       gcount  i  number of random groups (usually 1 or 0)
C       extend  l  may extensions be present in the FITS file?
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,bitpix,naxis,naxes(*),pcount,gcount,blank,status
        integer maxdim,nblank
        logical simple,extend
        double precision fill
        
        call ftgphx(iunit,maxdim,simple,bitpix,naxis,naxes,
     &        pcount,gcount,extend,fill,fill,blank,nblank,status)
        end
C----------------------------------------------------------------------
        subroutine ftgphx(iunit,maxdim,simple,bitpix,naxis,naxes,pcount
     &               ,gcount,extend,bscale,bzero,blank,nblank,status)

C       get the main primary header keywords which define the array structure
C
C       iunit   i  fortran unit number to use for reading
C       maxdim  i  maximum no. of dimensions to read; dimension of naxes
C       OUTPUT PARAMETERS:
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters (usually 0)
C       gcount  i  number of random groups (usually 1 or 0)
C       extend  l  may extensions be present in the FITS file?
C       bscale  d  scaling factor
C       bzero   d  scaling zero point
C       blank   i  value used to represent undefined pixels
C       nblank  i  number of trailing blank keywords immediately before the END
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,maxdim,bitpix,naxis
        integer naxes(maxdim),pcount,gcount,blank,status
        logical simple,extend
        character keynam*8,value*20,comm*48
        double precision bscale,bzero
        integer nkey,nblank,i,ibuff,taxes

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check that the first keyword is valid
        call ftgkyn(iunit,1,keynam,value,comm,status)
        if (status .gt. 0)return

        if (chdu(ibuff) .eq. 1)then
            if (keynam .eq. 'SIMPLE')then
                if (value .eq. 'T')then
                        simple=.true.
                else
C                       this is not a simple FITS file; try to process it anyway
                        simple=.false.
                end if
            else
                status=221
                go to 900
            end if
        else
             if (keynam .eq. 'XTENSION')then
                if (value(2:9) .ne. 'IMAGE   ' .and. 
     &              value(2:9) .ne. 'IUEIMAGE')then
                        status=233
                        go to 900
                 end if
                 simple=.true.
             else
                 status=225
             end if
        end if

C       check that BITPIX is the second keyword
        call ftgkyn(iunit,2,keynam,value,comm,status)
        if (status .gt. 0)go to 900
        if (keynam .ne. 'BITPIX')then
                status=222
                go to 900
        end if
C       convert character string to integer
        call ftc2ii(value,bitpix,status)
        if (status .gt. 0)then
C               bitpix value must be an integer
                status=211
        end if

C       test that bitpix has a legal value
        call fttbit(bitpix,status)

C       check that the third keyword is NAXIS
        call ftgtkn(iunit,3,'NAXIS',naxis,status)
        if (status .eq. 208)then
C               third keyword was not NAXIS
                status=223
        else if (status .eq. 209)then
C               NAXIS value was not an integer
                status=212
        end if
        if (status .gt. 0)go to 900

        do 10 i=1,naxis
C               construct keyword name
                call ftkeyn('NAXIS',i,keynam,status)
C               attempt to read the keyword
                call ftgkyj(iunit,keynam,taxes,comm,status)
                if (status .gt. 0)then
                        status=224
                        return
                end if
                if (i .le. maxdim)then
                        naxes(i)=taxes
                end if
10      continue

C       now look for other keywords of interest: bscale, bzero, blank, and END
C       and pcount, gcount, and extend
15      bscale=1.
        bzero=0.
        pcount=0
        gcount=1
        extend=.false.
C       choose a special value to represent the absence of a blank value
        blank=123454321

        nkey=3+naxis
18      nblank=0
20      nkey=nkey+1
        call ftgkyn(iunit,nkey,keynam,value,comm,status)
        if (status .gt. 0)go to 900

        if (keynam .eq. 'BSCALE')then
C               convert character string to floating pt.
                call ftc2dd(value,bscale,status)
        else if (keynam .eq. 'BZERO')then
C               convert character string to floating pt.
                call ftc2dd(value,bzero,status)
        else if (keynam .eq. 'BLANK')then
C               convert character string to integer
                call ftc2ii(value,blank,status)
        else if (keynam .eq. 'PCOUNT')then
C               convert character string to integer
                call ftc2ii(value,pcount,status)
        else if (keynam .eq. 'GCOUNT')then
C               convert character string to integer
                call ftc2ii(value,gcount,status)
        else if (keynam .eq. 'EXTEND')then
C               convert character string to logical
                call ftc2ll(value,extend,status)
        else if (keynam .eq. ' ' .and. comm .eq. ' ')then
C               need to ignore trailing blank records before the END card
                nblank=nblank+1
                go to 20
        else if (keynam .eq. 'END')then
                go to 900
        end if
        if (status .gt. 0)go to 900
        go to 18

900     continue
        end
C----------------------------------------------------------------------
        subroutine fttbit(bitpix,status)

C       test that bitpix has a legal value

        integer bitpix,status

        if (status .gt. 0)return

        if (bitpix .ne. 8 .and. bitpix .ne. 16 .and. bitpix .ne. 32
     &      .and. bitpix .ne. -32 .and. bitpix .ne. -64)then
                status=211
        end if
        end
C----------------------------------------------------------------------
        subroutine ftptbh(ounit,ncols,nrows,nfield,ttype,tbcol,
     &  tform,tunit,extnam,status)

C       write required standard header keywords for an ASCII table extension 
C       THE PREFERRED NAME FOR THIS SUBROUTINE HAS BEEN CHANGED TO FTPHTB
C
C       ounit   i  fortran output unit number
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tbcol   i  beginning column of each field (array)
C       tform   c  Fortran-77 format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,ncols,nrows,nfield,tbcol(*),status
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftphtb(ounit,ncols,nrows,nfield,ttype,tbcol,
     &  tform,tunit,extnam,status)
        end
C----------------------------------------------------------------------
        subroutine ftphtb(ounit,ncols,nrows,nfield,ttype,tbcol,
     &  tform,tunit,extnam,status)

C       write required standard header keywords for an ASCII table extension 
C
C       ounit   i  fortran output unit number
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tbcol   i  beginning column of each field (array)
C       tform   c  Fortran-77 format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,ncols,nrows,nfield,tbcol(*),status,i
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*48,tfm*8

        comm='ASCII table extension'
        call ftpkys(ounit,'XTENSION','TABLE',comm,status)

        comm='8-bit ASCII characters'
        call ftpkyj(ounit,'BITPIX',8,comm,status)

        comm='2-dimensional ASCII table'
        call ftpkyj(ounit,'NAXIS',2,comm,status)

        if (status .gt. 0)return

        if (ncols .ge. 0)then
                comm='width of table in characters'
                call ftpkyj(ounit,'NAXIS1',ncols,comm,status)
        else
C               illegal table width
                status=217
                return
        end if

        if (status .gt. 0)return

        if (nrows .ge. 0)then
                comm='number of rows in table'
                call ftpkyj(ounit,'NAXIS2',nrows,comm,status)
        else
C               illegal number of rows in table
                status=218
        end if

        if (status .gt. 0)return

        comm='no group parameters (required keyword)'
        call ftpkyj(ounit,'PCOUNT',0,comm,status)

        comm='one data group (required)'
        call ftpkyj(ounit,'GCOUNT',1,comm,status)

        if (status .gt. 0)return

        if (nfield .ge. 0)then
                comm='number of fields in each row'
                call ftpkyj(ounit,'TFIELDS',nfield,comm,status)
        else
C               illegal number of fields
                status=216
        end if

        if (status .gt. 0)return

        do 10 i=1,nfield
            if (ttype(i) .ne. ' ' .and. ichar(ttype(i)(1:1)).ne.0)then
                comm='label for field '
                write(comm(17:19),1000)i
1000            format(i3)      
                call ftpkns(ounit,'TTYPE',i,1,ttype(i),comm,status)
            end if

            comm='beginning column of field '
            write(comm(27:29),1000)i
            call ftpknj(ounit,'TBCOL',i,1,tbcol(i),comm,status)

            comm='Fortran-77 format of field'
C           make sure format characters are in upper case:
            tfm=tform(i)
            call ftupch(tfm)
            call ftpkns(ounit,'TFORM',i,1,tfm,comm,status)

            if (tunit(i) .ne. ' ' .and. ichar(tunit(i)(1:1)).ne.0)then
                comm='physical unit of field'
                call ftpkns(ounit,'TUNIT',i,1,tunit(i),comm,status)
            end if
        if (status .gt. 0)return
10      continue        

        if (extnam .ne. ' ' .and. ichar(extnam(1:1)) .ne. 0)then
                comm='name of this ASCII table extension'
                call ftpkys(ounit,'EXTNAME',extnam,comm,status)
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgtbh(iunit,ncols,nrows,nfield,ttype,tbcol,
     &                    tform,tunit,extnam,status)

C       read required standard header keywords from an ASCII table extension 
C       NOTE: this is an obsolete interface routine; it is left here only
C             so that  existing software which used older versions of FITSIO
C             does not break.
C
C       iunit   i  Fortran i/o unit number
C       OUTPUT PARAMETERS:
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array)
C       tbcol   i  beginning column of each field (array)
C       tform   c  Fortran-77 format of each field (array)
C       tunit   c  units of each field (array)
C       extnam  c  name of table (optional)
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,ncols,nrows,nfield,status,tbcol(*)
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftghtb(iunit,0,ncols,nrows,nfield,ttype,
     &                    tbcol,tform,tunit,extnam,status)
        end
C----------------------------------------------------------------------
        subroutine ftghtb(iunit,maxfld,ncols,nrows,nfield,ttype,
     &                    tbcol,tform,tunit,extnam,status)

C       read required standard header keywords from an ASCII table extension 
C
C       iunit   i  Fortran i/o unit number
C       maxfld  i  maximum no. of fields to read; dimension of ttype
C       OUTPUT PARAMETERS:
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array)
C       tbcol   i  beginning column of each field (array)
C       tform   c  Fortran-77 format of each field (array)
C       tunit   c  units of each field (array)
C       extnam  c  name of table (optional)
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,maxfld,ncols,nrows,nfield,status,tbcol(*)
        integer i,nfind,maxf
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*72

        call ftgttb(iunit,ncols,nrows,nfield,status)
        if (status .gt. 0)return

        if (maxfld .le. 0)then
                maxf=nfield
        else
                maxf=min(maxfld,nfield)
        end if

C       initialize optional keywords
        do 10 i=1,maxf
                ttype(i)=' '
                tunit(i)=' '
10      continue

        call ftgkns(iunit,'TTYPE',1,maxf,ttype,nfind,status)
        call ftgkns(iunit,'TUNIT',1,maxf,tunit,nfind,status)

        if (status .gt. 0)return

        call ftgknj(iunit,'TBCOL',1,maxf,tbcol,nfind,status)
        if (status .gt. 0 .or. nfind .ne. maxf)then
C               couldn't find the required TBCOL keywords
                status=231
                return
        end if

        call ftgkns(iunit,'TFORM',1,maxf,tform,nfind,status)
        if (status .gt. 0 .or. nfind .ne. maxf)then
C               couldn't find the required TFORM keywords
                status=232
                return
        end if

        extnam=' '
        call ftgkys(iunit,'EXTNAME',extnam,comm,status)
C       this keyword is not required, so ignore 'keyword not found' status
        if (status .eq. 202)status =0
        end
C----------------------------------------------------------------------
        subroutine ftgttb(iunit,ncols,nrows,nfield,status)

C       test that this is a legal ASCII table, and get some keywords
C
C       iunit   i  Fortran i/o unit number
C       OUTPUT PARAMETERS:
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,ncols,nrows,nfield,status

        if (status .gt. 0)return

C       check that the first keyword is XTENSION = 'TABLE'
        call fttkyn(iunit,1,'XTENSION','''TABLE   ''',status)
        if (status .eq. 208)then
C               XTENSION keyword not found
                status=225
        else if (status .eq. 209)then
C               this is not an ASCII table extension
                status=226
        end if

C       check that the second keyword is BITPIX = 8
        call fttkyn(iunit,2,'BITPIX','8',status)
        if (status .eq. 208)then
C               BITPIX keyword not found
                status=222
        else if (status .eq. 209)then
C               illegal value of BITPIX
                status=211
        end if

C       check that the third keyword is NAXIS = 2
        call fttkyn(iunit,3,'NAXIS','2',status)
        if (status .eq. 208)then
C               NAXIS keyword not found
                status=223
        else if (status .eq. 209)then
C               illegal value of NAXIS
                status=212
        end if

C       check that the 4th keyword is NAXIS1 and get it's value
        call ftgtkn(iunit,4,'NAXIS1',ncols,status)
        if (status .eq. 208)then
C               NAXIS1 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal NAXIS1 value
                status=213
        end if

C       check that the 5th keyword is NAXIS2 and get it's value
        call ftgtkn(iunit,5,'NAXIS2',nrows,status)
        if (status .eq. 208)then
C               NAXIS2 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal NAXIS2 value
                status=213
        end if

C       check that the 6th keyword is PCOUNT = 0
        call fttkyn(iunit,6,'PCOUNT','0',status)
        if (status .eq. 208)then
C               PCOUNT keyword not found
                status=228      
        else if (status .eq. 209)then
C               illegal PCOUNT value
                status=214
        end if

C       check that the 7th keyword is GCOUNT = 1
        call fttkyn(iunit,7,'GCOUNT','1',status)
        if (status .eq. 208)then
C               GCOUNT keyword not found
                status=229
        else if (status .eq. 209)then
C               illegal value of GCOUNT
                status=215
        end if

C       check that the 8th keyword is TFIELDS
        call ftgtkn(iunit,8,'TFIELDS',nfield,status)
        if (status .eq. 208)then
C               TFIELDS keyword not found
                status=230
        else if (status .eq. 209)then
C               illegal value of TFIELDS
                status=216
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpbnh(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       write required standard header keywords for a binary table extension 
C       THE PREFERRED NAME OF THIS SUBROUTINE IS FTPHBN
C
C       ounit   i  fortran output unit number
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nrows,nfield,pcount,status
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftphbn(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)
        end
C----------------------------------------------------------------------
        subroutine ftphbn(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       write required standard header keywords for a binary table extension 
C
C       ounit   i  fortran output unit number
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nrows,nfield,pcount,status
        integer i,lenrow,dtype,rcount,xbcol,length,width
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*48,tfm*8

        comm='binary table extension'
        call ftpkys(ounit,'XTENSION','BINTABLE',comm,status)

        comm='8-bit bytes'
        call ftpkyj(ounit,'BITPIX',8,comm,status)

        comm='2-dimensional binary table'
        call ftpkyj(ounit,'NAXIS',2,comm,status)

        if (status .gt. 0)return

C       calculate the total width of each row, in bytes
        lenrow=0
        do 10 i=1,nfield
C               get the numerical datatype and repeat count of the field
                call ftbnfm(tform(i),dtype,rcount,width,status)
C               get the width of the field
                call ftgtbc(1,dtype,rcount,xbcol,length,status)
                lenrow=lenrow+length
10      continue

        comm='width of table in bytes'
        call ftpkyj(ounit,'NAXIS1',lenrow,comm,status)

        if (status .gt. 0)return

        if (nrows .gt. 0)then
                comm='number of rows in table'
                call ftpkyj(ounit,'NAXIS2',nrows,comm,status)
        else
                status=218
        end if

        if (status .gt. 0)return

        if (pcount .ge. 0)then
                comm='size of special data area'
                call ftpkyj(ounit,'PCOUNT',pcount,comm,status)
        else
                status=214
        end if

        comm='one data group (required keyword)'
        call ftpkyj(ounit,'GCOUNT',1,comm,status)

        comm='number of fields in each row'
        call ftpkyj(ounit,'TFIELDS',nfield,comm,status)

        if (status .gt. 0)return

        do 20 i=1,nfield
            if (ttype(i) .ne. ' ' .and. ichar(ttype(i)(1:1)).ne.0)then
                comm='label for field '
                write(comm(17:19),1000)i
1000            format(i3)      
                call ftpkns(ounit,'TTYPE',i,1,ttype(i),comm,status)
            end if

            comm='data format of the field'
C           make sure format characters are in upper case:
            tfm=tform(i)
            call ftupch(tfm)

C           Add datatype to the comment string:
            call ftbnfm(tfm,dtype,rcount,width,status)
            if (dtype .eq. 21)then
                comm(25:)=': 2-byte INTEGER'
            else if(dtype .eq. 41)then
                comm(25:)=': 4-byte INTEGER'
            else if(dtype .eq. 42)then
                comm(25:)=': 4-byte REAL'
            else if(dtype .eq. 82)then
                comm(25:)=': DOUBLE PRECISION'
            else if(dtype .eq. 16)then
                comm(25:)=': ASCII Character'
            else if(dtype .eq. 14)then
                comm(25:)=': LOGICAL'
            else if(dtype .eq. 11)then
                comm(25:)=': BYTE'
            else if(dtype .eq. 1)then
                comm(25:)=': BIT'
            else if(dtype .eq. 83)then
                comm(25:)=': COMPLEX'
            else if(dtype .eq. 163)then
                comm(25:)=': DOUBLE COMPLEX'
            end if

            call ftpkns(ounit,'TFORM',i,1,tfm,comm,status)

            if (tunit(i) .ne. ' ' .and. ichar(tunit(i)(1:1)).ne.0)then
                comm='physical unit of field'
                call ftpkns(ounit,'TUNIT',i,1,tunit(i),comm,status)
            end if
            if (status .gt. 0)return
20      continue        

        if (extnam .ne. ' ' .and. ichar(extnam(1:1)) .ne. 0)then
                comm='name of this binary table extension'
                call ftpkys(ounit,'EXTNAME',extnam,comm,status)
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgbnh(iunit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       read required standard header keywords from a binary table extension 
C       NOTE: this is an obsolete interface routine; it is left here only
C             so that  existing software which used older versions of FITSIO
C             does not break.
C
C       iunit   i  Fortran i/o unit number
C       OUTPUT PARAMETERS:
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array)
C       extnam  c  name of table (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,nrows,nfield,pcount,status
        character*(*) ttype(*),tform(*),tunit(*),extnam

        call ftghbn(iunit,0,nrows,nfield,ttype,tform,
     &                    tunit,extnam,pcount,status)
        end
C----------------------------------------------------------------------
        subroutine ftghbn(iunit,maxfld,nrows,nfield,ttype,tform,
     &                    tunit,extnam,pcount,status)

C       read required standard header keywords from a binary table extension 
C
C       iunit   i  Fortran i/o unit number
C       maxfld  i  maximum no. of fields to read; size of ttype array
C       OUTPUT PARAMETERS:
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array)
C       extnam  c  name of table (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,maxfld,ncols,nrows,nfield,pcount,status
        integer maxf,i,nfind
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*72

C       check that this is a valid binary table and get parameters
        call ftgtbn(iunit,ncols,nrows,pcount,nfield,status)
        if (status .gt. 0)return

        if (maxfld .le. 0)then
              maxf=nfield
        else
              maxf=min(maxfld,nfield)
        end if
C       initialize optional keywords
        do 10 i=1,maxf
                ttype(i)=' '
                tunit(i)=' '
10      continue

        call ftgkns(iunit,'TTYPE',1,maxf,ttype,nfind,status)
        call ftgkns(iunit,'TUNIT',1,maxf,tunit,nfind,status)

        if (status .gt. 0)return

        call ftgkns(iunit,'TFORM',1,maxf,tform,nfind,status)
        if (status .gt. 0 .or. nfind .ne. maxf)then
                status=232
                return
        end if

        extnam=' '
        call ftgkys(iunit,'EXTNAME',extnam,comm,status)
C       this keyword is not required, so ignore status
        if (status .eq. 202)status =0
        end
C----------------------------------------------------------------------
        subroutine ftgtbn(iunit,ncols,nrows,pcount,nfield,status)

C       check that this is a valid binary table and get parameters
C
C       iunit   i  Fortran i/o unit number
C       ncols   i  width of each row of the table, in bytes
C       nrows   i  number of rows in the table
C       pcount  i  size of special data area following the table (usually = 0)
C       nfield  i  number of fields in the table
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,ncols,nrows,nfield,pcount,status
        character keynam*8,value*20,comm*48,xtens*8

C       check for correct type of extension
        call ftgkyn(iunit,1,keynam,value,comm,status)
        if (status .gt. 0)return
        if (keynam .ne. 'XTENSION')then
                status=227
                return
        end if
C       convert quoted string to simple string
        call ftc2s(value,xtens,status)

        if (status .gt. 0 .or. (xtens .ne. 'BINTABLE' 
     &  .and. xtens .ne. 'A3DTABLE' .and. xtens .ne. '3DTABLE'))then
C               this is not a binary table extension
                status=227
                return
        end if

C       check that the second keyword is BITPIX = 8
        call fttkyn(iunit,2,'BITPIX','8',status)
        if (status .eq. 208)then
C               BITPIX keyword not found
                status=222
        else if (status .eq. 209)then
C               illegal value of BITPIX
                status=211
        end if

C       check that the third keyword is NAXIS = 2
        call fttkyn(iunit,3,'NAXIS','2',status)
        if (status .eq. 208)then
C               NAXIS keyword not found
                status=223
        else if (status .eq. 209)then
C               illegal NAXIS value
                status=212
        end if

C       check that the 4th keyword is NAXIS1 and get it's value
        call ftgtkn(iunit,4,'NAXIS1',ncols,status)
        if (status .eq. 208)then
C               NAXIS1 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal value of NAXISnnn
                status=213
        end if

C       check that the 5th keyword is NAXIS2 and get it's value
        call ftgtkn(iunit,5,'NAXIS2',nrows,status)
        if (status .eq. 208)then
C               NAXIS2 keyword not found
                status=224
        else if (status .eq. 209)then
C               illegal value of NAXISnnn
                status=213
        end if

C       check that the 6th keyword is PCOUNT and get it's value
        call ftgtkn(iunit,6,'PCOUNT',pcount,status)
        if (status .eq. 208)then
C               PCOUNT keyword not found
                status=228      
        else if (status .eq. 209)then
C               illegal PCOUNT value
                status=214
        end if

C       check that the 7th keyword is GCOUNT = 1
        call fttkyn(iunit,7,'GCOUNT','1',status)
        if (status .eq. 208)then
C               GCOUNT keyword not found
                status=229
        else if (status .eq. 209)then
C               illegal value of GCOUNT
                status=215
        end if

C       check that the 8th keyword is TFIELDS and get it's value
        call ftgtkn(iunit,8,'TFIELDS',nfield,status)
        if (status .eq. 208)then
C               TFIELDS keyword not found
                status=230
        else if (status .eq. 209)then
C               illegal value of TFIELDS
                status=216
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftrhdu(iunit,xtend,status)

C       read the CHDU structure by reading the header keywords which define
C       the size and structure of the header and data units.

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       xtend   i  returned type of extension:   0 = the primary HDU
C                                                1 = an ASCII table
C                                                2 = a binary table
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
        
        integer iunit,xtend,status
        character keynam*8,exttyp*10,comm*30

C       read first keyword to determine the type of the CHDU
        call ftgkyn(iunit,1,keynam,exttyp,comm,status)
        if (status .gt. 0)go to 900

        if (keynam .eq. 'SIMPLE')then
C               initialize the parameters describing the primay HDU
                call ftpini(iunit,status)
                xtend=0
        else if (keynam.eq.'XTENSION')then
                if (exttyp(2:9) .eq. 'TABLE   ')then
C                       initialize the parameters for the ASCII table extension
                        call ftaini(iunit,status)
                        xtend=1
                else if (exttyp(2:9) .eq. 'BINTABLE' .or. exttyp(2:9) 
     &          .eq. 'A3DTABLE' .or. exttyp(2:9) .eq. '3DTABLE')then
C                       initialize the parameters for the binary table extension
                        call ftbini(iunit,status)
                        xtend=2
                else if (exttyp(2:9) .eq. 'IMAGE   ' .or. exttyp(2:9) 
     &                   .eq. 'IUEIMAGE')then
C                       initialize the parameters describing the IMAGE extension
                        call ftpini(iunit,status)
                        xtend=0
                else
C                       unknown extension type
                        status=251
                        xtend=-1
                end if
        else
C               unknown record 
                status=252
                xtend=-1
        end if
900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftpini(iunit,status)

C       initialize the parameters defining the structure of the primary data

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bitpix,naxis,naxes(99),pcnt,gcnt,ttype
        integer blank,bytlen,npix,i,nblank
        double precision bscale,bzero
        logical simple,extend

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       temporarily set the location of the end of the header to a huge number
        hdend(ibuff)=100000000

C       get the standard header keywords
        call ftgphx(iunit,99,simple,bitpix,naxis,naxes,
     &        pcnt,gcnt,extend,bscale,bzero,blank,nblank,status)
        if (status .gt. 0)return

        if (naxis .gt. 99)then
C               the image array has too many dimensions for me to handle
                status=111
                return
        end if

C       test  bitpix and set the datatype code value
        if (bitpix .eq. 8)then
                ttype=11
                bytlen=1
        else if (bitpix .eq. 16)then
                ttype=21
                bytlen=2
        else if (bitpix .eq. 32)then
                ttype=41
                bytlen=4
        else if (bitpix .eq. -32)then
                ttype=42
                bytlen=4
        else if (bitpix .eq. -64)then
                ttype=82
                bytlen=8
        end if
        
C       calculate the size of the primary array
        if (naxis .eq. 0)then
                npix=0
        else
                npix=1
                do 10 i=1,naxis
                        if (naxes(i) .gt. 0)npix=npix*naxes(i)
10              continue
        end if

C       now we know everything about the array; just fill in the parameters:
C       the 'END' record is 80 bytes before the current position, ignoring
C       any trailing blank keywords just before the END keyword.
30      hdend(ibuff)=nxthdr(ibuff)-80*(nblank+1)

C       the data unit begins at the beginning of the next logical block
        dtstrt(ibuff)=((nxthdr(ibuff)-80)/2880+1)*2880

C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &  dtstrt(ibuff)+((pcnt+npix)*bytlen*gcnt+2879)/2880*2880

C       quit if there is no data
        if (naxis .eq. 0)then
                tfield(ibuff)=0
                rowlen(ibuff)=0
                go to 900
        end if

C       the primary array is actually interpreted as a binary table.  There
C       are two columns: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data.  Each group is in a separate row of the table.

        tfield(ibuff)=2
        tdtype(1,ibuff)=ttype
        tdtype(2,ibuff)=ttype
        trept(1,ibuff)=pcnt
        trept(2,ibuff)=npix
        tnull(1,ibuff)=blank
        tnull(2,ibuff)=blank
        tscale(1,ibuff)=1.
        tscale(2,ibuff)=bscale
        tzero(1,ibuff)=0.
        tzero(2,ibuff)=bzero
        tbcol(1,ibuff)=0
        tbcol(2,ibuff)=pcnt*bytlen
        rowlen(ibuff)=(pcnt+npix)*bytlen

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftaini(iunit,status)

C       initialize the parameters defining the structure of an ASCII table 

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer nrows,tfld,nkey,ibuff,i,nblank
        character keynam*8,value*20,comm*8

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       temporarily set the location of the end of the header to a huge number
        hdend(ibuff)=1000000000

C       check that this is a valid ASCII table, and get parameters
        call ftgttb(iunit,rowlen(ibuff),nrows,tfld,status)
        if (status .gt. 0)go to 900

        if (tfld .gt. nf)then
C               arrays not dimensioned large enough for this many fields
                status=111
                go to 900
         end if

C       store the number of fields in the common block
        tfield(ibuff)=tfld

C       initialize the table field parameters
        do 5 i=1,tfld
                tscale(i,ibuff)=1.
                tzero(i,ibuff)=0.
C               choose special value to indicate that null value is not defined
                cnull(i,ibuff)=char(1)
5       continue

C       now read through the rest of the header looking for table column
C       definition keywords, and the END keyword.

        nkey=8
8       nblank=0
10      nkey=nkey+1
        call ftgkyn(iunit,nkey,keynam,value,comm,status)
        if (status .gt. 0)go to 900
        if (keynam(1:1) .eq. 'T')then
C               get the ASCII table parameter (if it is one)
                call ftgatp(ibuff,keynam,value,status)
        else if (keynam .eq. ' ' .and. comm .eq. ' ')then
                nblank=nblank+1
                go to 10
        else if (keynam .eq. 'END')then
                go to 20
        end if
        go to 8

20      continue

C       now we know everything about the table; just fill in the parameters:
C       the 'END' record begins 80 bytes before the current position,
C       ignoring any trailing blank keywords just before the END keyword
        hdend(ibuff)=nxthdr(ibuff)-80*(nblank+1)

C       the data unit begins at the beginning of the next logical block
        dtstrt(ibuff)=((nxthdr(ibuff)-80)/2880+1)*2880

C       reset header pointer to the first keyword
        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))

C       there is no special data following an ASCII table
        scount(ibuff)=0

C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &  dtstrt(ibuff)+(rowlen(ibuff)*nrows+2879)/2880*2880

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftbini(iunit,status)

C       initialize the parameters defining the structure of a binary table 

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer lenrow,nrows,pcnt,tfld,nkey,ibuff,i,j,nblank
        character keynam*8,value*20,comm*8

        if (status .gt. 0)return

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       temporarily set the location of the end of the header to a huge number
        hdend(ibuff)=1000000000

C       check that this is a valid binary table, and get parameters
        call ftgtbn(iunit,rowlen(ibuff),nrows,pcnt,tfld,status)
        if (status .gt. 0)go to 900

        if (tfld .gt. nf)then
C               arrays not dimensioned large enough for this many fields
                status=111
                go to 900
         end if

C       store the number of fields in the common block
        tfield(ibuff)=tfld

C       initialize the table field parameters
        do 5 i=1,tfld
                tscale(i,ibuff)=1.
                tzero(i,ibuff)=0.
                tnull(i,ibuff)=123454321
5       continue

C       initialize the default heap starting address (immediately following
C       the table data) and set the next empty heap address undefined
        theap(ibuff)=rowlen(ibuff)*nrows
        nxheap(ibuff)=100000000

C       now read through the rest of the header looking for table column
C       definition keywords, and the END keyword.

        nkey=8
8       nblank=0
10      nkey=nkey+1
        call ftgkyn(iunit,nkey,keynam,value,comm,status)
        if (status .gt. 0)go to 900
        if (keynam(1:1) .eq. 'T')then
C               get the binary table parameter (if it is one)
                call ftgbtp(ibuff,keynam,value,status)
        else if (keynam .eq. ' ' .and. comm .eq. ' ')then
                nblank=nblank+1
                go to 10
        else if (keynam .eq. 'END')then
                go to 20
        end if
        go to 8

20      continue

C       now we know everything about the table; just fill in the parameters:
C       the 'END' record begins 80 bytes before the current position, ignoring
C       any trailing blank keywords just before the END keyword
        hdend(ibuff)=nxthdr(ibuff)-80*(nblank+1)

C       the data unit begins at the beginning of the next logical block
        dtstrt(ibuff)=((nxthdr(ibuff)-80)/2880+1)*2880

C       reset header pointer to the first keyword
        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))

C       PCOUNT specifies the amount of special data following the table
        scount(ibuff)=pcnt

C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &  dtstrt(ibuff)+(rowlen(ibuff)*nrows+pcnt+2879)/2880*2880

C       determine the byte offset of the beginning of each field
        call ftgtbc(tfld,tdtype(1,ibuff),trept(1,ibuff),
     &              tbcol(1,ibuff),lenrow,status)

C       FITSIO deals with ASCII columns as arrays of strings, not
C       arrays of characters, so need to change the repeat count
C       to indicate the number of strings in the field, not the 
C       total number of characters in the field.
        do 30 i=1,tfld
                if (tdtype(i,ibuff) .eq. 16)then
                    j=trept(i,ibuff)/tnull(i,ibuff)
                    trept(i,ibuff)=max(j,1)
                end if
30      continue
        if (status .gt. 0)go to 900

C       check that the sum of the column widths = NAXIS2 value
        if (rowlen(ibuff) .ne. lenrow)then
                status=241
        end if

900     continue
        end
C--------------------------------------------------------------------------
        subroutine fttkyn(iunit,nkey,keynam,keyval,status)

C       test that the keyword number NKEY has name = KEYNAM
C       and has value = KEYVAL
C
C       iunit   i  Fortran I/O unit number
C       nkey    i  sequence number of the keyword to test
C       keynam  c  name that the keyword is supposed to have
C       keyval  c  value that the keyword is supposed to have
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C
        integer iunit,nkey,status
        character*(*) keynam,keyval
        character kname*8,value*30,comm*48

C       read the name and value of the keyword
        call ftgkyn(iunit,nkey,kname,value,comm,status)
        if (status .gt. 0)go to 900

C       test if the keyword has the correct name
        if (kname .ne. keynam)then
                status=208
                go to 900
        end if

C       check that the keyword has the correct value
        if (value .ne. keyval)then
                status=209
                go to 900
        end if

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgtkn(iunit,nkey,keynam,ival,status)

C       test that keyword number NKEY has name = KEYNAM and get the
C       integer value of the keyword.  Return an error if the keyword
C       name does not match the input KEYNAM, or if the value of the
C       keyword is not a positive integer.
C
C       iunit   i  Fortran I/O unit number
C       nkey    i  sequence number of the keyword to test
C       keynam  c  name that the keyword is supposed to have
C       OUTPUT PARAMETERS:
C       ival    i  returned value of the integer keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C
        integer iunit,nkey,status,ival
        character*(*) keynam
        character kname*8,value*30,comm*48

C       read the name and value of the keyword
        call ftgkyn(iunit,nkey,kname,value,comm,status)
        if (status .gt. 0)go to 900

C       test if the keyword has the correct name
        if (kname .ne. keynam)then
                status=208
                go to 900
        end if

C       convert character string to integer
        call ftc2ii(value,ival,status)
        if (status .gt. 0 .or. ival .lt. 0 )then
C               keyword value must be zero or positive integer
                status=209
        end if

900     continue
        end
C--------------------------------------------------------------------------
        subroutine ftgatp(ibuff,keynam,value,status)

C       Get ASCII Table Parameter
C       test if the keyword is one of the table column definition keywords
C       of an ASCII table. If so, decode it and update the value in the common 
C       block

C       ibuff   i sequence number of the data buffer
C       OUTPUT PARAMETERS:
C       keynam  c name of the keyword
C       value   c value of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ibuff,status
        character keynam*8,value*20

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
C       nb = number of file buffers = max. number of FITS file opened at once
C       nf = maximum number of fields allowed in a table
        integer nf,nb
        parameter (nb = 12)
        parameter (nf = 512)

C       tfield = number of fields in the table
C       tbcol = byte offset in the row of the beginning of the column
C       rowlen = length of one row of the table, in bytes
C       tdtype = integer code representing the datatype of the column
C       trept = the repeat count = number of data values/element in the column
C       tnull = the value used to represent an undefined value in the column
C       tscale = the scale factor for the column
C       tzero = the scaling zero point for the column
C       scount = the number of bytes of special data following the binary table
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)

C       cnull = character string representing nulls in character columns
C       cform = the Fortran format of the column
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer nfield,i,c2,bcol
        character tform*8

        if (status .gt. 0)return

        if (keynam(1:5) .eq. 'TFORM')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TFORMn keyword
                    status=0
                else
C                   get the TFORM character string, without quotes
                    call ftc2s(value,tform,status)
                    if (status .gt. 0)return
                    cform(nfield,ibuff)=tform
C                   set numeric data type code to indicate an ASCII table field
                    tdtype(nfield,ibuff)=16
C                   set the repeat count to 1
                    trept(nfield,ibuff)=1
C                   set the TNULL parameter to the width of the field:
                    c2=0
                    do 10 i=2,8
                        if (tform(i:i) .ge. '0' .and. tform(i:i)
     &                     .le. '9')then
                                c2=i
                        else
                                go to 20
                        end if
10                  continue
20                  continue

                    if (status .gt. 0)return
                    if (c2 .eq. 0)then
C                       no explicit field width, so assume width=1 character
                        tnull(nfield,ibuff)=1
                    else
                        call ftc2ii(tform(2:c2),tnull(nfield,ibuff),
     &                              status)
                        if (status .gt. 0)then
C                               error parsing the TFORM value string
                                status=261
                        end if
                    end if
                end if
        else if (keynam(1:5) .eq. 'TBCOL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TBCOLn keyword
                    status=0
                else
C                   get the beginning column number
                    call ftc2ii(value,bcol,status)
                    tbcol(nfield,ibuff)=bcol-1
                end if
        else if (keynam(1:5) .eq. 'TSCAL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TSCALn keyword
                    status=0
                else
C                   get the scale factor
                    call ftc2dd(value,tscale(nfield,ibuff),status)
                end if
        else if (keynam(1:5) .eq. 'TZERO')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TZEROn keyword
                    status=0
                else
C                   get the scaling zero point
                    call ftc2dd(value,tzero(nfield,ibuff),status)
                end if
        else if (keynam(1:5) .eq. 'TNULL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TFORMn keyword
                    status=0
                else
C                   get the Null value flag (character)
                    call ftc2s(value,cnull(nfield,ibuff),status)
                end if
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftgbtp(ibuff,keynam,value,status)

C       Get Binary Table Parameter
C       test if the keyword is one of the table column definition keywords
C       of a binary table. If so, decode it and update the values in the common
C       block

C       ibuff   i sequence number of the data buffer
C       OUTPUT PARAMETERS:
C       keynam  c name of the keyword
C       value   c value of the keyword
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ibuff,status,width
        character keynam*8,value*20

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
C       nb = number of file buffers = max. number of FITS file opened at once
C       nf = maximum number of fields allowed in a table
        integer nf,nb
        parameter (nb = 12)
        parameter (nf = 512)

C       tfield = number of fields in the table
C       tbcol = byte offset in the row of the beginning of the column
C       rowlen = length of one row of the table, in bytes
C       tdtype = integer code representing the datatype of the column
C       trept = the repeat count = number of data values/element in the column
C       tnull = the value used to represent an undefined value in the column
C       tscale = the scale factor for the column
C       tzero = the scaling zero point for the column
C       scount = the number of bytes of special data following the binary table
C       theap = the starting byte offset for the binary table heap, relative
C               to the start of the binary table data
C       nxheap = the next empty heap location

        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)

C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer nfield
        character tform*8

        if (status .gt. 0)return

        if (keynam(1:5) .eq. 'TFORM')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TFORMn keyword
                    status=0
                else
C                   get the TFORM character string, without quotes
                    call ftc2s(value,tform,status)
C                   get the datatype code and repeat count
                    call ftbnfm(tform,tdtype(nfield,ibuff),
     &                 trept(nfield,ibuff),width,status)
C                   store the width of the ASCII field in the TNULL parameter
                    if (tdtype(nfield,ibuff) .eq. 16)then
                        tnull(nfield,ibuff)=width
                   end if
                end if
        else if (keynam(1:5) .eq. 'TSCAL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TSCALn keyword
                    status=0
                else
C                   get the scale factor
                    call ftc2dd(value,tscale(nfield,ibuff),status)
                end if
        else if (keynam(1:5) .eq. 'TZERO')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TZEROn keyword
                    status=0
                else
C                   get the scaling zero point
                    call ftc2dd(value,tzero(nfield,ibuff),status)
                end if
        else if (keynam(1:5) .eq. 'TNULL')then
C               get the field number
                call ftc2ii(keynam(6:8),nfield,status)
                if (keynam(6:8) .eq. '   ' .or. status .gt. 0)then
C                   this must not have been a TNULLn keyword
                    status=0
                else
C                   get the Null value flag (Integer)
                    call ftc2ii(value,tnull(nfield,ibuff),status)
                end if
        else if (keynam(1:8) .eq. 'THEAP   ')then
C               get the heap offset value
                call ftc2ii(value,theap(ibuff),status)
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgabc(nfield,tform,space, rowlen,tbcol,status)

C       Get ASCII table Beginning Columns
C       determine the byte offset of the beginning of each field of a 
C       ASCII table, and the total width of the table

C       nfield i  number of fields in the binary table
C       tform  c  array of FITS datatype codes of each column.
C                 must be left justified in the string variable
C       space  i  number of blank spaces to insert between each column
C       OUTPUT PARAMETERS:
C       rowlen i  total width of the table, in bytes
C       tbcol  i  beginning position of each column (first column begins at 1)
C       status i  returned error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1992

        integer nfield,space,rowlen,tbcol(*),status
        character*(*) tform(*)
        integer i,j,ival

        if (status .gt. 0)return

        rowlen=0
        do 100 i=1,nfield
                if (tform(i)(2:2) .eq. ' ')then
C                       no explicit width; assume width=1
                        ival=1
                else
C                       find the field width characters
                        j=2
10                      j=j+1
                        if (tform(i)(j:j) .eq. ' ' .or. 
     &                      tform(i)(j:j) .eq. '.')then
C                           read the width
                            call ftc2ii(tform(i)(2:j-1),ival,status)
                        else
C                           keep looking for the end of the width field
                            go to 10
                        end if
                        tbcol(i)=rowlen+1
                        rowlen=rowlen+ival+space
                end if
100     continue

C       don't add space after the last field
        rowlen=rowlen-space
        end
C----------------------------------------------------------------------
        subroutine ftgtbc(tfld,tdtype,trept,tbcol,lenrow,status)

C       Get Table Beginning Columns
C       determine the byte offset of the beginning of each field of a 
C       binary table

C       tfld   i  number of fields in the binary table
C       tdtype i array of numerical datatype codes of each column
C       trept  i array of repetition factors for each column
C       OUTPUT PARAMETERS:
C       tbcol  i array giving the byte offset to the start of each column
C       lenrow i total width of the table, in bytes
C       status i  returned error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C       modified 6/17/92 to deal with ASCII column trept values measured
C       in units of characters rather than in terms of number of repeated
C       strings.

        integer tfld,tdtype(*),trept(*),tbcol(*),lenrow
        integer status,i,nbytes

        if (status .gt. 0)return

C       the first column always begins at the first byte of the row:
        tbcol(1)=0
        
        do 100 i=1,tfld-1
                if (tdtype(i) .eq. 16)then
C                       ASCII field; each character is 1 byte
                        nbytes=1
                else if (tdtype(i) .gt. 0)then
                        nbytes=tdtype(i)/10
                else 
C                       this is a descriptor field: 2J
                        nbytes=8
                end if

                if (nbytes .eq. 0)then
C                       this is a bit array
                        tbcol(i+1)=tbcol(i)+(trept(i)+7)/8
                else
                        tbcol(i+1)=tbcol(i)+trept(i)*nbytes
                end if
100     continue

C       determine the total row width
        if (tdtype(tfld) .eq. 16)then
C               ASCII field; each character is 1 byte
                nbytes=1
        else if (tdtype(tfld) .gt. 0)then
                nbytes=tdtype(tfld)/10
        else
                nbytes=8
        end if
        if (nbytes .eq. 0)then
C               this is a bit array
                lenrow=tbcol(tfld)+(trept(tfld)+7)/8
        else
                lenrow=tbcol(tfld)+trept(tfld)*nbytes
        end if

        end
C----------------------------------------------------------------------
        subroutine ftbnfm(form,dtype,rcount,width,status)

C       'Binary Format'
C       parse the binary table column format to determine the data
C       type and the repeat count (and string width, if it is an ASCII field)
C
C       form    c  format string
C       OUTPUT PARAMETERS:
C       dattyp  i  datatype code
C       rcount  i  repeat count
C       width   i  if ASCII field, this is the width of the unit string
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) form
        integer dtype,rcount,width,status
        character dattyp*1
        integer point,nc,c1,i,nw

        if (status .gt. 0)return

C       find first non-blank character
        nc=len(form)
        do 5 i=1,nc
                if (form(i:i) .ne. ' ')then
                        c1=i
                        go to 10
                end if
5       continue

C       error: TFORM is a blank string
        status=261
        return

10      continue

C       find the size of the field repeat count, if present
        nw=0
        do 20 i=c1,nc
                if (form(i:i) .ge. '0' .and. form(i:i) .le. '9')then
                        nw=nw+1
                else
                        go to 30
                end if
20      continue
30      continue
        if (nw .eq. 0)then
C               no explicit repeat count, so assume a value of 1
                rcount=1
        else
                call ftc2ii(form(c1:c1+nw-1),rcount,status)
                if (status .gt. 0)return
        end if

        c1=c1+nw

C       see if this is a variable length pointer column (e.g., 'rPt'); if so,
C       then add 1 to the starting search position in the TFORM string
        if (form(c1:c1) .eq. 'P')then
                point=-1
                c1=c1+1
                rcount=1
        else
                point=1
        end if

C       now the chararcter at position c1 should be the data type code
        dattyp=form(c1:c1)
C       make sure that datatype code is uppercase:
        call ftupch(dattyp)

C       set the numeric datatype code
        if (dattyp .eq. 'I')then
                dtype=21
        else if (dattyp .eq. 'J')then
                dtype=41
        else if (dattyp .eq. 'E')then
                dtype=42
        else if (dattyp .eq. 'D')then
                dtype=82
        else if (dattyp .eq. 'A')then
                dtype=16
        else if (dattyp .eq. 'L')then
                dtype=14
        else if (dattyp .eq. 'X')then
C            interprete the X datatype as a series of 8-bit bytes (B datatype)
                dtype=11
                rcount=(rcount+7)/8
        else if (dattyp .eq. 'B')then
                dtype=11
        else if (dattyp .eq. 'C')then
                dtype=83
        else if (dattyp .eq. 'M')then
                dtype=163
        else
C               unknown tform datatype code
                status=262
                return
        end if
                
C       set dtype negative if this is a variable length field ('P')
        dtype=dtype*point

C       if this is an ASCII field, determine its width
        if (dtype .eq. 16)then
                c1=c1+1
                nw=0
                do 40 i=c1,nc
                    if (form(i:i) .ge. '0' .and. form(i:i).le.'9')then
                        nw=nw+1
                else
                        go to 50
                end if
40              continue
50              continue
                if (nw .eq. 0)then
C                       no explicit width field, so assume that the
C                       width is the same as the repeat count
                        width=rcount
                else
                        call ftc2ii(form(c1:c1+nw-1),width,status)
                        if (status .gt. 0)then
C                       unrecognized characters following the 'A', so ignore it
                               width=rcount
                               status=0
                        end if
                end if
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftinit(funit,fname,block,status)

C       open a new FITS file with write access
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       block   i  input record length blocking factor
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer funit,status,block
        character*(*) fname

C       call the machine dependent routine which actually creates the file
        call ftopnx(funit,fname,1,1,block,status)
        end
C--------------------------------------------------------------------------
        subroutine ftopen(funit,fname,rwmode,block,status)

C       open an existing FITS file with readonly or read/write access
C
C       funit   i  Fortran I/O unit number
C       fname   c  name of file to be opened
C       rwmode  i  file access mode: 0 = readonly; else = read and write
C       block   i  returned record length blocking factor
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer funit,rwmode,block,status
        character*(*) fname

C       call the machine dependent routine which actually opens the file
        call ftopnx(funit,fname,0,rwmode,block,status)

C       determine the structure and size of the primary HDU
        call ftpini(funit,status)
        end
C--------------------------------------------------------------------------
        subroutine ftclos(iunit,status)

C       close a FITS file that was previously opened with ftopen or ftinit
C
C       iunit   i  Fortran I/O unit number
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

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
C       END OF COMMON BLOCK DEFINITIONS---------------------------------------

        integer ibuff

C       close the current HDU
        call ftchdu(iunit,status)

        ibuff=bufnum(iunit)

C       ignore end of file error message
C       move to the end of the file (to insure that all the records are saved)
        call ftmbyt(iunit,hdstrt(ibuff,maxhdu(ibuff)+1),.true.,status)

C       close the file
        close(iunit,err=900)

        bufnum(iunit)=0
        bufpnt(ibuff)=0
        return

900     status=110
        end
C--------------------------------------------------------------------------
        subroutine fthdef(ounit,moreky,status)

C       Header DEFinition
C       define the size of the current header unit; this simply lets
C       us determine where the data unit will start
C
C       ounit   i  Fortran I/O unit number
C       moreky  i  number of additional keywords to reserve space for
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,moreky,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,mkeys

        if (status .gt. 0)return

C       based on the number of keywords which have already been written,
C       plus the number of keywords to reserve space for, we then can
C       define where the data unit should start (it must start at the
C       beginning of a 2880-byte logical block).

        ibuff=bufnum(ounit)

        mkeys=max(moreky,0)
        dtstrt(ibuff)=((hdend(ibuff)+mkeys*80)/2880+1)*2880
        end
C--------------------------------------------------------------------------
        subroutine ftghsp(ounit,nexist,nmore,status)

C       Get Header SPace
C       return the number of additional keywords that will fit in the header
C
C       ounit   i  Fortran I/O unit number
C       nexist  i  number of keywords already present in the CHU
C       nmore   i  number of additional keywords that will fit in header
C                 -1 indicates that there is no limit to the number of keywords
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nexist,nmore,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff
        if (status .gt. 0)return
        ibuff=bufnum(ounit)

        nexist=(hdend(ibuff)-hdstrt(ibuff,chdu(ibuff)))/80
        if (dtstrt(ibuff) .lt. 0)then
C               the max size of the header has not been defined, so there
C               is no limit to the number of keywords which may be written.
                nmore=-1                
        else
                nmore=(dtstrt(ibuff)-hdend(ibuff))/80-1
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftgthd(tmplat,card,hdtype,status)

C       'Get Template HeaDer'
C       parse a template header line and create a formated
C       80-character string which is suitable for appending to a FITS header 

C       tmplat  c  input header template string
C       card    c  returned 80-character string = FITS header record
C       hdtype  i  type of operation that should be applied to this keyword:
C                      -1  =  delete this keyword 
C                       0  =  append (if it doesn't already exist) or 
C                             overwrite this keyword (if it does exist)
C                       1  =  append this comment keyword ('HISTORY', 
C                             'COMMENT', or blank keyword name) 
C                       2  =  this is an END record; do not append it
C                             to a FITS header! 
C       status  i  returned error status 
C               if a positive error status is returned then the first 
C               80 characters of the offending input line are returned
C               by the CARD parameter

        integer hdtype,status
        character*(*) tmplat
        character*80 card
        integer i1,i2,com1,strend,length
        character inline*100,keynam*8,ctemp*80
        logical number
        double precision dvalue

        if (status .gt. 0)return
        card=' '
        hdtype=0

        inline=tmplat

C       test if columns 1-8 are blank; if so, this is a FITS comment record;
C       just copy it verbatim to the FITS header
        if (inline(1:8) .eq. ' ')then
                card=inline(1:80)
                go to 999
        end if

C       parse the keyword name = the first token separated by a space or a '='
C       1st locate the first nonblank character (we know it is not all blank):
        i1=0
20      i1=i1+1
C       test for a leading minus sign which flags name of keywords to be deleted
        if (inline(i1:i1) .eq. '-')then
                hdtype=-1
                go to 20
        else if (inline(i1:i1) .eq. ' ')then
                go to 20
        end if

C       now find the last character of the keyword name
        i2=i1
30      i2=i2+1
        if (inline(i2:i2) .ne. ' ' .and. inline(i2:i2) .ne. '=')go to 30

C       test for legal keyword name length (max 8 characters)
        if (i2-i1 .gt. 8)then
                status=207
                card=inline(1:80)
                go to 999
        end if

        keynam=inline(i1:i2-1)

C       convert to upper case and test for illegal characters in keyword name
        call ftupch(keynam)
        call fttkey(keynam,status)
        if (status .gt. 0)then
                card=inline(1:80)
                go to 999
        end if

C       if this is the 'END' then this is the end of the input file
        if (keynam .eq. 'END     ')goto 998

C       copy the keyword name to the output record string
        card(1:8)=keynam

C       quite if this is just the name of keyword to be deleted
        if (hdtype .lt. 0)go to 999

C       test if this is a COMMENT or HISTORY record
        if (keynam .eq. 'COMMENT' .or. keynam .eq. 'HISTORY')then
C               append next 72 characters from input line to output record
                card(9:80)=inline(i2:)
                hdtype=1
                go to 999
        else
C               this keyword must have a value, so append the '= ' to output
                card(9:10)='= '
        end if

C       now locate the value token in the input line.  If it includes
C       embedded spaces it must be enclosed in single quotes. The value must
C       be separated by at least one blank space from the comment string

C       find the first character of the value string
        i1=i2
40      i1=i1+1
        if (i1 .gt. 100)then
C               no value is present in the input line
                status=204
                card=inline(1:80)
                go to 999
        end if
        if (inline(i1:i1) .eq. ' ' .or. inline(i1:i1) .eq. '=')go to 40

C       is the value a quoted string?
        if (inline(i1:i1) .eq. '''')then
C               find the closing quote 
                i2=i1
50              i2=i2+1
                if (i2 .gt. 100)then
C                       error: no closing quote on value string
                        status=205
                        card=inline(1:80)
                        go to 999
                end if
                if (inline(i2:i2) .eq. '''')then
                        if (inline(i2+1:i2+1) .eq. '''')then
C                               ignore 2 adjacent single quotes
                                i2=i2+1
                                go to 50
                        end if
                else
                        go to 50
                end if
C               value string can't be more than 70 characters long (cols 11-80)
                length=i2-i1
                if (length .gt. 69)then
                        status=205
                        card=inline(1:80)
                        go to 999
                end if

C               append value string to output, left justified in column 11
                card(11:11+length)=inline(i1:i2)
C               com1 is the starting position for the comment string
                com1=max(32,13+length)

C               FITS string must be at least 8 characters long
                if (length .lt. 9)then
                        card(11+length:11+length)=' '
                        card(20:20)=''''
                end if
        else
C               find the end of the value field
                i2=i1
60              i2=i2+1
                if (i2 .gt. 100)then
C                       error: value string is too long
                        status=205
                        card=inline(1:80)
                        go to 999
                end if
                if (inline(i2:i2) .ne. ' ')go to 60

C               test if this is a logical value
                length=i2-i1
                if (length .eq. 1 .and. (inline(i1:i1) .eq. 'T'
     &              .or. inline(i1:i1) .eq. 'F'))then
                        card(30:30)=inline(i1:i1)
                        com1=32
                else
C                   test if this is a numeric value; try reading it as 
C                   double precision value; if it fails, it must be a string
                    number=.true.
                    call ftc2dd(inline(i1:i2-1),dvalue,status)
                    if (status .gt. 0)then
                        status=0
                        number=.false.
                    end if

                    if (number)then
                        if (length .le. 20)then
C                               write the value right justified in col 30
                                card(31-length:30)=inline(i1:i2-1)
                                com1=32
                        else
C                               write the long value left justified in col 11
                                card(11:10+length)=inline(i1:i2-1)
                                com1=max(32,12+length)
                        end if
                    else
C                       value is a character string datatype
                        card(11:11)=''''
                        strend=11+length
                        card(12:strend)=inline(i1:i2-1)
C                       need to expand any embedded single quotes into 2 quotes
                        i1=11
70                      i1=i1+1
                        if (i1 .gt. strend) go to 80
                        if (card(i1:i1) .eq. '''')then
                                i1=i1+1
                                if (card(i1:i1) .ne. '''')then
C                                       have to insert a 2nd quote into string
                                        ctemp=card(i1:strend)
                                        card(i1:i1)=''''
                                        strend=strend+1
                                        i1=i1+1
                                        card(i1:strend)=ctemp
                                end if
                        end if
                        go to 70

80                      strend=max(20,strend+1)
                        card(strend:strend)=''''
                        com1=max(32,strend+2)
                    end if
                end if
        end if
        
C       is there room for a comment string?
        if (com1 .lt. 79)then
C               now look for the beginning of the comment string
                i1=i2
90              i1=i1+1
C               if no comment field then just quit
                if (i1 .gt. 100)go to 999
                if (inline(i1:i1) .eq. ' ')go to 90

C               append the comment field
                if (inline(i1:i1) .eq. '/')then
                        card(com1:80)=inline(i1:)
                else
                        card(com1:80)='/ '//inline(i1:)
                end if
        end if
        go to 999

C       end of input file was detected
998     hdtype=2

999     continue
        end
C--------------------------------------------------------------------------
        subroutine ftddef(ounit,bytlen,status)

C       Data DEFinition
C       re-define the length of the data unit
C       this simply redefines the start of the next HDU
C
C       ounit   i  Fortran I/O unit number
C       bytlen  i  new length of the data unit, in bytes
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,bytlen,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
        end if

        hdstrt(ibuff,chdu(ibuff)+1)=
     &          dtstrt(ibuff)+(bytlen+2879)/2880*2880
        end
C--------------------------------------------------------------------------
        subroutine ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,
     &                    status)

C       Primary data DEFinition
C       define the structure of the primary data unit or an IMAGE extension
C
C       ounit   i  Fortran I/O unit number
C       bitpix  i  bits per pixel value
C       naxis   i  number of data axes
C       naxes   i  length of each data axis (array)
C       pcount  i  number of group parameters
C       gcount  i  number of 'random groups'
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,bitpix,naxis,naxes(*),pcount,gcount,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,ttype,bytlen,npix,i,pcnt,gcnt

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
                if (status .gt. 0)return
        end if

C       check for error conditions
        if (naxis .lt. 0)then
                status=212
        else if (pcount .lt. 0)then
                status=214
        else if (gcount .lt. 0)then
                status=215
        else
                go to 5
        end if
        return

C       test that bitpix has a legal value and set the datatype code value
5       if (bitpix .eq. 8)then
                ttype=11
                bytlen=1
        else if (bitpix .eq. 16)then
                ttype=21
                bytlen=2
        else if (bitpix .eq. 32)then
                ttype=41
                bytlen=4
        else if (bitpix .eq. -32)then
                ttype=42
                bytlen=4
        else if (bitpix .eq. -64)then
                ttype=82
                bytlen=8
        else
C               illegal value of bitpix
                status=211
                return
        end if

C       calculate the number of pixels in the array
        if (naxis .eq. 0)then
C               no data
                npix=0
                gcnt=0
                pcnt=0
        else
C               make sure that the gcount is not zero
                gcnt=max(gcount,1)
                pcnt=pcount        
                npix=1
                do 10 i=1,naxis
                        if (naxes(i) .gt. 0)then
                                npix=npix*naxes(i)
                        else if (naxes(i) .lt. 0)then
                                status=213
                                return
                        end if
10              continue
        end if
C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &          dtstrt(ibuff)+((pcnt+npix)*bytlen*gcnt+2879)/2880*2880

C       the primary array is actually interpreted as a binary table.  There
C       are two columns: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data.  Each group is a separate row in the table.
C       The scaling and null values are set to the default values.

        tfield(ibuff)=2
        rowlen(ibuff)=(pcnt+npix)*bytlen
        tdtype(1,ibuff)=ttype
        tdtype(2,ibuff)=ttype
        trept(1,ibuff)=pcnt
        trept(2,ibuff)=npix
C       choose a speical value to represent the absence of a blank value
        tnull(1,ibuff)=123454321
        tnull(2,ibuff)=123454321
        tscale(1,ibuff)=1.
        tscale(2,ibuff)=1.
        tzero(1,ibuff)=0.
        tzero(2,ibuff)=0.
        tbcol(1,ibuff)=0
        tbcol(2,ibuff)=pcnt*bytlen
        end
C--------------------------------------------------------------------------
        subroutine ftadef(ounit,lenrow,nfield,bcol,tform,nrows,status)

C       Ascii table data DEFinition
C       define the structure of the ASCII table data unit
C
C       ounit   i  Fortran I/O unit number
C       lenrow  i  length of a row, in characters
C       nfield  i  number of fields in the table
C       bcol    i  starting position of each column, (starting with 1)
C       tform   C  the data format of the column
C       nrows   i  number of rows in the table
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,lenrow,nfield,bcol(*),nrows,status
        character*(*) tform(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,j,clen,c2

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
                if (status .gt. 0)return
        end if

        tfield(ibuff)=nfield
        if (nfield .eq. 0)then
C           no data; the next HDU begins in the next logical block 
            hdstrt(ibuff,chdu(ibuff)+1)=dtstrt(ibuff)
        else
C           initialize the table column parameters
            clen=len(tform(1))
            do 20 i=1,nfield
                tscale(i,ibuff)=1.
                tzero(i,ibuff)=0.
C               choose special value to indicate null values are not defined
                cnull(i,ibuff)=char(1)
                cform(i,ibuff)=tform(i)
                tbcol(i,ibuff)=bcol(i)-1
                tdtype(i,ibuff)=16
C               the repeat count is always one for ASCII tables
                trept(i,ibuff)=1
C               store the width of the field in TNULL
                c2=0
                do 10 j=2,clen
                        if (tform(i)(j:j) .ge. '0' .and.
     &                     tform(i)(j:j) .le. '9')then
                                c2=j
                        else
                                go to 15
                        end if
10              continue
15              continue
                if (c2 .eq. 0)then
C                       no explicit width, so assume width of 1 character
                        tnull(i,ibuff)=1
                else
                    call ftc2ii(tform(i)(2:c2),tnull(i,ibuff),status)
                    if (status .gt. 0)then
C                               error parsing TFORM to determine field width
                                status=261
                                return
                    end if
                end if
20           continue

C           ASCII tables have no special data area
            scount(ibuff)=0

C           calculate the start of the next header unit, based on the
C           size of the data unit
            rowlen(ibuff)=lenrow
            hdstrt(ibuff,chdu(ibuff)+1)=
     &          dtstrt(ibuff)+(lenrow*nrows+2879)/2880*2880

C           have to initialize the ASCII table by filling it with blanks
            call ftbfil(ounit,status)
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftbdef(ounit,nfield,tform,pcount,nrows,status)

C       Binary table data DEFinition
C       define the structure of the binary table data unit
C
C       ounit   i  Fortran I/O unit number
C       nfield  i  number of fields in the table
C       tform   C  the data format of the column
C       nrows   i  number of rows in the table
C       pcount  i  size in bytes of the special data block following the table
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nfield,nrows,pcount,status
        character*(*) tform(*)

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,j,width

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
                if (status .gt. 0)return
        end if

        tfield(ibuff)=nfield
        if (nfield .eq. 0)then
C           no data; the next HDU begins in the next logical block 
            hdstrt(ibuff,chdu(ibuff)+1)=dtstrt(ibuff)
        else
C           initialize the table column parameters
            do 5 i=1,nfield
                tscale(i,ibuff)=1.
                tzero(i,ibuff)=0.
C               choose special value to indicate that null value is not defined
                tnull(i,ibuff)=123454321
C               parse the tform strings to get the data type and repeat count
                call ftbnfm(tform(i),tdtype(i,ibuff),
     &                      trept(i,ibuff),width,status)
                if (tdtype(i,ibuff) .eq. 16)then
C                       store ASCII unit string length in TNULL parameter
                        tnull(i,ibuff)=width
                end if
                if (status .ne. 0)return
5           continue

C           store the size of the special data area, if any
            scount(ibuff)=pcount

C           determine byte offset of the beginning of each field and row length
            call ftgtbc(nfield,tdtype(1,ibuff),trept(1,ibuff),
     &           tbcol(1,ibuff),rowlen(ibuff),status)

C           FITSIO deals with ASCII columns as arrays of strings, not
C           arrays of characters, so need to change the repeat count
C           to indicate the number of strings in the field, not the 
C           total number of characters in the field.
            do 10 i=1,nfield
                if (tdtype(i,ibuff) .eq. 16)then
                    j=trept(i,ibuff)/tnull(i,ibuff)
                    trept(i,ibuff)=max(j,1)
                end if
10          continue

C           initialize the heap offset (=nrows x ncolumns)
            theap(ibuff)=nrows*rowlen(ibuff)
            nxheap(ibuff)=0

C           calculate the start of the next header unit, based on the
C           size of the data unit (table + special data)
            hdstrt(ibuff,chdu(ibuff)+1)=
     &       dtstrt(ibuff)+(rowlen(ibuff)*nrows+pcount+2879)/2880*2880
        end if
        end
C--------------------------------------------------------------------------
        subroutine ftpthp(ounit,heap,status)

C       Define the starting address for the heap for a binary table.
C       The default address is NAXIS1 * NAXIS2.  It is in units of
C       bytes relative to the beginning of the regular binary table data.
C       This subroutine also writes the appropriate THEAP keyword to the
C       FITS header.

C       ounit   i  Fortran I/O unit number
C       heap   i  starting address of the heap
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1991

        integer ounit,heap,status

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff

        if (status .gt. 0)return
        ibuff=bufnum(ounit)
        theap(ibuff)=heap        

C       write the keyword
        call ftpkyj(ounit,'THEAP',heap,'Byte offset of heap area',
     &              status)
        end
C--------------------------------------------------------------------------
        subroutine ftpscl(ounit,bscale,bzero,status)

C       Primary SCaLing factor definition
C       Define the scaling factor for the primary header data.
C       This must be the first HDU of the FITS file.
C
C       ounit   i  Fortran I/O unit number
C       bscale  d  scaling factor
C       bzero   d  scaling zero point
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,status
        double precision bscale,bzero

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,ngroup

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (tfield(ibuff) .le. 0)then
C               error: data structure must be defined first
                status=303
        end if

C       the primary array is actually interpreted as a binary table.  There
C       are two columns for each group: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data.  
        ngroup=tfield(ibuff)/2
        do 10 i=1,ngroup
                tscale(i*2,ibuff)=bscale
                tzero(i*2,ibuff)=bzero
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpnul(ounit,blank,status)

C       Primary Null value definition
C       Define the null value for an integer primary array.
C       This must be the first HDU of the FITS file.
C
C       ounit   i  Fortran I/O unit number
C       blank   i  the value to be use to signify undefined data
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,blank,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,i,ngroup

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (tfield(ibuff) .le. 0)then
C               error: data structure must be defined first
                status=303
        end if

C       the primary array is actually interpreted as a binary table.  There
C       are two columns for each group: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data. 

        ngroup=tfield(ibuff)/2
        do 10 i=1,ngroup
                tnull(i*2,ibuff)=blank
10      continue
        end
C--------------------------------------------------------------------------
        subroutine fttscl(ounit,colnum,bscale,bzero,status)

C       Table column SCaLing factor definition
C       Define the scaling factor for a table column.
C
C       ounit   i  Fortran I/O unit number
C       colnum  i  number of the column to be defined
C       bscale  d  scaling factor
C       bzero   d  scaling zero point
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,status
        double precision bscale,bzero

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (tfield(ibuff) .le. 0)then
C               error: data structure must be defined first
                status=303
        end if        

        tscale(colnum,ibuff)=bscale
        tzero(colnum,ibuff)=bzero
        end
C--------------------------------------------------------------------------
        subroutine fttnul(ounit,colnum,inull,status)

C       Table column NULl value definition
C       Define the null value for a table column
C
C       ounit   i  Fortran I/O unit number
C       colnum  i  number of the column to be defined
C       inull   i  the value to be use to signify undefined data
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,inull,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (tfield(ibuff) .le. 0)then
C               error: data structure must be defined first
                status=303
        end if

        tnull(colnum,ibuff)=inull
        end
C--------------------------------------------------------------------------
        subroutine ftsnul(ounit,colnum,nulval,status)

C       ascii table Column NULl value definition
C       Define the null value for an ASCII table column.
C
C       ounit   i  Fortran I/O unit number
C       colnum  i  number of the column to be defined
C       nulval  c  the string to be use to signify undefined data
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,status
        character*(*) nulval

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 12)
        parameter (ne = 128)
        parameter (nf = 512)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (tfield(ibuff) .le. 0)then
C               error: data structure must be defined first
                status=303
        end if

        cnull(colnum,ibuff)=nulval
        end
C--------------------------------------------------------------------------
        subroutine ftbfil(iunit,status)

C       initialize the ASCII table by filling it with blanks

C       iunit   i  Fortan i/o unit number
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        integer iunit,status

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
        character rec*2880, xdummy*2880
        common/ftheap/rec,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,nrec,i

        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        rec=' '

C       calculate the number of 2880-byte records needed to fill the data unit
C       (the data buffer is by definition an exact multiple of 2880 bytes long)
        nrec=(hdstrt(ibuff,chdu(ibuff)+1)-dtstrt(ibuff))/2880

C       move to the beginning of the data area
        call ftmbyt(iunit,dtstrt(ibuff),.true.,status)

        do 10 i=1,nrec
C               write 2880 blanks at a time to the output buffer:
                call ftpcbf(iunit,1,2880,rec,status)
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftgcno(iunit,exact,colnam,colnum,status)

C       determine the column number corresponding to an input column name.
C       this assumes that the first 16 characters uniquely define the name

C       iunit   i  Fortran i/o unit number
C       exact   l  true if an exact case match of the names is required
C       colnam  c  name of column as specified in a TTYPE keyword
C       colnum  i  number of the column (first column = 1)
C                       (a value of 0 is returned if the column is not found)
C       status  i  returned error status

        integer iunit,colnum,status
        character*(*) colnam
        logical exact

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,i,nfound
        character*16 tname,cname

        if (status .ne. 0)return
        ibuff=bufnum(iunit)

        cname=colnam
C       force to uppercase, unless exact case match is required
        if (.not. exact)then
                call ftupch(cname)
        end if

C       look for the name in the value of the TTYPE keywords
        do 10 i=1,tfield(ibuff)
                call ftgkns(iunit,'TTYPE',i,1,tname,nfound,status)
                if (status .ne. 0)return
                if (nfound .eq. 1)then
C                       force to uppercase, unless exact case match is required
                        if (.not. exact)then
                                call ftupch(tname)
                        end if
                        if (tname .eq. cname)then
                                colnum=i
                                return
                        end if
                end if
10      continue

C       error: couldn't find the named column
        colnum=0
        end        
C--------------------------------------------------------------------------
        subroutine ftgacl(iunit,colnum,xtype,xbcol,xunit,xform,
     &        xscal,xzero,xnull,xdisp,status)

C       Get information about an Ascii CoLumn
C       returns the parameters which define the column

C       iunit   i  Fortran i/o unit number
C       colnum  i  number of the column (first column = 1)
C       xtype   c  name of the column
C       xbcol   i  starting character in the row of the column
C       xunit   c  physical units of the column
C       xform   c  Fortran-77 format of the column
C       xscal   d  scaling factor for the column values
C       xzero   d  scaling zero point for the column values
C       xnull   c  value used to represent undefined values in the column
C       xdisp   c  display format for the column (if different from xform
C       status  i  returned error status

        integer iunit,colnum,xbcol,status
        double precision xscal,xzero
        character*(*) xtype,xunit,xform,xnull,xdisp

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,nfound

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       get the parameters which are stored in the common block
        xbcol=tbcol(colnum,ibuff)
        xform=cform(colnum,ibuff)
        xscal=tscale(colnum,ibuff)
        xzero=tzero(colnum,ibuff)
        xnull=cnull(colnum,ibuff)

C       read remaining values from the header keywords
        xtype=' '
        call ftgkns(iunit,'TTYPE',colnum,1,xtype,nfound,status)
        xunit=' '
        call ftgkns(iunit,'TUNIT',colnum,1,xunit,nfound,status)
        xdisp=' '
        call ftgkns(iunit,'TDISP',colnum,1,xdisp,nfound,status)
        end        
C--------------------------------------------------------------------------
        subroutine ftgbcl(iunit,colnum,xtype,xunit,dtype,rcount,
     &        xscal,xzero,xnull,xdisp,status)

C       Get information about a Binary table CoLumn
C       returns the parameters which define the column

C       iunit   i  Fortran i/o unit number
C       colnum  i  number of the column (first column = 1)
C       xtype   c  name of the column
C       xunit   c  physical units of the column
C       dtype   c  datatype of the column
C       rcount  i  repeat count of the column
C       xscal   d  scaling factor for the column values
C       xzero   d  scaling zero point for the column values
C       xnull   i  value used to represent undefined values in integer column
C       xdisp   c  display format for the column
C       status  i  returned error status

        integer iunit,colnum,rcount,xnull,status
        double precision xscal,xzero
        character*(*) xtype,xunit,dtype,xdisp

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,nfound,tcode
        logical descrp
        character ctemp*2,fwide*4

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       get the parameters which are stored in the common block
        rcount=trept(colnum,ibuff)
        xscal=tscale(colnum,ibuff)
        xzero=tzero(colnum,ibuff)
        xnull=tnull(colnum,ibuff)

C       translate the numeric data type code
        dtype=' '
        tcode=tdtype(colnum,ibuff) 
        if (tcode .lt. 0)then
                descrp=.true.
                tcode=-tcode
        else
                descrp=.false.
        end if

        if (tcode .eq. 21)then
                dtype='I'
        else if (tcode .eq. 41)then
                dtype='J'
        else if (tcode .eq. 42)then
                dtype='E'
        else if (tcode .eq. 82)then
                dtype='D'
        else if (tcode .eq. 16)then
C               this is an ASCII field; width of field is stored in TNULL
                write(fwide,1000)tnull(colnum,ibuff)
1000            format(i4)
                if (tnull(colnum,ibuff) .gt. 999)then
                    dtype='A'//fwide
                else if (tnull(colnum,ibuff) .gt. 99)then
                    dtype='A'//fwide(2:4)
                else if (tnull(colnum,ibuff) .gt. 9)then
                    dtype='A'//fwide(3:4)
                else if (tnull(colnum,ibuff) .gt. 0)then
                    dtype='A'//fwide(4:4)
                else
                    dtype='A'
                end if
        else if (tcode .eq. 14)then
                dtype='L'
        else if (tcode .eq. 1)then
                dtype='X'
        else if (tcode .eq. 11)then
                dtype='B'
        else if (tcode .eq. 83)then
                dtype='C'
        else if (tcode .eq. 163)then
                dtype='M'
        end if

        if (descrp)then
                ctemp='P'//dtype(1:1)
                dtype=ctemp
        end if

C       read remaining values from the header keywords
        xtype=' '
        call ftgkns(iunit,'TTYPE',colnum,1,xtype,nfound,status)
        xunit=' '
        call ftgkns(iunit,'TUNIT',colnum,1,xunit,nfound,status)
        xdisp=' '
        call ftgkns(iunit,'TDISP',colnum,1,xdisp,nfound,status)
        end        
C----------------------------------------------------------------------
        subroutine ftmbyt(iunit,bytno,igneof,status)

C       move i/o pointer so that it is pointing to the byte number BYTNUM
C       in the FITS file.  Subsequent read or write operations will begin
C       at this point.

C       iunit   i  fortran unit number
C       bytno   i  number of the byte to point to.
C       igneof  l  ignore end-of-file (107) error?
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,bytno,status
        logical igneof

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer nbuff,record,bytoff

        if (status .gt. 0)then
                return
        else if (bytno .lt. 0)then
C               error: negative byte number
                status=304
                return
        else

        nbuff=bufnum(iunit)

C       calculate the record number and byte within the record to move to

        record=bytno/reclen(iunit)+1
        bytoff=mod(bytno,reclen(iunit))

        if (record .ne. recnum(nbuff))then
C               read in the new record, but first, check if the current

C       if record has been modified write it to disk if we have write access
                if (modify(nbuff))then
                        if (wrmode(nbuff))then
                                call ftpbyx(iunit,recnum(nbuff),
     &                          reclen(iunit),nbuff,status)
                                if (status .gt. 0)return
                                modify(nbuff)=.false.
                        else
C                               error: file only has read access; can't modify
                                status=112
                                return
                        end if
                end if
C               now read new record from disk; 
                recnum(nbuff)=record
                call ftgbyx(iunit,record,
     &                  reclen(iunit),nbuff,status)
                if (status .gt. 0)then
C                       We have just initialized this new record, so set modify
                        if (wrmode(nbuff))then
                             modify(nbuff)=.true.
                        end if
C                       ignore end of file error message
                        if (igneof .and. status .eq. 107)status=0
                end if
        end if
C       set the pointer to the correct byte within the record
        bytnum(nbuff)=bytoff
        end if
        end
C----------------------------------------------------------------------
        subroutine ftmoff(iunit,offset,igneof,status)

C       move i/o pointer by the specified byte offset.
C       Subsequent read or write operations will begin at this point.

C       iunit   i  fortran unit number
C       offset  i  number of bytes to move the pointer
C       igneof  l  should end-of-file error (107) be ignored?
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,offset,status
        logical igneof

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bytno,nbuff,record,bytoff

        if (status .gt. 0)return

        nbuff=bufnum(iunit)
        bytno=(recnum(nbuff)-1)*reclen(iunit)+bytnum(nbuff)+offset
        if (bytno .lt. 0)then
C               error: negative byte number
                status=304
                return
        end if

C       calculate the record number and byte within the record to move to

        record=bytno/reclen(iunit)+1
        bytoff=mod(bytno,reclen(iunit))

        if (record .ne. recnum(nbuff))then
C               read in the new record, but first, check if the current
C               record has been modified.  If so, write it to disk.
                if (modify(nbuff))then
                        call ftpbyx(iunit,recnum(nbuff),
     &                  reclen(iunit),nbuff,status)
                        if (status .gt. 0)return
                        modify(nbuff)=.false.
                end if
C               now read new record from disk; 
                recnum(nbuff)=record
                call ftgbyx(iunit,record,
     &                  reclen(iunit),nbuff,status)
                if (status .gt. 0)then
C                       We have just initialize this new record, so set modify
                        if (wrmode(nbuff))then
                             modify(nbuff)=.true.
                        end if
C                       ignore end of file error message
                        if (igneof .and. status .eq. 107)status=0
                end if
        end if
C       set the pointer to the correct byte within the record
        bytnum(nbuff)=bytoff
        end
C----------------------------------------------------------------------
        subroutine ftmahd(iunit,extno,xtend,status)

C       Move to Absolute Header Data unit
C       move the i/o pointer to the specified HDU and initialize all
C       the common block parameters which describe the extension

C       iunit   i  fortran unit number
C       extno   i  number of the extension to point to.
C       xtend   i  returned type of extension:   0 = the primary HDU
C                                                1 = an ASCII table
C                                                2 = a binary table
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,extno,xtend,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,i,nloop

        ibuff=bufnum(iunit)

C       first, close out the current HDU before moving to the new one
        call ftchdu(iunit,status)

C       check if we know where this extension begins
        if (extno .le. maxhdu(ibuff)+1)then
C               yes; so go directly to this extension
                call ftgext(iunit,extno,xtend,status)
        else
C               we have to read through the file to find this extension
                nloop=extno-maxhdu(ibuff)
                do 10 i=1,nloop
                    call ftgext(iunit,maxhdu(ibuff)+1,xtend,status)
                    if (status .ne. 0)return
10              continue
        end if
        end        
C----------------------------------------------------------------------
        subroutine ftmrhd(iunit,extmov,xtend,status)

C       Move Relative Header Data unit
C       move the i/o pointer to the specified HDU and initialize all
C       the common block parameters which describe the extension

C       iunit   i  fortran unit number
C       extmov  i  number of the extension to point to, relative to the CHDU
C       xtend   i  returned type of extension:   0 = the primary HDU
C                                                1 = an ASCII table
C                                                2 = a binary table
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,extmov,xtend,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,extno

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       calculate the absolute HDU number
        extno=chdu(ibuff)+extmov

        if (extno .gt. 0)then
C               move to the desired HDU
                call ftmahd(iunit,extno,xtend,status)
        else
C               attempted to move before the beginning of the file
                status=304
        end if
        end        
C----------------------------------------------------------------------
        subroutine ftcopy(iunit,ounit,moreky,status)

C       copies the CHDU from IUNIT to the CHDU of OUNIT.
C       This will also reserve space in the header for MOREKY keywords
C       if MOREKY > 0.

C       iunit   i  fortran unit number of the input file to be copied
C       ounit   i  fortran unit number of the output file to be copied to
C       moreky  i  create space in header for this many more keywords
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Jan, 1992

        integer iunit,ounit,moreky,status

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
        character cbuff*2880, hrec*80, xdummy*2800
        common/ftheap/cbuff,hrec,xdummy

C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,obuff,nblock,i,xtend,nkeys,nadd
        if (status .gt. 0)return

C       find out the number of keywords which exist in the input CHDU
        call ftghsp(iunit,nkeys,nadd,status)

C       copy the keywords one at a time to the output CHDU
        do 10 i=1,nkeys
                call ftgrec(iunit,i,hrec,status)
                call ftprec(ounit,hrec,status)
10      continue

C       reserve space for more keywords (if moreky > 0)
        call fthdef(ounit,moreky,status)

C       close the header by padding it with blanks and writing the END record
        call ftchdu(ounit,status)

        ibuff=bufnum(iunit)
        obuff=bufnum(ounit)

C       move to the beginning of the data in the input and output files
        call ftmbyt(iunit,dtstrt(ibuff),.false.,status)

        call ftmbyt(ounit,dtstrt(obuff),.true.,status)

C       Calculate the number of bytes to be copied.  By definition there
C       will be an integral number of 2880-byte logical blocks to be copied.
        nblock=(hdstrt(ibuff,chdu(ibuff)+1)-dtstrt(ibuff))/2880

C       now copy the data one section at a time
        do 20 i=1,nblock
                call ftgcbf(iunit,0,2880,cbuff,status)
                call ftpcbf(ounit,0,2880,cbuff,status)
20      continue

C       now initialize the parameters describing the output CHDU:
C       move back to the beginning of the output extension 
        call ftmbyt(ounit,hdstrt(obuff,chdu(obuff)),.false.,status)

        if (status .gt. 0)return

C       the location of the END record is currently unknown, so 
C       temporarily just set it to a very large number
        hdend(obuff)=1000000000

C       read the various header keywords to determine the structure of the CHDU
        call ftrhdu(ounit,xtend,status)
        end
C----------------------------------------------------------------------
        subroutine ftchdu(iunit,status)

C       Close Header Data Unit
C       If we have write access to the file, then close the current HDU by:
C                      -padding remaining space in the header with blanks
C                      -writing the END keyword in the CHU
C                      -flushing the current buffer to disk

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,nblank,i,endpos
        character*80 rec

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check if there was an attempt to modify a readonly access file
        if (.not. wrmode(ibuff) .and. modify(ibuff))then
                status=112
                return

C       see if we have write access to this file
        else if (wrmode(ibuff))then

C               calculate the number of blank keyword slots in the header
                endpos=hdend(ibuff)
                nblank=(dtstrt(ibuff)-endpos)/80
C               move the i/o pointer to the end of the header keywords
                call ftmbyt(iunit,endpos,.true.,status)
C               fill all the slots with blanks
                rec=' '
                do 10 i=1,nblank
                        call ftpcbf(iunit,1,80,rec,status)
10              continue

C               The END keyword must either be placed 
C               immediately after the last keyword that was written 
C               (as indicated by the HDEND parameter), or must be in the
C               first 80 bytes of the FITS record immediately preceeding
C               the data unit, whichever is further in the file.
C               The latter will occur if the user reserved room for more
C               header keywords which have not (yet) been filled.

                endpos=max(endpos,dtstrt(ibuff)-2880)
C               move pointer to this position
                call ftmbyt(iunit,endpos,.true.,status)

                rec='END'
C               append the 80 characters to the output buffer:
                call ftpcbf(iunit,1,80,rec,status)

C               now flush the current buffer to disk (by writing 0 bytes)
                call ftpbyt(iunit,0,i,status)
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgext(iunit,extno,xtend,status)

C       'Get Extension'
C       move i/o pointer to another extension (or the primary HDU) and
C       initialize all the common block parameters which describe the
C       extension

C       iunit   i  fortran unit number
C       extno   i  number of the extension to point to.
C       xtend   i  type of extension:   0 = the primary HDU
C                                       1 = an ASCII table
C                                       2 = a binary table
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,extno,xtend,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff

        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check that we know where this extension begins
        if (extno .le. 0)then
C               error: attempt to move to negative record in file
                status=304
                go to 900
        else if (extno .gt. maxhdu(ibuff)+1)then
C               don't know where this HDU starts
                status=305
                go to 900
        end if

C       move to the beginning of the desired extension
        call ftmbyt(iunit,hdstrt(ibuff,extno),.false.,status)
        if (status .ne. 0)go to 900

C       initialize various parameters about the CHDU
        chdu(ibuff)=extno
        maxhdu(ibuff)=max(extno,maxhdu(ibuff))
C       the location of the END record is currently unknown, so 
C       temporarily just set it to a very large number
        hdend(ibuff)=1000000000

C       read the various header keywords to determine the structure of the CHDU
        call ftrhdu(iunit,xtend,status)

900     continue
        end
C----------------------------------------------------------------------
        subroutine ftcrhd(iunit,status)

C       'CReate Header Data unit'
C       create, initialize, and move the i/o pointer to a new extension at 
C       the end of the FITS file.

C       iunit   i  fortran unit number
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June, 1991

        integer iunit,status

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
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff

        if (status .gt. 0)return

C       close the current HDU
        call ftchdu(iunit,status)

        ibuff=bufnum(iunit)

C       move to the end of the highest known extension
        call ftmbyt(iunit,hdstrt(ibuff,maxhdu(ibuff)+1),.true.,status)

C       initialize various parameters about the CHDU
        maxhdu(ibuff)=maxhdu(ibuff)+1
        chdu(ibuff)=maxhdu(ibuff)
        nxthdr(ibuff)=hdstrt(ibuff,chdu(ibuff))
C       the logical location of the END record at the start of the header
        hdend(ibuff)=nxthdr(ibuff)
C       the data start location is undefined
        dtstrt(ibuff)=-1000000000
        end
C----------------------------------------------------------------------
        subroutine ftpdes(ounit,colnum,rownum,nelem,offset,status)

C       write the descriptor values to a binary table.  This is only
C       used for column which have TFORMn = 'P', i.e., for variable
C       length arrays.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       rownum  i  number of the row to write
C       nelem   i  input number of elements
C       offset  i  input byte offset of the first element
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1991

        integer ounit,colnum,rownum,nelem,offset,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bstart,iray(2)

        if (status .gt. 0)return
        if (rownum .lt. 1)then
C               error: illegal row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

C       check that this is really a 'P' type column
        if (tdtype(colnum,ibuff) .ge. 0)then
                status=317
                return
        end if

C       move to the specified column and row:
        bstart=dtstrt(ibuff)+(rownum-1)*rowlen(ibuff)
     &         +tbcol(colnum,ibuff)
        call ftmbyt(ounit,bstart,.true.,status)

C       now write the number of elements and the offset to the table:
        iray(1)=nelem
        iray(2)=offset
        call ftpi4b(ounit,2,0,iray,status)
        end
C----------------------------------------------------------------------
        subroutine ftgdes(iunit,colnum,rownum,nelem,offset,status)

C       read the descriptor values from a binary table.  This is only
C       used for column which have TFORMn = 'P', i.e., for variable
C       length arrays.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       rownum  i  number of the row to read
C       nelem   i  output number of elements
C       offset  i  output byte offset of the first element
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Nov 1991

        integer iunit,colnum,rownum,nelem,offset,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bstart,iray(2)

        if (status .gt. 0)return
        if (rownum .lt. 1)then
C               error: illegal row number
                status=307
                return
        end if

        ibuff=bufnum(iunit)

C       check that this is really a 'P' type column
        if (tdtype(colnum,ibuff) .ge. 0)then
                status=317
                return
        end if

C       move to the specified column and row:
        bstart=dtstrt(ibuff)+(rownum-1)*rowlen(ibuff)
     &         +tbcol(colnum,ibuff)
        call ftmbyt(iunit,bstart,.true.,status)

C       now the number of elements and the offset to the table:
        call ftgi4b(iunit,2,0,iray,status)
        nelem=iray(1)
        offset=iray(2)
        end
C----------------------------------------------------------------------
        subroutine ftpcls(ounit,colnum,frow,felem,nelem,sray,status)

C       write an array of character string values to the  specified column of 
C       the table.
C       The binary or ASCII table column being written to must have datatype 'A'

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       sray    c  array of data values to be written
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        character*(*) sray(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,strlen,c1,c2,repeat,twidth
        integer ibuff,i1,ntodo,rstart,estart,nchars,clen,tcode
        character sbuff*80,blank*80
        logical small,fill
        
        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if
        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        end if

        ibuff=bufnum(ounit)
        blank=' '
        i1=1

C       column must be character string data type
        tcode=tdtype(colnum,ibuff) 
        if (tcode .eq. 16)then
C               for ASCII columns, TNULL actually stores the field width
                twidth=tnull(colnum,ibuff) 
                ntodo=nelem
                rstart=frow-1
                repeat=trept(colnum,ibuff)
                if (felem .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
                estart=felem-1
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &                 +tbcol(colnum,ibuff)+estart*twidth
        else if (tcode .eq. -16)then
C               this is a variable length descriptor field
                tcode=16
C               the length of the output string is defined by nelem
                twidth=nelem
                ntodo=1
                repeat=1
C               write the number of string length and the starting offset:
                call ftpdes(ounit,colnum,frow,twidth,
     &                              nxheap(ibuff),status)
C               calc the i/o pointer position for the start of the string
                bstart=dtstrt(ibuff)+nxheap(ibuff)+theap(ibuff)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+twidth
        else
C               error: not a character string column
                status=309
                return
        end if

C       move the i/o pointer to the start of the sequence of pixels
        call ftmbyt(ounit,bstart,.true.,status)

C       is the input string short enough to completely fit in buffer?
        strlen=len(sray(1))
        if (strlen .gt. 80 .and. twidth .gt. 80)then
                small=.false.
        else
                small=.true.
        end if

C       do we need to pad the FITS string field with trailing blanks?
        if (twidth .gt. strlen)then
                fill=.true.
        else
                fill=.false.
        end if

C       process one string at a time
20      continue
        nchars=min(strlen,twidth)
        if (small)then
C               the whole input string fits in the temporary buffer
                sbuff=sray(i1)
C               output the string
                call ftpcbf(ounit,1,nchars,sbuff,status)
        else
C               have to write the string in several pieces
                c1=1
                c2=80
30              sbuff=sray(i1)(c1:c2)
C               output the string
                clen=c2-c1+1
                call ftpcbf(ounit,1,clen,sbuff,status)
                nchars=nchars-clen
                if (nchars .gt. 0)then
                        c1=c1+80
                        c2=min(c2+80,c1+nchars-1)
                        go to 30
                end if
        end if

C       pad any remaining space in the column with blanks
        if (fill)then
                nchars=twidth-strlen
40              clen=min(nchars,80)
                call ftpcbf(ounit,1,clen,blank,status)
                nchars=nchars-80
                if (nchars .gt. 0)go to 40
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-1
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+1
                estart=estart+1
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                        bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &                  tbcol(colnum,ibuff)
C                       move the i/o pointer 
                        call ftmbyt(ounit,bstart,.true.,status)
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpcll(ounit,colnum,frow,felem,nelem,lray,status)

C       write an array of logical values to the  specified column of the table.
C       The binary table column being written to must have datatype 'L'
C       and no datatype conversion will be perform if it is not.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       lray    l  array of data values to be written
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        logical lray(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,maxpix,i
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,tcode
        character*1 buffer(80)
        logical descrp
        
        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        else
                estart=felem-1
        end if

        i1=1
        ntodo=nelem
        rstart=frow-1
        ibuff=bufnum(ounit)
        maxpix=80

C       column must be logical data type
        tcode=tdtype(colnum,ibuff) 
        if (tcode .eq. 14)then
                descrp=.false.
                repeat=trept(colnum,ibuff)
                if (felem .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
        else if (tcode .eq. -14)then
                descrp=.true.
                tcode=14
                repeat=nelem+estart
C               write the number of elements and the starting offset:
                call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart
                call ftmbyt(ounit,bstart,.true.,status)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+repeat
        else 
C               error illegal data type code 
                status=310
                return
        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &      tbcol(colnum,ibuff)+estart
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       create the buffer of logical bytes
        do 10 i=1,itodo
                if (lray(i1))then
                        buffer(i)='T'
                else
                        buffer(i)='F'
                end if
                i1=i1+1
10      continue

C       write out the buffer
        call ftpcbf(ounit,1,itodo,buffer,status)

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpclb(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of unsigned byte data values to the 
C       specified column of the table.  

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   i  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        character*1 array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        double precision scale,zero,dval
        real rval
        character*40 sval
        logical tofits,lval,descrp
        integer*2 i2val
        character*1 i1val

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
        maxpix=bufdim/bytpix*4

C       incre is the byte offset between consecutive pixels
        incre=0
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               write multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
                        repeat=nelem+felem-1
C                       write the number of elements and the starting offset:
                        call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart*bytpix                
                        call ftmbyt(ounit,bstart,.true.,status)
C                       increment the empty heap starting address:
                        nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
                end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer, doing scaling and datatype conversion, if required
        if (tcode .eq. 11)then
C               column data type is B (byte)
                call fti1i1(array(i1),itodo,scale,zero,tofits,
     &          ival,i1val,i1val,lval,lval,chbuff)
C               do any machine dependent data conversion and write the byte data
                call ftpi1b(ounit,itodo,incre,chbuff,status)
        else if (tcode .eq. 21)then
C               column data type is I (I*2)
                call fti1i2(array(i1),itodo,scale,zero,tofits,
     &             ival,i1val,i2val,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*2 data
                call ftpi2b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                call fti1i4(array(i1),itodo,scale,zero,tofits,
     &          ival,i1val,ival,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*4 data
                call ftpi4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                call fti1r4(array(i1),itodo,scale,zero,tofits,
     &          ival,i1val,rval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*4 data
                call ftpr4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
                call fti1r8(array(i1),itodo,scale,zero,tofits,
     &          ival,i1val,dval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*8 data
                call ftpr8b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                if (cform(colnum,ibuff)(1:1) .eq. 'I')then
C                 column data type is integer
                  call fti1i4(array(i1),itodo,scale,zero,tofits,
     &            ival,i1val,ival,lval,lval,ival)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)ival
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'F'
     &            .or.  cform(colnum,ibuff)(1:1) .eq. 'E')then
C                 column data type is real
                  call fti1r4(array(i1),itodo,scale,zero,tofits,
     &            ival,i1val,rval,lval,lval,rval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)rval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'D')then
C                 column data type is double precision
                  call fti1r8(array(i1),itodo,scale,zero,tofits,
     &            ival,i1val,dval,lval,lval,dval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)dval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else
C                 error: illegal ASCII table format code
                  status=311
                  return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error writing formatted data value
        status=313
        end
C----------------------------------------------------------------------
        subroutine ftpcli(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of integer*2 data values to the specified column of
C       the table.  

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   i*2  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        integer*2 array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        real rval
        double precision scale,zero,dval
        character*40 sval
        logical tofits,lval,descrp
        integer*2 i2val
        character*1 i1val

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
        maxpix=bufdim/bytpix*4

C       incre is the byte offset between consecutive pixels
        incre=0                
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               write multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
                        repeat=nelem+felem-1
C                       write the number of elements and the starting offset:
                        call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
C                       increment the empty heap starting address:
                        nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
                end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer, doing scaling and datatype conversion, if required
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                call fti2i2(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,i2val,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*2 data
                call ftpi2b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                call fti2i4(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,ival,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*4 data
                call ftpi4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                call fti2r4(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,rval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*4 data
                call ftpr4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
                call fti2r8(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,dval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*8 data
                call ftpr8b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                call fti2i1(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,i1val,lval,lval,chbuff)
C               do any machine dependent data conversion and write the byte data
                call ftpi1b(ounit,itodo,incre,chbuff,status)
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                if (cform(colnum,ibuff)(1:1) .eq. 'I')then
C                 column data type is integer
                  call fti2i4(array(i1),itodo,scale,zero,tofits,
     &            ival,i2val,ival,lval,lval,ival)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)ival
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'F'
     &            .or.  cform(colnum,ibuff)(1:1) .eq. 'E')then
C                 column data type is real
                  call fti2r4(array(i1),itodo,scale,zero,tofits,
     &            ival,i2val,rval,lval,lval,rval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)rval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'D')then
C                 column data type is double precision
                  call fti2r8(array(i1),itodo,scale,zero,tofits,
     &            ival,i2val,dval,lval,lval,dval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)dval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else
C                 error: illegal ASCII table format code
                  status=311
                  return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error writing formatted data value
        status=313
        end
C----------------------------------------------------------------------
        subroutine ftpclj(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of integer data values to the specified column of
C       the table.  

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   i  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        integer array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        real rval
        double precision scale,zero,dval
        character*40 sval
        logical tofits,lval,descrp
        integer*2 i2val
        character*1 i1val

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
        maxpix=bufdim/bytpix*4

C       incre is the byte offset between consecutive pixels
        incre=0
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               write multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
                        repeat=nelem+felem-1
C                       write the number of elements and the starting offset:
                        call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
C                       increment the empty heap starting address:
                        nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
                end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer, doing scaling and datatype conversion, if required
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                call fti4i2(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,i2val,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*2 data
                call ftpi2b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                call fti4i4(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,ival,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*4 data
                call ftpi4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                call fti4r4(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,rval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*4 data
                call ftpr4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
                call fti4r8(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,dval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*8 data
                call ftpr8b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                call fti4i1(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,i1val,lval,lval,chbuff)
C               do any machine dependent data conversion and write the byte data
                call ftpi1b(ounit,itodo,incre,chbuff,status)
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                if (cform(colnum,ibuff)(1:1) .eq. 'I')then
C                 column data type is integer
                  call fti4i4(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,ival,lval,lval,ival)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)ival
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'F'
     &            .or.  cform(colnum,ibuff)(1:1) .eq. 'E')then
C                 column data type is real
                  call fti4r4(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,rval,lval,lval,rval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)rval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'D')then
C                 column data type is double precision
                  call fti4r8(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,dval,lval,lval,dval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)dval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else
C                 error: illegal ASCII table format code
                  status=311
                  return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error writing formatted data value
        status=313
        end
C----------------------------------------------------------------------
        subroutine ftpcle(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of real data values to the specified column of
C       the table.  

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   r  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        real array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        real rval
        double precision scale,zero,dval
        character*40 sval
        logical tofits,lval,descrp
        integer*2 i2val
        character*1 i1val

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
        maxpix=bufdim/bytpix*4

C       incre is the byte offset between consecutive pixels
        incre=0
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               write multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
                        repeat=nelem+felem-1
C                       write the number of elements and the starting offset:
                        call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
C                       increment the empty heap starting address:
                        nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
                end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer, doing scaling and datatype conversion, if required
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                call ftr4i2(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*2 data
                call ftpi2b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                call ftr4i4(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*4 data
                call ftpi4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                call ftr4r4(array(i1),itodo,scale,zero,tofits,
     &          ival,rval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*4 data
                call ftpr4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
                call ftr4r8(array(i1),itodo,scale,zero,tofits,
     &          ival,dval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*8 data
                call ftpr8b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                call ftr4i1(array(i1),itodo,scale,zero,tofits,
     &          ival,i1val,lval,lval,chbuff)
C               do any machine dependent data conversion and write the byte data
                call ftpi1b(ounit,itodo,incre,chbuff,status)
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                if (cform(colnum,ibuff)(1:1) .eq. 'I')then
C                 column data type is integer
                  call ftr4i4(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,lval,lval,ival)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)ival
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'F'
     &            .or.  cform(colnum,ibuff)(1:1) .eq. 'E')then
C                 column data type is real
                  call ftr4r4(array(i1),itodo,scale,zero,tofits,
     &            ival,rval,lval,lval,rval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)rval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'D')then
C                 column data type is double precision
                  call ftr4r8(array(i1),itodo,scale,zero,tofits,
     &            ival,dval,lval,lval,dval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)dval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else
C                 error: illegal ASCII table format code
                  status=311
                  return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error writing formatted data value
        status=313
        end
C----------------------------------------------------------------------
        subroutine ftpcld(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of double precision data values to the specified column 
C       of the table.  

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   d  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
        double precision array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        real rval
        double precision scale,zero,dval
        character*40 sval
        logical tofits,lval,descrp
        integer*2 i2val
        character*1 i1val

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(abs(tcode)/10,1)
        maxpix=bufdim/bytpix*4

C       incre is the byte offset between consecutive pixels
        incre=0
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               write multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
                        repeat=nelem+felem-1
C                       write the number of elements and the starting offset:
                        call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
C                       increment the empty heap starting address:
                        nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
                end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer, doing scaling and datatype conversion, if required
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                call ftr8i2(array(i1),itodo,scale,zero,tofits,
     &          ival,i2val,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*2 data
                call ftpi2b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                call ftr8i4(array(i1),itodo,scale,zero,tofits,
     &          ival,ival,lval,lval,buffer)
C               do any machine dependent data conversion and write the I*4 data
                call ftpi4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                call ftr8r4(array(i1),itodo,scale,zero,tofits,
     &          ival,rval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*4 data
                call ftpr4b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
                call ftr8r8(array(i1),itodo,scale,zero,tofits,
     &          ival,dval,lval,lval,buffer)
C               do any machine dependent data conversion and write the R*8 data
                call ftpr8b(ounit,itodo,incre,buffer,status)
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                call ftr8i1(array(i1),itodo,scale,zero,tofits,
     &          ival,i1val,lval,lval,chbuff)
C               do any machine dependent data conversion and write the byte data
                call ftpi1b(ounit,itodo,incre,chbuff,status)
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                if (cform(colnum,ibuff)(1:1) .eq. 'I')then
C                 column data type is integer
                  call ftr8i4(array(i1),itodo,scale,zero,tofits,
     &            ival,ival,lval,lval,ival)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)ival
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'F'
     &            .or.  cform(colnum,ibuff)(1:1) .eq. 'E')then
C                 column data type is real
                  call ftr8r4(array(i1),itodo,scale,zero,tofits,
     &            ival,rval,lval,lval,rval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)rval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else if (cform(colnum,ibuff)(1:1) .eq. 'D')then
C                 column data type is double precision
                  call ftr8r8(array(i1),itodo,scale,zero,tofits,
     &            ival,dval,lval,lval,dval)
C                 create the formated character string
                  write(sval,'('//cform(colnum,ibuff)//')',err=900)dval
C                 write the character string to the FITS file
                  call ftpcbf(ounit,1,tnull(colnum,ibuff),sval,status)
                else
C                 error: illegal ASCII table format code
                  status=311
                  return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error writing formatted data value
        status=313
        end
C----------------------------------------------------------------------
        subroutine ftpclc(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of single precision complex data values to the 
C       specified column of the table.  
C       The binary table column being written to must have datatype 'C'
C       and no datatype conversion will be perform if it is not.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   cmp  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
C       the input array is really complex data type
        real array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 200)
        integer buffer(bufdim),bytpix,bstart,tcode
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        real rval
        double precision scale,zero
        logical tofits,lval,descrp

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        i1=1
C       multiply by 2, because the complex data type has pairs of values
        ntodo=nelem*2
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        else
C               multiply by 2 because the complex data type has pairs of values
                estart=(felem-1)*2
        end if

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=4
        maxpix=bufdim/bytpix*4

        if (tcode .eq. 83)then
                repeat=trept(colnum,ibuff)*2
                if (felem*2 .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
                descrp=.false.
        else if (tcode .eq. -83)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=-tcode
                repeat=nelem+felem-1
C               write the number of elements and the starting offset:
                call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
                repeat=repeat*2
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                  theap(ibuff)+estart*bytpix
                call ftmbyt(ounit,bstart,.true.,status)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
        else
C               error illegal table data type code
                status=312
                return
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer,
        call ftr4r4(array(i1),itodo,scale,zero,tofits,
     &          ival,rval,lval,lval,buffer)
C       do any machine dependent data conversion and write the R*4 data
        call ftpr4b(ounit,itodo,0,buffer,status)

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpclm(ounit,colnum,frow,felem,nelem,array,status)

C       write an array of double precision complex data values to the 
C       specified column of the table.  
C       The binary table column being written to must have datatype 'M'
C       and no datatype conversion will be perform if it is not.

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       array   dcmp  array of data values to be written 
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status
C       array is really double precison complex
        double precision array(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 200)
        integer buffer(bufdim),bytpix,bstart,tcode
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        double precision scale,zero
        double precision dval
        logical tofits,lval,descrp

        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        ibuff=bufnum(ounit)

        i1=1
C       multiply by 2, because the complex data type has pairs of values
        ntodo=nelem*2
        rstart=frow-1
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
        tcode=tdtype(colnum,ibuff) 
C       the data are being scaled from internal format to FITS:
        tofits=.true.

        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        else
C               multiply by 2 because the complex data type has pairs of values
                estart=(felem-1)*2
        end if

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=8
        maxpix=bufdim/bytpix*4

        if (tcode .eq. 163)then
                repeat=trept(colnum,ibuff)*2
                if (felem*2 .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
                descrp=.false.
        else if (tcode .eq. -163)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=-tcode
                repeat=nelem+felem-1
C               write the number of elements and the starting offset:
                call ftpdes(ounit,colnum,frow,repeat,
     &                              nxheap(ibuff),status)
                repeat=repeat*2
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+nxheap(ibuff)+
     &                  theap(ibuff)+estart*bytpix
                call ftmbyt(ounit,bstart,.true.,status)
C               increment the empty heap starting address:
                nxheap(ibuff)=nxheap(ibuff)+repeat*bytpix
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       copy data to buffer
        call ftr8r8(array(i1),itodo,scale,zero,tofits,
     &          ival,dval,lval,lval,buffer)
C       do any machine dependent data conversion and write the R*8 data
        call ftpr8b(ounit,itodo,0,buffer,status)

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftpclu(ounit,colnum,frow,felem,nelem,status)

C       set elements of a table to be undefined

C       ounit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       felem   i  first element within the row to write
C       nelem   i  number of elements to write
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,colnum,frow,felem,nelem,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character snull*512,xdummy*5248
        common/ftheap/snull,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bytpix,bstart,i4null,tcode,nchars,i,offset
        integer ibuff,ntodo,itodo,repeat,rstart,estart
        integer*2 i2null,i1null
        real r4null
        double precision r8null
        logical descrp

        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if
        ibuff=bufnum(ounit)

        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        end if

        tcode=tdtype(colnum,ibuff)
        bytpix=max(abs(tcode)/10,1)

        descrp=.false.
        ntodo=nelem
        rstart=frow-1

        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        else
                estart=felem-1
        end if

        if (tcode .eq. 16)then
C               this is an ASCII field
                repeat=trept(colnum,ibuff)
                if (felem .gt. repeat)then
                        status=308
                        return
                end if

                if (cnull(colnum,ibuff) .eq. char(1))then
C                       error: null value has not been defined
                        status=314
                        return
                end if
C               the TNULL parameter stores the width of the character field
                bytpix=tnull(colnum,ibuff)
        else
C               this is a binary table

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(ounit,colnum,frow,repeat,
     &                              offset,status)
                        if (ntodo+estart .gt. repeat)then
C                               error:  tried to write past end of record
                                status=319
                                return
                        end if

C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(ounit,bstart,.true.,status)
                end if

                if (tcode.eq.11 .or. tcode.eq.21 .or. tcode.eq.41)then
                        if (tnull(colnum,ibuff) .eq. 123454321)then
C                               error: null value has not been defined
                                status=314
                                return
                        end if
                else
C                       set the floating point Not-a-Number values
                        call ftsrnn(r4null)
                        call ftsdnn(r8null)
                end if

        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &             +tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(ounit,bstart,.true.,status)
        end if

C       write the appropriate null value to the pixels
        if (tcode .eq. 21)then
C               column data type is I (I*2)
                i2null=tnull(colnum,ibuff)
                do 5 i=1,itodo
                        call ftpi2b(ounit,1,0,i2null,status)
5               continue
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
                i4null=tnull(colnum,ibuff)
                do 15 i=1,itodo
                        call ftpi4b(ounit,1,0,i4null,status)
15              continue
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
                do 25 i=1,itodo
                        call ftpbyt(ounit,4,r4null,status)
25              continue
        else if (tcode .eq. 82 .or. tcode .eq. 83)then
C               column data type is D (R*8), or C complex 2 x R*4
                do 35 i=1,itodo
                        call ftpbyt(ounit,8,r8null,status)
35              continue
        else if (tcode .eq. 16)then
C               this is an ASCII table column
                snull=cnull(colnum,ibuff)
C               write up to 512 characters in the column, remainder unchanged
                nchars=min(bytpix,512)
                do 45 i=1,itodo
                        call ftpcbf(ounit,1,nchars,snull,status)
45              continue
        else if (tcode .eq. 11)then
C               column data type is B (byte)
                i1null=tnull(colnum,ibuff)
C               make both bytes of the I*2 word identical, so it does not
C               matter what order they are in when one of them gets written
C               to the FITS file
                if (i1null .lt. 256)i1null=i1null*256+i1null
                do 55 i=1,itodo
                        call ftpbyt(ounit,1,i1null,status)
55              continue
        else if (tcode .eq. 163)then
C               column data type is double complex (M)
                do 65 i=1,itodo*2
                        call ftpbyt(ounit,8,r8null,status)
65              continue
        else if (tcode .eq. 14)then
C               column data type is logical (L)
                i1null=0
                do 85 i=1,itodo
                        call ftpbyt(ounit,1,i1null,status)
85              continue
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgcls(iunit,colnum,frow,felem,nelem,nultyp,nulval,
     &    sray,flgval,anynul,status)

C       read an array of character string values from the specified column of 
C       the table.
C       The binary or ASCII table column being read must have datatype 'A'
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  c  value that undefined pixels will be set to (if nultyp=1)
C       sray    c  array of data values to be read
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        logical flgval(*),anynul
        character*(*) sray(*),nulval

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,nulchk,twidth,tread,tcode,offset,repeat
        integer ibuff,i1,ntodo,rstart,estart,lennul,strlen
        character snull*8
        
        if (status .gt. 0)return

C       check for zero length array
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
        end if

        anynul=.false.
        ibuff=bufnum(iunit)
        i1=1

C       column must be character string data type

        tcode=tdtype(colnum,ibuff) 
        if (tcode .eq. 16)then
C               for ASCII columns, TNULL actually stores the field width
                twidth=tnull(colnum,ibuff) 
                ntodo=nelem
                rstart=frow-1
                repeat=trept(colnum,ibuff)
                if (felem .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
                estart=felem-1
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &                 +tbcol(colnum,ibuff)+estart*twidth
        else if (tcode .eq. -16)then
C               this is a variable length descriptor field
                tcode=16
                ntodo=1
C               read the string length and the starting offset:
                call ftgdes(iunit,colnum,frow,twidth,offset,status)
C               calc the i/o pointer position for the start of the string
                bstart=dtstrt(ibuff)+offset+theap(ibuff)
        else
C               error: not a character string column
                status=309
                return
        end if

C       define the max. number of charcters to be read: either
C       the length of the variable length field, or the length
C       of the character string variable, which ever is smaller
        strlen=len(sray(1))
        tread=min(twidth,strlen)

C       move the i/o pointer to the start of the sequence of pixels
        call ftmbyt(iunit,bstart,.false.,status)

        lennul=0
C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval .eq. ' ')then
C               user doesn't want to check for nulls
                nulchk=0
        else
                nulchk=nultyp
                snull=cnull(colnum,ibuff)
C               lennul = length of the string to check for null values
                lennul=min(len(sray(1)),8)
        end if

C       process one string at a time
20      continue
C       get the string of characters
        sray(i1)=' '
        call ftgcbf(iunit,1,tread,sray(i1),status)

C       check for null value, if required
        if (nulchk .ne. 0)then
                if (ichar(sray(i1)(1:1)) .eq. 0 .or.
     &              sray(i1)(1:lennul) .eq. snull(1:lennul))then
                        if (nulchk .eq. 1)then
                                sray(i1)=nulval
                                anynul=.true.
                        else
                                flgval(i1)=.true.
                                anynul=.true.
                        end if
                end if
        end if

C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-1
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+1
                estart=estart+1
                if (estart .eq. repeat)then
                        rstart=rstart+1
                        estart=0
                end if
C               move to the start of the next string; need to do
C               this every time in case we didn't read all the characters
C               from the previous string.
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)
     &                 +tbcol(colnum,ibuff)+estart*twidth
C               move the i/o pointer
                call ftmbyt(iunit,bstart,.false.,status)
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgclb(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of byte data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  b value that undefined pixels will be set to (if nultyp=1)
C       array   b  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        character*1 array(*),nulval
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 200)
        integer buffer(bufdim),bytpix,bstart,tcode,nchki4,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        integer offset
        integer*2 nchki2
        character*1 nchki1
        real rval
        double precision scale,zero,dval
        character sval*40,sform*13,snull*8
        logical tofits,descrp
        
        if (status .gt. 0)return
C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
C       the data are being scaled from FITS to internal format 
        tofits=.false.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(tcode/10,1)
C       check for important special case: no datatype conversion required
        if (tcode .eq. 11)then
                maxpix=nelem
        else
                maxpix=bufdim/bytpix*4
        end if

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=0                
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
C               construct the read format, and get the null value string
                sform='(BN,'//cform(colnum,ibuff)//')'
                snull=cnull(colnum,ibuff)                
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               read multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                        if (repeat .eq. 0)then
C                               error: null length vector
                                status=318
                                return
                        else if (estart+nelem .gt. repeat)then
C                               error: trying to read beyond end of record
                                status=319
                                return
                        end if
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. ichar(nulval) .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
C           see if the null value has been defined for this column
            nulchk=0
            if (tcode .eq. 11)then
C               check if byte datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        nchki1=char(tnull(colnum,ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 21)then
C               check if I*2 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        nchki2=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 41)then
C               check if I*4 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        nchki4=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 42 .or. tcode .eq. 82)then
C               have to check floating point data for NaN values
                nulchk=nultyp
            end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data from FITS file, doing datatype conversion and scaling
        if (tcode .eq. 11)then
C               column data type is B (byte)
C               read the data and do any machine dependent data conversion
C               note that we can use the input array directly
                call ftgi1b(iunit,itodo,incre,array(i1),status)
C              check for null values, and do scaling and datatype conversion
                call fti1i1(array(i1),itodo,scale,zero,tofits,
     &          nulchk,nchki1,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 21)then
C               column data type is I (I*2)
C               read the data and do any machine dependent data conversion
                call ftgi2b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti2i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,nchki2,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
C               read the data and do any machine dependent data conversion
                call ftgi4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti4i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,nchki4,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
C               read the data and do any machine dependent data conversion
                call ftgr4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr4i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
C               read the data and do any machine dependent data conversion
                call ftgr8b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr8i1(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 16)then
C               this is an ASCII table column; get the character string
                call ftgcbf(iunit,1,tnull(colnum,ibuff),sval,status)

C               check for null
                if (sval(1:8) .eq. snull)then
                        anynul=.true.
                        if (nultyp .eq. 1)then
                                array(i1)=nulval
                        else
                                flgval(i1)=.true.
                        end if
                        go to 30
                end if

C               now read the value, then do scaling and datatype conversion
                if (sform(5:5) .eq. 'I')then
                        read(sval,sform,err=900)ival
                        call fti4i1(ival,itodo,scale,zero,tofits,
     &                  0,nchki4,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5).eq.'F'.or. sform(5:5).eq.'E')then
                        read(sval,sform,err=900)rval
                        call ftr4i1(rval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5) .eq. 'D')then
                        read(sval,sform,err=900)dval
                        call ftr8i1(dval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else
C                       error: illegal ASCII table format code
                        status=311
                        return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error reading formatted data value
        status=315
        end
C----------------------------------------------------------------------
        subroutine ftgcli(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of integer*2 data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  i*2  value that undefined pixels will be set to (if nultyp=1)
C       array   i*2  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        integer*2 array(*),nulval
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,i4null,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        integer offset
        integer*2 i2null
        character*1 i1null
        real rval
        double precision scale,zero,dval
        character sval*40,sform*13,snull*8
        logical tofits,descrp
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
C       the data are being scaled from FITS to internal format 
        tofits=.false.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(tcode/10,1)
C       check for important special case: no datatype conversion required
        if (tcode .eq. 21)then
                maxpix=nelem
        else
                maxpix=bufdim/bytpix*4
        end if

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=0                
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
C               construct the read format, and get the null value string
                sform='(BN,'//cform(colnum,ibuff)//')'
                snull=cnull(colnum,ibuff)                
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               read multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                        if (repeat .eq. 0)then
C                               error: null length vector
                                status=318
                                return
                        else if (estart+nelem .gt. repeat)then
C                               error: trying to read beyond end of record
                                status=319
                                return
                        end if
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
C           see if the null value has been defined for this column
            nulchk=0
            if (tcode .eq. 11)then
C               check if byte null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i1null=char(tnull(colnum,ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 21)then
C               check if I*2 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i2null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 41)then
C               check if I*4 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i4null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 42 .or. tcode .eq. 82)then
C               have to check floating point data for NaN values
                nulchk=nultyp
            end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data from FITS file, doing datatype conversion and scaling
        if (tcode .eq. 21)then
C               column data type is I (I*2)
C               read the data and do any machine dependent data conversion
C               note that we can use the input array directly
                call ftgi2b(iunit,itodo,incre,array(i1),status)
C               check for null values, and do scaling and datatype conversion
                call fti2i2(array(i1),itodo,scale,zero,tofits,
     &          nulchk,i2null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
C               read the data and do any machine dependent data conversion
                call ftgi4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti4i2(buffer,itodo,scale,zero,tofits,
     &          nulchk,i4null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
C               read the data and do any machine dependent data conversion
                call ftgr4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr4i2(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
C               read the data and do any machine dependent data conversion
                call ftgr8b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr8i2(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 11)then
C               column data type is B (byte)
C               read the data and do any machine dependent data conversion
                call ftgi1b(iunit,itodo,incre,chbuff,status)
C               check for null values, and do scaling and datatype conversion
                call fti1i2(chbuff,itodo,scale,zero,tofits,
     &          nulchk,i1null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 16)then
C               this is an ASCII table column; get the character string
                call ftgcbf(iunit,1,tnull(colnum,ibuff),sval,status)

C               check for null
                if (sval(1:8) .eq. snull)then
                        anynul=.true.
                        if (nultyp .eq. 1)then
                                array(i1)=nulval
                        else
                                flgval(i1)=.true.
                        end if
                        go to 30
                end if

C               now read the value, then do scaling and datatype conversion
                if (sform(5:5) .eq. 'I')then
                        read(sval,sform,err=900)ival
                        call fti4i2(ival,itodo,scale,zero,tofits,
     &                  0,i4null,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5).eq.'F'.or. sform(5:5).eq.'E')then
                        read(sval,sform,err=900)rval
                        call ftr4i2(rval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5) .eq. 'D')then
                        read(sval,sform,err=900)dval
                        call ftr8i2(dval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else
C                       error: illegal ASCII table format code
                        status=311
                        return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error reading formatted data value
        status=315
        end
C----------------------------------------------------------------------
        subroutine ftgclj(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of integer*4 data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  i  value that undefined pixels will be set to (if nultyp=1)
C       array   i  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        integer array(*),nulval
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,i4null,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        integer offset
        integer*2 i2null
        character*1 i1null
        real rval
        double precision scale,zero,dval
        character sval*40,sform*13,snull*8
        logical tofits,descrp
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
C       the data are being scaled from FITS to internal format 
        tofits=.false.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(tcode/10,1)
C       check for important special case: no datatype conversion required
        if (tcode .eq. 41)then
                maxpix=nelem
        else
                maxpix=bufdim/bytpix*4
        end if

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=0                
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
C               construct the read format, and get the null value string
                sform='(BN,'//cform(colnum,ibuff)//')'
                snull=cnull(colnum,ibuff)                
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               read multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                        if (repeat .eq. 0)then
C                               error: null length vector
                                status=318
                                return
                        else if (estart+nelem .gt. repeat)then
C                               error: trying to read beyond end of record
                                status=319
                                return
                        end if
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
C           see if the null value has been defined for this column
            nulchk=0
            if (tcode .eq. 11)then
C               check if byte datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i1null=char(tnull(colnum,ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 21)then
C               check if I*2 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i2null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 41)then
C               check if I*4 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i4null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 42 .or. tcode .eq. 82)then
C               have to check floating point data for NaN values
                nulchk=nultyp
            end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data from FITS file, doing datatype conversion and scaling
        if (tcode .eq. 21)then
C               column data type is I (I*2)
C               read the data and do any machine dependent data conversion
                call ftgi2b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti2i4(buffer,itodo,scale,zero,tofits,
     &          nulchk,i2null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
C               read the data and do any machine dependent data conversion
C               note that we can use the input array directly
                call ftgi4b(iunit,itodo,incre,array(i1),status)
C               check for null values, and do scaling and datatype conversion
                call fti4i4(array(i1),itodo,scale,zero,tofits,
     &          nulchk,i4null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
C               read the data and do any machine dependent data conversion
                call ftgr4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr4i4(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
C               read the data and do any machine dependent data conversion
                call ftgr8b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr8i4(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 11)then
C               column data type is B (byte)
C               read the data and do any machine dependent data conversion
                call ftgi1b(iunit,itodo,incre,chbuff,status)
C               check for null values, and do scaling and datatype conversion
                call fti1i4(chbuff,itodo,scale,zero,tofits,
     &          nulchk,i1null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 16)then
C               this is an ASCII table column; get the character string
                call ftgcbf(iunit,1,tnull(colnum,ibuff),sval,status)

C               check for null
                if (sval(1:8) .eq. snull)then
                        anynul=.true.
                        if (nultyp .eq. 1)then
                                array(i1)=nulval
                        else
                                flgval(i1)=.true.
                        end if
                        go to 30
                end if

C               now read the value, then do scaling and datatype conversion
                if (sform(5:5) .eq. 'I')then
                        read(sval,sform,err=900)ival
                        call fti4i4(ival,itodo,scale,zero,tofits,
     &                  0,i4null,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5).eq.'F'.or. sform(5:5).eq.'E')then
                        read(sval,sform,err=900)rval
                        call ftr4i4(rval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5) .eq. 'D')then
                        read(sval,sform,err=900)dval
                        call ftr8i4(dval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else
C                       error: illegal ASCII table format code
                        status=311
                        return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error reading formatted data value
        status=315
        end
C----------------------------------------------------------------------
        subroutine ftgcle(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of real*4 data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two       ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  r  value that undefined pixels will be set to (if nultyp=1)
C       array   r  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        real array(*),nulval
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,i4null,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        integer offset
        integer*2 i2null
        character*1 i1null
        real rval
        double precision scale,zero,dval
        character sval*40,sform*13,snull*8
        logical tofits,descrp
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
C       the data are being scaled from FITS to internal format 
        tofits=.false.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(tcode/10,1)
C       check for important special case: no datatype conversion required
        if (tcode .eq. 42)then
                maxpix=nelem
        else
                maxpix=bufdim/bytpix*4
        end if

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=0                
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
C               construct the read format, and get the null value string
                sform='(BN,'//cform(colnum,ibuff)//')'
                snull=cnull(colnum,ibuff)                
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               read multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                        if (repeat .eq. 0)then
C                               error: null length vector
                                status=318
                                return
                        else if (estart+nelem .gt. repeat)then
C                               error: trying to read beyond end of record
                                status=319
                                return
                        end if
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
C           see if the null value has been defined for this column
            nulchk=0
            if (tcode .eq. 11)then
C               check if byte datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i1null=char(tnull(colnum,ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 21)then
C               check if I*2 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i2null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 41)then
C               check if I*4 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i4null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 42 .or. tcode .eq. 82)then
C               have to check floating point data for NaN values
                nulchk=nultyp
            end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data from FITS file, doing datatype conversion and scaling
        if (tcode .eq. 21)then
C               column data type is I (I*2)
C               read the data and do any machine dependent data conversion
                call ftgi2b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti2r4(buffer,itodo,scale,zero,tofits,
     &          nulchk,i2null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
C               read the data and do any machine dependent data conversion
                call ftgi4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti4r4(buffer,itodo,scale,zero,tofits,
     &          nulchk,i4null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
C               read the data and do any machine dependent data conversion
C               note that we can use the input array directly
                call ftgr4b(iunit,itodo,incre,array(i1),status)
C               check for null values, and do scaling and datatype conversion
                call ftr4r4(array(i1),itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
C               read the data and do any machine dependent data conversion
                call ftgr8b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr8r4(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 11)then
C               column data type is B (byte)
C               read the data 
                call ftgi1b(iunit,itodo,incre,chbuff,status)
C               check for null values, and do scaling and datatype conversion
                call fti1r4(chbuff,itodo,scale,zero,tofits,
     &          nulchk,i1null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 16)then
C               this is an ASCII table column; get the character string
                call ftgcbf(iunit,1,tnull(colnum,ibuff),sval,status)

C               check for null
                if (sval(1:8) .eq. snull)then
                        anynul=.true.
                        if (nultyp .eq. 1)then
                                array(i1)=nulval
                        else
                                flgval(i1)=.true.
                        end if
                        go to 30
                end if

C               now read the value, then do scaling and datatype conversion
                if (sform(5:5) .eq. 'I')then
                        read(sval,sform,err=900)ival
                        call fti4r4(ival,itodo,scale,zero,tofits,
     &                  0,i4null,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5).eq.'F'.or. sform(5:5).eq.'E')then
                        read(sval,sform,err=900)rval
                        call ftr4r4(rval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5) .eq. 'D')then
                        read(sval,sform,err=900)dval
                        call ftr8r4(dval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else
C                       error: illegal ASCII table format code
                        status=311
                        return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error reading formatted data value
        status=315
        end
C----------------------------------------------------------------------
        subroutine ftgcld(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of real*8 data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  d  value that undefined pixels will be set to (if nultyp=1)
C       array   d  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        double precision array(*),nulval
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
        character*8 cnull,cform
        common/ft0003/cnull(nf,nb),cform(nf,nb)
        character*1 chbuff(400),xdummy(5360)
        common/ftheap/chbuff,xdummy
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bufdim
        parameter (bufdim = 100)
        integer buffer(bufdim),bytpix,bstart,tcode,i4null,nulchk,incre
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart,maxpix,ival
        integer offset
        integer*2 i2null
        character*1 i1null
        real rval
        double precision scale,zero,dval
        character sval*40,sform*13,snull*8
        logical tofits,descrp
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        descrp=.false.
        i1=1
        ntodo=nelem
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        scale=tscale(colnum,ibuff)
        zero=tzero(colnum,ibuff)
C       the data are being scaled from FITS to internal format 
        tofits=.false.

C       calculate the maximum number of column pixels which fit in buffer
        bytpix=max(tcode/10,1)
C       check for important special case: no datatype conversion required
        if (tcode .eq. 82)then
                maxpix=nelem
        else
                maxpix=bufdim/bytpix*4
        end if

C       determine the repeat count and the first element position
C       incre is the byte offset between consecutive pixels
        incre=0                
        if (tcode .eq. 16)then
C               this is an ASCII table; table elements cannot be vectors
                repeat=1
                estart=0
C               construct the read format, and get the null value string
                sform='(BN,'//cform(colnum,ibuff)//')'
                snull=cnull(colnum,ibuff)                
        else
C               this is a binary table
                if (felem .lt. 1)then
C                       illegal element number
                        status=308
                        return
                else
                        estart=felem-1
                end if

                if (tcode .gt. 0)then
                        repeat=trept(colnum,ibuff)
                        if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                        end if
                        if (repeat .eq. 1 .and. nelem .gt. 1)then
C                               read multiple rows of data at one time
                                incre=rowlen(ibuff)
                                repeat=maxpix
                                estart=0
                        end if
                else
C                       this is a variable length descriptor column
                        descrp=.true.
                        tcode=-tcode
C                       read the number of elements and the starting offset:
                        call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                        if (repeat .eq. 0)then
C                               error: null length vector
                                status=318
                                return
                        else if (estart+nelem .gt. repeat)then
C                               error: trying to read beyond end of record
                                status=319
                                return
                        end if
C                       move the i/o pointer to the start of the pixel sequence
                        bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C           user does want to check for null values
C           see if the null value has been defined for this column
            nulchk=0
            if (tcode .eq. 11)then
C               check if byte datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i1null=char(tnull(colnum,ibuff))
                        nulchk=nultyp
                end if
            else if (tcode .eq. 21)then
C               check if I*2 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i2null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 41)then
C               check if I*4 datatype null value is defined, 
                if (tnull(colnum,ibuff).ne.123454321)then
                        i4null=tnull(colnum,ibuff)
                        nulchk=nultyp
                end if
            else if (tcode .eq. 42 .or. tcode .eq. 82)then
C               have to check floating point data for NaN values
                nulchk=nultyp
            end if
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data from FITS file, doing datatype conversion and scaling
        if (tcode .eq. 21)then
C               column data type is I (I*2)
C               read the data and do any machine dependent data conversion
                call ftgi2b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti2r8(buffer,itodo,scale,zero,tofits,
     &          nulchk,i2null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 41)then
C               column data type is J (I*4)
C               read the data and do any machine dependent data conversion
                call ftgi4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call fti4r8(buffer,itodo,scale,zero,tofits,
     &          nulchk,i4null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 42)then
C               column data type is E (R*4)
C               read the data and do any machine dependent data conversion
                call ftgr4b(iunit,itodo,incre,buffer,status)
C               check for null values, and do scaling and datatype conversion
                call ftr4r8(buffer,itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 82)then
C               column data type is D (R*8)
C               read the data and do any machine dependent data conversion
C               note that we can use the input array directly
                call ftgr8b(iunit,itodo,incre,array(i1),status)
C               check for null values, and do scaling and datatype conversion
                call ftr8r8(array(i1),itodo,scale,zero,tofits,
     &          nulchk,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 11)then
C               column data type is B (byte)
C               read the data 
                call ftgi1b(iunit,itodo,incre,chbuff,status)
C               check for null values, and do scaling and datatype conversion
                call fti1r8(chbuff,itodo,scale,zero,tofits,
     &          nulchk,i1null,nulval,flgval(i1),anynul,array(i1))
        else if (tcode .eq. 16)then
C               this is an ASCII table column; get the character string
                call ftgcbf(iunit,1,tnull(colnum,ibuff),sval,status)

C               check for null
                if (sval(1:8) .eq. snull)then
                        anynul=.true.
                        if (nultyp .eq. 1)then
                                array(i1)=nulval
                        else
                                flgval(i1)=.true.
                        end if
                        go to 30
                end if

C               now read the value, then do scaling and datatype conversion
                if (sform(5:5) .eq. 'I')then
                        read(sval,sform,err=900)ival
                        call fti4r8(ival,itodo,scale,zero,tofits,
     &                  0,i4null,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5).eq.'F'.or. sform(5:5).eq.'E')then
                        read(sval,sform,err=900)rval
                        call ftr4r8(rval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else if (sform(5:5) .eq. 'D')then
                        read(sval,sform,err=900)dval
                        call ftr8r8(dval,itodo,scale,zero,tofits,
     &                  0,nulval,flgval(i1),anynul,array(i1))
                else
C                       error: illegal ASCII table format code
                        status=311
                        return
                end if
        else
C               error illegal binary table data type code
                status=312
                return
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        if (incre .eq. 0)then
                                rstart=rstart+1
                        else
                                rstart=rstart+repeat
                        end if
                end if
                go to 20
        end if
        return

900     continue
C       error reading formatted data value
        status=315
        end
C----------------------------------------------------------------------
        subroutine ftgclc(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of complex data values from the specified column of
C       the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.
C       The binary table column being read to must have datatype 'C'
C       and no datatype conversion will be perform if it is not.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  cm  value that undefined pixels will be set to (if nultyp=1)
C       array   cm  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        real array(*),nulval(2)
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bytpix,bstart,tcode,nulchk,offset
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart

        logical descrp
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

C       determine the repeat count and the first element position
        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        else
                estart=(felem-1)*2
        end if

        i1=1
        ntodo=nelem*2
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        bytpix=4

        if (tcode .eq. 83)then
                repeat=trept(colnum,ibuff)*2
                if (felem*2 .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                end if
                descrp=.false.
        else if (tcode .eq. -83)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=83
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                repeat=repeat*2
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if (estart+ntodo .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                call ftmbyt(iunit,bstart,.true.,status)
        else
C               column must be complex data type
                status=312
                return
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval(1) .eq. 0 .and.
     &      nulval(2) .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C               have to check floating point data for NaN values
                nulchk=nultyp
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data 
        call ftgr4b(iunit,itodo,0,array(i1),status)
C       check for null values
        if (nulchk .ne. 0)then
          call ftnulc(array(i1),itodo,nulchk,nulval,flgval(i1),anynul)
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if

        end
C----------------------------------------------------------------------
        subroutine ftgclm(iunit,colnum,frow,felem,nelem,
     &   nultyp,nulval,array,flgval,anynul,status)

C       read an array of double precision complex data values from the 
C       specified column of the table.  
C       This general purpose routine will handle null values in one
C       of two ways: if nultyp=1, then undefined array elements will be
C       set equal to the input value of NULVAL.  Else if nultyp=2, then
C       undefined array elements will have the corresponding FLGVAL element
C       set equal to .TRUE.  If NULTYP=1 and NULVAL=0, then no checks for
C       undefined values will be made, for maximum efficiency.
C       The binary table column being read to must have datatype 'M'
C       and no datatype conversion will be perform if it is not.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read from
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nultyp  i  input code indicating how to handle undefined values
C       nulval  dcm  value that undefined pixels will be set to (if nultyp=1)
C       array   dcm  array of data values that are read from the FITS file
C       flgval  l  set .true. if corresponding element undefined (if nultyp=2)
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,nultyp,status
        double precision array(*),nulval(2)
        logical flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bytpix,bstart,tcode,nulchk,offset
        integer ibuff,i1,ntodo,itodo,repeat,rstart,estart
        logical descrp
        
        if (status .gt. 0)return

C       check for zero length array or bad first row number
        if (nelem .le. 0)return
        if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        end if

        if (felem .lt. 1)then
C               illegal element number
                status=308
                return
        else
                estart=(felem-1)*2
        end if

        i1=1
        ntodo=nelem*2
        rstart=frow-1
        anynul=.false.
        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)
        bytpix=8

        if (tcode .eq. 163)then
                repeat=trept(colnum,ibuff)*2
                if (felem*2 .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                end if
                descrp=.false.
        else if (tcode .eq. -163)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=83
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                repeat=repeat*2
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if (estart+ntodo .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart*bytpix
                call ftmbyt(iunit,bstart,.true.,status)
        else
C               column must be double complex data type
                status=312
                return
        end if

C       determine if we have to check for null values
        if (nultyp .eq. 1 .and. nulval(1) .eq. 0 .and.
     &      nulval(2) .eq. 0)then
C               user doesn't want to check for nulls
                nulchk=0
        else
C               have to check floating point data for NaN values
                nulchk=nultyp
        end if

C       process as many contiguous pixels as possible, up to buffer size
20      itodo=min(ntodo,repeat-estart)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart*bytpix
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       read the data 
        call ftgr8b(iunit,itodo,0,array(i1),status)
C       check for null values
        if (nulchk .ne. 0)then
          call ftnulm(array(i1),itodo,nulchk,nulval,flgval(i1),anynul)
        end if

C       find number of pixels left to do, and quit if none left
30      ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgcl(iunit,colnum,frow,felem,nelem,lray,status)

C       read an array of logical values from a specified column of the table.
C       The binary table column being read from must have datatype 'L'
C       and no datatype conversion will be perform if it is not.
C       This routine ignores any undefined values in the logical array.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       lray    l  returned array of data values that is read
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical lray(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,maxpix,offset,tcode
        integer ibuff,i,i1,ntodo,itodo,repeat,rstart,estart
        character*1 buffer(80)
        logical descrp
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)

C       check for zero length array
        if (nelem .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
        else if (felem .lt. 1)then
C                       illegal element number
                        status=308
        end if

        i1=0
        ntodo=nelem
        rstart=frow-1
        estart=felem-1
        maxpix=80

        if (tcode .eq. 14)then
                repeat=trept(colnum,ibuff)
                if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                end if
                descrp=.false.
        else if (tcode .eq. -14)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=14
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if (estart+ntodo .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
                call ftmbyt(iunit,bstart,.true.,status)
        else
C               column must be logical data type
                status=312
                return
        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &      tbcol(colnum,ibuff)+estart
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       get the array of logical bytes
        call ftgcbf(iunit,1,itodo,buffer,status)

C       decode the 'T' and 'F' characters, 
        do 10 i=1,itodo
                if (buffer(i) .eq. 'T')then
                        lray(i1+i)=.true.
                else if (buffer(i) .eq. 'F')then
                        lray(i1+i)=.false.
                else if (ichar(buffer(i)) .eq. 0)then
C                       ignore null values; leave input logical value unchanged
                else
C                       illegal logical value
                        status=316
                        return
                end if
10      continue
        
C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgcvs(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of string values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=' ', in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element in the row to read
C       nelem   i  number of elements to read
C       nulval  c  value that undefined pixels will be set to
C       array   c  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        character*(*) array(*),nulval

        call ftgcls(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcvb(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of byte values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  b  value that undefined pixels will be set to
C       array   b  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul

        character*1 array(*),nulval

        call ftgclb(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcvi(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of I*2 values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  i*2  value that undefined pixels will be set to
C       array   i*2 returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        integer*2 array(*),nulval

        call ftgcli(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcvj(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of I*4 values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  i  value that undefined pixels will be set to
C       array   i  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        integer array(*),nulval

        call ftgclj(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcve(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of R*4 values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  r  value that undefined pixels will be set to
C       array   r  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        real array(*),nulval

        call ftgcle(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcvd(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of r*8 values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  d  value that undefined pixels will be set to
C       array   d  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        double precision array(*),nulval

        call ftgcld(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcvc(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of complex values from a specified column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  cmp  value that undefined pixels will be set to
C       array   cmp  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        real array(*),nulval(2)

        call ftgclc(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcvm(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)

C       read an array of double precision complex values from a specified 
C       column of the table.
C       Any undefined pixels will be set equal to the value of NULVAL,
C       unless NULVAL=0, in which case no checks for undefined pixels
C       will be made.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  dcmp  value that undefined pixels will be set to
C       array   dcmp  returned array of data values that was read from FITS file
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval,anynul
        double precision array(*),nulval(2)

        call ftgclm(iunit,colnum,frow,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfs(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of string values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element in the row to read
C       nelem   i  number of elements to read
C       array   c  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        character*(*) array(*)
        character*8 dummy

        call ftgcls(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfl(iunit,colnum,frow,felem,nelem,lray,
     &          flgval,anynul,status)

C       read an array of logical values from a specified column of the table.
C       The binary table column being read from must have datatype 'L'
C       and no datatype conversion will be perform if it is not.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       lray    l  returned array of data values that is read
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical lray(*),flgval(*),anynul

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,maxpix,tcode,offset
        integer ibuff,i,i1,ntodo,itodo,repeat,rstart,estart
        character*1 buffer(80)
        logical descrp
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)
C       check for zero length array
        if (nelem .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
        else if (felem .lt. 1)then
C                       illegal element number
                        status=308
        end if

        if (status .gt. 0)return

C       initialize the null flag array
        do 5 i=1,nelem
                flgval(i)=.false.
5       continue
        anynul=.false.

        i1=0
        ntodo=nelem
        rstart=frow-1
        estart=felem-1
        maxpix=80
        tcode=tdtype(colnum,ibuff)

        if (tcode .eq. 14)then
                repeat=trept(colnum,ibuff)
                if (felem .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                end if
                descrp=.false.
        else if (tcode .eq. -14)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=14
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if (estart+ntodo .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
C               move the i/o pointer to the start of the pixel sequence
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
                call ftmbyt(iunit,bstart,.true.,status)
        else
C               column must be logical data type
                status=312
                return
        end if

C       process as many contiguous pixels as possible
20      itodo=min(ntodo,repeat-estart,maxpix)

        if (.not. descrp)then
C           move the i/o pointer to the start of the sequence of pixels
            bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &             tbcol(colnum,ibuff)+estart
            call ftmbyt(iunit,bstart,.false.,status)
        end if

C       get the array of logical bytes
        call ftgcbf(iunit,1,itodo,buffer,status)

C       decode the 'T' and 'F' characters, and look for nulls (0)
        do 10 i=1,itodo
                if (buffer(i) .eq. 'T')then
                        lray(i1+i)=.true.
                else if (buffer(i) .eq. 'F')then
                        lray(i1+i)=.false.
                else if (ichar(buffer(i)) .eq. 0)then
                        flgval(i1+i)=.true.
                        anynul=.true.
                else
                        status=316
                        return
                end if
10      continue
        
C       find number of pixels left to do, and quit if none left
        ntodo=ntodo-itodo
        if (ntodo .gt. 0)then
C               increment the pointers
                i1=i1+itodo
                estart=estart+itodo
                if (estart .eq. repeat)then
                        estart=0
                        rstart=rstart+1
                end if
                go to 20
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgcfb(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of byte values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   b  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul

        character*1 array(*),dummy

        call ftgclb(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfi(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of I*2 values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   i*2 returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        integer*2 array(*),dummy

        call ftgcli(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfj(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of I*4 values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   i  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        integer array(*),dummy

        call ftgclj(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfe(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of R*4 values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   r  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        real array(*),dummy

        call ftgcle(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfd(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of r*8 values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   d  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        double precision array(*),dummy

        call ftgcld(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfc(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of complex values from a specified column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   cmp  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        real array(*),dummy

        call ftgclc(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcfm(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       read an array of double precision complex values from a specified 
C       column of the table.
C       Any undefined pixels will be have the corresponding value of FLGVAL
C       set equal to .true., and ANYNUL will be set equal to .true. if
C       any pixels are undefined.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   dcmp  returned array of data values that was read from FITS file
C       flgval  l  set .true. if corresponding element undefined
C       anynul  l  set to .true. if any of the returned values are undefined
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,colnum,frow,felem,nelem,status
        logical flgval(*),anynul
        double precision array(*),dummy

        call ftgclm(iunit,colnum,frow,felem,nelem,2,dummy,
     &      array,flgval,anynul,status)
        end        
C----------------------------------------------------------------------
        subroutine ftgcx(iunit,colnum,frow,fbit,nbit,lray,status)

C       read an array of logical values from a specified bit or byte
C       column of the binary table.  A logical .true. value is returned
C       if the corresponding bit is 1, and a logical .false. value is
C       returned if the bit is 0.
C       The binary table column being read from must have datatype 'B'
C       or 'X'. This routine ignores any undefined values in the 'B' array.

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       fbit    i  first bit within the row to read
C       nbit    i  number of bits to read
C       lray    l  returned array of logical data values that is read
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Mar 1992

        integer iunit,colnum,frow,fbit,nbit,status
        logical lray(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,offset,tcode,fbyte,bitloc,ndone
        integer ibuff,i,ntodo,repeat,rstart,estart,buffer
        logical descrp,log8(8)
      
        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)

C       check input parameters
        if (nbit .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fbit .lt. 1)then
C               illegal element number
                status=308
                return
        end if

        fbyte=(fbit+7)/8
        bitloc=fbit-(fbit-1)/8*8
        ndone=0
        ntodo=nbit
        rstart=frow-1
        estart=fbyte-1

        if (tcode .eq. 11)then
                repeat=trept(colnum,ibuff)
                if (fbyte .gt. repeat)then
C                       illegal element number
                        status=308
                        return
                end if
                descrp=.false.
C               move the i/o pointer to the start of the sequence of pixels
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &          tbcol(colnum,ibuff)+estart
        else if (tcode .eq. -11)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=11
C               read the number of elements and the starting offset:
                call ftgdes(iunit,colnum,frow,repeat,
     &                              offset,status)
                repeat=(repeat+7)/8
                if (repeat .eq. 0)then
C                       error: null length vector
                        status=318
                        return
                else if ((fbit+nbit+6)/8 .gt. repeat)then
C                       error: trying to read beyond end of record
                        status=319
                        return
                end if
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
        else
C               column must be byte or bit data type
                status=312
                return
        end if

C       move the i/o pointer to the start of the pixel sequence
        call ftmbyt(iunit,bstart,.false.,status)

C       get the next byte
20      buffer=0
        call ftgbyt(iunit,1,buffer,status)

C       decode the bits within the byte into an array of logical values
        call ftgbit(buffer,log8)

        do 10 i=bitloc,8
                ndone=ndone+1
                lray(ndone)=log8(i)
                if (ndone .eq. ntodo)go to 100
10      continue
        
C       not done, so get the next byte
        if (.not. descrp)then
                estart=estart+1
                if (estart .eq. repeat)then
C                       move the i/o pointer to the next row of pixels
                        estart=0
                        rstart=rstart+1
                        bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &                         tbcol(colnum,ibuff)+estart
                        call ftmbyt(iunit,bstart,.false.,status)
                end if
        end if
        bitloc=1
        go to 20

100     continue
        end
C----------------------------------------------------------------------
        subroutine ftpclx(iunit,colnum,frow,fbit,nbit,lray,status)

C       write an array of logical values to a specified bit or byte
C       column of the binary table.   If the LRAY parameter is .true.,
C       then the corresponding bit is set to 1, otherwise it is set
C       to 0.
C       The binary table column being written to must have datatype 'B'
C       or 'X'. 

C       iunit   i  fortran unit number
C       colnum  i  number of the column to write to
C       frow    i  first row to write
C       fbit    i  first bit within the row to write
C       nbit    i  number of bits to write
C       lray    l  array of logical data values corresponding to the bits
C                        to be written
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Mar 1992
C       modified by Wm Pence May 1992 to remove call to system dependent
C                                     bit testing and setting routines.

        integer iunit,colnum,frow,fbit,nbit,status
        logical lray(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer bstart,offset,tcode,fbyte,bitloc,ndone
        integer ibuff,i,ntodo,repeat,rstart,estart,buffer
        logical descrp,wrbit(8),setbit(8)
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)
        tcode=tdtype(colnum,ibuff)

C       check input parameters
        if (nbit .le. 0)then
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fbit .lt. 1)then
C               illegal element number
                status=308
                return
        end if

        fbyte=(fbit+7)/8
        bitloc=fbit-(fbit-1)/8*8
        ndone=0
        ntodo=nbit
        rstart=frow-1
        estart=fbyte-1

        if (tcode .eq. 11)then
                descrp=.false.
C               N.B: REPEAT is the number of bytes, not number of bits
                repeat=trept(colnum,ibuff)
                if (fbyte .gt. repeat)then
C                               illegal element number
                                status=308
                                return
                end if
C               calc the i/o pointer location to start of sequence of pixels
                bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &          tbcol(colnum,ibuff)+estart
        else if (tcode .eq. -11)then
C               this is a variable length descriptor column
                descrp=.true.
                tcode=11
C               only bit arrays (tform = 'X') are supported for variable
C               length arrays.  REPEAT is the number of BITS in the array.        
                repeat=estart+ntodo
                offset=nxheap(ibuff)
C               write the number of elements and the starting offset:
                call ftpdes(iunit,colnum,frow,repeat,
     &                              offset,status)
C               calc the i/o pointer location to start of sequence of pixels
                bstart=dtstrt(ibuff)+offset+
     &                          theap(ibuff)+estart
C               increment the empty heap starting address (in bytes):
                repeat=(repeat+7)/8
                nxheap(ibuff)=nxheap(ibuff)+repeat
        else
C               column must be byte or bit data type
                status=310
                return
        end if

C       move the i/o pointer to the start of the pixel sequence
        call ftmbyt(iunit,bstart,.true.,status)

C       read the next byte (we may only be modifying some of the bits)
20      buffer=0
        call ftgbyt(iunit,1,buffer,status)
C       move back, to be able to overwrite the byte
        call ftmbyt(iunit,bstart,.false.,status)

C       reset flags indicating which bits are to be set
        wrbit(1)=.false.
        wrbit(2)=.false.
        wrbit(3)=.false.
        wrbit(4)=.false.
        wrbit(5)=.false.
        wrbit(6)=.false.
        wrbit(7)=.false.
        wrbit(8)=.false.

C       flag the bits that are to be set 
        do 10 i=bitloc,8
                wrbit(i)=.true.
                ndone=ndone+1
                if(lray(ndone))then
                        setbit(i)=.true.
                else
                        setbit(i)=.false.
                end if
                if (ndone .eq. ntodo)go to 100
10      continue

C       set or reset the bits within the byte
        call ftpbit(setbit,wrbit,buffer)

C       write the new byte
        call ftpbyt(iunit,1,buffer,status)
        
C       not done, so get the next byte
        bstart=bstart+1
        if (.not. descrp)then
                estart=estart+1
                if (estart .eq. repeat)then
C                       move the i/o pointer to the next row of pixels
                        estart=0
                        rstart=rstart+1
                        bstart=dtstrt(ibuff)+rstart*rowlen(ibuff)+
     &                         tbcol(colnum,ibuff)+estart
                        call ftmbyt(iunit,bstart,.true.,status)
                end if
        end if
        bitloc=1
        go to 20

100     continue
C       set or reset the bits within the byte
        call ftpbit(setbit,wrbit,buffer)

C       write the new byte
        call ftpbyt(iunit,1,buffer,status)
        end
C----------------------------------------------------------------------
        subroutine ftgbit(buffer,log8)

C       decode the individual bits within the byte into an array of 
C       logical values.  The corresponding logical value is set to 
C       true if the bit is set to 1.

C       buffer  i  input integer containing the byte to be decoded
C       log8    l  output array of logical data values corresponding 
C                  to the bits in the input buffer
C
C       written by Wm Pence, HEASARC/GSFC, May 1992

        integer buffer,tbuff
        integer*2 t2buff(2),t2temp
        logical log8(8)
        equivalence (tbuff,t2buff)

        log8(1)=.false.
        log8(2)=.false.
        log8(3)=.false.
        log8(4)=.false.
        log8(5)=.false.
        log8(6)=.false.
        log8(7)=.false.
        log8(8)=.false.

C       test for special case: no bits are set
        if (buffer .eq. 0)return

C       This algorithm tests to see if each bit is set by testing
C       the numerical value of the byte, starting with the most significant
C       bit.  If the bit is set, then it is reset to zero before testing
C       the next most significant bit, and so on.

C       depending on the machine, the byte to be decoded may be anywhere
C       in the 4 byte word.  So we must first
C       determine which byte to decode.  We can safely assume that
C       the other 3 bytes of the word will be set to zero on input.

        tbuff=buffer

C       on some machines (e.g. VAX) we have to swap words to avoid sign bit
        if (tbuff .lt. 0 .or. tbuff .gt. 65535)then
                t2temp=t2buff(2)
                t2buff(2)=t2buff(1)
                t2buff(1)=t2temp
        end if

C       if the next-to-least significant byte is to be tested, then
C       shift the bit pattern in that byte into the least significant byte
C       by dividing by 2**8
        if (tbuff .gt. 255)tbuff=tbuff/256

C       now decode the least significant byte
        if (tbuff .gt. 127)then
                log8(1)=.true.
                tbuff=tbuff-128
        end if
        if (tbuff .gt. 63)then
                log8(2)=.true.
                tbuff=tbuff-64
        end if
        if (tbuff .gt. 31)then
                log8(3)=.true.
                tbuff=tbuff-32
        end if
        if (tbuff .gt. 15)then
                log8(4)=.true.
                tbuff=tbuff-16
        end if
        if (tbuff .gt. 7)then
                log8(5)=.true.
                tbuff=tbuff-8
        end if
        if (tbuff .gt. 3)then
                log8(6)=.true.
                tbuff=tbuff-4
        end if
        if (tbuff .gt. 1)then
                log8(7)=.true.
                tbuff=tbuff-2
        end if
        if (tbuff .eq. 1)then
                log8(8)=.true.
        end if
        end            
C----------------------------------------------------------------------
        subroutine ftpbit(setbit,wrbit,buffer)

C       encode the individual bits within the byte as specified by
C       the input logical array. The corresponding bit is set to 
C       1 if the logical array element is true.  Only the bits
C       between begbit and endbit, inclusive, are set or reset;
C       the remaining bits, if any, remain unchanged.

C       setbit  l  input array of logical data values corresponding 
C                  to the bits to be set in the output buffer
C                  TRUE means corresponding bit is to be set.
C       wrbit   l  input array of logical values indicating which
C                  bits in the byte are to be modified.  If FALSE,
C                  then the corresponding bit should remain unchanged.
C       buffer  i  output integer containing the encoded byte
C
C       written by Wm Pence, HEASARC/GSFC, May 1992

        integer buffer,tbuff,outbit
        integer*2 t2buff(2),t2temp,out2(2)
        logical setbit(8),wrbit(8)
        equivalence (tbuff,t2buff)
        equivalence (outbit,out2)

C       depending on the machine, the byte to be encoded may either
C       be the least significant byte or the next-to-least significant
C       byte of the input buffer word (4 bytes).  Therefore, we will
C       encode both bytes (they will have the same 8-bit pattern) to
C       cover both possibilities.

        outbit=0
        tbuff=buffer

C       on some machines (e.g. VAX) we have to swap words to avoid sign bit
        if (tbuff .lt. 0 .or. tbuff .gt. 65535)then
                t2temp=t2buff(2)
                t2buff(2)=t2buff(1)
                t2buff(1)=t2temp
        end if

C       if the next-to-least significant byte is to be tested, then
C       shift the bit pattern in that byte into the least significant byte
C       by dividing by 2**8
        if (tbuff .gt. 255)tbuff=tbuff/256

C       now test each of the 8 bits, starting with the most significant
        if (tbuff .gt. 127)then
C           the bit is currently set in the word
            if (wrbit(1) .and. (.not.setbit(1)))then
C                only in this case do we reset the bit
            else
C               in all other cases we want the bit to be set
                outbit=outbit+32896
            end if
            tbuff=tbuff-128
        else
C           bit is currently not set; set it only if requested to
            if (wrbit(1) .and. setbit(1))outbit=outbit+32896
        end if

         if (tbuff .gt. 63)then
            if (wrbit(2) .and. (.not.setbit(2)))then
            else
                outbit=outbit+16448
            end if
            tbuff=tbuff-64
        else
            if (wrbit(2) .and. setbit(2))outbit=outbit+16448
        end if

        if (tbuff .gt. 31)then
            if (wrbit(3) .and. (.not.setbit(3)))then
            else
                outbit=outbit+8224
            end if
            tbuff=tbuff-32
        else
            if (wrbit(3) .and. setbit(3))outbit=outbit+8224
        end if

        if (tbuff .gt. 15)then
            if (wrbit(4) .and. (.not.setbit(4)))then
            else
                outbit=outbit+4112
            end if
            tbuff=tbuff-16
        else
            if (wrbit(4) .and. setbit(4))outbit=outbit+4112
        end if

        if (tbuff .gt. 7)then
            if (wrbit(5) .and. (.not.setbit(5)))then
            else
                outbit=outbit+2056
            end if
            tbuff=tbuff-8
        else
            if (wrbit(5) .and. setbit(5))outbit=outbit+2056
        end if

        if (tbuff .gt. 3)then
            if (wrbit(6) .and. (.not.setbit(6)))then
            else
                outbit=outbit+1028
            end if
            tbuff=tbuff-4
        else
            if (wrbit(6) .and. setbit(6))outbit=outbit+1028
        end if

        if (tbuff .gt. 1)then
            if (wrbit(7) .and. (.not.setbit(7)))then
            else
                outbit=outbit+514
            end if
            tbuff=tbuff-2
        else
            if (wrbit(7) .and. setbit(7))outbit=outbit+514
        end if

        if (tbuff .eq. 1)then
            if (wrbit(8) .and. (.not.setbit(8)))then
            else
                outbit=outbit+257
            end if
        else
            if (wrbit(8) .and. setbit(8))outbit=outbit+257
        end if

C       now duplicate the non-zero 2-byte word, so that all 4 bytes
C       of the I*4 word are identical (any byte will do)

        if (out2(1) .eq. 0)then
           out2(1)=out2(2)
        else
           out2(2)=out2(1)
        end if

        buffer=outbit
        end            
C----------------------------------------------------------------------
        logical function fttrnn(value)

C       test if a R*4 value has a IEEE Not-a-Number (NaN) value
C       A NaN has all the exponent bits=1, and the fractional part not=0.  
C       The exponent field occupies bits 23-30,  (least significant bit = 0)
C       The mantissa field occupies bits 0-22


C       written by Wm Pence, HEASARC/GSFC, May 1992

        integer value

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 12)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        integer*2 compid(2)
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

C       COMPID is used by FITSIO to determine at run time what type of
C       machine it is running on.  Two I*2 integers are equivalenced to
C       the R*4 value of 1.111111111.  The resulting value of the integers
C       is machine specific, depending on whether R*4 values are stored in
C       IEEE format or not, and whether the bytes are swapped or not.
C       COMPID(1) will have the following value on the following machines:
C        MACHINE              COMPID(1)
C          VAX                  16526
C          SUN                  16270
C          DecStation, IBM PC   14564
C          IBM mainframe        16657

        fttrnn=.false.
        if (compid(1) .eq. 16526)then
C           on the VAX we can assume that all NaNs will be set to all bits on
C           (which is equivalent to an integer with a value of -1) because
C           this is what the IEEE to VAX conversion MACRO program returns
            if (value .eq. -1)fttrnn=.true.
        else
C           the following test works on all other supported machines
C           the sign bit may be either 1 or 0 so have to test both possibilites
            if (value .gt. 2139095040 .or. (value .lt. 0 .and. 
     1          value .gt. -8388608))fttrnn=.true.
        end if
        end
C----------------------------------------------------------------------
        logical function fttdnn(value)

C       test if a R*8 value has a IEEE Not-a-Number value
C       A NaN has all the exponent bits=1, and the fractional part
C       not=0. 
C       Exponent field is in bits 20-30 in the most significant 4-byte word
C       Mantissa field is in bits 0-19 of most sig. word and entire 2nd word
C
C       written by Wm Pence, HEASARC/GSFC, May 1992

        integer value(2)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 12)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        integer*2 compid(2)
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

C       COMPID is used by FITSIO to determine at run time what type of
C       machine it is running on.  Two I*2 integers are equivalenced to
C       the R*4 value of 1.111111111.  The resulting value of the integers
C       is machine specific, depending on whether R*4 values are stored in
C       IEEE format or not, and whether the bytes are swapped or not.
C       COMPID(1) will have the following value on the following machines:
C        MACHINE              COMPID(1)
C          VAX                  16526
C          SUN                  16270
C          DecStation, IBM PC   14564
C          IBM mainframe        16657

        fttdnn=.false.
        if (compid(1) .eq. 16526)then
C           on the VAX we can assume that all NaNs will be set to all bits on
C           (which is equivalent to an integer with a value of -1) because
C           this is what the IEEE to VAX conversion MACRO program returns
            if (value(1) .eq. -1 .and. value(2) .eq. -1)fttdnn=.true.
        else
C           the following test works on all other supported machines
C           the sign bit may be either 1 or 0 so have to test both possibilites
            if (value(2) .eq. 0)then
                if (value(1) .gt. 2146435072 .or. (value(1) .lt. 0
     1           .and. value(1) .gt. -1048576))fttdnn=.true.
            else
                if (value(1) .ge. 2146435072 .or. (value(1) .lt. 0
     1           .and. value(1) .ge. -1048576))fttdnn=.true.
            end if 
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgtbs(iunit,frow,fchar,nchars,svalue,status)

C       read a consecutive string of characters from an ascii or binary
C       table. This will span multiple rows of the table if NCHARS+FCHAR is
C       greater than the length of a row.

C       iunit   i  fortran unit number
C       frow    i  starting row number (1st row = 1)
C       fchar   i  starting character/byte in the row to read (1st character=1)
C       nchars  i  number of characters/bytes to read (can span multiple rows)
C       svalue  c  returned string of characters
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,frow,fchar,nchars,status
        character*(*) svalue

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bstart,nget
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check for errors
        if (nchars .le. 0)then
C               zero or negative number of character requested
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fchar .lt. 1)then
C               error: illegal starting character
                status=308
                return
        end if
        
C       move the i/o pointer to the start of the sequence of characters
        bstart=dtstrt(ibuff)+(frow-1)*rowlen(ibuff)+fchar-1
        call ftmbyt(iunit,bstart,.false.,status)

C       get the string of characters, (up to the length of the input string)
        svalue=' '
        nget=min(nchars,len(svalue))
        call ftgcbf(iunit,1,nget,svalue,status)
        end
C----------------------------------------------------------------------
        subroutine ftgtbb(iunit,frow,fchar,nchars,value,status)

C       read a consecutive string of bytes from an ascii or binary
C       table. This will span multiple rows of the table if NCHARS+FCHAR is
C       greater than the length of a row.

C       iunit   i  fortran unit number
C       frow    i  starting row number (1st row = 1)
C       fchar   i  starting character/byte in the row to read (1st character=1)
C       nchars  i  number of characters/bytes to read (can span multiple rows)
C       value   i  returned string of bytes
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Dec 1991

        integer iunit,frow,fchar,nchars,status
        integer value(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bstart
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check for errors
        if (nchars .le. 0)then
C               zero or negative number of character requested
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fchar .lt. 1)then
C               error: illegal starting character
                status=308
                return
        end if
        
C       move the i/o pointer to the start of the sequence of characters
        bstart=dtstrt(ibuff)+(frow-1)*rowlen(ibuff)+fchar-1
        call ftmbyt(iunit,bstart,.false.,status)

C       get the string of bytes
        call ftgbyt(iunit,nchars,value,status)
        end
C----------------------------------------------------------------------
        subroutine ftptbs(iunit,frow,fchar,nchars,svalue,status)

C       write a consecutive string of characters to an ascii or binary
C       table. This will span multiple rows of the table if NCHARS+FCHAR is
C       greater than the length of a row.

C       iunit   i  fortran unit number
C       frow    i  starting row number (1st row = 1)
C       fchar   i  starting character/byte in the row to write (1st character=1)
C       nchars  i  number of characters/bytes to write (can span multiple rows)
C       svalue  c  string of characters to write
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Dec 1991

        integer iunit,frow,fchar,nchars,status
        character*(*) svalue

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bstart,nput
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check for errors
        if (nchars .le. 0)then
C               zero or negative number of character requested
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fchar .lt. 1)then
C               error: illegal starting character
                status=308
                return
        end if
        
C       move the i/o pointer to the start of the sequence of characters
        bstart=dtstrt(ibuff)+(frow-1)*rowlen(ibuff)+fchar-1
        call ftmbyt(iunit,bstart,.true.,status)

C       put the string of characters, (up to the length of the input string)
        nput=min(nchars,len(svalue))
        call ftpcbf(iunit,1,nput,svalue,status)
        end
C----------------------------------------------------------------------
        subroutine ftptbb(iunit,frow,fchar,nchars,value,status)

C       write a consecutive string of bytes to an ascii or binary
C       table. This will span multiple rows of the table if NCHARS+FCHAR is
C       greater than the length of a row.

C       iunit   i  fortran unit number
C       frow    i  starting row number (1st row = 1)
C       fchar   i  starting byte in the row to write (1st character=1)
C       nchars  i  number of bytes to write (can span multiple rows)
C       value   i  array of bytes to write
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Dec 1991

        integer iunit,frow,fchar,nchars,status
        integer value(*)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 12)
        parameter (nf = 512)
        parameter (ne = 128)
        integer bufnum,bufpnt,reclen,recnum,bytnum
        integer chdu,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        logical wrmode,modify
        real compid
        common/ft0001/bufnum(199),bufpnt(nb),reclen(199),recnum(nb),
     &  bytnum(nb),wrmode(nb),modify(nb),chdu(nb),maxhdu(nb),
     &  hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),compid
        integer tfield,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tbcol(nf,nb),rowlen(nb),tdtype(nf,nb),
     &  trept(nf,nb),tscale(nf,nb),tzero(nf,nb),tnull(nf,nb),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,bstart
        
        if (status .gt. 0)return

        ibuff=bufnum(iunit)

C       check for errors
        if (nchars .le. 0)then
C               zero or negative number of character requested
                return
        else if (frow .lt. 1)then
C               error: illegal first row number
                status=307
                return
        else if (fchar .lt. 1)then
C               error: illegal starting character
                status=308
                return
        end if
        
C       move the i/o pointer to the start of the sequence of characters
        bstart=dtstrt(ibuff)+(frow-1)*rowlen(ibuff)+fchar-1
        call ftmbyt(iunit,bstart,.true.,status)

C       put the string of bytes
        call ftpbyt(iunit,nchars,value,status)
        end
C----------------------------------------------------------------------
        subroutine ftpprb(ounit,group,felem,nelem,array,status)

C       Write an array of byte values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   b  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row

        character*1 array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclb(ounit,2,row,felem,nelem,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftppri(ounit,group,felem,nelem,array,status)

C       Write an array of i*2 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   i*2  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row
        integer*2 array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcli(ounit,2,row,felem,nelem,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpprj(ounit,group,felem,nelem,array,status)

C       Write an array of i*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   i  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row
        integer array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclj(ounit,2,row,felem,nelem,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftppre(ounit,group,felem,nelem,array,status)

C       Write an array of r*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   r  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row
        real array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcle(ounit,2,row,felem,nelem,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpprd(ounit,group,felem,nelem,array,status)

C       Write an array of r*8 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be written
C       array   d  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row
        double precision array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcld(ounit,2,row,felem,nelem,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftppru(ounit,group,felem,nelem,status)

C       set elements of the primary array equal to the undefined value

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be written (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be set to undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,felem,nelem,status,row

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclu(ounit,2,row,felem,nelem,status)
        end
C----------------------------------------------------------------------
        subroutine ftpgpb(ounit,group,fparm,nparm,array,status)

C       Write an array of group parmeters into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter to be written (starting with 1)
C       nparm   i  number of group parameters to be written
C       array   b  the array of group parameters to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,fparm,nparm,status,row

        character*1 array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclb(ounit,1,row,fparm,nparm,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpgpi(ounit,group,fparm,nparm,array,status)

C       Write an array of group parmeters into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter to be written (starting with 1)
C       nparm   i  number of group parameters to be written
C       array   i*2  the array of group parameters to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,fparm,nparm,status,row
        integer*2 array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcli(ounit,1,row,fparm,nparm,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpgpj(ounit,group,fparm,nparm,array,status)

C       Write an array of group parmeters into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter to be written (starting with 1)
C       nparm   i  number of group parameters to be written
C       array   i  the array of group parameters to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,fparm,nparm,status,row
        integer array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpclj(ounit,1,row,fparm,nparm,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpgpe(ounit,group,fparm,nparm,array,status)

C       Write an array of group parmeters into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter to be written (starting with 1)
C       nparm   i  number of group parameters to be written
C       array   r  the array of group parameters to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,fparm,nparm,status,row
        real array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcle(ounit,1,row,fparm,nparm,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftpgpd(ounit,group,fparm,nparm,array,status)

C       Write an array of group parmeters into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter to be written (starting with 1)
C       nparm   i  number of group parameters to be written
C       array   d  the array of group parameters to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,fparm,nparm,status,row
        double precision array(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(group,1)
        call ftpcld(ounit,1,row,fparm,nparm,array,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpvb(iunit,group,felem,nelem,nulval,
     &                    array,anynul,status)

C       Read an array of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will be set equal to NULVAL, unless NULVAL=0
C       in which case no checking for undefined values will be performed.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       nulval  b  the value to be assigned to undefined pixels
C       array   b  returned array of values that were read
C       anynul  l  set to .true. if any returned elements were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        character nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgclb(iunit,2,row,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpvi(iunit,group,felem,nelem,nulval,
     &                    array,anynul,status)

C       Read an array of i*2 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will be set equal to NULVAL, unless NULVAL=0
C       in which case no checking for undefined values will be performed.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       nulval  i*2  the value to be assigned to undefined pixels
C       array   i*2  returned array of values that were read
C       anynul  l  set to .true. if any returned elements were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        integer*2 nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgcli(iunit,2,row,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpvj(iunit,group,felem,nelem,nulval,
     &                    array,anynul,status)

C       Read an array of i*4 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will be set equal to NULVAL, unless NULVAL=0
C       in which case no checking for undefined values will be performed.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       nulval  i  the value to be assigned to undefined pixels
C       array   i  returned array of values that were read
C       anynul  l  set to .true. if any returned elements were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        integer nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgclj(iunit,2,row,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpve(iunit,group,felem,nelem,nulval,
     &                    array,anynul,status)

C       Read an array of r*4 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will be set equal to NULVAL, unless NULVAL=0
C       in which case no checking for undefined values will be performed.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       nulval  r  the value to be assigned to undefined pixels
C       array   r  returned array of values that were read
C       anynul  l  set to .true. if any returned elements were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        real nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgcle(iunit,2,row,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpvd(iunit,group,felem,nelem,nulval,
     &                    array,anynul,status)

C       Read an array of r*8 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will be set equal to NULVAL, unless NULVAL=0
C       in which case no checking for undefined values will be performed.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       nulval  b  the value to be assigned to undefined pixels
C       array   b  returned array of values that were read
C       anynul  l  set to .true. if any returned elements were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        double precision nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgcld(iunit,2,row,felem,nelem,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpfb(iunit,group,felem,nelem,
     &                    array,flgval,anynul,status)

C       Read an array of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will have the corresponding element of
C       FLGVAL set equal to .true.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       array   b  returned array of values that were read
C       flgval  l  set to .true. if the corresponding element is undefined
C       anynul  l  set to .true. if any returned elements are undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        character*1 nulval,array(*)
        logical anynul,flgval(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgclb(iunit,2,row,felem,nelem,2,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpfi(iunit,group,felem,nelem,
     &                    array,flgval,anynul,status)

C       Read an array of I*2 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will have the corresponding element of
C       FLGVAL set equal to .true.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       array   i*2  returned array of values that were read
C       flgval  l  set to .true. if the corresponding element is undefined
C       anynul  l  set to .true. if any returned elements are undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        integer*2 nulval,array(*)
        logical anynul,flgval(*)

        row=max(1,group)
        call ftgcli(iunit,2,row,felem,nelem,2,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpfj(iunit,group,felem,nelem,
     &                    array,flgval,anynul,status)

C       Read an array of I*4 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will have the corresponding element of
C       FLGVAL set equal to .true.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       array   i  returned array of values that were read
C       flgval  l  set to .true. if the corresponding element is undefined
C       anynul  l  set to .true. if any returned elements are undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        integer nulval,array(*)
        logical anynul,flgval(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgclj(iunit,2,row,felem,nelem,2,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpfe(iunit,group,felem,nelem,
     &                    array,flgval,anynul,status)

C       Read an array of r*4 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will have the corresponding element of
C       FLGVAL set equal to .true.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       array   r  returned array of values that were read
C       flgval  l  set to .true. if the corresponding element is undefined
C       anynul  l  set to .true. if any returned elements are undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        real nulval,array(*)
        logical anynul,flgval(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgcle(iunit,2,row,felem,nelem,2,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftgpfd(iunit,group,felem,nelem,
     &                    array,flgval,anynul,status)

C       Read an array of r*8 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).
C       Undefined elements will have the corresponding element of
C       FLGVAL set equal to .true.
C       ANYNUL is return with a value of .true. if any pixels were undefined.

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       felem   i  the first pixel to be read (this routine treats
C                  the primary array a large one dimensional array of
C                  values, regardless of the actual dimensionality).
C       nelem   i  number of data elements to be read
C       array   d  returned array of values that were read
C       flgval  l  set to .true. if the corresponding element is undefined
C       anynul  l  set to .true. if any returned elements are undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,felem,nelem,status,row 
        double precision nulval,array(*)
        logical anynul,flgval(*)

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
        row=max(1,group)
        call ftgcld(iunit,2,row,felem,nelem,2,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftggpb(iunit,group,fparm,nparm,array,status)

C       Read an array of group parameter values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter be read (starting with 1)
C       nparm   i  number of group parameters to be read
C       array   b  returned array of values that were read
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,fparm,nparm,status,row 
        character*1 nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
C       set nulval to blank to inhibit checking for undefined values
        nulval=' '
        row=max(1,group)
        call ftgclb(iunit,1,row,fparm,nparm,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftggpi(iunit,group,fparm,nparm,array,status)

C       Read an array of group parameter values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter be read (starting with 1)
C       nparm   i  number of group parameters to be read
C       array   i*2  returned array of values that were read
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,fparm,nparm,status,row
        integer*2 nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
C       set nulval to blank to inhibit checking for undefined values
        nulval=0
        row=max(1,group)
        call ftgcli(iunit,1,row,fparm,nparm,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftggpj(iunit,group,fparm,nparm,array,status)

C       Read an array of group parameter values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter be read (starting with 1)
C       nparm   i  number of group parameters to be read
C       array   i  returned array of values that were read
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,fparm,nparm,status,row 
        integer nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
C       set nulval to blank to inhibit checking for undefined values
        nulval=0
        row=max(1,group)
        call ftgclj(iunit,1,row,fparm,nparm,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftggpe(iunit,group,fparm,nparm,array,status)

C       Read an array of group parameter values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter be read (starting with 1)
C       nparm   i  number of group parameters to be read
C       array   r  returned array of values that were read
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,fparm,nparm,status,row 
        real nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
C       set nulval to blank to inhibit checking for undefined values
        nulval=0
        row=max(1,group)
        call ftgcle(iunit,1,row,fparm,nparm,1,nulval,
     &      array,flgval,anynul,status)
        end
C----------------------------------------------------------------------
        subroutine ftggpd(iunit,group,fparm,nparm,array,status)

C       Read an array of group parameter values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran unit number
C       group   i  number of the data group, if any
C       fparm   i  the first group parameter be read (starting with 1)
C       nparm   i  number of group parameters to be read
C       array   d  returned array of values that were read
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,group,fparm,nparm,status,row 
        double precision nulval,array(*)
        logical anynul,flgval

C       the primary array is represented as a binary table:
C               each group of the primary array is a row in the table,
C               where the first column contains the group parameters
C               and the second column contains the image itself
C       set nulval to blank to inhibit checking for undefined values
        nulval=0
        row=max(1,group)
        call ftgcld(iunit,1,row,fparm,nparm,1,nulval,
     &      array,flgval,anynul,status)
        end
C--------------------------------------------------------------------------
        subroutine ftp2db(ounit,group,dim1,nx,ny,array,status)

C       Write a 2-d image of byte values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   c*1  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        character*1 array(dim1,*)
        integer fpixel,row

        fpixel=1
        do 10 row = 1,ny
                call ftpprb(ounit,group,fpixel,nx,array(1,row),status)
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp2di(ounit,group,dim1,nx,ny,array,status)

C       Write a 2-d image of i*2 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   i*2  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        integer*2 array(dim1,*)
        integer fpixel,row

        fpixel=1
        do 10 row = 1,ny
                call ftppri(ounit,group,fpixel,nx,array(1,row),status)
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp2dj(ounit,group,dim1,nx,ny,array,status)

C       Write a 2-d image of i*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   i  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        integer array(dim1,*)
        integer fpixel,row

        fpixel=1
        do 10 row = 1,ny
                call ftpprj(ounit,group,fpixel,nx,array(1,row),status)
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp2de(ounit,group,dim1,nx,ny,array,status)

C       Write a 2-d image of r*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   r  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        real array(dim1,*)
        integer fpixel,row

        fpixel=1
        do 10 row = 1,ny
                call ftppre(ounit,group,fpixel,nx,array(1,row),status)
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp2dd(ounit,group,dim1,nx,ny,array,status)

C       Write a 2-d image of r*8 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   d  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        double precision array(dim1,*)
        integer fpixel,row

        fpixel=1
        do 10 row = 1,ny
                call ftpprd(ounit,group,fpixel,nx,array(1,row),status)
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp3db(ounit,group,dim1,dim2,nx,ny,nz,array,status)

C       Write a 3-d cube of byte values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   c*1  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        character*1 array(dim1,dim2,*)
        integer fpixel,row,band

        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
            call ftpprb(ounit,group,fpixel,nx,array(1,row,band),status)
            fpixel=fpixel+nx
10      continue
20      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp3di(ounit,group,dim1,dim2,nx,ny,nz,array,status)

C       Write a 3-d cube of i*2 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   i*2  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        integer*2 array(dim1,dim2,*)
        integer fpixel,row,band

        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
            call ftppri(ounit,group,fpixel,nx,array(1,row,band),status)
            fpixel=fpixel+nx
10      continue
20      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp3dj(ounit,group,dim1,dim2,nx,ny,nz,array,status)

C       Write a 3-d cube of i*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   i  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        integer array(dim1,dim2,*)
        integer fpixel,row,band

        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
            call ftpprj(ounit,group,fpixel,nx,array(1,row,band),status)
            fpixel=fpixel+nx
10      continue
20      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp3de(ounit,group,dim1,dim2,nx,ny,nz,array,status)

C       Write a 3-d cube of r*4 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   r  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        real array(dim1,dim2,*)
        integer fpixel,row,band

        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
            call ftppre(ounit,group,fpixel,nx,array(1,row,band),status)
            fpixel=fpixel+nx
10      continue
20      continue

        end
C--------------------------------------------------------------------------
        subroutine ftp3dd(ounit,group,dim1,dim2,nx,ny,nz,array,status)

C       Write a 3-d cube of r*8 values into the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being written).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   r*8  the array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        double precision array(dim1,dim2,*)
        integer fpixel,row,band

        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
            call ftpprd(ounit,group,fpixel,nx,array(1,row,band),status)
            fpixel=fpixel+nx
10      continue
20      continue

        end
C--------------------------------------------------------------------------
        subroutine ftg2db(ounit,group,nulval,dim1,nx,ny,
     &                    array,anyflg,status)

C       Read a 2-d image of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  c*1  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   c*1  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        character*1 array(dim1,*),nulval
        logical anyflg,ltemp
        integer fpixel,row

        anyflg=.false.
        fpixel=1
        do 10 row = 1,ny
                call ftgpvb(ounit,group,fpixel,nx,nulval,
     &              array(1,row),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftg2di(ounit,group,nulval,dim1,nx,ny,
     &                    array,anyflg,status)

C       Read a 2-d image of i*2 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  i*2  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   i*2  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        integer*2 array(dim1,*),nulval
        logical anyflg,ltemp
        integer fpixel,row

        anyflg=.false.
        fpixel=1
        do 10 row = 1,ny
                call ftgpvi(ounit,group,fpixel,nx,nulval,
     &              array(1,row),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftg2dj(ounit,group,nulval,dim1,nx,ny,
     &                    array,anyflg,status)

C       Read a 2-d image of i*4 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  i  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   i  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        integer array(dim1,*),nulval
        logical anyflg,ltemp
        integer fpixel,row

        anyflg=.false.
        fpixel=1
        do 10 row = 1,ny
                call ftgpvj(ounit,group,fpixel,nx,nulval,
     &              array(1,row),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftg2de(ounit,group,nulval,dim1,nx,ny,
     &                    array,anyflg,status)

C       Read a 2-d image of real values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  r  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   r  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        real array(dim1,*),nulval
        logical anyflg,ltemp
        integer fpixel,row

        anyflg=.false.
        fpixel=1
        do 10 row = 1,ny
                call ftgpve(ounit,group,fpixel,nx,nulval,
     &              array(1,row),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftg2dd(ounit,group,nulval,dim1,nx,ny,
     &                    array,anyflg,status)

C       Read a 2-d image of r*8 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  d  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   d  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        double precision array(dim1,*),nulval
        logical anyflg,ltemp
        integer fpixel,row

        anyflg=.false.
        fpixel=1
        do 10 row = 1,ny
                call ftgpvd(ounit,group,fpixel,nx,nulval,
     &              array(1,row),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue

        end
C--------------------------------------------------------------------------
        subroutine ftg3db(ounit,group,nulval,dim1,dim2,nx,ny,nz,
     &                    array,anyflg,status)

C       Read a 3-d cube of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  c*1  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   c*1  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        character*1 array(dim1,dim2,*),nulval
        logical anyflg,ltemp
        integer fpixel,row,band

        anyflg=.false.
        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
                call ftgpvb(ounit,group,fpixel,nx,nulval,
     &              array(1,row,band),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue
20      continue
        end
C--------------------------------------------------------------------------
        subroutine ftg3di(ounit,group,nulval,dim1,dim2,nx,ny,nz,
     &                    array,anyflg,status)

C       Read a 3-d cube of i*2 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  i*2  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   i*2  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        integer*2 array(dim1,dim2,*),nulval
        logical anyflg,ltemp
        integer fpixel,row,band

        anyflg=.false.
        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
                call ftgpvi(ounit,group,fpixel,nx,nulval,
     &              array(1,row,band),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue
20      continue
        end
C--------------------------------------------------------------------------
        subroutine ftg3dj(ounit,group,nulval,dim1,dim2,nx,ny,nz,
     &                    array,anyflg,status)

C       Read a 3-d cube of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  i  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   i  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        integer array(dim1,dim2,*),nulval
        logical anyflg,ltemp
        integer fpixel,row,band

        anyflg=.false.
        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
                call ftgpvj(ounit,group,fpixel,nx,nulval,
     &              array(1,row,band),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue
20      continue
        end
C--------------------------------------------------------------------------
        subroutine ftg3de(ounit,group,nulval,dim1,dim2,nx,ny,nz,
     &                    array,anyflg,status)

C       Read a 3-d cube of real values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  r  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   r  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        real array(dim1,dim2,*),nulval
        logical anyflg,ltemp
        integer fpixel,row,band

        anyflg=.false.
        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
                call ftgpve(ounit,group,fpixel,nx,nulval,
     &              array(1,row,band),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue
20      continue
        end
C--------------------------------------------------------------------------
        subroutine ftg3dd(ounit,group,nulval,dim1,dim2,nx,ny,nz,
     &                    array,anyflg,status)

C       Read a 3-d cube of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  d  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   d  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        double precision array(dim1,dim2,*),nulval
        logical anyflg,ltemp
        integer fpixel,row,band

        anyflg=.false.
        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
                call ftgpvd(ounit,group,fpixel,nx,nulval,
     &              array(1,row,band),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue
20      continue
        end
C--------------------------------------------------------------------------
        subroutine ftpssb(iunit,group,naxis,naxes,fpixel,lpixel,
     &                    array,status)

C       Write a subsection of byte values to the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be written, if any
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the subsection
C       array   c*1  array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        character*1 array(*)
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftpprb(iunit,group,pstart,i1,
     &              array(astart),status)
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftpssi(iunit,group,naxis,naxes,fpixel,lpixel,
     &                    array,status)

C       Write a subsection of integer*2 values to the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be written, if any
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the subsection
C       array   i*2  array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        integer*2 array(*)
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftppri(iunit,group,pstart,i1,
     &              array(astart),status)
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftpssj(iunit,group,naxis,naxes,fpixel,lpixel,
     &                    array,status)

C       Write a subsection of integer values to the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be written, if any
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the subsection
C       array   i  array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        integer array(*)
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftpprj(iunit,group,pstart,i1,
     &              array(astart),status)
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftpsse(iunit,group,naxis,naxes,fpixel,lpixel,
     &                    array,status)

C       Write a subsection of real values to the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be written, if any
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the subsection
C       array   r  array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        real array(*)
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftppre(iunit,group,pstart,i1,
     &              array(astart),status)
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftpssd(iunit,group,naxis,naxes,fpixel,lpixel,
     &                    array,status)

C       Write a subsection of double precision values to the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be written, if any
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the subsection
C       array   d  array of values to be written
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        double precision array(*)
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftpprd(iunit,group,pstart,i1,
     &              array(astart),status)
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftgssb(iunit,group,nulval,naxis,naxes,fpixel,lpixel,
     &                    array,anyflg,status)

C       Read a subsection of byte values from the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be read, if any
C       nulval  c*1  undefined pixels will be set to this value (unless = 0)
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the extracted subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the extracted subsection
C       array   c*1  output array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        character*1 array(*),nulval
        logical anyflg,ltemp
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1
        anyflg=.false.

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftgpvb(iunit,group,pstart,i1,nulval,
     &              array(astart),ltemp,status)
                if (ltemp) anyflg=.true.
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftgssi(iunit,group,nulval,naxis,naxes,fpixel,lpixel,
     &                    array,anyflg,status)

C       Read a subsection of Integer*2 values from the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be read, if any
C       nulval  i*2  undefined pixels will be set to this value (unless = 0)
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the extracted subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the extracted subsection
C       array   i*2  output array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        integer*2 array(*),nulval
        logical anyflg,ltemp
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1
        anyflg=.false.

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftgpvi(iunit,group,pstart,i1,nulval,
     &              array(astart),ltemp,status)
                if (ltemp) anyflg=.true.
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftgssj(iunit,group,nulval,naxis,naxes,fpixel,lpixel,
     &                    array,anyflg,status)

C       Read a subsection of integer values from the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be read, if any
C       nulval  i  undefined pixels will be set to this value (unless = 0)
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the extracted subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the extracted subsection
C       array   i  output array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        integer array(*),nulval
        logical anyflg,ltemp
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1
        anyflg=.false.

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftgpvj(iunit,group,pstart,i1,nulval,
     &              array(astart),ltemp,status)
                if (ltemp) anyflg=.true.
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftgsse(iunit,group,nulval,naxis,naxes,fpixel,lpixel,
     &                    array,anyflg,status)

C       Read a subsection of real values from the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be read, if any
C       nulval  r  undefined pixels will be set to this value (unless = 0)
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the extracted subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the extracted subsection
C       array   r  output array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        real array(*),nulval
        logical anyflg,ltemp
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1
        anyflg=.false.

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftgpve(iunit,group,pstart,i1,nulval,
     &              array(astart),ltemp,status)
                if (ltemp) anyflg=.true.
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftgssd(iunit,group,nulval,naxis,naxes,fpixel,lpixel,
     &                    array,anyflg,status)

C       Read a subsection of double precision values from the primary array.
C       A subsection is defined to be any contiguous rectangular
C       array of pixels within the n-dimensional FITS data file.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       iunit   i  Fortran input unit number
C       group   i  number of the data group to be read, if any
C       nulval  d  undefined pixels will be set to this value (unless = 0)
C       naxis   i  number of data axes in the FITS array
C       naxes   i  (array) size of each FITS axis
C       fpixel  i  (array) the first pixel in each dimension to be included
C                  in the extracted subsection (first pixel = 1)
C       lpixel  i  (array) the last pixel in each dimension to be included
C                  in the extracted subsection
C       array   d  output array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        integer iunit,group,naxis,naxes(*),fpixel(*),lpixel(*),status
        double precision array(*),nulval
        logical anyflg,ltemp
        integer fpix(7),irange(7),dimen(7),astart,pstart
        integer off2,off3,off4,off5,off6,off7
        integer st10,st20,st30,st40,st50,st60,st70
        integer st1,st2,st3,st4,st5,st6,st7
        integer i,i1,i2,i3,i4,i5,i6,i7

        if (status .gt. 0)return

        if (naxis .gt. 7)then
C               this routine only supports up to 7 dimensions
                status=212
                return
        end if

C       calculate the sizes and number of loops to perform in each dimension
        do 10 i=1,7
             fpix(i)=1
             irange(i)=1
             dimen(i)=1
10      continue

        do 20 i=1,naxis
             fpix(i)=fpixel(i)
             irange(i)=lpixel(i)-fpixel(i)+1
             dimen(i)=naxes(i) 
20      continue
        i1=irange(1)

C       compute the pixel offset between each dimension
        off2=     dimen(1)
        off3=off2*dimen(2)
        off4=off3*dimen(3)
        off5=off4*dimen(4)
        off6=off5*dimen(5)
        off7=off6*dimen(6)

        st10=fpix(1)
        st20=(fpix(2)-1)*off2
        st30=(fpix(3)-1)*off3
        st40=(fpix(4)-1)*off4
        st50=(fpix(5)-1)*off5
        st60=(fpix(6)-1)*off6
        st70=(fpix(7)-1)*off7

C       store the initial offset in each dimension
        st1=st10
        st2=st20
        st3=st30
        st4=st40
        st5=st50
        st6=st60
        st7=st70

        astart=1
        anyflg=.false.

        do 170 i7=1,irange(7)
        do 160 i6=1,irange(6)
        do 150 i5=1,irange(5)
        do 140 i4=1,irange(4)
        do 130 i3=1,irange(3)
        pstart=st1+st2+st3+st4+st5+st6+st7
        do 120 i2=1,irange(2)
                call ftgpvd(iunit,group,pstart,i1,nulval,
     &              array(astart),ltemp,status)
                if (ltemp) anyflg=.true.
                astart=astart+i1
                pstart=pstart+off2
120     continue
        st2=st20
        st3=st3+off3        
130     continue
        st3=st30
        st4=st4+off4
140     continue
        st4=st40
        st5=st5+off5
150     continue
        st5=st50
        st6=st6+off6
160     continue
        st6=st60
        st7=st7+off7
170     continue
        end        
C--------------------------------------------------------------------------
        subroutine ftnulc(input,n,chktyp,setval,flgray,anynul)

C       check input complex array for nulls
C       if chktyp=1 then set the undefined pixel = SETVAL
C       if chktyp=2 then set the corresponding FLGRAY = .true.

C       input   r  input array of values
C       n       i  number of values 
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  r  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array

        real input(*),setval(2)
        integer n,i,chktyp,n2
        logical flgray(*),anynul
        logical fttrnn
        external fttrnn
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n/2
                        flgray(i)=.false.
5               continue
        end if

        do 10 i=1,n
                if (fttrnn(input(i)))then
                        anynul=.true.
                        if (chktyp .eq. 1)then
                                n2=(i-1)/2*2+1
C                               set both parts of the complex number to the
C                               specified special value
                                input(n2)=setval(1)
                                input(n2+1)=setval(2)
                        else
C                               set the corresponding flag value to true
                                n2=(i-1)/2+1
                                flgray(n2)=.true.
                        end if
                end if
10      continue
        end
C--------------------------------------------------------------------------
        subroutine ftnulm(input,n,chktyp,setval,flgray,anynul)

C       check input double complex array for nulls
C       if chktyp=1 then set the undefined pixel = SETVAL
C       if chktyp=2 then set the corresponding FLGRAY = .true.

C       input   d  input array of values
C       n       i  number of values 
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  d  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array

        double precision input(*),setval(2)
        integer n,i,chktyp,n2
        logical flgray(*),anynul
        logical fttdnn
        external fttdnn
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n/2
                        flgray(i)=.false.
5               continue
        end if

        do 10 i=1,n
                if (fttdnn(input(i)))then
                        anynul=.true.
                        if (chktyp .eq. 1)then
                                n2=(i-1)/2*2+1
C                               set both parts of the complex number to the
C                               specified special value
                                input(n2)=setval(1)
                                input(n2+1)=setval(2)
                        else
C                               set the corresponding flag value to true
                                n2=(i-1)/2+1
                                flgray(n2)=.true.
                        end if
                end if
10      continue
        end
C----------------------------------------------------------------------
        subroutine ftsrnn(value)

C       set a 32-bit pattern equal to an IEEE Not-a-Number value
C       A NaN has all the exponent bits=1, and the fractional part
C       not=0.  
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer value

C       there are many NaN values;  choose a simple one in which all bits=1
        value=-1
        end
C----------------------------------------------------------------------
        subroutine ftsdnn(value)

C       set a 64-bit pattern equal to an IEEE Not-a-Number value
C       A NaN has all the exponent bits=1, and the fractional part 
C       not=0.  
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        integer value(2)

C       there are many NaN values;  choose a simple one in which all bits=1
        value(1)=-1
        value(2)=-1
        end
C----------------------------------------------------------------------
        subroutine ftpi1b(ounit,nvals,incre,chbuff,status)

C       Write an array of Integer*1 bytes to the output FITS file.

        integer nvals,incre,ounit,status,i,offset
        character*1 chbuff(nvals)

C       ounit   i  fortran unit number
C       nvals   i  number of pixels in the i2vals array
C       incre   i  byte increment between values
C       chbuff  c*1 array of input byte values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftpcbf(ounit,0,nvals,chbuff,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-1
                do 10 i=1,nvals
                        call ftpcbf(ounit,0,1,chbuff(i),status)
                        call ftmoff(ounit,offset,.true.,status)
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine ftgi1b(ounit,nvals,incre,chbuff,status)

C       Read an array of Integer*1 bytes from the input FITS file.

        integer nvals,incre,ounit,status,i,offset
        character*1 chbuff(nvals)

C       ounit   i  fortran unit number
C       nvals   i  number of pixels in the i2vals array
C       incre   i  byte increment between values
C       chbuff  c*1 array of input byte values
C       status  i  output error status

        if (incre .eq. 0)then
                call ftgcbf(ounit,0,nvals,chbuff,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-1
                do 10 i=1,nvals
                        call ftgcbf(ounit,0,1,chbuff(i),status)
                        call ftmoff(ounit,offset,.false.,status)
10              continue
        end if
        end
C----------------------------------------------------------------------
        subroutine fti1i1(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*1 values to output i*1 values, doing optional
C       scaling and checking for null values

C       input   c*1 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  c*1 value in the input array that is used to indicated nulls
C       setval  c*1 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  c*1 returned array of values

        character*1 input(*),chkval
        character*1 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                    output(i)=char(nint((ichar(input(i))-zero)/scale))
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                            do 40 i=1,n
                       output(i)=char(int(ichar(input(i))*scale+zero))
40                          continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                            output(i)=input(i)
                                        end if
50                              continue
                        else
                          do 60 i=1,n
                            if (input(i) .eq. chkval)then
                                    anynul=.true.
                                    if (chktyp .eq. 1)then
                                        output(i)=setval
                                    else
                                        flgray(i)=.true.
                                    end if
                            else
                      output(i)=char(int(ichar(input(i))*scale+zero))
                            end if
60                        continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti1i2(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*1 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   c*1 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  c*1 value in the input array that is used to indicated nulls
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values

        character*1 input(*),chkval
        integer*2 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=ichar(input(i))
10                      continue
                else
                        do 20 i=1,n
                          output(i)=nint((ichar(input(i))-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=ichar(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=ichar(input(i))*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                             output(i)=ichar(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                 output(i)=ichar(input(i))*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti1i4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*1 values to output i*4 values, doing optional
C       scaling and checking for null values

C       input   c*1 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  c*1 value in the input array that is used to indicated nulls
C       setval  i   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i   returned array of values

        character*1 input(*),chkval
        integer output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=ichar(input(i))
10                      continue
                else
                        do 20 i=1,n
                          output(i)=nint((ichar(input(i))-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=ichar(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=ichar(input(i))*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                          output(i)=ichar(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                 output(i)=ichar(input(i))*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti1r4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*1 values to output r*4 values, doing optional
C       scaling and checking for null values

C       input   c*1 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  c*1 value in the input array that is used to indicated nulls
C       setval  r   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  r   returned array of values

        character*1 input(*),chkval
        real output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=ichar(input(i))
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(ichar(input(i))-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=ichar(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=ichar(input(i))*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                            output(i)=ichar(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                  output(i)=ichar(input(i))*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti1r8(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*1 values to output r*8 values, doing optional
C       scaling and checking for null values

C       input   c*1 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  c*1 value in the input array that is used to indicated nulls
C       setval  d   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  d   returned array of values

        character*1 input(*),chkval
        double precision output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=ichar(input(i))
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(ichar(input(i))-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=ichar(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=ichar(input(i))*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                          output(i)=ichar(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                  output(i)=ichar(input(i))*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti2i1(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*2 values to output i*1 values, doing optional
C       scaling and checking for null values

C       input   i*2 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i*2 value in the input array that is used to indicated nulls
C       setval  c*1 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  c*1 returned array of values

        integer*2 input(*),chkval
        character*1 output(*),setval
        integer n,i,chktyp,itemp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
C       have to use a temporary variable because of IBM mainframe
                                itemp=input(i)
                                output(i)=char(itemp)
10                      continue
                else
                        do 20 i=1,n
                            output(i)=char(nint((input(i)-zero)/scale))
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 2)then
C                       initialize the null flag values
                        do 5 i=1,n
                                flgray(i)=.false.
5                       continue
                end if

                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
C       have to use a temporary variable because of IBM mainframe
                                        itemp=input(i)
                                        output(i)=char(itemp)
30                              continue
                        else
                            do 40 i=1,n
                              output(i)=char(int(input(i)*scale+zero))
40                          continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
C       have to use a temporary variable because of IBM mainframe
                                                itemp=input(i)
                                            output(i)=char(itemp)
                                        end if
50                              continue
                        else
                          do 60 i=1,n
                            if (input(i) .eq. chkval)then
                                    anynul=.true.
                                    if (chktyp .eq. 1)then
                                        output(i)=setval
                                    else
                                        flgray(i)=.true.
                                    end if
                            else
                              output(i)=char(int(input(i)*scale+zero))
                            end if
60                        continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti2i2(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*2 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   i*2 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i*2 value in the input array that is used to indicated nulls
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values

        integer*2 input(*),output(*),chkval,setval,j
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if

        if (tofits)then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
C                       Have to use internal variable j to work around
C                       a bug in the Microsoft v5.0 compiler on IBM PCs
                                j=input(i)
                                output(i)=j
10                      continue
                else
                        do 20 i=1,n
                                output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
C                               Have to use internal variable j to work around
C                               a bug in the Microsoft v5.0 compiler on IBM PCs
                                        j=input(i)
                                        output(i)=j
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (chktyp .eq. 2)then
C                               initialize the null flag values
                                do 5 i=1,n
                                        flgray(i)=.false.
5                               continue
                        end if
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
C                               Have to use internal variable j to work around
C                               a bug in the Microsoft v5.0 compiler on IBM PCs
                                                j=input(i)
                                                output(i)=j
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti2i4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*2 values to output i*4 values, doing optional
C       scaling and checking for null values

C       input   i*2 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i*2 value in the input array that is used to indicated nulls
C       setval  i   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i   returned array of values

        integer*2 input(*),chkval
        integer output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti2r4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*2 values to output r*4 values, doing optional
C       scaling and checking for null values

C       input   i*2 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i*2 value in the input array that is used to indicated nulls
C       setval  r   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  r   returned array of values

        integer*2 input(*),chkval
        real output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti2r8(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*2 values to output r*8 values, doing optional
C       scaling and checking for null values

C       input   i*2 input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i*2 value in the input array that is used to indicated nulls
C       setval  d   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  d   returned array of values

        integer*2 input(*),chkval
        double precision output(*),setval
        integer n,i,chktyp
        double precision scale,zero
       logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti4i1(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*4 values to output i*1 values, doing optional
C       scaling and checking for null values

C       input   i input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i value in the input array that is used to indicated nulls
C       setval  c*1 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  c*1 returned array of values

        integer input(*),chkval
        character*1 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=char(input(i))
10                      continue
                else
                        do 20 i=1,n
                          output(i)=char(nint((input(i)-zero)/scale))
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=char(input(i))
30                              continue
                        else
                            do 40 i=1,n
                              output(i)=char(int(input(i)*scale+zero))
40                          continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                            output(i)=char(input(i))
                                        end if
50                              continue
                        else
                            do 60 i=1,n
                                if (input(i) .eq. chkval)then
                                    anynul=.true.
                                    if (chktyp .eq. 1)then
                                        output(i)=setval
                                    else
                                        flgray(i)=.true.
                                    end if
                                else
                              output(i)=char(int(input(i)*scale+zero))
                                end if
60                          continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti4i2(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*4 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   i  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i  value in the input array that is used to indicated nulls
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values

        integer input(*),chkval
        integer*2 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                           output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti4i4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*4 values to output i*4 values, doing optional
C       scaling and checking for null values

C       input   i  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i   value in the input array that is used to indicated nulls
C       setval  i   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i   returned array of values

        integer input(*),chkval
        integer output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti4r4(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*4 values to output r*4 values, doing optional
C       scaling and checking for null values

C       input   i  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i  value in the input array that is used to indicated nulls
C       setval  r  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  r  returned array of values

        integer input(*),chkval
        real output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti4r8(input,n,scale,zero,tofits,
     &          chktyp,chkval,setval,flgray,anynul,output)

C       copy input i*4 values to output r*8 values, doing optional
C       scaling and checking for null values

C       input   i  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       chkval  i  value in the input array that is used to indicated nulls
C       setval  d  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  d  returned array of values

        integer input(*),chkval
        double precision output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (input(i) .eq. chkval)then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr4i1(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*4 values to output i*1 values, doing optional
C       scaling and checking for null values

C       input   r input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  c*1 value to set  array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  c*1 returned array of values

        real input(*)
        character*1 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttrnn
        external fttrnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=char(nint(input(i)))
10                      continue
                else
                        do 20 i=1,n
                            output(i)=char(nint((input(i)-zero)/scale))
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=char(nint(input(i)))
30                              continue
                        else
                            do 40 i=1,n
                              output(i)=char(nint(input(i)*scale+zero))
40                          continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=char(nint(input(i)))
                                        end if
50                              continue
                        else
                            do 60 i=1,n
                              if (fttrnn(input(i)))then
                                    anynul=.true.
                                    if (chktyp .eq. 1)then
                                        output(i)=setval
                                    else
                                        flgray(i)=.true.
                                    end if
                              else
                               output(i)=char(nint(input(i)*scale+zero))
                              end if
60                          continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr4i2(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*4 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   r  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values

        real input(*)
        integer*2 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttrnn
        external fttrnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=nint(input(i))
10                      continue
                else
                        do 20 i=1,n
                                output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=nint(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                   output(i)=nint(input(i)*scale+zero)
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                           output(i)=nint(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                  output(i)=nint(input(i)*scale+zero)
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr4i4(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*4 values to output i*4 values, doing optional
C       scaling and checking for null values

C       input   r  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  i   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i   returned array of values

        real input(*)
        integer output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttrnn
        external fttrnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=nint(input(i))
10                      continue
                else
                        do 20 i=1,n
                                output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=nint(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=nint(input(i)*scale+zero)
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                            output(i)=nint(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                   output(i)=nint(input(i)*scale+zero)
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr4r4(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*4 values to output r*4 values, doing optional
C       scaling and checking for null values

C       input   r  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  r  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  r  returned array of values

        real input(*)
        real output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttrnn
        external fttrnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr4r8(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*4 values to output r*8 values, doing optional
C       scaling and checking for null values

C       input   r  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  d  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  d  returned array of values

        real input(*)
        double precision output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttrnn
        external fttrnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttrnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr8i1(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*8 values to output i*1 values, doing optional
C       scaling and checking for null values

C       input   d input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  c*1 value to set  array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  c*1 returned array of values

        double precision input(*)
        character*1 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttdnn
        external fttdnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=char(nint(input(i)))
10                      continue
                else
                        do 20 i=1,n
                          output(i)=char(nint((input(i)-zero)/scale))
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=char(nint(input(i)))
30                              continue
                        else
                            do 40 i=1,n
                              output(i)=char(nint(input(i)*scale+zero))
40                          continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=char(nint(input(i)))
                                        end if
50                              continue
                        else
                            do 60 i=1,n
                              if (fttdnn(input(i)))then
                                    anynul=.true.
                                    if (chktyp .eq. 1)then
                                        output(i)=setval
                                    else
                                        flgray(i)=.true.
                                    end if
                              else
                              output(i)=char(nint(input(i)*scale+zero))
                              end if
60                          continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr8i2(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*8 values to output i*2 values, doing optional
C       scaling and checking for null values

C       input   d  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  i*2 value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i*2 returned array of values

        double precision input(*)
        integer*2 output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttdnn
        external fttdnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=nint(input(i))
10                      continue
                else
                        do 20 i=1,n
                              output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=nint(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=nint(input(i)*scale+zero)
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                          output(i)=nint(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                  output(i)=nint(input(i)*scale+zero)
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr8i4(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*8 values to output i*4 values, doing optional
C       scaling and checking for null values

C       input   d  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  i   value to set output array to if value is undefined
C       flgray  l   array of logicals indicating if corresponding value is null
C       anynul  l   set to true if any nulls were set in the output array
C       output  i   returned array of values

        double precision input(*)
        integer output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttdnn
        external fttdnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=nint(input(i))
10                      continue
                else
                        do 20 i=1,n
                                output(i)=nint((input(i)-zero)/scale)
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=nint(input(i))
30                              continue
                        else
                                do 40 i=1,n
                                  output(i)=nint(input(i)*scale+zero)
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                          output(i)=nint(input(i))
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                   output(i)=nint(input(i)*scale+zero)
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr8r4(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*8 values to output r*4 values, doing optional
C       scaling and checking for null values

C       input   d  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  r  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  r  returned array of values

        double precision input(*)
        real output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttdnn
        external fttdnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftr8r8(input,n,scale,zero,tofits,
     &          chktyp,setval,flgray,anynul,output)

C       copy input r*8 values to output r*8 values, doing optional
C       scaling and checking for null values

C       input   d  input array of values
C       n       i  number of values 
C       scale   d  scaling factor to be applied
C       zero    d  scaling zero point to be applied
C       tofits  l  true if converting from internal format to FITS
C       chktyp  i  type of null value checking to be done if TOFITS=.false.
C                       =0  no checking for null values
C                       =1  set null values = SETVAL
C                       =2  set corresponding FLGRAY value = .true.
C       setval  d  value to set output array to if value is undefined
C       flgray  l  array of logicals indicating if corresponding value is null
C       anynul  l  set to true if any nulls were set in the output array
C       output  d  returned array of values

        double precision input(*)
        double precision output(*),setval
        integer n,i,chktyp
        double precision scale,zero
        logical tofits,flgray(*),anynul,noscal
        logical fttdnn
        external fttdnn

        if (scale .eq. 1. .and. zero .eq. 0)then
                noscal=.true.
        else
                noscal=.false.
        end if
        
        if (chktyp .eq. 2)then
C               initialize the null flag values
                do 5 i=1,n
                        flgray(i)=.false.
5               continue
        end if

        if (tofits) then
C               we don't have to worry about null values when writing to FITS
                if (noscal)then
                        do 10 i=1,n
                                output(i)=input(i)
10                      continue
                else
                        do 20 i=1,n
                                output(i)=(input(i)-zero)/scale
20                      continue
                end if
        else
C               converting from FITS to internal format; may have to check nulls
                if (chktyp .eq. 0)then
C                       don't have to check for nulls
                        if (noscal)then
                                do 30 i=1,n
                                        output(i)=input(i)
30                              continue
                        else
                                do 40 i=1,n
                                        output(i)=input(i)*scale+zero
40                              continue
                        end if
                else 
C                       must test for null values
                        if (noscal)then
                                do 50 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                                output(i)=input(i)
                                        end if
50                              continue
                        else
                                do 60 i=1,n
                                        if (fttdnn(input(i)))then
                                            anynul=.true.
                                            if (chktyp .eq. 1)then
                                                output(i)=setval
                                            else
                                                flgray(i)=.true.
                                            end if
                                        else
                                         output(i)=input(i)*scale+zero
                                        end if
60                              continue
                        end if
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine fti2c(ival,cval,status)
C       convert an integer value to a C*20 character string, right justified
        integer ival,status
        character*20 cval

        if (status .gt. 0)return

        write(cval,1000,err=900)ival
1000    format(i20)
        if (cval(1:1) .eq. '*')go to 900
        return
900     status=401
        end
C----------------------------------------------------------------------
        subroutine ftl2c(lval,cval,status)
C       convert a logical value to a C*20 right justified character string 
        integer status
        logical lval
        character*20 cval

        if (status .gt. 0)return

        if (lval)then
                cval='                   T'
        else
                cval='                   F'
        end if
        end
C----------------------------------------------------------------------
        subroutine fts2c(in,cval,lenval,status)
C       convert an input string to a left justified quoted string
C               The minimum length FITS string is 8 characters, so
C               pad the quoted string with spaces if necessary.
C       cval = returned quoted string
C       lenval = length of the cval string, including the 2 quote characters
        character*(*) in,cval
        integer length,i,j,i1,i2,lenval,status

        if (status .gt. 0)return

        i1=1
        i2=1
C       test for blank input string
        if (in .eq. ' ')then
                cval='''        '''
                lenval=10
                return
        end if

        length=len(in)
C       find first and last non-blank characters
        do 5 i=1,length
                i1=i
                if (in(i:i) .ne. ' ')go to 10
5       continue
10      continue
        do 15 i=length,1,-1
                i2=i
                if (in(i:i) .ne. ' ')go to 20
15      continue
20      continue

        cval=''''//in(i1:i2)

C       test if there are any single quotes in the string;  if so, replace
C       them with two successive single quotes
        lenval=i2-i1+2
        do 30 i=lenval,2,-1
                if (cval(i:i) .eq. '''')then
C                  shift all the characters over 1 space
                   do 40 j=len(cval),i+1,-1
                      cval(j:j)=cval(j-1:j-1)
40                 continue
                   i2=i2+1
                end if
30      continue

C       find location of closing quote
        lenval=max(10,i2-i1+3)  
        lenval=min(lenval,len(cval))
        cval(lenval:lenval)=''''
        end
C----------------------------------------------------------------------
        subroutine ftr2f(val,dec,cval,status)

C       convert real value to F20.* format character string
C       val     r  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       status  i  output error status (0 = OK)

        real val
        integer dec,status
        character*20 cval,form*8

        if (status .gt. 0)return

        if (dec .ge. 0 .and. dec .le. 9)then
                write(form,2000)dec
2000            format('(f20.',i1,')')
        else if (dec .ge. 10 .and. dec .lt.18)then
                write(form,2001)dec
2001            format('(f20.',i2,')')
        else
                status=411
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return
900     status=402
        end
C----------------------------------------------------------------------
        subroutine ftr2e(val,dec,cval,status)

C       convert real value to E20.* format character string
C       val     r  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       status  i  output error status (0 = OK)

        real val
        integer dec,status
        character*20 cval,form*10

        if (status .gt. 0)return

        if (dec .ge. 1 .and. dec .le. 9)then
                write(form,2000)dec
2000            format('(1pe20.',i1,')')
        else if (dec .ge. 10 .and. dec .le. 13)then
                write(form,2001)dec
2001            format('(1pe20.',i2,')')
        else
C               illegal number of decimal places were specified
                status=411
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return

900     status=402
        end
C----------------------------------------------------------------------
        subroutine ftd2f(val,dec,cval,status)

C       convert double precision value to F20.* format character string
C       NOTE: some precision may be lost
C       val     d  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       status  i  output error status (0 = OK)

        double precision val
        integer dec,status
        character*20 cval,form*8

        if (status .gt. 0)return

        if (dec .ge. 0 .and. dec .le. 9)then
                write(form,2000)dec
2000            format('(f20.',i1,')')
        else if (dec .ge. 10 .and. dec .lt.18)then
                write(form,2001)dec
2001            format('(f20.',i2,')')
        else
C               illegal number of decimal places were specified
                status=411
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return
900     status=402
        end
C----------------------------------------------------------------------
        subroutine ftd2e(val,dec,cval,vlen,status)

C       convert a double precision value to an E format character string
C       If it will fit, the value field will be 20 characters wide;
C       otherwise it will be expanded to up to 35 characters, left
C       justified.
C
C       val     d  input value to be converted 
C       dec     i  number of decimal places to display in output string
C       cval    c  output character string
C       vlen    i  length of output string
C       status  i  output error status (0 = OK)

        double precision val
        integer dec,vlen,status
        character*35 cval,form*10

        if (status .gt. 0)return

        if (dec .ge. 1 .and. dec .le. 9)then
                vlen=20
                write(form,2000)dec
2000            format('(1pe20.',i1,')')
        else if (dec .ge. 10 .and. dec .le. 28)then
                vlen=max(20,dec+7)
                write(form,2001)vlen,dec
2001            format('(1pe',i2,'.',i2,')')
        else
C               illegal number of decimal places were specified
                status=411
                return
        endif

        write(cval,form,err=900)val
        if (cval(1:1) .eq. '*')go to 900
        return

900     status=402
        end
C----------------------------------------------------------------------
        subroutine ftc2i(cval,ival,status)
C       convert a character string to an integer
C       perform datatype conversion, if required

        integer ival,status
        character*(*) cval
        character*1 dtype
        logical lval
        character*8 sval
        double precision dval

C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .eq. 'I')then
C               no datatype conversion required, so just return
        else if (dtype .eq. 'F')then
C               need to convert from floating point to integer
                ival=dval
        else if (dtype .eq. 'L')then
C               need to convert from logical to integer
                if (lval)then
                        ival=1
                else
                        ival=0
                end if
        else if (dtype .eq. 'C')then
C               can't convert a string to an integer, so return error
                ival=0
                status=403
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2l(cval,lval,status)

C       convert a character string to a logical value
C       perform datatype conversion, if required

        logical lval
        integer ival,status
        character*(*) cval
        character*1 dtype
        character*8 sval
        double precision dval


C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .ne. 'L')then
C              this is not a logical keyword, so return error
                status=404
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2r(cval,rval,status)
C       convert a character string to a real value
C       perform datatype conversion, if required

        character*(*) cval
        real rval
        integer ival,status
        character*1 dtype
        logical lval
        character*8 sval
        double precision dval


C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .eq. 'F')then
C               convert from double to single precision
                rval=dval
        else if (dtype .eq. 'I')then
C               convert from integer to real
                rval=ival
        else if (dtype .eq. 'L')then
C               need to convert from logical to real
                if (lval)then
                        rval=1.
                else
                        rval=0.
                end if
        else if (dtype .eq. 'C')then
C               can't convert a string to a real, so return error
                rval=0
                status=405
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2d(cval,dval,status)
C       convert a character string to a double precision value
C       perform datatype conversion, if required

        character*(*) cval
        integer ival,status
        character*1 dtype
        logical lval
        character*8 sval
        double precision dval


C       convert string to its intrinsic data type
        call ftc2x(cval,dtype,ival,lval,sval,dval,status)
        if (status .gt. 0)return

        if (dtype .eq. 'F')then
C               no datatype conversion required, so just return
        else if (dtype .eq. 'I')then
C               convert from integer to double precision
                dval=ival
        else if (dtype .eq. 'L')then
C               need to convert from logical to double precision
                if (lval)then
                        dval=1.
                else
                        dval=0.
                end if
        else if (dtype .eq. 'C')then
C               can't convert a string to double precision, so return error
                dval=0
                status=406
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2s(in,cval,status)
C       convert an input quoted string to an unquoted string
C
C       The first character of the input string must be a quote character (')
C       and at least one additional quote character must also be present in the 
C       input string. This routine then simply outputs all the characters 
C       between the first and last quote characters in the input string.
C
C       in      c  input quoted string
C       cval    c  output unquoted string
C       status  i  output error status (0=ok, 1=first quote missing,
C                  2=second quote character missing.

        character*(*) in,cval
        integer length,i,j,i2,status
        character*1 dtype

C       test for datatype
        call ftdtyp(in,dtype,status)
        if (status .ne. 0)return
        if (dtype .ne. 'C')then
C               do no conversion and just return the raw character string
                cval=in
        else
C               convert character string to unquoted string

C               find closing quote character
                length=len(in)
                i2=length-1
                do 10 i=length,2,-1
                        if (in(i:i) .eq. '''')go to 20
                        i2=i2-1
10              continue
20              continue

                if (i2 .eq. 0)then
C                       there was no closing quote character
                        status=205
                else if (i2 .eq. 1)then
C                       null string
                        cval=' '
                else
                        cval=in(2:i2)

C                       test for double single quote characters; if found,
C                       then  delete one of the quotes (FITS uses 2 single
C                       quote characters to represent a single quote)
                        i2=i2-2
                        do 30  i=1,i2
                            if (cval(i:i) .eq. '''')then
                                if (cval(i+1:i+1) .eq. '''')then
                                   do 40 j=i+1,i2
                                         cval(j:j)=cval(j+1:j+1)
40                                 continue
                                   cval(i2:i2)=' '
                                end if
                            end if
30                      continue
                end if
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2x(cval,dtype,ival,lval,sval,dval,status)

C       convert a character string into it intrinsic data type

C       cval  c  input character string to be converted
C       dtype c  returned intrinsic datatype of the string (I,L,C,F)
C
C       one of  the following values is returned, corresponding to the
C       value of dtype:
C               ival i integer value
C               lval l logical value
C               sval c string value
C               dval d double precision value
C       statue i returned error status

        character*(*) cval
        character*1 dtype
        integer ival,status
        logical lval
        character*(*) sval
        double precision dval

C       determine intrinsic datatype
        call ftdtyp(cval,dtype,status)

C       convert string into its intrinsic datatype
        if (dtype .eq. 'I')then
                call ftc2ii(cval,ival,status)
        else if (dtype .eq. 'F')then
                call ftc2dd(cval,dval,status)
        else if (dtype .eq. 'L')then
                call ftc2ll(cval,lval,status)
        else if (dtype .eq. 'C')then
                call ftc2s(cval,sval,status)
        end if
        end
C----------------------------------------------------------------------
        subroutine ftdtyp(value,dtype,status)

C       determine datatype of a FITS value field
C       This assumes value field conforms to FITS standards and may not
C          detect all invalid formats.
C       value   c  input value field from FITS header record only,
C                  (usually the value field is in columns 11-30 of record)
C                  The value string is left justified.
C       dtype   c  output type (C,L,I,F) for Character string, Logical,
C                    Integer, Floating point, respectively
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        character*(*)value,dtype
        integer status

        if (status .gt. 0)return

        dtype=' '

        if (value(1:1) .eq. '''')then
C               character string
                dtype='C'
        else if (value(1:1).eq.'T' .or. value(1:1).eq.'F')then
C               logical
                dtype='L'
        else if (index(value,'.') .gt. 0)then
C               floating point
                dtype='F'
        else
C               assume it must be an integer, since it isn't anything else
                dtype='I'
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2ii(cval,ival,status)
C       convert a character string to an integer
C       (assumes that the input string is left justified)

        integer ival,status,nleng
        character*(*) cval
        character*8 iform

        if (status .gt. 0)return

C       find length of the input integer character string
        nleng=index(cval,' ')-1
        if (nleng .eq. -1)nleng=len(cval)

C       construct the format statement to read the character string
        if (nleng .le. 9)then
                write(iform,1000)nleng
1000            format('(I',I1,')')
        else
                write(iform,1001)nleng
1001            format('(I',I2,')')
        end if

        read(cval,iform,err=900)ival
        return

900     status=407
        end
C----------------------------------------------------------------------
        subroutine ftc2ll(cval,lval,status)
C       convert a character string to a logical value 
        integer status
        logical lval
        character*(*) cval

        if (status .gt. 0)return

C       convert character string to logical
        if (cval(1:1) .eq.'T')then
                lval=.true.
        else
C               any other character is considered false
                lval=.false.
        end if
        end
C----------------------------------------------------------------------
        subroutine ftc2rr(cval,val,status)

C       convert a character string to a real value
C       cval    c  input character string to be converted 
C       val     r  output value 
C       status  i  output error status (0 = OK)

        character*(*) cval
        real val
        integer status,nleng
        character*8 iform

        if (status .gt. 0)return

C       find length of the input integer character string
        nleng=index(cval,' ')-1
        if (nleng .eq. -1)nleng=len(cval)

C       construct the format statement to read the character string
        if (nleng .le. 9)then
                write(iform,1000)nleng
1000            format('(F',I1,'.0)')
        else
                write(iform,1001)nleng
1001            format('(F',I2,'.0)')
        end if

        read(cval,iform,err=900)val
        return

900     status=408
        end
C----------------------------------------------------------------------
        subroutine ftc2dd(cval,val,status)

C       convert a character string to double prec.
C       cval    c  input character string to be converted 
C       val     d  output value 
C       status  i  output error status (0 = OK)

        character*(*) cval
        double precision val
        integer status,nleng
        character*8 iform

        if (status .gt. 0)return

C       find length of the input integer character string
        nleng=index(cval,' ')-1
        if (nleng .eq. -1)nleng=len(cval)

C       construct the format statement to read the character string
        if (nleng .le. 9)then
                write(iform,1000)nleng
1000            format('(F',I1,'.0)')
        else
                write(iform,1001)nleng
1001            format('(F',I2,'.0)')
        end if

        read(cval,iform,err=900)val
        return

900     status=409
        end
