c reads 2-D FITS file and writes into the data arrays either a
c slice or a sum across the image
	subroutine fitred(pfnam, title, xlab, ylab, x, y, delx, dely,
     +msiz, npts, xfit, yfit, xfite, yfite, nptsf, ifail, istar)
	include	'robcom'
      CHARACTER*(*) TITLE,XLAB,YLAB,PFNAM
	integer gval, readfits
	double precision getpix
      REAL X(*),Y(*),DELY(*),DELX(*)
      REAL XFIT(*),YFIT(*),XFITE(*),YFITE(*)

      IFAIL=0


c reset values
	ifail = 0
	npts = 0
	do 3 i = 1, msiz
		x(i) = 0.0
		y(i) = 0.0
		xfit(i) = 0.0
		yfit(i) = 0.0
		delx(i) = 0.0
		dely(i) = 0.0
		xfite(i) = 0.0
		yfite(i) = 0.0
3	continue
		
	ainbuf = title


	call sprompt('Give FITS file name')
	write(tstring, *)'Default = ',ainbuf(1:nnl(ainbuf))
	call sprompt(tstring)
	call getit(pfnam,  1)
	if(pfnam.eq.' ')pfnam = ainbuf
c Keep C happy with a NULL at the end
		do 30 i2 = 1, len(pfnam)
			if(pfnam(i2:i2) .eq. ' ')then
				pfnam(i2:i2) = char(0)
				goto 31
			end if
30		continue
31		continue

	isize =  readfits(pfnam)
c	print*, 'isize = ', isize
	title = pfnam
	if(isize .le. 0) then
		ifail = 1
		return
	end if

	naxis1 = gval("NAXIS1")
	naxis2 = gval("NAXIS2")

	write(tstring, *)'Image size is ', naxis1, ' by ', naxis2
	call totext(tstring)

	if(naxis1.gt.msiz)then
		call xtext("NAXIS1 is too large for the array sizes")
		write(tstring, *)'Only up to ',msiz, ' allowed'
		call totext(tstring)
		ifail = 1
		return
	end if
5	continue
	call preset()
	call sprompt('Please select line (or line range) to bin on')
	call sprompt('(e.g. 3 or 1 400)')
	write(tstring, *)'default = 1 ', naxis2
	call sprompt(tstring)
	call getit(ainbuf,  1)
		call dcode(ainbuf, ain1, ifix, aingrd, k)
	if(k.gt.2.or.k.lt.0) then
		call xtext("Wrong number of arguments given")
		call xtext("Please try again")
		goto 5
	end if
	if(k.eq.0) then
		istart = 1
	else
		istart = ain1(1)
	end if

	if(k.eq.2) then
		iend = ain1(2)
	else if(k.eq.1) then
		iend = istart
	else if(k.eq.0) then
		iend = naxis2
	end if
	
	write(tstring, *)'Binning from ', istart,' to ',iend
	call totext(tstring)


	do 10 i = istart, iend
		do 20 j = 1, naxis1
c		do 20 j = 1, 25
		y(j) = y(j) + getpix(i, j)
20	continue
10	continue
		
	do 25 i = 1, naxis1
		x(i) = i
		yfit(i) = y(i)
		xfit(i) = x(i)
25	continue

	npts = naxis1

c labels
	write(xlab, *)"Rows ",istart,"-",iend



c free the image pointer
	call freef()

	end

c reads 2-D FITS file and writes into the data arrays either a
c slice or a sum across the image
c, kludge to get cross-section the other way
c combine this with fitred!!!
	subroutine fitred2(pfnam, title, xlab, ylab, x, y, delx, dely,
     +msiz, npts, xfit, yfit, xfite, yfite, nptsf, ifail, istar)
	include	'robcom'
      CHARACTER*(*) TITLE,XLAB,YLAB,PFNAM
	integer gval, readfits
	double precision getpix
      REAL X(*),Y(*),DELY(*),DELX(*)
      REAL XFIT(*),YFIT(*),XFITE(*),YFITE(*)

      IFAIL=0

c reset values
	ifail = 0
	npts = 0
	do 3 i = 1, msiz
		x(i) = 0.0
		y(i) = 0.0
		xfit(i) = 0.0
		yfit(i) = 0.0
		delx(i) = 0.0
		dely(i) = 0.0
		xfite(i) = 0.0
		yfite(i) = 0.0
3	continue
		
	ainbuf = title

	call sprompt("Please give FITS file name")
	write(tstring, *)'(Default = ',ainbuf,')'
	call sprompt(tstring)
	call getit(pfnam,  1)
	if(pfnam.eq.' ')pfnam = ainbuf
c Keep C happy with a NULL at the end
		do 30 i2 = 1, len(pfnam)
			if(pfnam(i2:i2) .eq. ' ')then
				pfnam(i2:i2) = char(0)
				goto 31
			end if
30		continue
31		continue

	isize =  readfits(pfnam)
	title = pfnam
	if(isize .le. 0) then
		ifail = 1
		return
	end if

	naxis1 = gval("NAXIS1")
	naxis2 = gval("NAXIS2")

	write(tstring, *)'Image size is ', naxis1, ' by ', naxis2
	call sprompt(tstring)

	if(naxis1.gt.msiz)then
		call xtext("NAXIS1 is too large for the array sizes")
		write(tstring, *)'Only up to ',msiz, ' allowed'
		call totext(tstring)
		ifail = 1
		return
	end if
5	call sprompt('Please select line (or line range) to bin on')
	call sprompt('(e.g. 3 or 1 400')
	write(tstring, *)'default = 1 ', naxis1
	call sprompt(tstring)
	call getit(ainbuf,  1)
		call dcode(ainbuf, ain1, ainfix, igrd, k)
	if(k.gt.2.or.k.lt.0) then
		call xtext("ERROR: WRONG NUMBER OF ARGUMENTS GIVEN")
		call xtext("Please try again")
		goto 5
	end if
	if(k.eq.0) then
		istart = 1
	else
		istart = ain1(1)
	end if

	if(k.eq.2) then
		iend = ain1(2)
	else if(k.eq.1) then
		iend = istart
	else if(k.eq.0) then
		iend = naxis1
	end if
	
	write(tstring, *)'Binning from ', istart,' to ',iend
	call totext(tstring)


	do 10 i = 1, naxis2
		do 20 j = istart, iend
			y(i) = y(i) + getpix(i, j)
20		continue
10	continue

	do 25 i = 1, naxis2
		x(i) = i
		yfit(i) = y(i)
		xfit(i) = x(i)
25	continue

	npts = naxis2

c free the image pointer
	call freef()

c labels
	write(xlab, *)"Columns ",istart,"-",iend
	end

