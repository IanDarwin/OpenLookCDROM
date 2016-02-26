c trial implementation of reading FITS files
c (Astro D "bright" format), kinda works for Rosat PSPC data
c for interface to Robot

c**********
c format is currently somewhat fixed - may fall over for anything else 


	subroutine fits3d(pfnam, title, xlab, ylab, x, y, delx, dely,
     +msiz, npts, xfit, yfit, xfite, yfite, nptsf, ifail, istar)

	include 'robcom'
	include 'adcom'

      character*(*) title,xlab,ylab,pfnam
      real x(*),y(*),dely(*),delx(*)
      real xfit(*),yfit(*),xfite(*),yfite(*)
	integer blocksize, status, hdutype, unit
	integer phacol, xcol, ycol, gcol
	integer pha9col, pha1col, pha2col, pha3col, pha4col
	integer pha5col, pha6col, pha7col, pha8col
c get anything which looks vaguely right as a column name!
	logical exact
	logical faint, nogrd



	integer grade
	integer xpos, ypos 
	integer pha, phas(9)
	character*60 comment
	character*8 keyword
	integer*2	itbuff(1), i2buff(12)
	logical anyf
	parameter(unit = 42)
	parameter(nullval = -99)

	data xpmin /0/
	data ypmin /0/
	data xpmax /10000/
	data ypmax /10000/
	data gmin, gmax /0, 3/
	data threshold /0/
	data exact/.FALSE./

	ifail = 0
	npts = 0
	status = 0
c initial assumption is that it's a bright mode file
c (i.e. a file with a single pha value)
	faint = .FALSE.
c and that it has grade information
	nogrd = .FALSE.


c open FITS file
	call ftopen(unit, adname, 0, blocksize, status)
	if(status.ne.0)goto 950
c find binary extension
5	call ftmrhd(unit, 1, hdutype, status)
	if(status.ne.0)goto 950
	if(hdutype.ne.2) goto 5
	
c see if the file is called 'EVENTS'
	call ftgkys(unit, 'EXTNAME ', keyword, comment, status)
	if(keyword.ne.'EVENTS') goto 5
c Get no. of events/rows in extension
	call ftgkyj(unit, 'NAXIS2  ', nrows, comment, status)
	if(status.ne.0)goto 950
	print*,'NAXIS2 =', nrows


c initialise arrays
	do 100 i = 1, msiz
		x(i) = i
		xfit(i) = x(i)
		y(i) = 0.0
		delx(i) = 0.0
		dely(i) = 0.0
		xfite(i) = 0.0
		yfite(i) = 0.0

100	continue
c initially assume only four fields - increase to 11 for faint mode
c all fields assumed integer
	nfield = 4
	phacol = -99
	pha1col = -99
	gcol = -99
c field for grade
	call ftgcno(unit, exact, 'Grade', gcol, status)
	print*,'GRADE is in column ', gcol, 'status =', status
	if(gcol.eq.-99.or.status.ne.0)then
		print*,'No grade information'
		nogrd = .TRUE.
	end if
c which field is PHA?
	call ftgcno(unit, exact, 'PHA', phacol, status)
	print*,'PHA is in column ', phacol, 'status =', status
		if(phacol.eq.-99.or.status.ne.0)then
		  print*,'No PHA value - maybe faint mode?'
		  call ftgcno(unit, exact, 'PHA1', pha1col, status)
		  if(pha1col.eq.-99.or.status.ne.0)then
			print*,'NO PHA VALUES AT ALL'
			return
		   else
		     FAINT = .TRUE.
		     nogrd = .FALSE.
		     nfield = 11
                     call ftgcno(unit, exact, 'PHA2', pha2col, status)
		     call ftgcno(unit, exact, 'PHA3', pha3col, status)
		     call ftgcno(unit, exact, 'PHA4', pha4col, status)
		     call ftgcno(unit, exact, 'PHA5', pha5col, status)
		     call ftgcno(unit, exact, 'PHA6', pha6col, status)
		     call ftgcno(unit, exact, 'PHA7', pha7col, status)
		     call ftgcno(unit, exact, 'PHA8', pha8col, status)
		     call ftgcno(unit, exact, 'PHA9', pha9col, status)
	print*,'cols are:', pha1col, pha2col, pha3col, pha4col, pha5col,
     +				pha6col, pha7col, pha8col
		  end if
	end if
	call ftgcno(unit, exact, 'X', xcol, status)
c	print*,'X is in column ', xcol, 'status =', status
	call ftgcno(unit, exact, 'Y', ycol, status)
c	print*,'Y is in column ', ycol, 'status =', status
	
c get the data values
	do 200 irow = 1, nrows 
		do 250 iloop = 1, nfield
			call ftgcvi(unit, iloop, irow, 1, 1,
     +				nullval, itbuff, anyf, status)
				if(status.ne.0)goto 950
			i2buff(iloop) = itbuff(1)
250		continue
		xpos = i2buff(xcol)
		ypos = i2buff(ycol)
		if(FAINT)then
			phas(1) = i2buff(pha1col)
			phas(2) = i2buff(pha2col)
			phas(3) = i2buff(pha3col)
			phas(4) = i2buff(pha4col)
			phas(5) = i2buff(pha5col)
			phas(6) = i2buff(pha6col)
			phas(7) = i2buff(pha7col)
			phas(8) = i2buff(pha8col)
			phas(9) = i2buff(pha9col)
			call fnttobrt(phas, pha, grade)
		else
			pha = i2buff(phacol)
			grade = i2buff(gcol)
		end if
c	if(irow.lt.10)print*,xpos, ypos, pha, grade
c		print*,'xpos, ypos, pha, grade', xpos, ypos, pha, grade
c apply filters
		if(xpos.ge.xpmin.and.xpos.le.xpmax.and.
     +		 ypos.ge.ypmin.and.ypos.le.ypmax.and.
     + 			grade.ge.gmin.and.grade.le.gmax)then
				y(pha) = y(pha) + 1
				if(pha.gt.npts)npts = pha
		end if
200	continue
c close FITS file
	call ftclos(unit, status)
	if(status.ne.0)goto 950

	title = adname
	xlab = 'PHA channel'
	ylab = 'counts/channel'

	do 500 i = 1, npts
		yfit(i) = y(i)
500	continue

900	return

950	continue
c error messages
	call xtext('***WARNING***')
	write(tstring, *)'Error in FITSIO call, status = ', status
	call xtext(tstring)

	end



c Called by a 'C' routine contained in main.c which
c enables limits to be changed via sliders etc.

	subroutine fadset(which, value)
	integer which, value

	include 'adcom'

	if(which.eq.0)then
		xpmin = value
	else if(which.eq.1)then
		xpmax = value
	else if(which.eq.2)then
		ypmin = value
	else if(which.eq.3)then
		ypmax = value
	else if(which.eq.4)then
		gmin = value
	else if(which.eq.5)then
		gmax = value
	else if(which.eq.6)then
		value = threshold
	end if

	end

c Called by a 'C' routine contained in main.c which
c finds out current limits.

	subroutine fadget(which, value)
	integer which, value

	include 'adcom'

	if(which.eq.0)then
		value = xpmin
	else if(which.eq.1)then
		value = xpmax
	else if(which.eq.2)then
		value = ypmin
	else if(which.eq.3)then
		value = ypmax
	else if(which.eq.4)then
		value = gmin
	else if(which.eq.5)then
		value = gmax
	else if(which.eq.6)then
		value = threshold
	end if

	end

c plus one more for setting the input file name
	subroutine fdnam(name)

	character name(*)
	include 'adcom'
	integer imax, i

	imax = len(adname)

	do 10 i = 1, imax
		adname(i:i) = name(i)
10	continue

	call blankit(adname)
	
	end

c and to go the other way (read adname)
	subroutine fdnamg(name)
	character*60 name
	include 'adcom'

	name = adname
	
	end


c convert an array of 9 PHA values to a single value
c and allocate a "grade" value
c call a slightly modified version of the Yoshida routine

	subroutine fnttobrt(phas, pha, grade)
	include 'adcom'
	integer phas(*)
	integer pha, grade
	integer above


	call classify(phas, threshold, pha, grade, above)

	end
