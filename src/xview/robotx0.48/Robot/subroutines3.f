c set the pan instruction up
	subroutine setpan(inst, prange, itype)
c itype = 0 = right, itype = 1 = left,
c itype = 2 = up, itype = 3 = down
	character*(*) inst
	real prange

	real xrange, yrange

        common/limits/xminp,xmaxp,yminp,ymaxp	

	xrange = (xmaxp - xminp)*prange/200.
	yrange = (ymaxp - yminp)*prange/200.

	if(itype .eq. 0)then
		write(inst, *)'dflimits (',xminp+xrange,
     +              yminp, xmaxp+xrange, ymaxp,
     +		    '); n; g'
	else if(itype .eq. 1)then
		write(inst, *)'dflimits (',xminp-xrange,
     +              yminp, xmaxp-xrange, ymaxp,
     +		    '); n; g'
	else if(itype .eq. 2)then
		write(inst, *)'dflimits (',xminp,
     +              yminp+yrange, xmaxp, ymaxp+yrange,
     +		    '); n; g'
	else if(itype .eq. 3)then
		write(inst, *)'dflimits (',xminp,
     +              yminp-yrange, xmaxp, ymaxp-yrange,
     +		    '); n; g'
	else 
	    call xtext('Internal error in S/R setpan')
	end if

	end





c reset loop counter to 1
c (This counter is for the no. of instructions/data lines
c stored within each loop)
	subroutine bloop()
	include 'robcom'

	lnow = 1

	end

c for storing instructions during a loop
	subroutine ploop(buff)
	character*(*) buff
	include 'robcom'


	if(lnow .le. ilmax)then
		lbuff(lnow) = buff
	else
		call xtext('Loop has too much in it')
	end if

	lnow = lnow+1


	end

c for getting stored instructions during a loop
	subroutine gloop(buff)
	character*(*) buff
	include 'robcom'


	if(lnow .le. ilmax)then
		buff = lbuff(lnow)
	else
c no error message - ploop probably does more than enough
		continue
	end if

	lnow = lnow+1

	end




c decode an instruction into multiple instructions 
c separated by semi-colons
	subroutine noinst(inst, instn, ninst)

	character*(*) inst
	character*(*) instn(*)
	integer ninst

	parameter(iscoln = ichar(';'))


	integer i, n, ic

	ninst = 0

	n = nnl(inst)

	istart = 1

c testing 1,2 3
c	ninst = 1
c	instn(1) = inst(istart:n)
c	return

	do 10 i = 1, n
	   ic = ichar(inst(i:i))
	   if(ic .eq. iscoln)then
		ninst = ninst + 1
		write(instn(ninst), *)inst(istart:i-1)
		istart = i+1
	   end if
10	continue
	
	ninst = ninst + 1
	instn(ninst) = inst(istart:n)


	end




c indentify the nearest point to the specified value
c (providing its inside the plotting area)
	subroutine nneigh(xpos, ypos, xminp,yminp,xmaxp,ymaxp)
	include 'msizcom'
	include 'robcom'


c only valid if we have data
	if(npts.le.0)return
c only valid in plot area
	if(xpos.lt.xminp.or.xpos.gt.xmaxp.or.
     +		ypos.lt.yminp.or.ypos.gt.ymaxp) return

	xscale = (xmaxp - xminp)**2
	yscale = (ymaxp - yminp)**2

	distmin = (xpos-x(1))**2/xscale + (ypos-y(1))**2/yscale
	imin = 1
	
	do 10 i = 1, npts
		dist = (xpos-x(i))*(xpos-x(i))/xscale + 
     +			(ypos-y(i))*(ypos-y(i))/yscale
		if(dist.lt.distmin)then
			distmin = dist
			imin = i
		end if
10	continue

	write(tstring,*)'Nearest point is no.',imin
	call xtext(tstring)
	write(tstring,*)'x = ',x(imin), ' y = ',y(imin)
	call xtext(tstring)

	if(delx(imin).ne.0.0)then
		write(tstring,*)'delta x = ',delx(imin)
		call xtext(tstring)
	end if

	if(dely(imin).ne.0.0)then
		write(tstring,*)'delta y = ',dely(imin)
		call xtext(tstring)
	end if


	end
	




c set line style to the appropriate type
	subroutine slstyle(lstyle)
	character*(*) lstyle
	if(lstyle.eq.'SOLID')then
		call nodash()
	else if(lstyle.eq.'DASH')then
		call dodash()
	else if(lstyle.eq.'DOTDASH')then
		call dotdash()
	else if(lstyle.eq.'DOT')then
		call dodot()
	else
		call xtext('ERROR - sltstyle called with unknow style')
		call xtext(lstyle)
	end if

	end



c routine to draw either a polyline or a polygon
c (difference is whether first point is connected to last or not)
c itype specifies which is done
	subroutine pline(itype, kboard)
	integer itype
	logical  kboard
	include 'robcom'

      common/inrob/inter
	logical inter
	
	integer press


	call xtext('Use cursor to select points')
	call xtext('Left button selects, middle button terminates')
	
	call sprompt('Specify start of line')
	call sprompt('(<RETURN> = use cursor)')
	ainbuf = ' '
	if(kboard.or.(.not.inter).or.HAVEDATA)call getit(ainbuf, 0)
	if(ainbuf.ne.' ')then
		call savdata(ainbuf)
          	call dcode(ainbuf,ain1,ainfix,aingrd,k)
	else
		call cursor(ain1(1), ain1(2))
		call fvout(ain1, 2)
	end if
	xfirst = ain1(1)
	yfirst = ain1(2)
	xtemp = xfirst
	ytemp = yfirst
	call movxy(xfirst, yfirst)
102	continue
        if(kboard.or.(.not.inter).or.HAVEDATA)then
		call sprompt('Next coordinate, <return> terminates')
		call getit(ainbuf, 0)
          	call dcode(ainbuf,ain1,ainfix,aingrd,k)
		if(ainbuf.eq.' ')then
			press = 2
		else
			press = 1
		end if
	else
		call rband(xtemp, ytemp, 0, ain1(1), ain1(2), 1+2, press)
	end if
	if(press.eq.1)then
c floating point comparison - to try to save writing duplicate numbers
c out
		if(ain1(1).ne.xtemp.and.ain1(2).ne.ytemp)then
			call linxy(ain1(1), ain1(2))
			xtemp = ain1(1)
			ytemp = ain1(2)
			call fvout(ain1, 2)
		end if
		goto 102
	end if
c terminate stored data with a blank line
	call savdata(' ')

c close if polygon
	if(itype.eq.2)then
		call linxy(xfirst, yfirst)
	end if


	end


c draw a filled polygon

	subroutine pfill(kboard)
	logical  kboard
	include 'robcom'

      common/inrob/inter
	logical inter
	
	integer press


	call xtext('Use cursor to select points')
	call xtext('Left button selects, middle button terminates')
	
	call sprompt('Specify start of line')
	call sprompt('(<RETURN> = use cursor)')
	ainbuf = ' '
	if(kboard.or.(.not.inter).or.HAVEDATA)call getit(ainbuf, 0)
	if(ainbuf.ne.' ')then
		call savdata(ainbuf)
          	call dcode(ainbuf,ain1,ainfix,aingrd,k)
	else
		call cursor(ain1(1), ain1(2))
		call fvout(ain1, 2)
	end if
	xfirst = ain1(1)
	yfirst = ain1(2)
	xtemp = xfirst
	ytemp = yfirst
	call arkpoly1(xfirst, yfirst)
102	continue
        if(kboard.or.(.not.inter).or.HAVEDATA)then
		call sprompt('Next coordinate, <return> terminates')
		call getit(ainbuf, 0)
          	call dcode(ainbuf,ain1,ainfix,aingrd,k)
		if(ainbuf.eq.' ')then
			press = 2
		else
			press = 1
		end if
	else
		call rband(xtemp, ytemp, 0, ain1(1), ain1(2), 1+2, press)
	end if
	if(press.eq.1)then
c floating point comparison - to try to save writing duplicate numbers
c out
		if(ain1(1).ne.xtemp.and.ain1(2).ne.ytemp)then
c we indicate where the polygon is going to be using lines
c however, switch off storing these in the "postscript" array
c as this isn't needed

			call haltps()
			call linxy(ain1(1), ain1(2))
			call startps()
			call arkpoly(ain1(1), ain1(2))
			xtemp = ain1(1)
			ytemp = ain1(2)
			call fvout(ain1, 2)
		end if
		goto 102
	end if
c terminate stored data with a blank line
	call savdata(' ')

c terminate polygon
	call arkpoly2()

	end


	subroutine nline(x1, y1, x2, y2, itype, s1, s2)
	integer press
	character*(*) s1, s2
	
	call update()

10	call xtext(s1)
	call cursor(x1, y1)
	call xtext(s2)
	call rband(x1, y1, itype, x2, y2, 5, press)
	if(press.eq.3) goto 10
	end


c getit is intended to be the main I/O routine for
c dialogue interaction within Robot.
c It will either read from the screen or from a file
c depending on whether INTER is true or not.
c This now also automatically writes the input to unit
c no. 7 too if DOIT is 1.
	subroutine getit(buff, doit)
      common/inrob/inter
	logical inter
	include 'robcom'
	character*(*) buff
	integer doit
c obtain a text string, either read from a file
c if non-intereactive of else call up the appropriate X
c windows subroutine
c first option is to check whether we already have data from
c somewhere else
	DONEDATA = .FALSE.
	buff = ' '
	if(HAVEDATA)then
		DONEDATA = .TRUE.
		read(bufinst(ibufstr),'(a)', err = 900)buff
		ibufstr = ibufstr + 1
c see whether we've now run out of stored data
		if(ibufstr.ge.maxbuf)HAVEDATA = .FALSE.
	else if(inter) then
		call gettextxv(buff)
		call blankit(buff)
	else
		if((.not.(loop)).or.rloop.eq.1) then
			read(ifil, '(a)', err = 900, end = 900)buff
		else if(loop.and.rloop.gt.1)then
			if(ifil .eq. uloop)then
				call gloop(buff)
			else
			   read(ifil, '(a)',err =900,end=901)buff
			end if
			
		end if
		
	end if
c C routines leave a NULL at the end of a character string
c to make sure there is no junk after this change everything
c after a null to a space
                         
	call blankit(buff)
                   
                   
c how data is stored depends on value of doit and whether we're
c looping. If looping just store data for first pass for retrieval
c on subsequent passes
	if(doit.eq.1) call savdata(buff)
c reset prompt 
	call preset()                                   
	return

       
900	call xtext('Error occurred during a read from the input file')
	return
901	call xtext('Internal Error: End of file detected in getit')

	end
    
c save data in unit number 7 and/or write in arrays used in looping
c as required
	subroutine savdata(buffin)
c	character buffin(*)
	character*(*) buffin
	character*80 buff
	include 'robcom'

	integer imax, i


	imax = len(buff)
	do 10 i = 1, imax
		buff(i:i) = buffin(i:i)
10	continue
	call blankit(buff)

	if(.not.(loop)) write(ifil7,'(a)')buff(1:nnl(buff))
	if(loop.and.rloop.eq.1)then
		write(ifil7,'(a)')buff
		if(.not.(DONEDATA) .and. 
     +			ifil .eq. uloop)then
				call ploop(buff)
		end if
	end if

	DONEDATA = .FALSE.        

	end
    
c in a kludgey type of way see whether the instruction has anything
c after the main instruction - if so assume it's data
	subroutine chkdat(inst)
	character*(*)	inst
	character*80 	buffer
	imax = len(inst)
	buffer = ' '
c find where first blank occurs
	do 10 i = 1, imax
		if(inst(i:i).eq.' ')then
			goto 11
		end if
10	continue
c didn't find any blank I think
	return
11	continue
	j = i
c check whether there's a NON-blank after this
	do 20 i = j, imax
		if(inst(i:i).ne.' ')then
			goto 21
		end if
20	continue
c didn't find any non-blank I suppose
	return
21	continue
	buffer = inst(i:imax)
	call buffset(buffer)
c and finally remove the nasty old data from the instruction
	do 30 i = j, imax
		inst(i:i) = ' '
30	continue
           
	end


c list the aliases available
	subroutine lalias(useral1, useral2, noual)
	character*(*) useral1(*), useral2(*)
	include 'robcom'

	if(noual.eq.0)then
		call xtext('No aliases in use')
	else
		call xtext('Defined aliases are:')
	end if

	do 10 i = 1, noual
		tstring = useral1(i)
		call xtext(tstring)
		tstring = useral2(i)
		call xtext(tstring)
10	continue

	end


c add another alias to the list or change an existing definition
	subroutine adalias(buff1, buff2, useral1, useral2, noual, maxual)
	character*(*) buff1, buff2, useral1(*), useral2(*)

c convert to upper case
	call lctouc(buff1)

c now see whether the alias is already in our list
	do 10 i = 1, noual
		if(buff1(1:nnl(buff1)).eq.useral1(i))then
			 useral2(i) = buff2
			 goto 20
		end if
10	continue

c seems we need to add a new alias to the list
	if(noual.ge.maxual)then
		call xtext('ERROR: TOO MANY ALIASES')
	else
		noual = noual + 1
		useral1(noual) = buff1
		useral2(noual) = buff2
	end if

20	continue

	end



c check if an instruction is an alias and remove it if it is
	subroutine chkal(inst, useral1, useral2, noual)
	character*(*) inst, useral1(*), useral2(*)

	character*80 inst2
	integer ilen

	inst2 = inst

	call lctouc(inst2)


	ilen = nnl(inst2)


	do 10 i = 1, noual
		if(inst2(1:ilen).eq.useral1(i))then
			 inst = useral2(i)
c an additional data check in addition to that in the main routine
c			 call chkdat(inst)
c and make it upper case
c			 call lctouc(inst)
			 goto 20
		end if
10	continue
20	continue

	end


c remove a name from the alias list
 	subroutine dalias(buff, useral1, useral2, noual)
	character*(*) buff, useral1(*), useral2(*)

	do 10 i = 1, noual
		if(buff(1:nnl(buff)).eq.useral1(i))then
			do 20 j = i, noual
			  useral1(j) = useral1(j+1)
			  useral2(j) = useral2(j+1)
20			continue
			noual = noual - 1
			goto 30
		end if
10	continue
	call xtext('ERROR: ALIAS NOT FOUND')
30	continue
	end

 
c used by external routines to set up a buffer which can be read
c from within robot
	subroutine buffset(bufft)
	character*80 buffer
	character bufft(*)
	include 'robcom'
	integer imax, i


	imax = len(buffer)
	do 10 i = 1, imax
		buffer(i:i) = bufft(i)
10	continue

c check if there's enough space in the buffer
	if(ibufstr.gt.ibufmax)then
		print*,'ERROR: not enough space in buffers.'
		print*,'complain to corbet@astro.psu.edu'
		return
	end if


	HAVEDATA = .TRUE.
	call blankit(buffer)
	write(bufinst(ibufstr),'(a)')buffer
	ibufstr = ibufstr + 1
	maxbuf = ibufstr
	end
	
 
c reset data buffer
	subroutine bufrst()
	include 'robcom'
	HAVEDATA = .FALSE.
	ibufstr = 1
	end
    
c send a value to log file (also potentially stores in loop array via
c call to savdata)
c fvout - FORTRAN values out
	subroutine fvout(value, n)
	real value(*)
	integer n
	character*80 mybuff
                    
                    
	write(mybuff, *)(value(i), i = 1, n)
	call savdata(mybuff)
	end
    
c fiout - FORTRAN integer values out
	subroutine fiout(ivalue, n)
	integer ivalue(*)
	integer n
	character*80 mybuff
                    
	write(mybuff, *)(ivalue(i), i = 1, n)
	call savdata(mybuff)
	end
    
c send an ***instruction*** file 7
c Hence no call to savdata
c faout - FORTRAN ASCII out
	subroutine saveinst(texti)
	character*(*) texti
	character*80 text
	include 'robcom'
	integer imax, i

	imax = len(text)

	do 10 i = 1, imax
		text(i:i) = texti(i:i)
10	continue
                 
	call blankit(text)
                   
                   
                   
	if((.not.(loop)).or.rloop.eq.1)then
		write(ifil7, '(a)')text(1:nnl(text))
	end if
c first passage through loop store instruction		
c	if(loop.and.rloop.eq.1)then
c		write(rinst(rinstno),'(a)')text
c		rinstno = rinstno + 1
c	end if
	end
	
 
 
c This takes a string returned by a C routine and replaces
c anything after a NULL with a space
                                    
	subroutine blankit(buff)
	character*(*) buff
	integer i, imax
	logical	isend
	isend = .FALSE.
	
	imax = len(buff)
	
	do 10 i = 1, imax
		if(buff(i:i).eq.char(0)) isend = .TRUE.
		if(isend) buff(i:i) = ' '
10	continue
           
	end

c test whether we are to carry out the next instruction
c because it's an IF related instruction or the
c IF tests in operation will let us
c returns 0 if OK, otherwise 1

	integer function ifcheck(inst, condition, iflevel)
	character*(*) inst
	logical condition(*)
	integer iflevel
	ifcheck = 0
	if(iflevel.eq.0)return
	if(inst.eq.'IF'.or.
     +		inst.eq.'ELSE'.or.
     +		inst.eq.'ELSEIF'.or.
     +		inst.eq.'ENDIF')return
	do 10 i = 1, iflevel
		if(.not.condition(i))then
			ifcheck = 1
			return
		end if
10	continue
	end



c pause using ca calculation (hence system speed dependent)
	function waiter(a)
	n = nint(a)
	k = n/2
	waiter = 0.
	do 20 m = 1, k
	do 10 i = 1, n
	x = float(n)
	y = x**1.2
	z = cos(x)*sin(y)
	z2 = sqrt(abs(float(i-m)))
	waiter = waiter+z2+z
10	continue
20	continue
	end


c list user variables and their values
	subroutine lvars()
	include 'robcom'
	
	do 10 i = 1, nouvar
		write(tstring, *)uservn(i), '=', uservv(i)
		call xtext(tstring)
10	continue
	write(tstring, *)'A1 =',auser1
	call xtext(tstring)
	write(tstring, *)'A2 =',auser2
	call xtext(tstring)
	write(tstring, *)'A3 =',auser3
	call xtext(tstring)
	write(tstring, *)'A4 =',auser4
	call xtext(tstring)
	write(tstring, *)'A5 =',auser5
	call xtext(tstring)

	end

c add a variable to the existing list
	subroutine addvar(buff)
	character*(*) buff
	include 'robcom'

	if(nouvar.ge.maxuvar)then
		call xtext('ERROR: TOO MANY VARIABLES')
		return
	end if

c it was too complicated to keep case sensitivity
	call lctouc(buff)

c check whether variable already exists
	do 10 i = 1, nouvar
		if(uservn(i) .eq. buff)then
		  write(tstring, *)'ERROR: VARIABLE "',
     +		     buff(1:nnl(buff)), '" ALREADY IN USE'
			call xtext(tstring)
			return
		end if
10	continue

	nouvar = nouvar + 1

	uservn(nouvar) = buff

	end


c assign a value to a user variable
c either a pregrogrammed one (a1 to a5)
c or a user specified one
	subroutine assign(name, value, ifail)
	include 'robcom'
	character*(*) name
	real	value
	integer ifail
	ifail = 0
	do 10 i = 1, nouvar
		if(name.eq.uservn(i))then
			uservv(i) = value
			return
		end if
10	continue
		if(name.eq.'A1')then
			auser1 = value
			return
		else if(name.eq.'A2')then
			auser2 = value
			return
		else if(name.eq.'A3')then
			auser3 = value
			return
		else if(name.eq.'A4')then
			auser4 = value
			return
		else if(name.eq.'A5')then
			auser5 = value
			return
		end if

	ifail = 1
	end




c simply a stop statement to be called by C!
	subroutine fstop()
	stop
	end
    
c flush out log and info files
	subroutine liflsh()
	include 'robcom'
	call flush(ifil7)
	call flush(ifil8)
	end    
    
c opens a "FILE" containing Robot instructions
c if the name passed isn't found we also try adding ".rob"
c to the end of it
c ifil is set equal to ulevel and then ulevel
c is incremented
	subroutine fopen(newfil, ulevel, ifil, ifail)
	character*(*) newfil
	integer ifail, ulevel, ifil
                            
	ifail = 0

	
	open(ulevel, file=newfil, status='old', err = 10)
c increase ulevel for next read to a read to file
	ifil = ulevel
	ulevel = ulevel + 1
	return
       
10	continue
	newfil = newfil(1:nnl(newfil))//'.rob'
	open(ulevel, file=newfil, status='old', err = 20)
c increase ulevel for next read to a read to file
	ifil = ulevel
	ulevel = ulevel + 1


	return
       
20	continue
	ifail = 1
	end
    
	
	function nnl(buff)
c returns lengths of string which is not spaces or nulls
	character*(*) buff
	parameter (ispace = ichar(' '))
	integer ic

	ilen = len(buff)
	do 10 i = ilen, 1, -1
		ic = ichar(buff(i:i))
		if(ic .ne. ispace .and. ic .ne. 0) goto 11
10	continue
11	nnl = i
	end
    
c adds a real number to the end of a character string which
c has already had something added to it.
c Needed to avoid problems if f2c is used to obtain 'C' code
	subroutine addreal(buff, x)
	character*(*) buff
	real x
	write(buff(nnl(buff)+1:len(buff)), *)x
	end
c one for add integer
	subroutine addint(buff, i)
	character*(*) buff
	integer i
	write(buff(nnl(buff)+1:len(buff)), *)i
	end
c a routine for adding a character string
	subroutine addtext(buff, buff2)
	character*(*) buff
	character*(*) buff2
	buff = buff(1:nnl(buff)+1)//buff2
	end
c equivalents of the above hard-wired for writing to "tstring"
c real routine
	subroutine addrealt(x)
	include 'robcom'
	real x
	write(tstring(nnl(tstring)+1:len(tstring)), *)x
	end
c one for add integer
	subroutine addintt(i)
	include 'robcom'
	integer i
	write(tstring(nnl(tstring)+1:len(tstring)), *)i
	end
c a routine for adding a character string
	subroutine addtextt(buff2)
	include 'robcom'
	character*(*) buff2
	tstring = tstring(1:nnl(tstring)+1)//buff2
	end
    
c close down units 7 and 8
	subroutine clf()

	include 'robcom'

	close(unit = ifil7, err = 1000)
	close(unit = ifil8, err = 1000)

	return
1000	call xtext('ERROR: closing log/info file')


	end 





c call to create log and info files
c called once at start of robot
c also called if we delete the log files and want new ones
	subroutine olf()
	include 'robcom'


	ainbuf = 'rl'
	ifil7 = 7
	call ofile(ifil7, ainbuf)
c store log file name for later use
	logf = ainbuf
	ifil8 = 8
	ainbuf = 'ri'
	call ofile(ifil8, ainbuf)
c store info file name for later use
	infof = ainbuf
	end


c open units 7 (ROBOTLOG) and 8 (ROBOTINF)
c if these fail write to /dev/null
	subroutine ofile(ifile, text)
	include 'robcom'
	integer ifile
	integer fpid
	character*(*) text
	character*5 pbuf
	character*100 text2
	ipid = fpid()
	write(pbuf, '(i5.5)')ipid
        
	text2 = '/tmp/'//pbuf(1:nnl(pbuf))//text
                                        
	open(ifile, file = text2, status = 'new', err = 6)
	text = text2
	return
       
6	print*,'**WARNING***'
	print*,'COULDN''T OPEN ', text2
	print*,'WRITING TO /dev/null'
	open(ifile, file = '/dev/null', status = 'old', err = 7)
	return
7	print*,'COULDN''T EVEN OPEN /dev/null!!!'
	return
	end
    
    
c get log file name - used in C routines
	subroutine getfl(buff)
	include 'robcom'
	character*(*) buff
	integer i, length

	length = nnl(logf)
	
	do 10 i = 1, length
		buff(i:i) = logf(i:i)
10	continue

	buff(length+1:length+1) = char(0)

	end
    
c get info file name - used in C routines
	subroutine getfi(buff)
	include 'robcom'
	character*(*) buff
	integer i, length

	length = nnl(infof)

	do 10 i = 1, length
		buff(i:i) = infof(i:i)
10	continue

	buff(length+1:length+1) = char(0)

	end


c write a character string to ifil8 (done this
c way so that a call to nnl doesn't have to be written
c every time)

	subroutine write8(string)

	character*(*) string

	include 'robcom'

	write(ifil8, *)string(1 : nnl(string))

	end

	
    
    
	
 
c read a Ginga spectrum (48 channels)
      subroutine specred(pfnam,title,xlab,ylab,
     +x,y,delx,dely,npts)
	include 'robcom'
      real x(*),y(*),delx(*),dely(*)
	character*(*) title,xlab,ylab,pfnam
      open(unit=42,file=pfnam,status='old',err=903)
	npts=48
	read(42,'(a)')title
	do 10 i=1,19
10	read(42,'(a)')xlab
c get the bin energies
	read(42,66)itemp,(x(i),i=1,12)
	read(42,66)itemp,(x(i),i=13,24)
	read(42,66)itemp,(x(i),i=25,36)
	read(42,77)itemp,(x(i),i=37,49)
	read(42,66)itemp,(y(i),i=1,12)
	read(42,66)itemp,(y(i),i=13,24)
	read(42,66)itemp,(y(i),i=25,36)
	read(42,66)itemp,(y(i),i=37,48)
	read(42,66)itemp,(dely(i),i=1,12)
	read(42,66)itemp,(dely(i),i=13,24)
	read(42,66)itemp,(dely(i),i=25,36)
	read(42,66)itemp,(dely(i),i=37,48)
66	format(i1,12d12.5)
77	format(i1,13d12.5)
c sort out x values and errors
	do 20 i=1,48
	delx(i)=(x(i+1)-x(i))/2.
	x(i)=(x(i)+x(i+1))/2.
20	continue
	
	
	xlab='Energy (keV)'
	ylab='Counts'
	close(unit=42)
	return
903	call xtext('ERROR IN ROUTINE SPECRED!!!')
	end
    
      subroutine filred(pfnam,title,xlab,ylab,x,y,delx,dely,msiz,npts,
     +xfit,yfit,xfite,yfite,nptsf,ifail,istar)
	include 'robcom'
c maxcol is maximum number of columns that filred can cope with
      parameter (maxcol=10)
      common/table/index,ftext
      logical ftext
      common/echoc/echo
      logical echo
      integer index(4)
      character*(*) title,xlab,ylab,pfnam
c arrays for read routine
      real ain(maxcol)
      integer ifix(maxcol),igrd(maxcol)
      real x(msiz),y(msiz),dely(msiz),delx(msiz)
      real xfit(*),yfit(*),xfite(*),yfite(*)
      ifail=0
      open(unit=42,file=pfnam,status='old',err=2)
	goto 3
2	continue
c we failed to open the name specified - let's see if adding
c a ".dat" to the end will help us
	pfnam = pfnam(1:nnl(pfnam))//'.dat'
      open(unit=42,file=pfnam,status='old',err=903)
3	continue
C READ IN DATA FROM PLOT FILE
      if(ECHO)then
        write(tstring,*)'Data file name is: ',pfnam
	call totext(tstring)
	call xtext('Reading data...')
      end if	
c text at top of file?
      if (ftext)then
c identifying text
        read(42,'(a)',end=994,err=995)title
c x title
        read(42,'(a)',end=994,err=995)xlab
c y title
        read(42,'(a)',end=994,err=995)ylab
      else
c title is file name
	title = pfnam
c and clear x and y labels
	xlab = ' '
	ylab = ' '
      end if
c zero ain
      do 4 i=1,maxcol
4     ain(i)=0.0
                
c read in data values
      i=istar
5     read(42,'(a)',end=50,err=993)ainbuf
                                       
c Use dcode2 - like dcode but with far fewer if statements
c so can't use key words or comments or split variables up
c with different types of brackets
                                  
      call dcode2(ainbuf,ain,ifix,igrd,k)
      if(k.eq.1)then
        y(i)=ain(1)
        x(i)=real(i)
        delx(i)=0.0
        dely(i)=0.0
c ignore blank lines
	else if(k.eq.0)then
		goto 44
      else
        if(index(1).gt.0)then
          x(i)=ain(index(1))
        else
          x(i)=float(i)
        end if
        if(index(2).gt.0)then
          y(i)=ain(index(2))
        else
          y(i)=float(i)
        end if
        if(index(3).gt.0)then
          dely(i)=ain(index(3))
        else
          dely(i)=0.0
        end if
        if(index(4).gt.0)then
          delx(i)=ain(index(4))
        else
          delx(i)=0.0
        end if
      end if
            
      xfit(i)=x(i)
      yfit(i)=y(i)
      xfite(i)=delx(i)
      yfite(i)=dely(i)
      i=i+1
      if(i.gt.msiz)goto 992
c zero ain
      do 44 j=1,maxcol
44     ain(j)=0.0
      goto 5
50    continue
              
      npts = i - 1
      nptsf = npts
      if(echo)then
		write(tstring,*)'no. of points = ',npts
		call totext(tstring)
	end if
      close(unit=42)
      return
C ERROR MESSAGES
992   call xtext('ERROR: THERE IS TOO MUCH DATA')
      write(tstring,*)'A MAXIMUM OF',MSIZ,' POINTS IS ALLOWED'
	call totext(tstring)
	npts = i - 1
	nptsf = npts
      return

903   call xtext('ERROR OPENING DATA FILE')
      IFAIL=1
      return
993   call xtext('ERROR READING DATA FROM DATA FILE')
      write(tstring,*)'AT LINE NUMBER',I
	call totext(tstring)
      return
994   call xtext('THERE IS NO DATA IN THE DATA FILE')
	ifail = 1
	return
995   call xtext('ERROR READING TEXT HEADER FROM THE DATA FILE')
	ifail = 1
	return
      end
         
C ENTER DATA DIRECTLY INTO PROGRAM
      subroutine type(x,y,delx,dely,npts,
     +title,xlab,ylab,msiz,istar, ilabels)
	include 'robcom'
C MAXCOL IS MAXIMUM NUMBER OF COLUMNS THAT FILRED CAN COPE WITH
      parameter (maxcol=10)
      common/table/index,ftext
      logical ftext
      integer index(4)
	character*(*) title,xlab,ylab
C ARRAYS FOR READ ROUTINE
      real ain(maxcol)
      integer ifix(maxcol),igrd(maxcol)
      real x(*),y(*),dely(*),delx(*)
	character*5 helper(4)
	data helper/'x','y','yerr ','xerr '/
C Text at top of file?
      IF (FTEXT.and.(ilabels.eq.1))then
C IDENTIFYING TEXT
        call sprompt('Give title for graph')
        call getit(title,  1)
C X TITLE
        call sprompt('Give label for X-axis')
        call getit(xlab,  1)
C Y TITLE
        call sprompt('Give label for Y-axis')
        call getit(ylab,  1)
      end if
C ZERO AIN
      do 4 i=1,maxcol
4     ain(i)=0.0
c explain which columns are which
	write(tstring,*)helper(index(1)),helper(index(2)),
     +		helper(index(3)),helper(index(4))
	call sprompt(tstring)
C READ IN DATA VALUES
      i=istar
5     call sprompt('Give data, blank line to end')
        call getit(ainbuf,  1)
      call dcode(ainbuf,ain,ifix,igrd,k)
      if(k.eq.0)goto 50
      if(k.eq.1)then
        y(i)=ain(1)
        x(i)=real(i)
        delx(i)=0.0
        dely(i)=0.0
      else
        if(index(1).gt.0)then
          x(i)=ain(index(1))
        else
          x(i)=float(i)
        end if
        if(index(2).gt.0)then
          y(i)=ain(index(2))
        else
          y(i)=0.0
        end if
        if(index(3).gt.0)then
          dely(i)=ain(index(3))
        else
          dely(i)=float(i)
        end if
        if(index(4).gt.0)then
          delx(i)=ain(index(4))
        else
          delx(i)=0.0
        end if
      end if
            
      i=i+1
      if(i.gt.msiz)goto 992
C ZERO AIN
      do 44 j=1,maxcol
44     ain(j)=0.0
      goto 5
50    continue
              
      npts=i-1
      write(tstring,*)'no. of points = ',NPTS
	call totext(tstring)
      return
C ERROR MESSAGES
992   write(tstring,'(a, i)')'ERROR: THERE IS TOO MUCH DATA IN UNIT', IFIL
	call totext(tstring)
      write(tstring,*)'A MAXIMUM OF'
	call addintt(MSIZ)
	call addtextt('POINTS IS ALLOWED')
	call totext(tstring)
      return
c995   write(tstring,'(a, i)')'ERROR READING TEXT HEADER FROM UNIT', IFIL
c	call totext(tstring)
c      return
      end
         
C OUTPUT LABELS AND X,Y POINTS TO SPECIFIED FILE
      subroutine filwrt(pfnam,title,xlab,ylab,x,y,delx,dely,
     +npts)
	include 'robcom'
	common/test/xtmin, xtmax, ytmin, ytmax
      character*(*) title,xlab,ylab,pfnam
      real x(*),y(*),dely(*),delx(*)
      if(pfnam.ne.' ')then
        open(unit=42, file=pfnam, status = 'new', err = 999)
c write data to plot file
c identifying text
        write(42,'(a)')title
c x title
        write(42,'(a)')xlab
c y title
        write(42,'(a)')ylab
c write out data values
        do 5 i=1,npts
	if(x(i).gt.xtmax.or.x(i).lt.xtmin.or.
     +		y(i).gt.ytmax.or.y(i).lt.ytmin) goto 5
         write(42,*, err = 1000)x(i),y(i),dely(i),delx(i)
5	continue
        close(unit=42)
      else
        call xtext(title)
        call xtext(xlab)
        call xtext(ylab)
C WRITE OUT DATA VALUES
        do 15 i=1,npts
	if(x(i).gt.xtmax.or.x(i).lt.xtmin.or.
     +		y(i).gt.ytmax.or.y(i).lt.ytmin) goto 15
        write(tstring,*)x(i),y(i),dely(i),delx(i)
	call totext(tstring)
15	continue
      end if
	return
c error messages
999	call xtext('ERROR: OPENING FILE FOR OUTPUT')
	return
1000	call xtext('ERROR: WRITING TO OUTPUT FILE')
      end
         
C OUTPUT ONLINE SHORT VERSION OF MANUAL DURING INTERACTIVE SESSION
      subroutine manual
	include 'robcom'
      open(unit=42,file='manual', status = 'old', err=900)
5     read(42,'(a)',end=10, err= 900)ainbuf
      call xtext(ainbuf)
      goto 5
10    continue
      close(unit=42)
      return
900   call xtext('*** ERROR OPENING MANUAL ***')
      end
         
      subroutine dmax(x,y,dely,delx,xmax,ymax,xmin,ymin,npts)
	include 'robcom'
c find data limits
      real x(*),y(*),dely(*),delx(*)
	real xdel, ydel
c taking absolute values allows for having negative
c errors (these may result if the data values are multiplied
c by a negative number)
	xdel = abs(delx(1))
	ydel = abs(dely(1))
      xmax=x(1)+xdel
      xmin=x(1)-xdel
      ymin=y(1)-ydel
      ymax=y(1)+ydel
      do 10 i=2,npts
	xdel = abs(delx(i))
	ydel = abs(dely(i))
	xmax=max(xmax,x(i)+xdel)
	ymax=max(ymax,y(i)+ydel)
	xmin=min(xmin,x(i)-xdel)
	ymin=min(ymin,y(i)-ydel)
10    continue
      end
         
c plot data as a filled histogram
                                 
                                 
	subroutine fhist(npts, x, y)
	real x(*), y(*)
	COMMON/TEST/xtmin, xtmax, ytmin, ytmax
                                       
C FIND START POINT INSIDE DATA LIMITS
       DO 18 I=1,NPTS
	 xp = x(i)
	 yp = y(i)
       if(XP.gt.xtmin.AND.XP.lt.xtmax.AND.YP.gt.Ytmin.
     +AND.YP.lt.Ytmax)then
c        CALL MOVXY(X(I),Y(I))
	x1 = x(i)
	y1 = y(i)
        GOTO 19
       end if
18      continue
19      J=I
C EXTEND LINE BACKWARD IF POSSIBLE
       if(j.gt.1)xp=(x(i)+x(i-1))/2.
       if(j.eq.1)xp=x(i)-((x(i+1)-x(i))/2.)
       yp=y(i)
       if(xp.lt.xtmin)xp=xtmin
       if(yp.lt.ytmin)yp=ytmin
       if(xp.gt.xtmax)xp=xtmax
       if(yp.gt.ytmax)yp=ytmax
	call fbox(x1, ytmin, xp, yp)
	x1 = x(j)
	y2 = y(j)
          
       do 110 i=j+1,npts
         xp=(x(i)+x(i-1))/2.
         yp=y(i-1)
         if(yp.gt.ytmax)yp=ytmax
         if(xp.lt.xtmin)xp=xtmin
         if(yp.lt.ytmin)yp=ytmin
         if(xp.gt.xtmax)goto 111
	call fbox(x1, ytmin, xp, yp)
         yp=y(i)
         if(yp.gt.ytmax)yp=ytmax
         if(yp.lt.ytmin)yp=ytmin
 	x1 = xp
        xp=x(i)
         if(xp.gt.xtmax)goto 111
	call fbox(x1, ytmin, xp, yp)
	x1 = xp
110     continue
C TERMINATE HISTOGRAM
       xp=x(npts)+((x(npts)-x(npts-1))/2.)
       if(xp.gt.xtmax)xp=xtmax
	call fbox(x1, ytmin, xp, yp)
	
        goto 112
111     continue
	call fbox(x1, ytmin, xtmax, yp)
112     continue
	return
	end
    
C PLOT A SYMBOL
C The symbol is the ASCII character specified by SYMB
c this is not very accurately centred in the Y direction
      subroutine tplot(npts,x,y,symb)
	include 'robcom'
      common/chsiz/xchsiz,ychsiz,size
	common/test/xtmin, xtmax, ytmin, ytmax
      real x(*),y(*)
      character*(*) symb
	logical hasbs

	hasbs = .FALSE.

	imax = nnl(symb)
c check for control character
	call getlen(symb, imax, iret, hasbs, xlen)

                    
	ainbuf = symb
              
      do 10 i=1,npts
        if(x(i).lt.xtmin.or.x(i).gt.xtmax.or.y(i).lt.ytmin.or.y(i).gt.
     +ytmax)goto 10
	if(hasbs)then
		start=x(i)-xlen/2.
        	call txtm(start,y(i)-ychsiz/2.3,symb,imax)
	else
		call style(x(i), y(i)-ychsiz/2.3, ainbuf, nnl(ainbuf), 0)
	end if
10    continue
      end

c plot text centred in x direction on given position
	subroutine ctext(xpos, ypos, text)
	real xpos, ypos
	character*(*) text
	include 'robcom'
	logical hasbs

	integer imax

	imax = MIN(len(ainbuf), nnl(text))
	
	hasbs = .FALSE.
	ainbuf = text(1: imax)

c is there a backspace?	
	call getlen(text, imax, iret, hasbs, xlen)

	if(hasbs)then
		start=xpos-xlen/2.
        	call txtm(start,ypos-ychsiz/2.3,text,imax)
	else
		call style(xpos, ypos-ychsiz/2.3, ainbuf, nnl(ainbuf), 0)
	end if

	end
	
         
c plot a symbol
c tplot2 plots a limited set of symbols accurately centred
c on the point positions (both on screen and in the postscript
c outputlines
      subroutine tplot2(npts,x,y,isymb)
	include 'robcom'
	common/test/xtmin, xtmax, ytmin, ytmax
      real x(*),y(*)
      integer isymb
                   
      do 10 i=1,npts
        if(x(i).lt.xtmin.or.x(i).gt.xtmax.or.y(i).lt.ytmin.or.y(i).gt.
     +ytmax)goto 10
        call symbol(x(i),y(i), isymb)
10    continue
      end

c plot data as pillars (this is more useful in 3D mode)
	subroutine pillar(npts, x, y)
	real x(*), y(*)
	common/test/xtmin, xtmax, ytmin, ytmax

	do 10 i = 1, npts
		if(x(i).gt.xtmax.or.x(i).lt.xtmin)goto 10
		xplot = x(i)
		yplot = y(i)
		if(yplot.gt.ytmax)yplot = ytmax
		if(yplot.lt.ytmin)yplot = ytmin
		call movxy(xplot, yplot)
		call linxy(xplot, ytmin)
10	continue
	end

	
c plot data as dashed lines
      subroutine odlplot(npts,x,y,fmplot,fnplot)
	include 'robcom'
      real x(*),y(*)
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
      ratio=1.0
      xscl=(0.821*fnplot/fmplot)**2
      gs=(xmaxp-xminp)**2*xscl+(ymaxp-yminp)**2
      do 10 i=1,npts-1
        x1=x(i)
        x2=x(i+1)
        y1=y(i)
        y2=y(i+1)
        if(y1.gt.ytmax)y1=ytmax
        if(y1.lt.ytmin)y1=ytmin
        if(x1.gt.xtmax)x1=xtmax
        if(x1.lt.xtmin)x1=xtmin
        if(y2.gt.ytmax)y2=ytmax
        if(y2.lt.ytmin)y2=ytmin
        if(x2.gt.xtmax)x2=xtmax
        if(x2.lt.xtmin)x2=xtmin
        fndash=sqrt(((x1-x2)**2*xscl+(y1-y2)**2)/gs)*50.
        call dline(x1,y1,x2,y2,fndash,ratio)
10    continue
      end
         
	subroutine dlplot(npts, x, y)
	include 'robcom'
	real x(*), y(*)
	common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
	call dodash()
	do 8 i=1,npts
	if(x(i).ge.xtmin.and.x(i).le.xtmax.and.y(i).ge.ytmin.
     +and.y(i).le.ytmax)then
        call movxy(x(i),y(i))
        goto 9
	end if
8	continue
9	j=i
	do 10 i=j,npts
	xp=x(i)
	yp=y(i)
	if(xp.ge.xtmin.and.xp.le.xtmax.and.yp.ge.ytmin.
     +and.yp.le.ytmax)then
		call linxy(xp,yp)
	end if
10	continue
	call nodash()
	end



c crude 3D plotting routines
c only some of the standard 2D plot modes are supported
c long term ambition is to have hidden line removal
	subroutine tdplot(npts, x, y, zfalse, 
     +pmode, isymb, symb, sxl, syl, sxh, syh, ntickx, nticky,
     +paxes, ccode, labx, xlab, ylab, zlab,
     +fiplot, fmplot, fjplot, fnplot, tdfill)
	include 'robcom'
	real x(*), y(*), zfalse(*)
	logical paxes, labx, ccode, tdfill
	common/limits/xminp,xmaxp,yminp,ymaxp
	common/limitz/zminp, zmaxp
	common/scales/xscale, yscale, zscale
	common/test/xtmin, xtmax, ytmin, ytmax
	character*(*) pmode, symb, xlab, ylab, zlab

	common/trig/cost1, sint1, cost2, sint2, cost3, sint3

c some local variables
	real x3min, x3max, y3min, y3max
	character*(60) buffer
c degree to radian conversion
 	DATA DTR/0.0174532/
c colours if needed

c fudge for npz
	ntickz = (ntickx + nticky)/2

	buffer = symb

	cost1 = cos(angle(1)*dtr)
	sint1 = sin(angle(1)*dtr)
	cost2 = cos(angle(2)*dtr)
	sint2 = sin(angle(2)*dtr)
	cost3 = cos(angle(3)*dtr)
	sint3 = sin(angle(3)*dtr)


c find x/y/z minimum
	zmin = zfalse(1)
	zmax = zfalse(1)
	xhmin = x(1)
	yhmin = y(1)
	xhmax = x(1)
	yhmax = y(1)
	do 5 i = 2, npts
		zmin =  min(zmin, zfalse(i))
		zmax = max(zmax, zfalse(i))
		xhmin = min(xhmin, x(i))
		xhmax = max(xhmax, x(i))
		yhmax = max(yhmax, y(i))
		yhmin = min(yhmin, y(i))
5	continue

	call linsc2(xhmin,xhmax,ntickx,xminp,xmaxp,npx,distx)
	call linsc2(yhmin,yhmax,nticky,yminp,ymaxp,npy,disty)
	call linsc2(zmin,zmax,ntickz,zminp,zmaxp,npz,distz)

	xscale = xmaxp-xminp
	yscale = ymaxp-yminp
	zscale = zmaxp-zminp

c find max and min of x3 and y3 (3d equivalents of x and y)
c and the corners of the box
	call rotate2(xminp, yminp, zminp, x3, y3)
	x3max = x3
	x3min = x3
	y3max = y3
	y3min = y3
	
	call rotate2(xmaxp, yminp, zminp, x3, y3)
	call maxmin3(x3max, x3min, y3max, y3min, x3, y3)
	call rotate2(xminp, ymaxp, zminp, x3, y3)
	call maxmin3(x3max, x3min, y3max, y3min, x3, y3)
	call rotate2(xminp, yminp, zmaxp, x3, y3)
	call maxmin3(x3max, x3min, y3max, y3min, x3, y3)
	call rotate2(xmaxp, ymaxp, zmaxp, x3, y3)
	call maxmin3(x3max, x3min, y3max, y3min, x3, y3)


c the actual data
	do 10 i = 1, npts
		call rotate2(x(i), y(i), zfalse(i), x3, y3)
		call maxmin3(x3max, x3min, y3max, y3min, x3, y3)
10	continue

c define plot limits
      sxh2=(sxh-sxl)*fiplot/fmplot+sxl
      sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
      syh2=(syh-syl)*fjplot/fnplot+syl
      syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
      call gap(sxl2, syl2, sxh2, syh2)

      if(tdfill)then
      	call limsq(sxl2,syl2,sxh2,syh2, x3min, y3min, x3max, y3max)
      else
      	call limsq(sxl2,syl2,sxh2,syh2, -1.41, -1.41, 1.41, 1.41)
      end if

c draw axes if desired
	if(paxes)then
		call rotate2(xminp, yminp, zminp, x3c, y3c)
		call movxy(x3c,y3c)
		call rotate2(xminp, yminp, zmaxp, x3, y3)
		call linxy(x3,y3)
		if(labx)then
			call rotate2(xminp, yminp, (zminp+zmaxp)/2., x3, y3)
			call txtm(x3, y3, zlab, 60)
		end if
		call movxy(x3c,y3c)
		call rotate2(xminp, ymaxp, zminp, x3, y3)
		call linxy(x3,y3)
		if(labx)then
			call rotate2(xminp, (ymaxp+yminp)/2., zminp, x3, y3)
			call txtm(x3, y3, ylab, 60)
		end if
		call movxy(x3c,y3c)
		call rotate2(xmaxp, yminp, zminp, x3, y3)
		call linxy(x3, y3)
		if(labx)then
			call rotate2((xminp+xmaxp)/2., yminp, zminp, x3, y3)
			call txtm(x3, y3, xlab, 60)
		end if
c complete the box
	call box3d(xminp, yminp, zminp, xmaxp, ymaxp, zmaxp)



	end if
	


	call rotate2(x(1), y(1), zfalse(1), x3, y3)
	call movxy(x3, y3)
	if(abs(rgbd(1)-rgb(1)).gt.2.or.abs(rgbd(2)-rgb(2)).gt.2.or.
     +	abs(rgbd(3) -rgb(3)).gt.2)then
		call farkc(rgbd(1),rgbd(2),rgbd(3))
	end if
	

	if(pmode.eq.'NICE')then
	do 50 i = 1, npts
		call rotate2(x(i), y(i), zfalse(i), x3, y3)
			if(CCODE)call datcol(zfalse(i), zminp, zscale)
			call symbol(x3, y3, isymb)
50	continue

	else if(pmode.eq.'CONTOUR')then
		call cont(x, y, zfalse, npts, ccode, .TRUE.)
	else if(pmode.eq.'SYMBOL')then
	do 55 i = 1, npts
		if(CCODE)call datcol(zfalse(i), zminp, zscale)
		call rotate2(x(i), y(i), zfalse(i), x3, y3)
			call ctit(x3,y3,buffer, nnl(buffer))
55		continue
	else if(pmode.eq.'PILLAR')then
	do 60 i = 1, npts
		if(CCODE)call datcol(zfalse(i), zminp, zscale)
		call rotate2(x(i), y(i), zfalse(i), x3, y3)
			call movxy(x3, y3)
			call rotate2(x(i), y(i), zminp, x3, y3)
			call linxy(x3, y3)
60	continue

	else if(pmode.eq.'LINES')then
	do 65 i = 1, npts
		if(CCODE)call datcol(zfalse(i), zminp, zscale)
		call rotate2(x(i), y(i), zfalse(i), x3, y3)
			call linxy(x3, y3)
65	continue
	else if(pmode.eq.'DASHEDLINES')then
	call dodash()
	do 70 i = 1, npts
		call rotate2(x(i), y(i), zfalse(i), x3, y3)
			call linxy(x3, y3)
70	continue
	call nodash()

	end if
c make sure colour is back to normal
	call farkc(rgb(1),rgb(2),rgb(3))
	end
		
c 3D box 
	subroutine box3d(x1, y1, z1, x2, y2, z2)

	call mov3d(x1, y1, z1)
	call lin3d(x2, y1, z1)
	call lin3d(x2, y2, z1)
	call lin3d(x1, y2, z1)
	call lin3d(x1, y1, z1)

	call mov3d(x1, y1, z2)
	call lin3d(x2, y1, z2)
	call lin3d(x2, y2, z2)
	call lin3d(x1, y2, z2)
	call lin3d(x1, y1, z2)

	call mov3d(x1, y1, z1)
	call lin3d(x1, y1, z2)

	call mov3d(x1, y2, z1)
	call lin3d(x1, y2, z2)

	call mov3d(x2, y2, z1)
	call lin3d(x2, y2, z2)

	call mov3d(x2, y1, z1)
	call lin3d(x2, y1, z2)

	end



	subroutine mov3d(x, y, z)
	call rotate2(x, y, z, x3, y3)
	call movxy(x3, y3)
	end

	subroutine lin3d(x, y, z)
	call rotate2(x, y, z, x3, y3)
	call linxy(x3, y3)
	end


		


	subroutine rotate(x, y, z, cost1, sint1, cost2, sint2,
     +				cost3, sint3, x3, y3)
c rotate about three axes in succession
c	y3 = z*cost1 - y*sint1
c	x3 = x
c	z3 = y*cost1 + z*sint1
c next rotation
c	x3 = x3*cost2 + z3*sint2
c	y3 = y3
c and last one
c	oldx3 = x3
c	x3 = y3*sint3 + x3*cost3
c	y3 = y3*cost3 - oldx3*sint3
	real xt, yt, zt

	call rcex(xt, yt, zt, x, y, z)

c try "standard" system (as defined in Foley and van Dam)
	call r1(xt, yt, zt, x3, y3, z3, cost1, sint1)
	call rcex(xt, yt, zt, x3, y3, z3)
	call r2(xt, yt, zt, x3, y3, z3, cost2, sint2)
	call rcex(xt, yt, zt, x3, y3, z3)
	call r3(xt, yt, zt, x3, y3, z3, cost3, sint3)
	end
	
	subroutine r1(xin, yin, zin, xout, yout, zout, c, s)

	xout = xin*c - yin*s
	yout = xin*s + yin*c
	zout = zin
	end

	subroutine r2(xin, yin, zin, xout, yout, zout, c, s)

	xout = xin
	yout = yin*c - zin*s
	zout = yin*s + zin*c
	end

	subroutine r3(xin, yin, zin, xout, yout, zout, c, s)

	xout = xin*c - zin*s
	yout = yin
	zout = -xin*s + zin*c
	end

	subroutine rcex(x1, y1, z1, x2, y2, z2)
	x1 = x2
	y1 = y2
	z1 = z2
	end

c scale to max and min
	subroutine rotate2(x, y, z, x3, y3)
	common/trig/cost1, sint1, cost2, sint2, cost3, sint3
	common/scales/xscale, yscale, zscale
	common/limits/xminp,xmaxp,yminp,ymaxp
	common/limitz/zminp, zmaxp
	x2 = (x-xminp)/xscale
	y2 = (y-yminp)/yscale
	z2 = (z-zminp)/zscale
	call rotate(x2, y2, z2, cost1, sint1, cost2, sint2,
     +				cost3, sint3, x3, y3)

	end


	subroutine rotate3(x, y, z, x3, y3, threed)
	logical threed
	include 'robcom'
	common/scales/xscale, yscale, zscale
	common/limits/xminp,xmaxp,yminp,ymaxp
	common/limitz/zminp, zmaxp
c degree to radian conversion
 	data DTR/0.0174532/
	if(threed)then
		cost1 = cos(angle(1)*dtr)
		sint1 = sin(angle(1)*dtr)
		cost2 = cos(angle(2)*dtr)
		sint2 = sin(angle(2)*dtr)
		cost3 = cos(angle(3)*dtr)
		sint3 = sin(angle(3)*dtr)
		x2 = (x-xminp)/xscale
		y2 = (y-yminp)/yscale
		z2 = (z-zminp)/zscale
		call rotate(x2, y2, z2, cost1, sint1, cost2, sint2,
     +				cost3, sint3, x3, y3)
	else
		x3 = x
		y3 = y
	end if
	end


c assign maximum and minimum values
	subroutine maxmin3(x3max, x3min, y3max, y3min, x3, y3)
	x3max = max(x3max, x3)
	x3min = min(x3min, x3)
	y3max = max(y3max, y3)
	y3min = min(y3min, y3)
	end
   
c interpolate colour value for data point
c used to help show z scale in plot
	subroutine datcol(zdat, zminp, zscale)
	include 'robcom'
	real zdat, zminp, zscale
	integer icol(3)

	real temp
	integer i
	do 10 i = 1, 3
		temp = zdat-zminp
		temp = temp/zscale
		temp = temp*(rgbhi(i) - rgblo(i)) + rgblo(i)
		icol(i) = nint(temp)
10	continue
		call farkc(icol(1), icol(2), icol(3))
	end

	subroutine barplt(npts, xp, yp, xerr, yerr)
	real xp(*), yp(*), xerr(*), yerr(*)
	include 'robcom'
c do a 'bar' plot
	common/test/xtmin, xtmax, ytmin, ytmax
	do 1000 i = 1, npts
		x = xp(i)
		y = yp(i)
		xl = x - xerr(i)
		xr = x + xerr(i)
		yb = ytmin
		if(xl.gt.xtmax)goto 1000
		if(xr.lt.xtmin)goto 1000
		xl = MAX(xl, xtmin)
		xr = MIN(xr, xtmax)
		yt = MIN(ytmax, y)
		call fbox(xl, yb, xr, yt)
1000	continue
	end

