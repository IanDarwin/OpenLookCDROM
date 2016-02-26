c         PROGRAM ROBOT
C Robin Corbet, February 1991
c corbet@astro.psu.edu
                             
C This is the XView version.
C (A spicy blend of FORTRAN and C routines)
                                           
C Robot is a general purpose graph plotting and data reduction program
C Versions of this program are available that run on
C Sun, Macintosh (Plus and SE), and Fushitsu (sic) machines.
c However, the Mac and Fuji versions are no longer supported.
                                                            
                            
                                              

C These routines are to be considered prototype, developmental software and
C we reserve the right to make changes and improvements to these routines at
C any time.  We retain full copyright protection for the software posted on
C this directory.  Users are authorized to make copies and use this software
C at their own risk. PSU makes no representations about  the
C suitability of  this software for any purpose.  It is provided ``as is''
C without express or implied warranty.

                                             
c The "main" routine is now the C code in main.c
c This calls "robot" as necessary with the appropriate
c Robot instruction. 

	subroutine robotf(inst)
	save
C IF YOU NEED BIGGER DATA ARRAYS CHANGE MSIZ IN
C THE INCLUDE FILE LISTED BELOW!!!
c msiz is size of data arrays, maxpol is the maximum
c polynomial allowed in fitting, ifmax is the depth of
c if tests allowed
	include 'msizcom'
      parameter (maxpol=10, ifmax = 30)
	parameter (maxual = 20)
	character*20 useral1(maxual)
	character*80 useral2(maxual)
	common/chsiz/xchsiz,ychsiz,fsize
c and unit numbers for files being used
	include 'robcom'
	real ain(10)
	include 'fitcom'
	common/test/xtmin, xtmax, ytmin, ytmax
        common/robmod/bpoly,norder,fixpar,ifpar,nmodel,mtype
        common/roba/afit, sigmaa
        real fixpar(maxpar)
        integer ifpar(maxpar)
	real bpoly(10)
        character*15 mtype(maxmod)
        real afit(maxpar), sigmaa(maxpar)
        logical titles, paxes, logxaxis, logyaxis
c is the main title to be at the left or centred?
	integer titpos
	logical minaxes, labx, laby, nogrid, ftext, extickx, exticky
c which axes are to be drawn?
	logical taxis, baxis, laxis, raxis
c dashed or solid lines?
	character*10 lstyle
c default x and y tick marks
	logical dxtck, dytck
c ratio of size of major to minor tick mark size
	real mmrat, dmmrat
	logical pgrid
	logical threed, tdfill, ccode
	logical radec
	common/rad/radec
	common/inrob/inter
	logical first
	logical inter,wasint
c	logical prompt
	common/echoc/echo
 	logical echo
	common/robspm/ismod
        common/atri/ymean,yvar,stime
        double precision stime
	common/curpos/xcurse,ycurse
c user variables
c default file name for file command
	character*80 title,xlab,ylab,pfnam,newfil,inst,text,crud
c multiple instructions on one line
	character*80 instn(15)
	character*80 zlab
c we have two locally stored previous instructions 
c one for interactive, one for non interactive.
c This enables a "file" instruction to be repeated without
c it picking up the "return" at the end of the file.
	character*80 oldinst
	character*80 obinst
c additional titles for optional use
	character*80 title2,title3,title4,xlab2,xlab3,xlab4,ylab2
	character*80 buff, buff2
	character*30 pmode
	character*80 symb
	real apoly(maxpol)
c extra array for use in arithmetic routines
	real z(msiz)
	real xfit(msiz),yfit(msiz),xfite(msiz),yfite(msiz)
	real yfit2(msiz),wk2(msiz)
c allow access to data values from other programs
	integer ifix(10),igrd(10)
	common/table/index,ftext
	integer index(4)
	common/convg/fchi,nloop
	common/limits/xminp,xmaxp,yminp,ymaxp
	common/minors/distxm, distym, mmr
	common/kontor/value,nval,nsval
	real value(10)
	logical nsval
	logical condition(ifmax), iftest
	integer iflevel


c Are things like text, lines etc. to automatically use
c the mouse to get coordinates or are coordinates to be
c prompted for?
	logical kboard

c for rubber banding routines
c	parameter(b1 = 1,  b12 = 1+2, b123 = 1+2+3, b13 = 1+3)

c default values for zooming and panning
	real zrange1, prange1
	parameter(zrange1 = 20., prange1 = 20.)

	common/limit2/xmin,xmax,ymin,ymax
	integer bs1x(10),bs2x(10)
	integer bs1y(10),bs2y(10)
	common/timefl/bs1x,bs2x,bs1y,bs2y,isf,idb
c parameters for grid, nogrid says whether user defined grid has
c been defined (nogrid=false) or is undefined (nogrid=true)
	common/robgrd/nogrid,gstar,gend,gstep
c	only initalise on first call to main
	data first/.TRUE./
	data kboard/.FALSE./
	data ulevel/50/
	data ulev1/50/
	data iftest/.FALSE./
c lines are solid to begin with
	data lstyle/'SOLID'/
	
c default approximate ratio of Minor to Major tick frequency
	data mmrdef/6/
c default arrow size
	data dasize/15.0/
c default arrow angle
	data daang/45./
c default symbol size
	data dssize/2.0/
c no alias defined to begin with
	data noual/0/
	data radec/.false./
	data ismod/0/
	data echo /.false./
	data nogrid /.true./
	data ifil /5/

c First time assignments
	if(first)  then
	robot = .TRUE.
	tdfill = .FALSE.
	threed = .FALSE.
	ifstyle = 2
	xgap = 0.0
	ygap = 0.0
	tasize = dasize
	taang = daang
	tssize = dssize
	mmr = mmrdef
c default convergence conditions for curfit are fractional
c change in chi**2 <0.01, no. of loops = 10
	fchi=0.01
	nloop=10
c there are initially no user defined names
	nouvar = 0
C CURSOR POSITION AT 0,0
	xcurse = 0.0
	ycurse = 0.0
c open units 7 (robotlog.rob) and 8 (robotinf)
c if these fail write to /dev/null
c the precise names are assigned by the ofile routine
	call olf()

	do 88 i=1,10
	  bs1x(i)=1
	  bs2x(i)=0
	  bs1y(i)=1
	  bs2y(i)=0
88	continue
	nval=10
	nsval=.true.
	inter=.true.
	wasint=.false.
	loop = .false.
	havedata = .false.
	radec=.false.
c	prompt=.false.
	ismod=0
c default mode for fitting is weight by y errors
      data imode/1/
c use 20-90% of paper in both x and y directions
      data sxl,syl,sxh,syh/20.,20.,90.,90./
c split plotting window into fmplot(1) by fnplot(1) sub-windows
      data fmplot,fnplot/1.,1./
c and of these plot in sub-window fiplot,fjplot
      data fiplot,fjplot/1.,1./
c plot data using error bars
      data pmode/'BARS'/
c label graph (title, x/y title) and plot axes with labels
	data titles, paxes, labx, laby/4*.TRUE./
	data titpos/0/
	data minaxes, extickx, exticky, pgrid/4*.FALSE./
	data taxis, baxis, laxis, raxis /4*.TRUE./

	data dxtck, dytck/2*.TRUE./
c default ratio of major to minor tick size
	data dmmrat/0.5/
c no points to begin with
	data npts,nptsf/0,0/
c plot text at an angle of 0 degrees
	data tang/0.0/
c use approximately 5 tick marks on x and y axis
c ntickxd and ntickyd are "deafult" values
c (used later if "reset" is called)
	data ntickxd, ntickyd/5,5/
      	ntickx = ntickxd
	nticky = ntickyd
	echo = .TRUE.
	ftext = .TRUE.
	nogrid = .TRUE.
	data title2,title3,title4,xlab2,xlab3,xlab4,ylab2/7*' '/
	data title,xlab,ylab/'Robot',' ',' '/
c default convergence conditions for curfit are fractional
c change in chi**2 <0.01, no. of loops = 10
c      data fchi,nloop/0.01,10/
	fchi=0.01
	nloop=10
c array index indicates which elements in a table contain
c x(1),y(2),dely(3) and delx(4)
c      data index/1,2,3,4/
	index(1)=1
	index(2)=2
	index(3)=3
	index(4)=4
c kludge initial limits
	xminp=0.0
	yminp=0.0
	xmaxp=100.
	ymaxp=100.
	xmin = xminp
	xmax = xmaxp
	ymin = yminp
	ymax = ymaxp
	xtmin = xmin
	ytmin = ymin
	xtmax = xmax
	ytmax = ymax
        call limit(sxl,syl,sxh,syh,xminp,yminp,xmaxp,ymaxp)
	call pusechk(sxl, syl, sxh, syh)

	call scol(0., 0., 0.)

	zrange = zrange1
	prange = prange1
	call szr(zrange)
	call spr(prange)

	first = .FALSE.
	end if
2     continue
      if(.not.INTER)then
	if(loop.and.rloop.gt.1 .and. ifil.eq.uloop)then
		call gloop(inst)
	else
c make an end of file like a RETURN
        	read(ifil,'(a)',err=901,end=80)inst
		goto 81
80		inst = 'RETURN'
81		continue
	end if
      end if
c Remove comments, i.e. a command followed by !
3     continue	

      if(loop .and. rloop .eq. 1 .and.
     +	   ifil .eq. uloop)then
		 call ploop(inst)
	end if


      call update()

c if there's nothing left after the instruction has
c had comments removed skip to end
	if(nnl(inst).le.0)then
		goto 998
	end if

	call remcom(inst)

c see if we're supposed to repeat the previous instruction
c (only set for commands entered interactively)
	if(inst.eq.'^^'.and.inter)inst = oldinst
	if(inst.eq.'^^'.and.(.not.inter))inst = obinst
	if(inter)oldinst = inst
	if(.not.inter)obinst = inst

c check for aliases
	if(noual.ge.1)then
	   call chkal(inst, useral1, useral2, noual)
	end if

c how many instructions in the command?
c (separated by semi-colons)
4	call noinst(inst, instn, ninst)
	do 1001 iinst = 1, ninst
	inst = instn(iinst)
	call remcom(inst)

c if there's nothing left after the instruction has
c had comments removed skip to end
	if(nnl(inst).le.0)then
		goto 1001
	end if

c ensure array ifix is zero (could otherwise be fooled
c if values come from a cursor call)
	call zifix(ifix, 10)

c try and spot any data after instruction
	call chkdat(inst)
c and convert to upper case
	call lctouc(inst)

c try and switch on required items
	call switcher(inst)
                                         
      if(inst.eq.'END'.or.inst.eq.'E')then
      	write(ifil7,'(a)')'return   !End of input'
      else if(inst.eq.'FILE'.or.inst.eq.'F')then
	if(.not.(loop) .or. rloop .eq. 1)
     +        write(ifil7,'(a)')'!Reading from a file:'
      else if(inst.eq.'RETURN')then
        write(ifil7,'(a)')'! End of input from file'
      else if(inst.eq.'MANUAL'.or.inst.eq.'M'.or.inst.eq.'HELP'
     +.or.inst.eq.'H')then
        write(ifil7,'(a)')'! help/manual called interactively'
      else if(inst.ne.' '.and.inst.ne.'IDLE')then
                                                 
c store instructions in file 
	call saveinst(inst)
      end if


c set pointer to begining of buffer
	ibufstr = 1


c see whether there's an IF test in operation
c and whether we should carry out this operation
c or not
	if(ifcheck(inst, condition, iflevel).ne.0)goto 998
            
c end of plotting
      if(inst.eq.'END'.or.inst.eq.'E')then
	   close(ifil7, err = 1000)
1000	   close(ifil8, err = 2000)
2000	   continue
c Start loop
	else if(inst.eq.'LOOP')then
c stupid robot doesn't yet allow nested loops
		if(loop)goto 940
		call sprompt(' Give start, end and step values')
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		maxloop = nint ((ain(2) - ain(1)) / ain (3)) + 1
		vloop = ain(1)
		sloop = ain(3)
		loop = .TRUE.
		call bloop()
c instructions/data to be saved are only from this
c file "level". Indentify this.
		uloop = ifil

		
		rloop = 1
		if(inter)wasint = .TRUE.
c Finish loop
	else if(inst.eq.'ENDLOOP')then
		rloop = rloop + 1
		if(rloop.gt.maxloop)then
			loop = .FALSE.
			if(wasint.and.ifil.eq.5)inter = .TRUE.
		else
c			rinstno = 1
c			rdatano = 1
			vloop = vloop + sloop
			call bloop()
c we now want to go out of interactive mode if this
c is presently on
			inter = .FALSE.
		end if
		
c IF test
	else if(inst.eq.'IF')then
		iftest = .TRUE.
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		iflevel = iflevel + 1
		if(iflevel.gt.ifmax)then
			call xtext('WARNING - TOO MANY NESTED IF TESTS')
		end if
		if(ain(1).gt.0.0)then
			condition(iflevel) = .TRUE.
		else
			condition(iflevel) = .FALSE.
		end if
	else if(inst.eq.'ELSE')then
		condition(iflevel) = .not.(condition(iflevel))
	else if(inst.eq.'ELSEIF')then
		if(iflevel.lt.1)then
			call xtext('WARNING - ELSEIF WITHOUT IF')
		end if
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		if(ain(1).gt.0.0)then
			condition(iflevel) = .TRUE.
		else
			condition(iflevel) = .FALSE.
		end if
	else if(inst.eq.'ENDIF')then
		iflevel = iflevel - 1
		if(iflevel.eq.0)then
			iftest = .FALSE.
		else if(iflevel.lt.0)then
			call xtext('WARNING')
			call xtext('ENDIF without matching IF')
			iflevel = 0
		end if
c read instructions from a file
	else if(inst.eq.'FILE'.or.inst.eq.'F')then
		if(inter) wasint=.TRUE.
		call sprompt('Give file name to read from')
		call sprompt('<default = robotlog>')
		call getit(newfil, 0)
c command aborted?
		if(newfil.eq.'CANCEL/CANCEL')goto 910
        	if(newfil.eq.' ')newfil='robotlog'
        	write(ifil7,'(a)')'!'//newfil
		call fopen(newfil, ulevel, ifil, ifail)
		if(ifail.eq.1)goto 903
        	inter = .FALSE.
		if(.not.(loop) .or. rloop .eq. 1)then
                   write(tstring,*)'Reading robot commands from ',newfil
	    	   call totext(tstring)
		end if
c help facility
      else if(inst.eq.'HELP'.or.inst.eq.'H'.or.inst.eq.'?')then
        if(inter)then
           call help()
        else
          call xtext('HELP FACILITY ONLY FOR INTERACTIVE VERSION')
        end if
      else if(inst.eq.'MANUAL'.or.inst.eq.'M')then
        if(inter)then
          call manual
        else
          PRINT*,'ONLINE MANUAL FACILITY ONLY FOR INTERACTIVE VERSION'
        end if
C PRINT OUT VALUES OF THINGS IN COMMON BLOCKS
      else if(inst.eq.'VALUES')then
        call values(npts,msiz)
C Get laser printer copy of plot window
	else if(inst.eq.'PRINT')then
c update to current values
	  call printhow()
	  call psbusy()
	  call xtext('Printing...')
          call print()
	  call psfree()
	  call xtext('Print Complete')
C Get Cursor position
	else if(inst.eq.'CURSOR')then
	  call cursor(xpos,ypos)
	  write(tstring,*)'Cursor position is:',xpos,ypos
	  call totext(tstring)
	  call xtext('(Variables xcursor and ycursor set to these values)')
c check on nearest neightbours
	  call nneigh(xpos, ypos,xminp, yminp, xmaxp, ymaxp)
	  xcurse=xpos
	  ycurse=ypos
C RESET DEFAULTS
      else if(inst.eq.'RESET')then
        sxl = 20.0
        syl = 20.0
        sxh = 90.0
        syh = 90.0
        fmplot = 1.
        fnplot = 1.
        fiplot = 1.
        fjplot = 1.
	call divcheck(fmplot, fnplot)
	call selcheck(fiplot, fjplot)
	call pusechk(sxl, syl, sxh, syh)
	xgap = 0.0
	ygap = 0.0
	call gapcheck(xgap, ygap)
        imode=1
        ntickx=ntickxd
        nticky=ntickyd
        radec=.false.
        pmode='BARS'
        titles=.TRUE.
	titpos = 0
        echo=.TRUE.
        paxes=.TRUE.
	logxaxis = .FALSE.
	logyaxis = .FALSE.
c array index indicates which elements in a table contain
c x(1),y(2),dely(3) and delx(4)
	index(1)=1
	index(2)=2
	index(3)=3
	index(4)=4
c default Arrow head style
	call astyle(0)
c defualt arrow size
	call asize(dasize)
	tasize = dasize
c defualt arrow angle
	call aangle(daang)
	taang = daang
c one arrow head
	call asingle()
c default symbol size
	call ssize(dssize)
	tssize = dssize
c default fill style
	ifstyle = 2
c default is 2d plotting
	threed = .FALSE.
c 3D plots don't fill page
	tdfill = .FALSE.
c don't colour code 3D plots
	ccode = .FALSE.
c no minor tick marks
	minaxes=.FALSE.
c default size for minor tick marks
	mmrat = dmmrat
c default frequency for minor tick marks
	mmr = mmrdef
c use internal tick marks
	extickx=.FALSE.
	exticky=.FALSE.
c no grid
	pgrid = .FALSE.
c use default sizes for tick marks
	dxtck = .TRUE.
	dytck = .TRUE.
c label axes
        labx=.TRUE.
        laby=.TRUE.
c use all four axes
	laxis = .TRUE.
	raxis = .TRUE.
	baxis = .TRUE.
	taxis = .TRUE.
c text at angle zero
        tang=0.0
        call zreset(z,msiz)
c default fitting parameters
        fchi=0.01
        nloop=10
c input file has a text header
	ftext = .TRUE.
        title2=' '
        title3=' '
        title4=' '
        xlab2=' '
        xlab3=' '
        xlab4=' '
        ylab2=' '


c get rid of all user defined variables
	nouvar = 0
c pen colour back to black
	call scol(0., 0., 0.)
c "normal" font
	call tstyle(0)
c Times-Roman family
	call tfamly(0)
c at zero degrees
	call txangm(0.0)
c narrowest lines
	call lwidth(0)
c solid lines
	call nodash()
	lstyle = 'SOLID'
c 14 point text
	chsz = 14.0
	call txtset(chsz)
	zrange = zrange1
	prange = prange1
	call szr(zrange)
	call spr(prange)

           
c define plot limits
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
c straight reset of z array
      else if(inst.eq.'ZRESET')then
        call zreset(z,msiz)
c convert from ra/dec to galactic coordinates
      else if(inst.eq.'RADECTOGAL')then
        call eqtgal(x,y,npts)
c grid for ra/dec (galactic plots)
      else if(inst.eq.'RADECGRID')then
c multiply ra by cos dec if not already done so
        if(.not.radec)then
          call raconv(x,y,npts)
          radec=.true.
        end if
        call radecg(fiplot,fjplot,fmplot,fnplot,sxl,syl,sxh,syh)
c plot ra/dec points
      else if(inst.eq.'RADECPLOT')then
          if(.NOT.RADEC)then
            call raconv(x,y,npts)
            radec=.true.
          end if
          call radecp(x,y,dely,npts,pmode,isymb)
c return to reading instructions from default input
      else if(inst.eq.'RETURN')then
	ulevel = ulevel - 1
        close(unit=ulevel,err=904)
	if(ulevel.eq.ulev1)then
		ifil = 5
	else
        	ifil = ulevel - 1
	end if
C RETURN TO INTERACTIVE MODE IF WAS INTERACTIVE BEFORE CALL
C TO FILE
        if(wasint.and.ifil.eq.5)inter=.true.
C NEW PAGE
      else if(inst.eq.'NXTPAG'.or.inst.eq.'N')then
	call nxtpag()
c start a new plotting area
	else if(inst.eq.'NEWPLOTAREA')then
		call newpa()
c flush graphics
	else if(inst.eq.'FLUSH')then
		call gshow()
c wait for a while
	else if(inst.eq.'WAIT')then
		call sprompt('How long to wait?')
		call getit(crud, 1)
		call dcode(crud, ain, ifix, igrd, k)
		wait = waiter(ain(1))
c temporary pause in plotting
	else if(inst.eq.'PAUSE')then
		call toarkt('Paused - Press button to continue')
c abort program
	else if(inst.eq.'ABORT')then
		call sprompt('Really abort?')
		call sprompt('(y/n)')
		call getit(crud, 1)
		call lctouc(crud)
		if(crud .eq. 'Y' .or. crud .eq. 'YES')then
			call rabort()
		end if
c how much to zoom by for zoomin or zoomout commands
	else if(inst.eq.'ZOOMVALUE')then
		call sprompt('What percentage to zoom by?')
		write(tstring, *)'Present value = ',zrange
		call sprompt(tstring)           
		call getit(crud, 1)
		call dcode(crud, ain, ifix, igrd, k)
c restrict to zoom values less than 50.
		zrange = MIN(ain(1), 50.)
c make sure panel is updated
		call szr(zrange)

	else if(inst.eq.'PANVALUE')then
		call sprompt('What percentage to pan by?')
		write(tstring, *)'Present value = ',prange
		call sprompt(tstring)           
		call getit(crud, 1)
		call dcode(crud, ain, ifix, igrd, k)
c restrict to pan values less than 50.
		prange = MIN(ain(1), 50.)
c make sure panel is updated
		call spr(prange)

c Zoom and Pan commands
	else if(inst.eq.'ZOOMIN')then
		xrange = (xmaxp - xminp)*zrange/200.
		yrange = (ymaxp - yminp)*zrange/200.
		write(inst, *)'dflimits (',xminp+xrange,
     +              yminp+yrange, xmaxp-xrange, ymaxp-yrange,
     +		    '); n; g'
		goto 4
	else if(inst.eq.'ZOOMOUT')then
		xrange = (xmaxp - xminp)*zrange/200.
		yrange = (ymaxp - yminp)*zrange/200.
		write(inst, *)'dflimits (',xminp-xrange,
     +              yminp-yrange, xmaxp+xrange, ymaxp+yrange,
     +		    '); n; g'
		goto 4
	else if(inst.eq.'PANRIGHT')then
		call setpan(inst, prange, 0)
		goto 4
	else if(inst.eq.'PANLEFT')then
		call setpan(inst, prange, 1)
		goto 4
	else if(inst.eq.'PANUP')then
		call setpan(inst, prange, 2)
		goto 4
	else if(inst.eq.'PANDOWN')then
		call setpan(inst, prange, 3)
		goto 4

c change pen colour
      else if(inst.eq.'PENCOLOUR'.or.inst.eq.'PENCOLOR'
     +.or.inst.eq.'COLOUR'.or.inst.eq.'COLOR')then
         call sprompt('Give colour')
         call sprompt('(white(W),blue(B),green(G),yellow(Y),')
         call sprompt('red(R),magenta(M),cyan(C),black(K))')
	call getit(crud, 1)
	call lctouc(crud)
	if(crud.eq.'BLACK'.or.crud.eq.'K')then
		call scol(0., 0., 0.)
	else if(crud.eq.'WHITE'.or.crud.eq.'W')then
		call scol(255., 255., 255.)
	else if(crud.eq.'BLUE'.or.crud.eq.'B')then
		call scol(0., 0., 255.)
	else if(crud.eq.'GREEN'.or.crud.eq.'G')then
		call scol(0., 255., 0.)
	else if(crud.eq.'YELLOW'.or.crud.eq.'Y')then
		call scol(255., 255., 0.)
	else if(crud.eq.'RED'.or.crud.eq.'R')then
		call scol(255., 0., 0.)
	else if(crud.eq.'MAGENTA'.or.crud.eq.'M')then
		call scol(0., 255., 255.)
	else if(crud.eq.'CYAN'.or.crud.eq.'C')then
		call scol(255., 0., 255.)
	else
		call xtext('ERROR: UNRECOGNISED COLOR')
	end if
c specify colour in terms of red, green and blue intensities
c (scaled from 0 - 255)
      else if(inst.eq.'RGB')then
	call sprompt('Give R, G and B values')
	call sprompt('must be between 0-255')
	call getit(crud, 1)
	call dcode(crud, ain, ifix, igrd, k)
	call scol(ain(1), ain(2), ain(3)) 
c just a colour for the data
	else if(inst.eq.'RGBDATA')then
		call sprompt('Give R, G and B values for data')
		call sprompt('must be between 0-255')
		call getit(crud, 1)
		call dcode(crud, ain, ifix, igrd, k)
		call sdatcol(ain(1), ain(2), ain(3)) 
c just a colour for the NON-data
	else if(inst.eq.'RGBREST')then
		call sprompt('Give R, G and B values for non-data')
		call sprompt('must be between 0-255')
		call getit(crud, 1)
		call dcode(crud, ain, ifix, igrd, k)
		rgb(1) = nint(ain(1))
		rgb(2) = nint(ain(2))
		rgb(3) = nint(ain(3))
		call farkc(rgb(1), rgb(2), rgb(3))
	
c use mouse as default for getting coordinats in interactive mode
	else if(inst.eq.'USEMOUSE'.or.inst.eq.'MOUSE')then
		kboard = .FALSE.
c use keyboard input as default for getting coordinats in interactive mo
	else if(inst.eq.'NOUSEMOUSE'.or.inst.eq.'KEYBOARD')then
		kboard = .TRUE.
C ENABLE ECHO OF COMMANDS (DEFAULT)
      else if(inst.eq.'ECHO')then
        echo=.true.
C DISABLE ECHO OF COMMANDS (DEFAULT)
      else if(inst.eq.'NOECHO')then
        ECHO=.FALSE.
C NO TITLES
      else if(inst.eq.'NOTITLES')then
        titles=.false.
C ENABLE TITLES
      else if(inst.eq.'TITLES')then
        titles=.true.
C Center the title (default is place at left)
      else if(inst.eq.'CENTRETITLE'.or.inst.eq.'CENTERTITLE')then
	titpos = 1
c put the title at the right
      else if(inst.eq.'RIGHTTITLE')then
	titpos = 2
c back to default title position
      else if(inst.eq.'LEFTTITLE')then
	titpos = 0
C NO AXES
      else if(inst.eq.'NOAXES')then
        paxes=.FALSE.
C ENABLE AXES
      else if(inst.eq.'AXES')then
        paxes=.TRUE.
c enable log axes
	else if(inst.eq.'LOGAXES')then
		logxaxis = .TRUE.
		logyaxis = .TRUE.
	else if(inst.eq.'LINEARAXES')then
		logxaxis = .FALSE.
		logyaxis = .FALSE.
	else if(inst.eq.'LOGXAXIS')then
		logxaxis = .TRUE.
	else if(inst.eq.'LOGYAXIS')then
		logyaxis = .TRUE.
	else if(inst.eq.'LINEARXAXIS')then
		logxaxis = .FALSE.
	else if(inst.eq.'LINEARYAXIS')then
		logyaxis = .FALSE.
c enable minor axes
	else if(inst.eq.'MINORAXES')then
		minaxes = .TRUE.
c disable minor axes
	else if(inst.eq.'NOMINORAXES')then
		minaxes = .FALSE.
c External tick marks
	else if(inst.eq.'EXTERNALTICKS')then
		extickx = .TRUE.
		exticky = .TRUE.
c Internal tick marks
	else if(inst.eq.'INTERNALTICKS')then
		extickx = .FALSE.
		exticky = .FALSE.
c External X tick marks
	else if(inst.eq.'EXTERNALXTICKS')then
		extickx = .TRUE.
c Internal X tick marks
	else if(inst.eq.'INTERNALXTICKS')then
		extickx = .FALSE.
c External Y tick marks
	else if(inst.eq.'EXTERNALYTICKS')then
		exticky = .TRUE.
c Internal Y tick marks
	else if(inst.eq.'INTERNALYTICKS')then
		exticky = .FALSE.
c Specify tick size for X axis
	else if(inst.eq.'XTICKSIZE')then
		call sprompt('Give size of tick marks for X axis')
		call sprompt('units are Y dimensions')
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		by = ain(1)
		dxtck = .FALSE.
c Specify tick size for Y axis
	else if(inst.eq.'YTICKSIZE')then
		call sprompt('Give size of tick marks for Y axis')
		call sprompt('units are X dimensions')
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		bx = ain(1)
		dytck = .FALSE.
c speify tick size for minor tick marks
	else if(inst.eq.'MINORTICKSIZE')then
		call sprompt('Give ratio of minor to major tick sizes')
		write(tstring, *)'Present value = ',mmrat
		call sprompt(tstring)
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		mmrat = ain(1)
c speify tick size for minor tick marks
	else if(inst.eq.'MINORTICKFREQUENCY')then
		call sprompt('Give ratio of minor to major tick frequency')
		write(tstring, *)'Present value = ',mmr
		call sprompt(tstring)
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		mmr = nint(ain(1))
c Use default tick size
	else if(inst.eq.'DEFAULTTICKSIZE')then
		dxtck = .TRUE.
		dytck = .TRUE.
		mmrat = dmmrat
c Show all axes
	else if(inst.eq.'ALLAXES')then
		taxis = .TRUE.
		baxis = .TRUE.
		laxis = .TRUE.
		raxis = .TRUE.
c spefications for individual axes
	else if(inst.eq.'TOPAXIS')then
		taxis = .TRUE.
	else if(inst.eq.'NOTOPAXIS')then
		taxis = .FALSE.
	else if(inst.eq.'BOTTOMAXIS')then
		baxis = .TRUE.
	else if(inst.eq.'NOBOTTOMAXIS')then
		baxis = .FALSE.
	else if(inst.eq.'LEFTAXIS')then
		laxis = .TRUE.
	else if(inst.eq.'NOLEFTAXIS')then
		laxis = .FALSE.
	else if(inst.eq.'RIGHTAXIS')then
		raxis = .TRUE.
	else if(inst.eq.'NORIGHTAXIS')then
		raxis = .FALSE.
c plot grid
	else if(inst.eq.'PLOTGRID')then
		pgrid = .TRUE.
	else if(inst.eq.'NOPLOTGRID')then
		pgrid = .FALSE.
c no axis labels
      else if(inst.eq.'NOLABELAXES')then
	labx = .FALSE.
	laby = .FALSE.
C ENABLE AXIS LABELS
      else if(inst.eq.'LABELAXES')then
        labx=.TRUE.
        laby=.TRUE.
C DON'T LABEL X AXIS
      else if(inst.eq.'NOLABELXAXIS')then
        labx=.FALSE.
c do label x axis
      else if(inst.eq.'LABELXAXIS')then
        labx=.TRUE.
c don't label y axis
      else if(inst.eq.'NOLABELYAXIS')then
        laby=.FALSE.
c do label y axis
      else if(inst.eq.'LABELYAXIS')then
        laby=.TRUE.
      
            
c change the columns where filread expects to find the x,y,delx and dely
c values
      else if(inst.eq.'TABLE')then
          call sprompt('Give columns for x, y, y err and x err')
          write(tstring, *)'current values are: ',index
	  call sprompt(tstring)
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        index(1)=nint(ain(1))
        index(2)=nint(ain(2))
        index(3)=nint(ain(3))
        index(4)=nint(ain(4))
c there is no text at the top of the input file
      else if(inst.eq.'INFILENOTEXT')then
        ftext=.FALSE.
C THERE ARE THREE LINES OF TEXT AT THE TOP OF THE INPUT FILE
C DEFAULT
      else if(inst.eq.'INFILETEXT')then
        ftext=.TRUE.
C DIVISION BETWEEN TWO BIG IF BLOCKS
                                    
C READ FILENAME FOR PLOTTING
      else if(inst.eq.'PLOTFILE'.or.inst.eq.'P'
     +			.or.inst.eq.'DATAFILE')then
	call sprompt('Give name of input data file')
	write(tstring, *)'<default = ',pfnam,'>'
	call sprompt(tstring)
	call getit(crud, 1)
	if(crud.ne.' ')pfnam=crud
        call filred(pfnam,title,xlab,ylab,x,y,delx,dely,msiz,npts,
     +xfit,yfit,xfite,yfite,nptsf,ifail,1)
        if(npts.le.1)goto 991
        if(ifail.eq.1)goto 920
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
c reset RADEC conversion flag
	radec = .FALSE.
                                                                      
c FITS "standard" file - one dimension only
c allow undocumented 'RFITS' for IRAF similarity
	else if(inst.eq.'FITS'.or.inst.eq.'RFITS')then
	call fitred(pfnam, title, xlab, ylab, x, y, delx, dely,
     +msiz, npts, xfit, yfit, xfite, yfite, nptsf, ifail, istar)
	if(npts.le.1)goto 991
	if(ifail.eq.1)goto 920
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
c FITS "standard" file - one dimension only
c allow undocumented 'RFITS' for IRAF similarity
c in opposite direction
	else if(inst.eq.'FITSX'.or.inst.eq.'RFITSX')then
	call fitred2(pfnam, title, xlab, ylab, x, y, delx, dely,
     +msiz, npts, xfit, yfit, xfite, yfite, nptsf, ifail, istar)
	if(npts.le.1)goto 991
	if(ifail.eq.1)goto 920
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
c FITS "3D"
c place restrictions on parameters to be read
c only certain "hard-wired" values can be restricted
	else if(inst.eq.'FITS3DRESTRICT')then
		if(inter)then
		  call sprompt('Give parameter to be restricted')
		  call xtext('0 = xmin, 1 = xmax, 2 = ymin, 3 = ymax')
		  call xtext('4 = gmin, 5 = gmax, 6 = split event level')
		  call xtext('7 = File name')
		end if
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		itemp = nint(ain(1))
		if(itemp.le.6)then
		   call sprompt('Give value')
		   call getit(buff, 1)
		   call dcode(buff, ain, ifix, igrd, k)
		   call fadset(itemp, nint(ain(1)))
		else
		   call sprompt('Give file name')
		   call getit(buff, 1)
		   call fdnam(buff)
		end if
        
c FITS "3D" (i.e. binary table) read
	else if(inst.eq.'FITS3D')then
	call fits3d(pfnam, title, xlab, ylab, x, y, delx, dely,
     +msiz, npts, xfit, yfit, xfite, yfite, nptsf, ifail, istar)
	if(npts.le.1)goto 991
	if(ifail.eq.1)goto 920
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
	
 
 
 
c Ginga X-ray spectrum
      else if(inst.eq.'SPECTRUM')then
		call sprompt('Give name of Ginga spectrum')
		call getit(pfnam, 1)
      call specred(pfnam,title,xlab,ylab,
     +x,y,delx,dely,npts)
        if(npts.le.1)goto 991
        if(ifail.eq.1)goto 920
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
                                                                      
c read data from plotfile and add to what exists already, then rescale
      else if(inst.eq.'MOREPLOT')then
		call sprompt('Give name of data file to be added on')
       
	call getit(pfnam, 1)
	if(pfnam.eq.' ')pfnam=title
                            
        call filred(pfnam,title,xlab,ylab,x,y,delx,dely,msiz,npts,
     +xfit,yfit,xfite,yfite,nptsf,ifail,npts+1)
        if(npts.le.1)goto 991
        if(ifail.eq.1)goto 920
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
                                                                      
C TYPE IN DATA FOR PLOTTING
      else if(inst.eq.'TYPE')then
        call type(x,y,delx,dely,npts,title,xlab,ylab,msiz,1,1)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
	radec = .FALSE.
                                                                      
c type in data for plotting and add to existing data
c No longer re-reads lables for plot
      else if(inst.eq.'MORETYPE'.or.inst.eq.'TYPEMORE')then
        call type(x,y,delx,dely,npts,title,xlab,ylab,msiz,npts+1,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)

c type in data for plotting without rescaling
c No longer re-reads lables for plot
      else if(inst.eq.'OVERTYPE')then
        call type(x,y,delx,dely,npts,title,xlab,ylab,msiz,1,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
	radec = .FALSE.
                                       
C RESCALE PLOT AREA, E.G. AFTER ARITMETIC OPERATION ON AN ARRAY
      else if(inst.eq.'RESCALE')then
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
          call fitlim(4,xfit,yfit,xfite,yfite,
     +x,y,delx,dely,npts,nptsf)
C READ FILENAME FOR PLOTTING *WITHOUT RESETING DATA LIMITS*
      else if(inst.eq.'OVERPLOT')then
	  call sprompt('Give file name')
	call getit(pfnam, 1)
        call filred(pfnam,title,xlab,ylab,x,y,delx,dely,msiz,npts,
     +xfit,yfit,xfite,yfite,nptsf,ifail,1)
	radec = .FALSE.
        if(IFAIL.eq.1)GOTO 920
C RESTRICT DATA FOR FITTING, EVALUATING AREA UNDER CURVE ETC.
C RESTRICT BY X VALUES
      else if(inst.eq.'XFITLIMITS')then
        call fitlim(1,xfit,yfit,xfite,yfite,
     +x,y,delx,dely,npts,nptsf)
C RESTRICT BY Y VALUES
      else if(inst.eq.'YFITLIMITS')then
        call fitlim(2,xfit,yfit,xfite,yfite,
     +x,y,delx,dely,npts,nptsf)
C RESTRICT BY BOTH X AND Y VALUES
      else if(inst.eq.'XYFITLIMITS')then
        call fitlim(3,xfit,yfit,xfite,yfite,
     +x,y,delx,dely,npts,nptsf)
C OUTPUT DATA POINTS TO A PLOT FILE
      else if(inst.eq.'WRITEDATA')then
		call sprompt('Give name of output file')
		call sprompt('<RETURN> = "write to the screen"')
	call getit(pfnam, 1)
	if(pfnam.ne.' ')then
		write(tstring, *)'Writing to file: ',PFNAM
		call xtext(tstring)
		call xtext('(Only data in defined area is written)')
	end if
        call filwrt(pfnam,title,xlab,ylab,x,y,delx,dely,npts)
        if(npts.le.1)goto 991
C REDEFINE TITLE FOR PLOT
      else if(inst.eq.'TITLE')then
	  call sprompt('Give new title for the plot')
	  call sprompt('(Present title is:')
	  call sprompt(title)
	  call getit(title, 1)
C REDEFINE X LABEL FOR PLOT
      else if(inst.eq.'XLABEL')then
	  call sprompt('Give new X label')
	call getit(xlab, 1)
C REDEFINE Y LABEL FOR PLOT
      else if(inst.eq.'YLABEL')then
	  call sprompt('Give new Y label')
	call getit(ylab, 1)
C Z axis label for use in 3D plots
      else if(inst.eq.'ZLABEL')then
	  call sprompt('Give new Z label')
	call getit(zlab, 1)
C REDEFINE OPTIONAL TITLE NO. 2 FOR PLOT
      else if(inst.eq.'TITLE2')then
        call sprompt('Give new title 2')
	call getit(title2, 1)
C REDEFINE OPTIONAL TITLE NO. 3 FOR PLOT
      else if(inst.eq.'TITLE3')then
        call sprompt('Give new title 3')
	call getit(title3,  1)
C REDEFINE OPTIONAL TITLE NO. 4 FOR PLOT
      else if(inst.eq.'TITLE4')then
        call sprompt('Give new title 4')
	call getit(title4,  1)
C REDEFINE OPTIONAL X LABEL NO. 2 FOR PLOT
      else if(inst.eq.'XLABEL2')then
        call sprompt('Give new X label 2')
	call getit(xlab2,  1)
C REDEFINE OPTIONAL X LABEL NO. 3 FOR PLOT
      else if(inst.eq.'XLABEL3')then
        call sprompt('Give new X label 3')
	call getit(xlab3,  1)
c redefine optional x label no. 4 for plot
      else if(inst.eq.'XLABEL4')then
        call sprompt('Give new X label 4')
	call getit(xlab4,  1)
c redefine optional y label no. 2 for plot
      else if(inst.eq.'YLABEL2')then
        call sprompt('Give new Y label 2')
	call getit(ylab2,  1)
c general access to other titles
	else if(inst.eq.'OTHERTITLES')then
		call sprompt('Which label?')
		call sprompt('Title2, Title3, Title4, Xlabel2')
		call sprompt('Xlabel3, Xlabel4, Ylabel2, Zlabel')
		call getit(buff, 1)
		call lctouc(buff)
		call sprompt('Give text')
		call getit(buff2, 1)
		if(buff.eq.'TITLE2')then
			title2 = buff2
		else if(buff.eq.'TITLE3')then
			title3 = buff2
		else if(buff.eq.'TITLE4')then
			title4 = buff2
		else if(buff.eq.'XLABEL2')then
			xlab2 = buff2
		else if(buff.eq.'XLABEL3')then
			xlab3 = buff2
		else if(buff.eq.'XLABEL4')then
			xlab4 = buff2
		else if(buff.eq.'YLABEL2')then
			ylab2 = buff2
		else if(buff.eq.'ZLABEL')then
			zlab = buff2
		endif
c Position plot window at specified place
	else if(inst .eq. 'POSITIONPLOT')then
		call sprompt('Give coordinates of top left corner')
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		call posplot(ain(1), ain(2))

C DIVIDE PLOTTING WINDOW INTO FNPLOT BY FMPLOT SUB-WINDOWS
      else if(inst.eq.'DIVWINDOW')then
	call sprompt('Give number of windows in X and Y directions')
	call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	fmplot = ain(1)
	fnplot = ain(2)

c are fiplot and fjplot still valid?
	if(fiplot .gt. fmplot)fiplot = fmplot
	if(fjplot .gt. fnplot)fjplot = fnplot
	call selcheck(fiplot, fjplot)

C AND CALL LIMIT IF DATA LIMITS EXIST
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
	call divcheck(fmplot, fnplot)

C SELECT ONE OF THE PREVIOUSLY DEFINED SUB-WINDOWS
      else if(inst.eq.'SELWINDOW')then
         call sprompt('Select window in X and Y directions')
         write(tstring, *)
     +	'(Page now ',nint(fmplot),' by ',nint(fnplot),' windows)'
       	call sprompt(tstring)
                             
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	if(ain(1).gt.fmplot)then
		write(tstring,*)'WARNING: x window =', nint(fiplot)
		call xtext(tstring)
		write(tstring, *)
     +		  'larger than value set in DIVWINDOW,', nint(fmplot)

		call xtext(tstring)
	else
		fiplot = ain(1)
	end if
	if(ain(2).gt.fnplot)then
		write(tstring,*)'WARNING: y window =', nint(fjplot)
		call xtext(tstring)
		write(tstring, *)
     +		  'larger than value set in DIVWINDOW,', nint(fnplot)
		call xtext(tstring)
	else
		fjplot = ain(2)
	end if
C AND CALL LIMIT IF DATA LIMITS EXIST
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
	call selcheck(fiplot, fjplot)
C CHANGE WAY OF PLOTTING POINTS (DEFAULT IS ERROR BARS)
      else if(inst.eq.'PLOTMODE')then
        if(inter)then
          call sprompt('Give way of plotting data')
          call xtext('(Bars, Bars2, Boxes, Crosses, Diamonds, Lines')
          call xtext('Dashedlines, Histogram, Symbol')
	  call xtext('Filledboxes, Ellipses, Nodata')
          call xtext('Nice-symbol, Filled-histogram')
	  call xtext('Pillar, Bar-Graph, Spline)')
          write(tstring, *)'Current mode is: ',pmode
	call sprompt(tstring)
        end if
        call getit(pmode,  1)
	call lctouc(pmode)
c reduce possible names by 1
	if(pmode .eq. 'NICE-SYMBOL') pmode = 'NICE'
        if(pmode.eq.'SYMBOL')then
         call sprompt('Give character string to be plotted')
        call getit(symb,  1)
        else if(pmode.eq.'NICE')then
	call sprompt('Specify symbol to be  be plotted')
        call sprompt('Star, Box, Cross, Circle, Filled-Circle')
	call sprompt('Filled-Box, Diamond, Filled-Diamond')
        call sprompt('Triangle, Filled-Triangle')
	call smenu(1)
        call getit(buff, 1)
	call lctouc(buff)
	if(buff.eq.'STAR')then
		isymb = 1
	else if(buff.eq.'BOX')then
		isymb = 2
	else if(buff.eq.'CROSS')then
		isymb = 3
	else if(buff.eq.'CIRCLE')then
		isymb = 4
	else if(buff.eq.'FILLED-CIRCLE'.or.buff.eq.'FILLEDCIRCLE')then
		isymb = 5
	else if(buff.eq.'FILLED-BOX'.or.buff.eq.'FILLEDBOX')then
		isymb = 6
	else if(buff.eq.'DIAMOND')then
		isymb = 7
	else if(buff.eq.'FILLED-DIAMOND'.or.buff.eq.'FILLEDDIAMOND')then
		isymb = 8
	else if(buff.eq.'TRIANGLE')then
		isymb = 9
	else if(buff.eq.'FILLED-TRIANGLE'.or.buff.eq.'FILLEDTRIANGLE')then
		isymb = 10
	else
        	call dcode(buff,ain,ifix,igrd,k)
c in case there was garbage in the reply set to 1 as default
		isymb = MAX(nint(ain(1)), 1)
	end if
	end if
c change size of symbols used in "nice symbol"
	else if(inst.eq.'SYMBOLSIZE')then
          call sprompt('Give size of symbols')
	  write(tstring, *)'(symbol size is now', tssize,')'
          call sprompt(tstring)
	  call getit(buff, 1)
	  call dcode(buff,ain,ifix,igrd,k)
	  tssize = ain(1)
	  call ssize(tssize)
	else if(inst.eq.'3DCOLOURCODE'.or.
     +			inst.eq.'3DCOLORCODE')then
		ccode = .TRUE.
		call sprompt('Give colour of lowest data point')
		call sprompt('(Red, green and blue values)')
		call getit(buff, 1)
		call dcode(buff,ain,ifix,igrd,k)
		rgblo(1) = nint(ain(1))
		rgblo(2) = nint(ain(2))
		rgblo(3) = nint(ain(3))
		call sprompt('Give colour of highest data point')
		call sprompt('(Red, green and blue values)')
		call getit(buff, 1)
		call dcode(buff,ain,ifix,igrd,k)
		rgbhi(1) = nint(ain(1))
		rgbhi(2) = nint(ain(2))
		rgbhi(3) = nint(ain(3))
	else if(inst.eq.'3DNOCOLOURCODE'.or.
     +			inst.eq.'3DNOCOLORCODE')then
		ccode = .FALSE.
	
c How to fill if plotmode is "fill"
	else if(inst .eq. 'FILLSTYLE')then
		call sprompt('Give fill style')
		call sprompt('Zero, PlotMinimum, PlotMaximum, Join')
		call sprompt('Specify, DataMinimum, DataMaximum')
		call getit(buff, 1)
		call lctouc(buff)
		if(buff .eq. 'ZERO')then
			ifstyle = 1
		else if(buff .eq. 'PLOTMINIMUM')then
			ifstyle = 2
		else if(buff .eq. 'PLOTMAXIMUM')then
			ifstyle = 3
		else if(buff .eq. 'JOIN')then
			ifstyle = 0
		else if(buff .eq. 'SPECIFY')then
			ifstyle = 4
		else if(buff .eq. 'DATAMINIMUM')then
			ifstyle = 5
		else if(buff .eq. 'DATAMAXIMUM')then
			ifstyle = 6
		else
			call dcode(buff, ain, ifix, igrd, k)
			ifstyle = nint(ain(1))
		end if

c user specified value
		if(ifstyle .eq. 4)then
			call sprompt('Give value to fill to')
			write(tstring, *)'Present value =',fillvalue
			call sprompt(tstring)
			call getit(buff, 1)
			call dcode(buff, ain, ifix, igrd, k)
			fillvalue = ain(1)
		end if


C FOLD DATA ON SPECIFIED PERIOD AND EPOCH
C FOLDS FIT ARRAY AND OVERWRITES DATA ARRAYS
      else if(inst.eq.'FOLD')then
	  call sprompt('Give period and epoch')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	period = ain(1)
	epoch = ain(2)
        call fold(x,y,delx,dely,npts,nptsf,xfit,yfit,xfite,yfite,
     +period,epoch)
        write(xlab,*)'Phase (period = ',period,')'
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C FOLD DATA ON BEST PERIOD FOUND FROM PERIODOGRAM
      else if(inst.eq.'FOLDBEST')then
        call fold(x,y,delx,dely,npts,nptsf,xfit,yfit,xfite,yfite,
     +bp,epoch)
        write(xlab,*)'Phase (period = ',BP,')'
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C FOLD DATA ON SPECIFIED PERIOD AND EPOCH AND PUT INTO PHASE BINS
C THIS OVERWRITES THE X AND Y ARRAYS
      else if(inst.eq.'BINFOLD')then
         call sprompt('Give period, epoch and no. of bins')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	period = ain(1)
	epoch = ain(2)
	nbins = nint(ain(3))
        call bfold(xfit,yfit,xfite,yfite,nptsf,
     +period,epoch,nbins,chisqr,1)
        write(xlab,*)'Phase (period = ',period,')'
        write(tstring, *)'Chi**2 of folded curve = ',chisqr
	call totext(tstring)
        call write8(tstring)
C USE COPYXY TO COPY FIT ARRAY TO DATA (OPPOSITE OF NORMAL USE)
       call copyxy(xfit,yfit,xfite,yfite,
     +x,y,delx,dely,nptsf,npts)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C FOLD DATA ON BEST PERIOD AND EPOCH AND PUT INTO PHASE BINS
C THIS OVERWRITES THE X AND Y ARRAYS
      else if(inst.eq.'BINFOLDBEST')then
        call bfold(xfit,yfit,xfite,yfite,nptsf,
     +bp,epoch,nbins,chisqr,1)
        write(tstring, *)'Chi**2 of folded curve is: ',CHISQR
	call totext(tstring)
C USE COPYXY TO COPY FIT ARRAY TO DATA (OPPOSITE OF NORMAL USE)
       call copyxy(xfit,yfit,xfite,yfite,
     +x,y,delx,dely,nptsf,npts)
        write(xlab,*)'Phase (period = ',bp,')'
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C FIND PERIOD BY FOLDING AND CALCULATING CHI**2
      else if(inst.eq.'FINDPERIOD')then
        call pfind(xfit,yfit,xfite,yfite,nptsf,bp,nbins)
C FIND PERIOD BY FOLDING AND CALCULATING CHI**2, THEN PLOT THE
C RESULTS
      else if(inst.eq.'PERIODOGRAM')then
          call sprompt('Give start period, end period,')
     	  call sprompt('step size and no. of')
     	  call sprompt('bins to fold data into')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	start = ain(1)
	stop = ain(2)
	step = ain(3)
	fnbin = ain(4)
        call pgram(xfit,yfit,xfite,yfite,nptsf,bp,nbins,
     +sxh,sxl,syh,syl,start,stop,step,fnbin, wk2, msiz)
C SMOOTH DATA BY CONVOLVING WITH TRIANGLE
      else if(inst.eq.'SMOOTH')then
        call smooth(y,npts,1,msize)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C SMOOTH DATA N TIMES
      else if(inst.eq.'SMOOTHN')then
         call sprompt('Give no. of times to smooth data')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	ntime = nint(ain(1))
        call smooth(y,npts,ntime,msize)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C REBIN DATA
      else if(inst.eq.'REBIN')then
        call rebin(x,y,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C REPLACE Y VALUES WITH RANDOM NUMBERS WHICH HAVE THE SAME
C MEAN AND STANDARD DEVIATION AS THE ORIGINAL NUMBERS
      else if(inst.eq.'RANDOMISE'.or.inst.eq.'RANDOMIZE')then
        call random(x,y,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c define a user variable
	else if(inst.eq.'VARIABLE')then
		call sprompt('Give variable name')
		call sprompt('<return> = list variables in use')
		call getit(buff, 1)
		if(buff.eq.' ')then
			call lvars()
		else
			call addvar(buff)
		end if
c assign values to user variables
	else if(inst.eq.'ASSIGN')then
		call sprompt('Which variable?')
		call sprompt('A1-5 or user defined?')
		call getit(buff, 1)
c it was too complicated to keep case sensitivity
		call lctouc(buff)
		call sprompt('Give value')
		call getit(buff2, 1)
		call dcode(buff2, ain, ifix, igrd, k)
		call assign(buff, ain(1), ifail)
	if(ifail.eq.1)then
			call xtext('***WARNING***')
			call xtext('CAN''T USE THAT VARIABLE NAME')
		end if
c write out the value of a variable
	else if(inst .eq. 'WRITE')then
		call sprompt('Give name of variable to write')
		call getit(buff, 1)
		call dcode(buff, ain, ifix, igrd, k)
		write(tstring, *)buff(1:nnl(buff)), ' ='
		call xtext(tstring)
		call write8(tstring)
		do i = 1, k
		   write(tstring, *)ain(i)
		   call xtext(tstring)
		   call write8(tstring)
		end do

c define an alias
	else if(inst.eq.'ALIAS')then
		call sprompt('Give alias name')
		call sprompt('<return> = list aliases in use')
		call getit(buff, 1)
		if(buff.eq.' ')then
		   call lalias(useral1, useral2, noual)
		else
c it was too complicated to keep case sensitivity
		  call sprompt('Alias it to what?')
		  call getit(ainbuf, 1)
		  call adalias(buff, ainbuf, useral1, useral2,
     +					noual, maxual)
c update menu
		  call aalias(useral1, noual)
c show the button if needed
		  if(noual .eq. 1) call salias()
		end if
c undefine an alias
	else if(inst.eq.'UNALIAS')then
		call sprompt('Give alias to disable')
		call getit(buff, 1)
		call lctouc(buff)
		call dalias(buff, useral1, useral2, noual)
c update menu
		  call aalias(useral1, noual)
c hide button if no aliases left
		if(noual .le. 0) call halias()
c undefine all aliases
	else if(inst.eq.'NOALIASES')then
		noual = 0
c don't show the button
		call halias()

c change current directory
	else if(inst.eq.'CD')then
		call sprompt('Give new directory')
		call getit(buff, 1)
c change directory
		call cd(buff, nnl(buff))
c then reset the file browser to show current working directory
		call resetd()

c arithmetic on x values
      else if(inst.eq.'XARITH')then
        call arith(1,x,y,z,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c arithmetic on y values
      else if(inst.eq.'YARITH')then
        call arith(2,x,y,z,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c arithmetic on z values
      else if(inst.eq.'ZARITH')then
        call arith(3,x,y,z,delx,dely,npts)
c arithmetic on x errors
      else if(inst.eq.'XERRARITH')then
        call arith(4,x,y,z,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c arithmetic on y errors
      else if(inst.eq.'YERRARITH')then
        call arith(5,x,y,z,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c function of x values
      else if(inst.eq.'XFUNCTION')then
        call rfunc(1,x,y,z,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c function of y values
      else if(inst.eq.'YFUNCTION')then
        call rfunc(2,x,y,z,delx,dely,npts)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
c function on z values
      else if(inst.eq.'ZFUNCTION')then
        call rfunc(3,x,y,z,delx,dely,npts)
c mode by which fits to data will be made
      else if(inst.eq.'FITMODE')then
          call sprompt('1=weight by errors, 0=no weight, -1=inverse Y')
          write(tstring, *)'current mode =',IMODE
	call sprompt(tstring)
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	imode = nint(ain(1))
c plot spline fit to data
      else if(inst.eq.'SPLINE')then
        call spline(xfit,yfit,yfit2,nptsf,msiz,wk2)
c kendall's tau test - non-parametric correlation test
      else if(inst.eq.'KENDALL')then
        call kendl(xfit,yfit,nptsf)
C Straight line fit when errors are significant on both the x
c and y values.
C  Reference: D. York, "Least squares fitting of a straight line",
C                      Canadian Journal of Physics, 44, 1079-1086,
C                      1966.
	else if(inst.eq.'YORK')then
		call york(nptsf, xfit, yfit, xfite, yfite,
     +			wk2, fint, sfint, grad, sgrad)	
                                        
C LEAST SQUARES STRAIGHT LINE FIT TO DATA (BEVINGTON ROUTINE)
C ALSO GIVES LINEAR CORRELATION COEFFICIENT
      else if(inst.eq.'LINFIT')then
        call linfit(xfit,yfit,yfite,nptsf,imode,
     +fint,sfint,grad,sgrad,r)
C PLOT RESULTS OF IF REQUESTED
      else if(inst.eq.'PLOTFIT')then
        call pltfit(fint,grad)
C CROSS CORRELATE WITH ANOTHER FILE
      else if(inst.eq.'CCF')then
		call sprompt('Give name of file to cross correlate against')
		write(tstring,*)'<default = ',title,'>'
		call sprompt(tstring)
	call getit(buff, 1)
	if(buff.eq.' ')buff=title
        call ccf(xfit,yfit,nptsf,crud,
     +yfit2,wk2,buff,pfnam)
C CHANGE WHICH ENERGY BANDS IN TIMING FILE ARE TO BE ADDED
	else if(inst.eq.'TIMEFILEADD')then
	  	call sprompt('Assign energy bands for addition')
		call sprompt('present values are:')
		write(tstring, *)BS1Y
		call sprompt(tstring)
	  call getit(buff, 1)
	  call dcode(buff,ain,ifix,igrd,k)
	  call asar(bs1y,ain,k)
C CHANGE WHICH ENERGY BANDS IN TIMING FILE ARE TO BE DIVIDED
	else if(inst.eq.'TIMEFILEDIV')then
	  	call sprompt('Assign energy bands for division')
		call sprompt('present values are:')
		write(tstring, *)bs2y
		call sprompt(tstring)
	  call getit(buff, 1)
	  call dcode(buff,ain,ifix,igrd,k)
	  call asar(bs2y,ain,k)
C CHANGE WHICH ENERGY BANDS IN TIMING FILE ARE TO BE ADDED
C FOR PLOTS AGAINST X-RAY DATA
	else if(inst.eq.'TIMEFILEADDX')then
	  	call sprompt('Assign x energy bands for addition')
		call sprompt('present values are:')
		write(tstring, *)BS1X
		call sprompt(tstring)
	  call getit(buff, 1)
	  call dcode(buff,ain,ifix,igrd,k)
	  call asar(bs1x,ain,k)
C CHANGE WHICH ENERGY BANDS IN TIMING FILE ARE TO BE ADDED
C FOR PLOTS AGainST X-RAY DATA
	else if(inst.eq.'TIMEFILEDIVX')then
	  	call sprompt('Assign x energy bands for division')
		call sprompt('present values are:')
		write(tstring, *)bs2x
		call sprompt(tstring)
	  call getit(buff, 1)
	  call dcode(buff,ain,ifix,igrd,k)
	  call asar(bs2x,ain,k)
C READ HAYSHIDA FORMAT FILE FOR PLOT VS. TIME *WITHOUT RESCALING*
      else if(inst.eq.'OVERHFILE')then
        call hfil(x,y,delx,dely,npts,msiz,title,xlab,ylab,
     +xfit,yfit,xfite,yfite,nptsf,1,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
        if(npts.le.1)goto 991
C READ HAYSHIDA FORMAT FILE FOR PLOT VS. TIME
      else if(inst.eq.'HFILE')then
        call hfil(x,y,delx,dely,npts,msiz,title,xlab,ylab,
     +xfit,yfit,xfite,yfite,nptsf,1,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
        if(npts.le.1)goto 991
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C READ HAYSHIDA FORMAT FILE FOR PLOT VS. X-RAY DATA
C *WITHOUT RESCALING*
      else if(inst.eq.'OVERHCORPLT')then
        call hfil(x,y,delx,dely,npts,msiz,title,xlab,ylab,
     +xfit,yfit,xfite,yfite,nptsf,2,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
        if(npts.le.1)goto 991
C READ HAYSHIDA FORMAT FILE FOR PLOT VS. X-RAY DATA
      else if(inst.eq.'HCORPLT')then
        call hfil(x,y,delx,dely,npts,msiz,title,xlab,ylab,
     +xfit,yfit,xfite,yfite,nptsf,2,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
        if(npts.le.1)goto 991
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C GENERALISED FUNCTION FIT TO DATA
      else if(inst.eq.'CURFIT'.or.inst.eq.'C')then
        call crvfit(xfit,yfit,xfite,yfite,imode,yfit2,nptsf)
C PLOT RESULTS OF GENERAL FUNCTION FIT IF REQUESTED
      else if(inst.eq.'PLOTCUR')then
        call pltcur(300)
C PLOT RESULTS OF GENERAL FUNCTION FIT AS A DASHED LINE
      else if(inst.eq.'PLOTCURD')then
        call pltcrd()
	call slstyle(lstyle)
C PLOT RESULTS OF GENERAL FUNCTION FIT IF REQUESTED
C PLOTTING INDIVIDUAL MODELS WITH THE OTHERS SET EQUAL TO ZERO
      else if(inst.eq.'CURMODELPLOT')then
        call pltcrm(300)
C PLOT RESULTS OF GENERAL FUNCTION FIT IF REQUESTED
C PLOTTING INDIVIDUAL MODELS WITH THE OTHERS SET EQUAL TO ZERO
C		---- DASHED LINE
      else if(inst.eq.'CURMODELPLOTD')then
        call pltcmd()
	call slstyle(lstyle)
C CHANGE CONVERGENCE CONDITIONS FROM DEFAULTS
      else if(inst.eq.'CONVERGENCE')then
          call sprompt('Give fractional change in chi2, no. of loops')
          write(tstring, *)'(current values are:',fchi,nloop,')'
	  call sprompt(tstring)
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	fchi = ain(1)
	floop = ain(2)
        nloop=nint(floop)
C USER DEFINED GRID PARAMETERS
      else if(inst.eq.'GRID')then
        nogrid=.false.
	call sprompt('give start, stop and step values for the grid')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        gstar=ain(1)
        gend=ain(2)
        gstep=ain(3)
C SUBTRACT FITTED MODEL FROM DATA
      else if(inst.eq.'SUBCUR')then
        call subcur(x,y,npts,0)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C ADD FITTED MODEL TO DATA
      else if(inst.eq.'ADDCUR')then
        call subcur(x,y,npts,1)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C POLYNOMIAL FIT TO DATA
      else if(inst.eq.'POLFIT')then
C READ IN ORDER OF POLYNOMIAL
        call sprompt('Give polynomial order')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	forder = ain(1)
        nterms=nint(forder)+1
        do 15 i=1,maxpol
15       apoly(i)=0.0
        if(nterms.gt.maxpol)then
c too high order for polfit as coded
         call xtext('TOO HIGH AN ORDER OF POLYNOMIAL')
         call xtext('THE DATA STATEMENTS IN ROBOT ALLOW ONLY POLYS')
         write(tstring, *)'UP TO ',MAXPOL-1
	call totext(tstring)
        else if((nptsf-nterms).le.0) then
c too high an order for the amount of data
		call xtext('WARNING')
		call xtext('Too high polynomial order')
		call xtext('You don''t have that many data points')
	else
c do fit
         call polfit(xfit,yfit,yfite,nptsf,nterms,imode,apoly,chisqr)
         write(tstring, *, err = 920)'Coefficients are:'
	 call totext(tstring)
	call oarray(apoly, nterms)
         WRITE(ifil8,*)'Coefficients of polynomial are:',APOLY
	 write(tstring, *, err = 920)'Chi**2 = ', chisqr
	 call totext(tstring)
	 call write8(tstring)
        end if
C PLOT RESULTS OF POLYNOMIAL FIT IF REQUESTED
      else if(inst.eq.'PLOTPOLY')then
        call pltpol(apoly,nterms)
C SUBTRACT POLYNOMIAL FIT FROM DATA
      else if(inst.eq.'SUBPOLY')then
        call xtext('Subtracting polynomial')
        call subpol(x,y,npts,apoly,nterms)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
C CALCULATE MOMENTS OF X AND Y DATA
      else if(inst.eq.'MOMENTS')then
        call moment(xfit,yfit,nptsf)
C MEDIAN OF Y IN SPECIFIED RANGE
      else if(inst.eq.'MEDIAN')then
        call mdian(yfit,nptsf)
C MEDIAN OF X IN SPECIFIED RANGE
      else if(inst.eq.'MEDIANX')then
        call mdian(xfit,nptsf)
C SORT DATA INTO INCREASING ORDER OF X (DO Y BUT NOT Z AT SAME TIME)
      else if(inst.eq.'SORT')then
        call sort4(npts,x,y,delx,dely)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
      else if(inst.eq.'SORTY')then
        call sort4(npts,y,x,dely,delx)
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C SUM Y DATA BETWEEN SPECIFIED LIMITS (XSTART TO XEND)
      else if(inst.eq.'SUMDATA')then
        call sumdat(xfit,yfit,nptsf)
C SUM MODEL BETWEEN SPECIFIED LIMITS (XSTART TO XEND)
      else if(inst.eq.'SUMMODEL')then
        call summod(xfit,yfit,nptsf)
C INTEGRATE DATA BETWEEN SPECIFIED LIMITS
      else if(inst.eq.'INTEGRATE')then
        call carea(xfit,yfit,nptsf,3)
C PLOT SOME TEXT
      else if(inst.eq.'TEXT')then
        call sprompt('Give text to be plotted')
        call getit(text,  1)
	buff = ' '
		call sprompt('Specify coordinates')
		if(kboard)call sprompt('<RETURN>= use cursor')
	buff = ' '
	if(kboard.or.(.not.inter).or.HAVEDATA) call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
	  call xtext('Use cursor to specify text position')
c set cursor to show text to be written
c for text at about zero degrees show actual text
c otherwise just set text cursor
	  if(tang.gt.0.001)then
		call update()
	  	call tcur(text)
	  	call cursor(ain(1),ain(2))
	  else
	  	call tband(ain(1), ain(2), text)
	  end if
	  call fvout(ain, 2)
	end if
        call txtm(ain(1),ain(2),text,nnl(text))
C PLOT SOME TEXT centred at given position
      else if(inst.eq.'CENTRETEXT'.or.inst.eq.'CENTERTEXT')then
        call sprompt('Give text to be plotted')
        call getit(text,  1)
	buff = ' '
		call sprompt('Specify coordinatesof centre')
		if(kboard)call sprompt('<RETURN>= use cursor')
	buff = ' '
	if(kboard.or.(.not.inter).or.HAVEDATA) call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
	  call xtext('Use cursor to specify text central position')
	  call tcur(text)
	  call cursor(ain(1),ain(2))
	  call fvout(ain, 2)
	end if
        call ctext(ain(1),ain(2),text)
C CHANGE ANGLE FOR PLOTTING TEXT (DEFAULT=0 DEGREES)
      else if(inst.eq.'TANGLE')then
        call sprompt('Give angle of text (degrees)')
	write(tstring, *)'Present value = ',tang
	call sprompt(tstring)
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	tang = ain(1)
        call txangm(tang)
C CHANGE TEXT SIZE FROM DEFAULT - WAS IN X AXIS UNITS
C FOR FUJITSU VERSION, IN POINTS FOR MAC
      else if(inst.eq.'TEXTSIZE')then
        call sprompt('Give size of characters (in >points<)')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	chsz = ain(1)
		call txtset(chsz)
      else if(inst.eq.'TENPOINT') then
		call txtset(10.0)
      else if(inst.eq.'TWELVEPOINT') then
		call txtset(12.0)
      else if(inst.eq.'FOURTEENPOINT') then
		call txtset(14.0)
      else if(inst.eq.'EIGHTEENPOINT') then
		call txtset(18.0)
C Bold, italic etc.
	else if(inst.eq.'TEXTSTYLE')then
		call sprompt('Give style')
		call sprompt('Normal, bold, italic, bold_italic')
		call getit(crud,  1)
		call lctouc(crud)
c If font is specified this way we also need to alter menu item
c activation
		call switcher(crud)
		if(crud.eq.'NORMAL')ifnt = 0
		if(crud.eq.'BOLD')ifnt = 1
		if(crud.eq.'ITALIC'.or.crud.eq.'OBLIQUE')ifnt = 2
		if(crud.eq.'BOLD_ITALIC')ifnt = 3
		call tstyle(ifnt)
	else if(inst.eq.'NORMAL')then
		call tstyle(0)
	else if(inst.eq.'BOLD')then
		call tstyle(1)
	else if(inst.eq.'ITALIC'.or.inst.eq.'OBLIQUE')then
		call tstyle(2)
	else if(inst.eq.'BOLDITALIC')then
		call tstyle(3)

c change the font family

	else if(inst.eq.'FONT')then
		call sprompt('Give font family')
		call sprompt('Times, Helvetica, Courier, Palatino')
		call sprompt('AvantGarde, Bookman, HelveticaN')
		call sprompt('NewCentury, Symbol, Kanji, Dingbats, Chancery')
        	call getit(buff, 1)
		call lctouc(buff)
c If font is specified this way we also need to alter menu item
c activation
		call switcher(buff)
		if(buff.eq.'TIMES')then
			call tfamly(0)
		else if(buff.eq.'HELVETICA')then
			call tfamly(1)
		else if(buff.eq.'COURIER')then
			call tfamly(2)
		else if(buff.eq.'PALATINO')then
			call tfamly(3)
		else if(buff.eq.'AVANTGARDE')then
			call tfamly(4)
		else if(buff.eq.'BOOKMAN')then
			call tfamly(5)
		else if(buff.eq.'HELVETICAN'
     +			.or.buff.eq.'HELVETICA NARROW')then
			call tfamly(6)
		else if(buff.eq.'NEWCENTURY')then
			call tfamly(7)
		else if(buff.eq.'SYMBOL')then
			call tfamly(400)
		else if(buff.eq.'KANJI')then
			call tfamly(500)
		else if(buff.eq.'DINGBATS'.or.buff.eq.'ZAPFDINGBATS')then
			call tfamly(600)
		else if(buff.eq.'CHANCERY'.or.buff.eq.'ZAPFCHANCERY')then
			call tfamly(99)
		else
			call xtext('ERROR: Unknown font')
			call xtext(buff)
		end if
		
	else if(inst.eq.'TIMES')then
		call tfamly(0)
	else if(inst.eq.'HELVETICA')then
		call tfamly(1)
	else if(inst.eq.'COURIER')then
		call tfamly(2)
	else if(inst.eq.'PALATINO')then
		call tfamly(3)
	else if(inst.eq.'AVANTGARDE')then
		call tfamly(4)
	else if(inst.eq.'BOOKMAN')then
		call tfamly(5)
	else if(inst.eq.'HELVETICAN'.or.inst.eq.'HELVETICANARROW')then
		call tfamly(6)
	else if(inst.eq.'NEWCENTURY')then
		call tfamly(7)
	else if(inst.eq.'SYMBOL')then
		call tfamly(400)
	else if(inst.eq.'KANJI')then
		call tfamly(500)
	else if(inst.eq.'DINGBATS')then
		call tfamly(600)
	else if(inst.eq.'CHANCERY')then
		call tfamly(99)
	
c change line width
	else if(inst.eq.'WIDTH0')then
		call lwidth(0)
	else if(inst.eq.'WIDTH1')then
		call lwidth(1)
	else if(inst.eq.'WIDTH2')then
		call lwidth(2)
	else if(inst.eq.'WIDTH4')then
		call lwidth(4)
	else if(inst.eq.'WIDTH8')then
		call lwidth(8)
c Line styles

	else if(inst.eq.'LINESTYLE')then
		call sprompt('Give line style')
		call sprompt('DashedLines, DottedLines')
		call sprompt('SolidLines, DotDashedLines')

        	call getit(buff, 1)
		call lctouc(buff)
c If line style is specified this way we also need to alter menu item
c activation
		call switcher(buff)
		if(buff.eq.'DASHEDLINES')then
			call dodash()
			lstyle = 'DASH'
		else if(buff.eq.'DOTTEDLINES')then
			call dodot()
			lstyle = 'DOT'
		else if(buff.eq.'SOLIDLINES')then
			call nodash()
			lstyle = 'SOLID'
		else if(buff.eq.'DOTDASHEDLINES')then
			call dotdash()
			lstyle = 'DOTDASH'
		else
			call xtext('ERROR: Unknown line style')
			call xtext(buff)
		end if


	else if(inst.eq.'DASHEDLINES')then
		call dodash()
		lstyle = 'DASH'
	else if(inst.eq.'DOTTEDLINES')then
		call dodot()
		lstyle = 'DOT'
	else if(inst.eq.'SOLIDLINES')then
		call nodash()
		lstyle = 'SOLID'
	else if(inst.eq.'DOTDASHEDLINES')then
		call dotdash()
		lstyle = 'DOTDASH'
                
C CHANGE TEXT SIZE FROM DEFAULT - IN Y AXIS UNITS
      else if(inst.eq.'TEXTSIZEY')then
        call sprompt('Give size of characters (in y axis units)')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	chsz = ain(1)
        call chrszu(0.0,chsz)
C CHANGE DEFAULT APPROXIMATE NO. OF X TICK MARKS
      else if(inst.eq.'NTICKX')then
        call sprompt('Give approximate no. of x axis tick marks')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	fxtick = ain(1)
        ntickx=nint(fxtick)
C CHANGE DEFAULT APPROXIMATE NO. OF Y TICK MARKS
      else if(inst.eq.'NTICKY')then
        call sprompt('Give approximate no. of y axis tick marks')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	fytick = ain(1)
        nticky=nint(fytick)
C DRAW A LINE BETWEEN POINTS X1,Y1 AND X2,Y2
      else if(inst.eq.'LINE')then
	  call sprompt('Specify start and end coordinates of line')
	  call sprompt('(<RETURN> = use cursor)')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	  call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				0,
     +		'Put cursor at start of line...',
     +		'Now end of line...')
	  call fvout(ain, 4)
	end if
        call movxy(ain(1),ain(2))
        call linxy(ain(3),ain(4))
c Polyline
	else if(inst.eq.'POLYLINE')then
		call pline(1, kboard)
c Polygon
	else if(inst.eq.'POLYGON')then
		call pline(2, kboard)
c Filled polygon
	else if(inst.eq.'POLYFILL')then
		call pfill(kboard)
	

c draw an arrow between points x1,y1 and x2,y2
      else if(inst.eq.'ARROW')then
	  call sprompt('Specify start and end coordinates of arrow')
	  if(kboard) call sprompt('(<RETURN> = use cursor)')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	  call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				4,
     +		'Put cursor at base of arrow...',
     +		'Now end of arrow...')
	  call fvout(ain, 4)
	end if
        call arrow(ain(1), ain(2), ain(3), ain(4))
c Change arrow head styles
	else if(inst.eq.'ARROWHOLLOW')then
		call astyle(2)
	else if(inst.eq.'ARROWFILL')then
		call astyle(1)
	else if(inst.eq.'ARROWLINE')then
		call astyle(0)
c Change size of arrow head
	else if(inst.eq.'ARROWSIZE')then
		call sprompt('Give size of arrow head')
		write(tstring, *)'(arrow size is now', tasize,')'
		call sprompt(tstring)
		call getit(buff, 1)
          	call dcode(buff,ain,ifix,igrd,k)
		tasize = ain(1)
		call asize(tasize)
	else if(inst.eq.'ARROWANGLE')then
		call sprompt('Give angle of arrow head (degrees)')
		write(tstring, *)'(arrow angle is now', taang,')'
		call sprompt(tstring)
		call getit(buff, 1)
          	call dcode(buff,ain,ifix,igrd,k)
		taang = ain(1)
		call aangle(taang)
c double or single arrow head?
	else if(inst.eq.'DOUBLEARROW')then
		call adble()
	else if(inst.eq.'SINGLEARROW')then
		call asingle()
C DRAW A LINE BETWEEN TO SPECIFIED POINTS FROM PRESENT POSITION
      else if(inst.eq.'LINETO')then
	  call sprompt('Specify end coordinates of line')
	  if(kboard) call sprompt('(<RETURN> = use cursor)')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	  call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
	  call xtext('Put cursor at end of line')
	  call cursor(ain(1),ain(2))
	  call fvout(ain, 2)
	end if
        call linxy(ain(1),ain(2))
C SET CURRENT PLOT POSITION TO SPECIFIED POINT
      else if(inst.eq.'MOVETO')then
	  call sprompt('Specify coordinates of point')
	  if(kboard) call sprompt('(<RETURN> = use cursor)')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	  call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
	  call xtext('Put cursor at end of line')
	  call cursor(ain(1),ain(2))
	  call fvout(ain, 2)
	end if
        call movxy(ain(1),ain(2))
C DRAW A DASHED LINE BETWEEN POINTS X1,Y1 AND X2,Y2
C WITH NO. OF DASHES = FNDASH, RATIO BETWEEN LINE TO SPACE OF 'RATIO'
      else if(inst.eq.'DASHEDLINE')then
	  call sprompt('Specify start and end coordinates of line')
	  if(kboard)call sprompt('(<RETURN> = use cursor)')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				0,
     +		'Put cursor at start of line...',
     +		'Now end of line')
	end if
	call preset()
	call sprompt('Give number of dashes and ratio of space/dash')
        call getit(buff, 1)
        call dcode(buff,ain2,ifix,igrd,k)
	fndash = ain2(1)
	ratio = ain2(2)
        call dline(ain(1),ain(2),ain(3),ain(4),fndash,ratio)
       
c draw an arc defined by three points
	else if(inst.eq.'ARC')then
		call sprompt('Give center, radius, angle1, angle2')
		call getit(buff, 1)
		call dcode(buff, ain1, ifix, igrd, k)
		ifill = 0
		call arc(ain1(1), ain1(2), ain1(3), ain1(4),
     +       			ain1(5), ifill)


c draw a circle defined by points x1,y1 and radius
c this is slighly convoluted because the circle rband routine
c returns (x,y) coordinates of a defining box for the circle
c however it seems more natural for this command to use
c radius as the argument.
       else if(inst.eq.'CIRCLE')then
	   call sprompt('Specify coordinates of centre  and radius')
	   call sprompt('(Radius in units of x axis')
	   if(kboard)call sprompt('<RETURN> = "Use Cursor"')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	  temp = ain(3) + ain(1)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				2,
     +		'Put cursor at centre...',
     +		'and now specify radius')
		temp = ain(3)
		ain(3) = ain(3) - ain(1)
		call fvout(ain, 3)
	end if
        call circle(ain(1),ain(2),temp,ain(4))
c draw an ellipse defined by points x1,y1 and corner
       else if(inst.eq.'ELLIPSE')then
	   call sprompt('Specify coordinates of centre  and corner')
	   if(kboard)call sprompt('<RETURN> = "Use Cursor"')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				3,
     +		'Put cursor at centre...',
     +		'and now specify corner')
	end if
        call elipse(ain(1),ain(2),ain(3),ain(4))
C DRAW A BOX DEFINED BY POINTS X1,Y1 AND X2,Y2
       else if(inst.eq.'BOX')then
	   call sprompt('Specify coordinates of bottom L and top R corners')
	   if(kboard)call sprompt('<RETURN> = "Use Cursor"')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				1,
     +		'Put cursor at one corner of the box...',
     +		'and now the other')
		call fvout(ain, 4)
	end if
        call boxm(ain(1),ain(2),ain(3),ain(4))
C DRAW A FILLED BOX DEFINED BY POINTS X1,Y1 AND X2,Y2
       else if(inst.eq.'FILLEDBOX')then
	   call sprompt('Give coordinates of bottom L and top R corners')
	   call sprompt('<RETURN> = "Use Cursor"')
	buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
	   call savdata(buff)
          call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4),
     +				1,
     +		'Put cursor at one corner of the box...',
     +		'and now the other')
		call fvout(ain, 4)
	end if
        call fbox(ain(1),ain(2),ain(3),ain(4))
C TAKE LOG10 OF X VALUES
      else if(inst.eq.'LOGX')then
        do 20 i=1,npts
        if(x(i).le.0.0)goto 20
        temp=x(i)
        x(i)=log10(x(i))
        if(delx(i).ne.0.0)delx(i)=log10(temp+delx(i))-log10(temp)
20      continue
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C TAKE LOG10 OF Y VALUES
      else if(inst.eq.'LOGY')then
        do 22 i=1,npts
        if(y(i).le.0.0)goto 22
        temp=y(i)
        y(i)=log10(y(i))
        if(dely(i).ne.0.0)dely(i)=log10(temp+dely(i))-log10(temp)
22      continue
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C TAKE LOG10 OF X AND Y VALUES
      else if(inst.eq.'LOGXY')then
        do 24 i=1,npts
        if(x(i).le.0.0)goto 23
	temp=x(i)
        x(i)=log10(x(i))
        if(delx(i).ne.0.0)delx(i)=log10(temp+delx(i))-log10(temp)
23      if(y(i).le.0.0)goto 24
	temp=y(i)
        y(i)=log10(y(i))
        if(dely(i).ne.0.0)dely(i)=log10(temp+dely(i))-log10(temp)
24      continue
       call copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
C USER DEFINED LINEAR AXIS
      else if(inst.eq.'USERAXIS')then
         call sprompt('Give id, tick mark size, x, y,')
     	 call sprompt('distance, no. of ticks')
         call sprompt('(id=0 for X axis, id=1 for Y axis)')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        call lnax(nint(ain(1)),ain(2),
     +ain(3),ain(4),ain(5),nint(ain(6)))
C USER LINEAR AXIS LABELS
      else if(inst.eq.'USERAXISLABEL')then
        call sprompt('Give start and increment values for labels')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        call lnlbvl(ain(1),ain(2))
        call sprompt('Give ID, X start, Y start, distance, N')
        call sprompt('[ID=2 for X,ID=3 for Y, Offset labels')
        call sprompt('size, ID=0 for X,ID=1 for Y, No Offset]')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        call lnlbl(nint(ain(1)),ain(2),
     +ain(3),ain(4),nint(ain(5)))
C USER DEFINED LOG AXIS
      else if(inst.eq.'LOGAXIS')then
          call sprompt('Give ID, Tick mark size,')
     	  call sprompt('X, Y, Distance, No of ticks')
           call sprompt('(ID=0 for X axis, ID=1 FOR Y axis)')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        call lgax(nint(ain(1)),ain(2),
     +ain(3),ain(4),ain(5),nint(ain(6)))
C USER LOG AXIS LABELS
      else if(inst.eq.'LOGAXISLABEL')then
        call sprompt('Give start and increment label values')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        call lnlbvl(ain(1),ain(2))
          call sprompt('Give ID, X start,')
          call sprompt('Y start, Distance, No. of labels')
          call sprompt('(ID=0 for X axis, id=1 for Y axis)')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
        call lglbl(nint(ain(1)),ain(2),
     +ain(3),ain(4),nint(ain(5)))
C CHANGE % USAGE OF PAPER
      else if(inst.eq.'PAGEUSE')then
        call sprompt('Give bottom left')
     	call sprompt('and top right corners in % page')
        call getit(buff, 1)
        call dcode(buff,ain,ifix,igrd,k)
	sxl = ain(1)
	syl = ain(2)
	sxh = ain(3)
	syh = ain(4)
	call pusechk(sxl, syl, sxh, syh)
C AND CALL LIMIT IF DATA LIMITS EXIST
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
c change window spacing (both X and Y)
	else if(inst.eq.'WINDOWGAP')then
	call sprompt('Give window gap for x AND y (%)')
	write(tstring, *)'Present values are: ',xgap,ygap
	call sprompt(tstring)
	call getit(buff, 1)
	call dcode(buff, ain, ifix, igrd, k)
	xgap = ain(1)/100.
	ygap = xgap
C AND CALL LIMIT IF DATA LIMITS EXIST
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
	call gapcheck(xgap, ygap)
c change X window spacing
	else if(inst.eq.'XWINDOWGAP')then
	call sprompt('Give X window gap (%)')
	write(tstring, *)'Present value = ',xgap
	call sprompt(tstring)
	call getit(buff, 1)
	call dcode(buff, ain, ifix, igrd, k)
	xgap = ain(1)/100.
C AND CALL LIMIT IF DATA LIMITS EXIST
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
	call gapcheck(xgap, ygap)
c change Y window spacing
	else if(inst.eq.'YWINDOWGAP')then
	call sprompt('Give Y window gap (%)')
	write(tstring, *)'Present value = ',xgap
	call sprompt(tstring)
	call getit(buff, 1)
	call dcode(buff, ain, ifix, igrd, k)
	ygap = ain(1)/100.
C AND CALL LIMIT IF DATA LIMITS EXIST
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
	call gapcheck(xgap, ygap)
c user defined plot limits rather than automatic
c this step is triggered by both the datalimits and dflimits commands
      else if(inst.eq.'DATALIMITS'.or.inst.eq.'DFLIMITS')then
        if(inter)then
          call sprompt('Give low X, low Y, high X and high Y')
          call sprompt('(Plot limits may be changed by robot if not')
          call sprompt('enclosed in (brackets))')
	  call sprompt('<RETURN>="Use Cursor"')
        end if
	 buff = ' '
        if(kboard.or.(.not.inter).or.HAVEDATA)call getit(buff, 0)
	if(buff.ne.' ')then
		call savdata(buff)
        	call dcode(buff,ain,ifix,igrd,k)
	else
		call nline(ain(1), ain(2), ain(3), ain(4), 1,
     +		    'Put cursor at one corner of the box...',
     +		    'and now the other')

		call fvout(ain, 4)
	end if
	if(ain(1).lt.ain(3))then
		xmin=ain(1)
		xmax=ain(3)
	else
		xmin=ain(3)
		xmax=ain(1)
	end if
	if(ain(2).lt.ain(4))then
		ymin=ain(2)
		ymax=ain(4)
	else
		ymin=ain(4)
		ymax=ain(2)
	end if
c allow plotting backwards/upside down if both x and y values are fixed
c are "fixed" (i.e. don't swop x and y values
	if(ifix(1).eq.1.and.ifix(3).eq.1)then
		xmin = ain(1)
		xmax = ain(3)
	end if
	if(ifix(2).eq.1.and.ifix(4).eq.1)then
		ymin = ain(2)
		ymax = ain(4)
	end if
       
       
       
C IF DFLIMITS CALL FITLIM IN MODE 4
        if(inst.eq.'DFLIMITS')then
          call fitlim(4,xfit,yfit,xfite,yfite,
     +x,y,delx,dely,npts,nptsf)
       end if
c check on limits for plotting
	xtmin = min(xmax, xmin)
	xtmax = max(xmax, xmin)
	ytmin = min(ymax, ymin)
	ytmax = max(ymax, ymin)
c use linsc2 to get sensible limits for plotting
c minor and major axes for X
c minor
	call linsc2(xtmin, xtmax, ntickx*mmr,xmaxp,xminp,npxm,distxm)
c major
	if(xmax.le.xmin)then
      	   call linsc2(xtmin,xtmax,ntickx,xmaxp,xminp,npx,distx)
	else
      	   call linsc2(xtmin,xtmax,ntickx,xminp,xmaxp,npx,distx)
	end if
c minor and major axes for Y
c minor
	call linsc2(ytmin, ytmax, nticky*mmr, ymaxp,yminp,npym,distym)
c major
	if(ymax.le.ymin)then
      	  call linsc2(ytmin, ytmax, nticky, ymaxp, yminp, npy, disty)
	else
      	  call linsc2(ytmin, ytmax, nticky, yminp, ymaxp, npy, disty)
	end if


      if(ifix(1).eq.1)xminp=xmin
      if(ifix(2).eq.1)yminp=ymin
      if(ifix(3).eq.1)xmaxp=xmax
      if(ifix(4).eq.1)ymaxp=ymax

c reset xtmin and friends to the values returned by linsc2
	xtmin = min(xmaxp, xminp)
	xtmax = max(xmaxp, xminp)
	ytmin = min(ymaxp, yminp)
	ytmax = max(ymaxp, yminp)

      if(ECHO)then
        call xtext('Plot scaled to (xminp,yminp,xmaxp,ymaxp)')
        write(tstring, *)xminp,yminp,xmaxp,ymaxp
	call totext(tstring)
      end if
C DEFINE PLOT LIMITS
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
c allow for window gaps
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
                                                               
      end if
C CONTOUR LEVELS FOR CONTOUR PLOT
      else if(inst.eq.'CONTOURVALUES')then
        call conval()
C CONTOUR PLOT
      else if(inst.eq.'CONTOUR')then
        call cont(xfit,yfit,dely,nptsf, ccode, threed)
C PLOT SOME DATA (2D version)
	else if(inst.eq.'GOPLOT'.or.inst.eq.'G')then
     	 if(.not.(threed))then
c abort GOPLOT if limts bad
	if(xminp.eq.xmaxp.or.yminp.eq.ymaxp)then
		call xtext('ERROR: MIN = MAX for plot')
		goto 930
	end if
c no plot if no points
	if(npts.le.0)goto 935
                      
C BOXES, AXES AND LABELS IF WANTED (DEFAULT)
      if(PAXES)then
c are the axes to be internal or external?
		
c the axes don't actually go all the way to the end of the
c box so pad them out with a line
      if(raxis) call linxy2(xmaxp, yminp, xmaxp, ymaxp)
      if(laxis) call linxy2(xminp, yminp, xminp, ymaxp)
      if(taxis) call linxy2(xminp, ymaxp, xmaxp, ymaxp)
      if(baxis) call linxy2(xminp, yminp, xmaxp, yminp)

      xsiz=xmaxp-xminp
      ysiz=ymaxp-yminp
      if(dytck) bx=xsiz/40.
      if(dxtck) by=ysiz/40.

c are tick marks external or internal?
	if(extickx)then
		if(ymaxp.gt.yminp)then
			by = -abs(by)
		else
			by = abs(by)
		end if
		yxst = yminp + (by)
	else
		if(ymaxp.gt.yminp)then
			by = abs(by)
		else
			by = -abs(by)
		end if
		yxst = yminp
	end if

	if(exticky)then
		if(xmaxp.gt.xminp)then
			bx = -abs(bx)
		else
			bx = abs(bx)
		end if
		xyst = xminp + (bx)
	else
		if(xmaxp.gt.xminp)then
			bx = abs(bx)
		else
			bx = -abs(bx)
		end if
		xyst = xminp
	end if
c axes + labels
c are they log axes
	if(LOGXAXIS)then
		xst = int(xtmin)
		xend = int(xtmax)
                   
                   
		npx = nint(xend - xst)
                        
		call lgax2(0, by, xst, yminp, 1., npx+1, xminp, xmaxp)
		call lgax2(0, -by, xst, ymaxp, 1., npx+1, xminp, xmaxp)
		if(PGRID)then
			call dodash
      			call lgaxg(0,-(ymaxp-yminp),xst,ymaxp, 1.,
     +					npx+1, xminp, xmaxp)
			call slstyle(lstyle)
		end if 
	else
      		xst=int(xtmin/distx)*distx
		call getmin(xst, xtmin, distx)
      		xend=int((xtmax)/distx)*distx

                                    
      		npx=nint((xend-xst)/distx)-1
c kludgey test that number of tick marks isn't too great
		if((xst + npx*distx).ge.xtmax)then
			 npx = npx - 1
		end if
		if((xtmax - (xst + distx*npx)).gt.distx*1.05)then
			npx = npx + 1
		end if

      		if(baxis) call lnax(0,by,xst,yminp,distx,npx)
      		if(taxis) call lnax(0,-by,xst,ymaxp,distx,npx)
		if(PGRID)then
			call dodash()
      			call lnax(0,-(ymaxp-yminp),xst,ymaxp,distx,npx)
			call slstyle(lstyle)
		end if 
c Minor axes if desired
		if(minaxes) then
			bym = by*mmrat
      			xstm=int(xtmin/distxm)*distxm
			call getmin(xstm, xtmin, distxm)
      			xendm=int(xtmax/distxm)*distxm
     	 		npxm=int((xendm-xstm)/distxm)
			if(xstm+npxm*distxm.gt.xtmax)npxm = npxm - 1
		if((xtmax - (xstm + distxm*npxm)).gt.distxm)then
				npxm = npxm + 1
		end if
      			if(baxis) call lnax(0,bym,xstm,yminp,distxm,npxm)
      			if(taxis) call lnax(0,-bym,xstm,ymaxp,distxm,npxm)
		end if
	end if
       
       
	if(LOGYAXIS)then
		yst = int(ytmin)
		yend = int(ytmax)
                   
		npy = nint(yend - yst)
                        
      		call lgax2(1,-bx,xminp,yst,1.,npy+1, yminp, ymaxp)
      		call lgax2(1,bx,xmaxp,yst,1.,npy+1, yminp, ymaxp)
		if(PGRID)then
			call dodash
		      	call lgaxg(1,-(xmaxp-xminp),xminp,yst, 1.,
     +				npy+1, yminp, ymaxp)
			call slstyle(lstyle)
		end if 
	else
      		yst=int(ytmin/disty)*disty
		call getmin(yst, ytmin, disty)
      		yend=int((ytmax)/disty)*disty
                                     
      		npy=nint((yend-yst)/disty)-1
c kludgey test that number of tick marks isn't too great
		if((yst + npy*disty).ge.ytmax) npy = npy - 1

c now a simple test for being too small                                    
		if((ytmax - (yst + disty*npy)).gt.disty*1.05)then
			npy = npy + 1
		end if

      		if(laxis) call lnax(1,-bx,xminp,yst,disty,npy)
      		if(raxis) call lnax(1,bx,xmaxp,yst,disty,npy)
		if(PGRID)then
			call dodash
		      	call lnax(1,(xmaxp-xminp),xmaxp,yst,disty,npy+1)
			call slstyle(lstyle)
		end if 
	
c Minor axes if desired
		if(minaxes) then
			bxm = bx*mmrat
              
      			ystm=int(ytmin/distym)*distym
			call getmin(ystm, ytmin, distym)
      			yendm=int((ytmax)/distym)*distym
                                                  
      			npym=int((yendm-ystm)/distym)
			if(ystm+npym*distym.gt.ytmax)npym = npym - 1
		if((ytmax - (ystm + distym*npym)).gt.distym)then
				npym = npym + 1
		end if

      			if(laxis) call lnax(1,-bxm,xminp,ystm,distym,npym)
      			if(raxis) call lnax(1,bxm,xmaxp,ystm,distym,npym)
		end if
        
	end if
       
c axis labels if wanted (default)
      call txangm(0.0)
       if(labx)then
	if(logxaxis)then
		if(xst.lt.xminp)xst = xst + 1.
        	call lnlbvl(xst,1.)
		if((xst+npx).gt.xmaxp)then
        		call lglbl(2,xst,yxst,1.,npx-1)
		else
        		call lglbl(2,xst,yxst,1.,npx)
		end if
	else
        	call lnlbvl(xst,distx)

c no of labels depends on whether there's a chance on them running
c into the next subwindow
		if((fmplot .lt. 2.0 ) .and.
     +      (((npx+1)*distx + xst) .le. xmaxp)) then
        	     call lnlbl(2,xst,yxst,distx,npx+1)
		else
        	     call lnlbl(2,xst,yxst,distx,npx)
		end if
       end if
	end if
       if(laby)then
	if(logyaxis)then
		if(yst.lt.yminp)yst = yst + 1.
        	call lnlbvl(yst,1.)
		if((yst+npy).gt.ymaxp)then
        		call lglbl(3,xyst,yst,1.,npy-1)
		else
        		call lglbl(3,xyst,yst,1.,npy)
		endif
	else
        	call lnlbvl(yst,disty)
		if((fnplot .lt. 2.0 ) .and.
     +      ((npy+1)*disty + yst) .le. ymaxp) then
			call lnlbl(3,xyst,yst,disty,npy+1)
		else
			call lnlbl(3,xyst,yst,disty,npy)
		end if
       end if
	end if
      call txangm(tang)
      end if
            
c and now for some actual data
c set data plot colour if needed
	if(abs(rgbd(1)-rgb(1)).ge.1.or.abs(rgbd(2)-rgb(2)).ge.1.or.
     +	abs(rgbd(3) -rgb(3)).ge.1)then
		call farkc(rgbd(1),rgbd(2),rgbd(3))
	end if
      if(pmode.eq.'BARS')then
       call erbar(npts,x,y,delx,delx,dely,dely,1)
      else if(pmode.eq.'BARS2')then
       call erbar(npts,x,y,delx,delx,dely,dely,2)
c just axes but no data
      else if(pmode.eq.'NODATA')then
       continue
      else if(pmode.eq.'SYMBOL')then
       call tplot(npts,x,y,symb)
      else if(pmode.eq.'NICE')then
       call tplot2(npts,x,y,isymb)
      else if(pmode.eq.'PILLAR')then
	call pillar(NPTS,X,Y)
      else if(pmode.eq.'BOXES')then
       call erbox(npts,x,y,delx,delx,dely,dely)
      else if(pmode.eq.'FILLEDBOXES')then
       call erboxf(npts,x,y,delx,delx,dely,dely)
      else if(pmode.eq.'CROSSES')then
       call ercrs(npts,x,y,delx,delx,dely,dely)
      else if(pmode.eq.'DIAMONDS')then
       call erdia(npts,x,y,delx,delx,dely,dely)
      else if(pmode.eq.'ELLIPSES')then
       call erell(npts,x,y,delx,delx,dely,dely)
      else if(pmode.eq.'DASHEDLINES')then
       call dlplot(npts,x,y)
	call slstyle(lstyle)
      else if(pmode.eq.'SPLINE')then
       call spline(xfit,yfit,yfit2,nptsf,msiz,wk2)
      else if(pmode.eq.'FILL')then
	call fplot(npts, x, y, ifstyle, fillvalue)

      else if(pmode.eq.'LINES')then
c check on data limits
       do 8 i=1,npts
       if(x(i).ge.xtmin.and.x(i).le.xtmax.and.y(i).ge.ytmin.
     +and.y(i).le.ytmax)then
        call movxy(x(i),y(i))
        goto 9
       end if
8      continue
9      j=i
       do 10 i=j,npts
       xp=x(i)
       yp=y(i)
       if(xp.ge.xtmin.and.xp.le.xtmax.and.yp.ge.ytmin.
     +and.yp.le.ytmax)then
       		call linxy(XP,YP)
	end if
10     continue
C HISTOGRAM
        else if(pmode.eq.'HISTOGRAM')then
C FIND START POINT INSIDE DATA LIMITS
       do 18 i=1,npts
	 xp = x(i)
	 yp = y(i)
       if(xp .ge. xtmin .and. xp .le. xtmax .and. yp .ge. ytmin
     +         .and. yp .le. ytmax)then
        call movxy(x(i),y(i))
        goto 19
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
       call linxy(xp,yp)
       call movxy(x(j),y(j))
                            
       do 110 i=j+1,npts
         xp=(x(i)+x(i-1))/2.
         yp=y(i-1)
         if(yp.gt.ytmax)yp=ytmax
         if(xp.lt.xtmin)xp=xtmin
         if(yp.lt.ytmin)yp=ytmin
         if(xp.gt.xtmax)goto 111
         call linxy(xp,yp)
         yp=y(i)
         if(yp.gt.ytmax)yp=ytmax
         if(yp.lt.ytmin)yp=ytmin
         call linxy(xp,yp)
         xp=x(i)
         if(xp.gt.xtmax)goto 111
         call linxy(xp,yp)
110     continue
C TERMINATE HISTOGRAM
       xp=x(npts)+((x(npts)-x(npts-1))/2.)
       if(xp.gt.xtmax)xp=xtmax
       call linxy(xp,yp)
        goto 112
111     continue
        call linxy(xmaxp,yp)
112     continue
c      end if
             
	else if(pmode.eq.'FILLED-HISTOGRAM'
     +		.or.pmode.eq.'FILLEDHISTOGRAM')then
		call fhist(npts, x, y)
	else if(pmode.eq.'BARGRAPH'.or.pmode.eq.'BAR-GRAPH')then
		call barplt(NPTS,X,Y,DELX,DELY)
	else if(pmode.eq.'STACKEDBAR'.or.pmode.eq.'STACKED-BAR')then
		call sbarplt(NPTS,X,Y,DELX,DELY)
	else if(pmode .eq. 'PIE')then
		call pieplot(npts, x, y, delx, dely)
	else
		call xtext('ERROR: Unknown plotting mode:')
		write(tstring, *)pmode
		call xtext(tstring)
	end if

c reset colour back if needed       
	if(abs(rgbd(1)-rgb(1)).ge.1.or.abs(rgbd(2)-rgb(2)).ge.1.or.
     +	abs(rgbd(3) -rgb(3)).ge.1)then
		call farkc(rgb(1),rgb(2),rgb(3))
	end if
      if(titles)then
      delta = (ymaxp - yminp)/200.
      call txangm(0.0)
      if(titpos.eq.1)then
		call ctit(ymaxp+delta, title, 'x')
      else if(titpos.eq.2)then
		call rtit(xmaxp, ymaxp+delta, title)
      else
		call txtm(xminp,ymaxp+delta,title,60)
      end if

      if(title2.ne.' ')call txtm(xminp,ymaxp+2.*fnplot*by,title2,60)
      if(title3.ne.' ')call rtit(xmaxp,ymaxp+delta,title3)
      if(title4.ne.' ')call rtit(xmaxp,ymaxp+2.*fnplot*by,title4)
      call ctit(yxst-3.0*ychsiz,xlab,'x')
      if(xlab2.ne.' ')call ctit(yxst-4.5*ychsiz,xlab2,'x')
      if(xlab3.ne.' ')call rtit(xmaxp,yxst-4.5*ychsiz,xlab3)
      if(xlab4.ne.' ')call txtm(xminp,yxst-4.5*ychsiz,xlab4,60)
      call txangm(90.0)
      call ctit(xyst-7.8*xchsiz,ylab,'y')
      if(ylab2.ne.' ')call ctit(xyst-10.*xchsiz,ylab2,'y')
      call txangm(tang)
      end if
	else if(threed)then
c no plot if no points
		if(npts.le.0)goto 935
		call tdplot(npts, x, y, dely, pmode, isymb, symb,
     +sxl, syl, sxh, syh, ntickx, nticky, paxes, ccode,
     +labx, xlab, ylab, zlab,
     +fiplot, fmplot, fjplot, fnplot, tdfill)
	end if
c place to escape to from errors in GOPLOT command
930	continue

	else if(inst.eq.'3D')then
		threed = .TRUE.
		call sprompt('Give rotation angles')
		call sprompt('(Present angles are:')
		write(tstring, *)angle(1), angle(2), angle(3)
		call sprompt(tstring)
		call getit(buff, 1)
		call dcode(buff,ain,ifix,igrd,k)
		angle(1) = ain(1)
		angle(2) = ain(2)
		angle(3) = ain(3)
c fill 3D plots?
	else if(inst.eq.'3DFILL')then
		tdfill = .TRUE.
	else if(inst.eq.'3DNOFILL')then
		tdfill = .FALSE.
c back to 2D and rescale the data
	else if(inst.eq.'2D')then
		threed = .FALSE.
      		call rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
  

	else
	   call xtext('ERROR: Unrecognised command:')
	   call xtext(inst)
      end if

998	continue

c end of ninst loop
	call bufrst()
1001	continue
c  The end of selecting the correct action for the instruction
	if(inter) then
		return
	else
c say we used up all of any externally set data
		call bufrst()
		goto 2
	end if
C ERROR MESSAGES
c900   	continue
c	call xtext('ERROR READING INPUT')
c      if(inter)then
c		return
c	else
c		inst = 'return'
c		goto 3
c	end if
       
901   call xtext('ERROR READING COMMANDS')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
       
c902   call xtext('File read to end')
c     if(inter)then
c		return
c	else
c		inst = 'return'
c		goto 3
c	end if
       
903   call xtext('ERROR: IN FILE OPEN FROM COMMAND "FILE"')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
904   call xtext('ERROR: IN FILE CLOSE FROM COMMAND "RETURN"')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
       
910	call xtext('(Command canceled)')
	if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if



920   call xtext('UNSPECIFIED ERROR FOUND')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
       
935   call xtext('ERROR: No data to plot')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
940   call xtext('ERROR: Nested Loops not possible')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
       
991   	continue
	write(tstring,*)'THERE ARE ONLY ',NPTS,' POINTS IN YOUR DATA FILE'
	call totext(tstring)
      call xtext('command ignored')
      if(inter)then
		return
	else
		inst = 'return'
		goto 3
	end if
       
      END
                    
                    
                    
                    

	
 
 




