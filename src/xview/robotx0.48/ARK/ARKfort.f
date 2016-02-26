c The ARK plotting package - FORTRAN part.
c Mainly by Alan Smale, Robin Corbet, Koji Mukai and Tim Naylor.
c This particular version has been hacked on considerably by
c RC and so may differ somewhat from other implementations of ARK


c FORTRAN set-up (includes some things from original pltopn)
	subroutine fsetup()
	include "comfil"
	include "axecom"
	af='*'
	istrt=0
	fsize=14.
	angle1 = 0.0
	call txangm(angle1)
	end
    
c move to a new position
	subroutine movxy(x, y)
	real x, y
	call xmove(x, y)
	end
    
c draw a line from the present position to a new position
	subroutine linxy(x, y)
	real x, y
	call xline(x, y)
	end

c draw a line between two points
	subroutine linxy2(x1, y1, x2, y2)
	real x1, y1, x2, y2
	call movxy(x1, y1)
	call linxy(x2, y2)
	end
    
c map user coordinates onto the plotting area
	subroutine limitm(xlow, ylow, xhi, yhi)
	double precision whatx, whaty
	include "comfil"
	include "axecom"
	call xlimitm(xlow, ylow, xhi, yhi)
	xu = xhi
	yu = yhi
	xl = xlow
	yl = ylow
	fx = whatx()
	fy = whaty()
	call txtsiz(xchsiz, ychsiz)
                       
	end

c for square plotting area
	subroutine limitms(xlow, ylow, xhi, yhi)
	double precision whatx, whaty
	include "comfil"
	include "axecom"
	call xlimits(xlow, ylow, xhi, yhi)
	xu = xhi
	yu = yhi
	xl = xlow
	yl = ylow
	fx = whatx()
	fy = whaty()
	call txtsiz(xchsiz, ychsiz)
                       
	end

    
    
    

	subroutine style(x, y, text, n, istyle)
c text centred or right justified
	character*(*) text
	character*81 string
	integer imax
	
	imax = MIN(len(string), n)
                     
	  do 11 i=1,n
11	  string(i:i)=text(i:i)

c tack a null on the end if there's space
	if(n.lt.imax)then
		text(n+1:n+1) = char(0)
	end if

	call xsptxt(x, y, string, n, istyle)

	do 30 i = 1,imax
30	string(i:i) = ' '
                  
	end
	


	subroutine nxtpag()
c clears the graph
	call xnewpage()
	end
    
c use text cursor instead of the default
	subroutine tcur(text)
	character*(*) text
	character*4 text2
	text2 = text
	text2(4:4) = char(0)
	call tcurs(text2)
	end  



  
	subroutine cursor(x,y)
	integer gotc
c  Read the cursor using Xlib calls
                                   
                                   
	call xcurse()
              
c infinite loop
10	continue
	call wherec()
	i = gotc()
	if(i.eq.1)goto 11
	goto 10
11	continue
	call getc(x, y)
            
	do 20 i = 1,30
	call wherec()
20	continue
           
	end

	subroutine tband(xout, yout, text)
c draws a text string at the cursor position (using xor)
	character*(*) text
	real xout, yout
	integer gotc

	maxlen = len(text)

c stop postscript output
	call haltps()
c change plot mode to XOR
	call gcxor()

c make the cursor invisible so text location is clearer
	call bcurs()
	call xcurse()
	
	call wherec()
	call getc(xout, yout)

	oldx = xout
	oldy = yout

c save current font status
	call savfont()

	call txtm(xout, yout, text, maxlen)	
110	continue
	call wherec()
	i = gotc()
	call getc(x, y)
	if(i.eq.1)goto 111
	if(x.ne.oldx.or.y.ne.oldy)then
		call rstfont()
		call txtm(oldx, oldy, text, maxlen)
		call rstfont()
		call txtm(x, y, text, maxlen)
		oldx = x
		oldy = y
	end if
	goto 110
111	continue


	call rstfont()
	call txtm(oldx, oldy, text, maxlen)
	
	call getc(x, y)
	xout = x
	yout = y     
c restore X plot mode to copy
	call rstfont()
	call gccopy()
c and back to postscript output again
	call startps()

	do 120 i = 1,10
	call wherec()
120	continue
	
	end



	subroutine rband(xin, yin, itype, xout, yout, set, press)
c rubber bands from an input position (xin, yin) and
c returns final cursor position (xout, yout)
c itype determines what is drawn
c itype = 0 = line
c itype = 1 = rectangle
c itype = 2 = circle
c itype = 3 = ellipse
c itype = 4 = arrow

c poly determines whether we are doing to do a polygon etc.
c in which case we accept mouse button 2 as well as 1

c press is the mouse button pressed
c this will always be 1 if we aren't in the poly mode

	real xin, yin, xout, yout
	integer itype
	integer gotc

	integer set
	integer press

	call setmask(set)

c stop sending postscript output
	call haltps()
c change plot mode to XOR
	call gcxor()
	
	oldx = xin
	oldy = yin

		if(itype.eq.0)then
			call movxy(xin, yin)
			call linxy(oldx, oldy)
		else if(itype.eq.1)then
			call boxm(xin, yin, oldx, oldy)
		else if(itype.eq.2)then
			call circle(xin, yin, oldx, oldy)
		else if(itype.eq.3)then
			call elipse(xin, yin, oldx, oldy)
		else if(itype.eq.4)then
			call arrow(xin, yin, oldx, oldy)
		end if

	call xcurse()
110	continue
	call wherec()
	i = gotc()
	call getc(x, y)
	if(i.ge.1)goto 111
	if(x.ne.oldx.or.y.ne.oldy)then
		if(itype.eq.0)then
			call movxy(xin, yin)
			call linxy(oldx, oldy)
			call movxy(xin, yin)
			call linxy(x, y)
		else if(itype.eq.1)then
			call boxm(xin, yin, oldx, oldy)
			call boxm(xin, yin, x, y)
		else if(itype.eq.2)then
			call circle(xin, yin, oldx, oldy)
			call circle(xin, yin, x, y)
		else if(itype.eq.3)then
			call elipse(xin, yin, oldx, oldy)
			call elipse(xin, yin, x, y)
		else if(itype.eq.4)then
			call arrow(xin, yin, oldx, oldy)
			call arrow(xin, yin, x, y)
		end if
		oldx = x
		oldy = y
	end if
	goto 110
111	continue


	press = i	

c make sure we erase last item drawn
	if(itype.eq.0)then
		call movxy(xin, yin)
		call linxy(oldx, oldy)
	else if(itype.eq.1)then
		call boxm(xin, yin, oldx, oldy)
	else if(itype.eq.2)then
		call circle(xin, yin, oldx, oldy)
	else if(itype.eq.3)then
		call elipse(xin, yin, oldx, oldy)
	else if(itype.eq.4)then
		call arrow(xin, yin, oldx, oldy)
	end if
	
	call getc(x, y)
	xout = x
	yout = y
c move current plotting position to original place
	call movxy(xin, yin)    
c restore X plot mode to copy
	call gccopy()
c and back to postscript output again
	call startps()


c restore button mask to only accepting button 1
	call setmask(1)

	
	end
    
    
	subroutine print()
	call psprint()
	end

         subroutine lnxax(tick,x,y,dist,nint)
c       included for mssl compatability.
         call lnax(0,tick,x,y,dist,nint)
         end
            
         subroutine lnyax(tick,x,y,dist,nint)
c       included for mssl compatability.
         call lnax(1,tick,x,y,dist,nint)
         end

c draws box with lower left corner =  (xl,yl)
c                upper right corner = (xu,yu)
         subroutine boxm(xl,yl,xu,yu)
         call movxy(xl,yl)
         call linxy(xl,yu)
         call linxy(xu,yu)
         call linxy(xu,yl)
         call linxy(xl,yl)
         end
            
         subroutine limit(sxl, syl, sxh, syh, xmin, ymin, xmax, ymax)
                 
c This routine can be used as  an  alternative  to  limitm.  The
c coordinates  (sxl,syl) and (sxh, syh) are the positions of the bottom
c left and top right of the plot as a  percentage  of  the  available
c plotting  area.  typical values might be (15.0, 15.0) (95.0, 95.0)
c the paramters (xmin, ymin) and (xmax, ymax) are the same  position
c but in data co-ordinates.
                                 
c       device independent version.  Tim Naylor, May 1986.
                                                          
	include "comfil"
	include "axecom"
                        
         if (sxh-sxl .eq. 0.0) then
           call errmes(0)
        call arktext( '* PLOTTING PACKAGE S/R LIMIT.  ERROR IN CALL' )
           WRITE(*,110) SXH
 110       FORMAT(' * THE HIGH AND LOW X PERCENTAGES WERE BOTH ', F4.1)
           return
         else
           xfac=(xmax-xmin)/(sxh-sxl)
         end if
         if (syh-syl .eq. 0.0) then
           call errmes(0)
           call arktext( '* PLOTTING PACKAGE S/R LIMIT.  ERROR IN CALL')
           WRITE(*,120) SYH
 120       FORMAT(' * THE HIGH AND LOW Y PERCENTAGES WERE BOTH ', F4.1)
           return
         else
           yfac=(ymax-ymin)/(syh-syl)
         end if
      call limitm(xmin-xfac*sxl, ymin-yfac*syl,xmax+xfac*(100.0-sxh),
     +ymax+yfac*(100.0-syh) )
                             
c       tell the label routines the default start postions.
         xstrt=xmin
         ystrt=ymin
         istrt=1
c       and where the title is to go.
         txpos=xmin
         typos=ymax+0.5*ychsiz
                              
         end
            
c analog of limit which uses a "square" plotting area
         subroutine limsq(sxl, syl, sxh, syh, xmin, ymin, xmax, ymax)
	include "comfil"
	include "axecom"
                        
         if (sxh-sxl .eq. 0.0) then
           call errmes(0)
        call arktext( '* PLOTTING PACKAGE S/R LIMIT.  ERROR IN CALL' )
           WRITE(*,110) SXH
 110       FORMAT(' * THE HIGH AND LOW X PERCENTAGES WERE BOTH ', F4.1)
           return
         ELSE
           XFAC=(XMAX-XMIN)/(SXH-SXL)
         END IF
         IF (SYH-SYL .EQ. 0.0) THEN
           CALL errmes(0)
           call arktext( '* PLOTTING PACKAGE S/R LIMIT.  ERROR IN CALL')
           WRITE(*,120) SYH
 120       FORMAT(' * THE HIGH AND LOW Y PERCENTAGES WERE BOTH ', F4.1)
           return
         ELSE
           YFAC=(YMAX-YMIN)/(SYH-SYL)
         END IF
      call LIMITMS(XMIN-XFAC*SXL, YMIN-YFAC*SYL,XMAX+XFAC*(100.0-SXH),
     +YMAX+YFAC*(100.0-SYH) )
                             
c       tell the label routines the default start postions.
         xstrt=xmin
         ystrt=ymin
         istrt=1
c       and where the title is to go.
         txpos=xmin
         typos=ymax+0.5*ychsiz
                              
         end
	

   
c set line width to "width" pixels
	subroutine lwidth(width)
	integer width
	call fwidth(width)
	end
    
    
	subroutine txtpnt(size)
	real	size
	call fsize(size)
	end
    
c set font style
	subroutine tstyle(style)
	integer style
	call fstyle(style)
	end

c set font family
	subroutine tfamly(famly)
	integer famly
	call ffam(famly)
	end
	
 
 
c This is essentially a replacement for the routine below
c but takes the text size in "points" as the argument
	subroutine txtset(chsz)
	real chsz
	include "axecom"
	call txtpnt(chsz)
	call txtsiz(xchsiz, ychsiz)
	fsize = chsz
	end


         subroutine chrszu(xsiz,ysiz)
         entry chrsz(xsiz, ysiz)
                                
c   selects size of characters (in user units) either  in
c   terms of x or y co-ordinates.  the ratio between the two should  be
c   xsize:ysize = 4:7.  if it  is  not,  the  size  resulting  in   the
c   biggest characters is taken.
                                
	include "comfil"
	include "axecom"
c       tell the axis routines the new character size.
                                                      
         if (7.0*xsiz/4.0 .gt. ysiz) then
           xchsiz=xsiz
           ychsiz=7.0*xsiz*fx/(fy*4.0)
         else
           ychsiz=ysiz
           xchsiz=4.0*ysiz*fy/(7.0*fx)
         end if
         end
            
         subroutine txtm(x,y,text,nchar)
           
	include "axecom"
	include "comfil"
        character text(*)
	character*82 string
	integer imax

	imax = MIN(len(string), nchar)
	
c       chop off the trailing blanks.
         do 10 mchar=imax, 1, -1
           if (text(mchar) .ne. ' ') then
             goto 20
           end if
 10      continue
                 
 20      continue
                     
	  do 11 i=1,mchar
11	  string(i:i)=text(i)
	  if(imax.lt.len(string))then
	  	string(mchar+1:mchar+1) = char(0)
	end if
                          
	call xtxtm(x, y, string, nchar)

 	do 50 i = 1,mchar+1
50	string(i:i) = ' '
      
                                       
         end
            
            
         subroutine lnlbl(nway,xstart,ystart,dist,ns)
                 
c       nway specifies direction
c 1) 0 = x direction, 1 = y direction and  (xstart,  ystart)   is
c the postion of the bottom left corner of the left character of
c the label.
c 2) 2 = x direction, 3 = y direction and (xstart, ystart)is  the
c the  position  of  the  bottom  left  corner of the axis to be
c labelled.
c dist is the separation between labels on the plot.
c ns is the no. of intervals
c original by Robin Corbet.  New version (including free format
c labels and default positioning) Tim Naylor march 1986.
                                                        
	include "axecom"
	include "comfil"
         character*1 lab1(20)
         character lab2*20, formt*10
         equivalence(lab1(1),lab2)
         integer form
c how many approximate character widths the y axis labels
c will be offset from the y axis for nway = 3
	data fractn/0.6/

                  
         if (nway.lt.0 .or. nway.gt.3) then
          write(tstring, *)'SUBROUTINE LNLBL CALLED IN UNKNOWN NWAY', NWAY
	   call toarkt(tstring)
	   return
         end if
               
         if (istrt .eq. 0) then
           call arktext('NEITHER LIMIT OR LNLBVL HAS BEEN CALLED.')
           return
         else if (istrt .eq. 1) then
           if (nway.eq.0 .or. nway.eq.2) then
             first=xstrt
           else if (nway.eq.1 .or. nway.eq.3) then
             first=ystrt
           end if
           step=dist
         else if (istrt .eq. 2) then
           first=start
           istrt=0
         else if (istrt .eq. 3) then
           first=start
           istrt=1
         end if
               
c the following positions to plot the labels in can be decided here.
         if (nway .eq. 0) then
           yplot=ystart
         else if (nway .eq. 1) then
           xplot=xstart
         else if (nway .eq. 2) then
           yplot=ystart - 1.3*ychsiz
         end if
C       NWAY=3 MUST WAIT UNTIL WE KNOW HOW MANY CHARACTERS THERE ARE.
                                                                     
         IF (AF.EQ.'F' .OR. AF.EQ.'f') THEN
C         "F" FORMAT.
           DO 190 I=1, NS+1
             FLABEL=FIRST + REAL(I-1)*STEP
C           SEE HOW MANY FIGURES THERE ARE BEFORE THE DECIMAL POINT
             IF (ABS(FLABEL/STEP) .LT. 1E-5) THEN
               NCHAR=1
             ELSE
               NCHAR=LOG10(ABS(FLABEL))+1
C             ALLOW FOR A MINUS SIGN.
               IF(FLABEL .LT. 0.0) NCHAR=NCHAR+1
C             ALLOW FOR A ZERO.
               IF (ABS(FLABEL) .LT. 1.0) NCHAR=NCHAR+1
             END IF
             NCHAR=NCHAR+ND+1
C           CONVERT TO CHARACTER FORMAT HERE
c             WRITE(LAB2,110)FLABEL
c alter to try to get f2c to cope with it
		write(lab2, *)flabel
 110         FORMAT(F20.5)
c make it just one approximate character size offset
	     if(nway .eq. 3) xplot = xstart - fractn*xchsiz
             if (nway .eq. 0) then
               xplot=xstart+(i-1)*dist
             else if (nway .eq. 1) then
               yplot=ystart+(i-1)*dist
             else if (nway .eq. 2) then
		xplot = xstart + (i-1)*dist
             else if (nway .eq. 3) then
               yplot=ystart - 2.0*ychsiz/7.0 + (i-1)*dist
             end if
	     if(nway.eq.2)then
		call style(xplot, yplot, lab1(16-nchar+nd), nchar, 0)
	     else if(nway.eq.3)then
		call style(xplot, yplot, lab1(21-nchar), nchar, 1)
	     else
               call txtm(xplot,yplot,lab1(16-nchar+nd),nchar)
	     end if
 190       continue
         ELSE IF (AF.EQ.'I' .OR. AF.EQ.'i') THEN
c         integer format.
           do 290 i=1, ns+1
             flabel=first + real(i-1)*step
             if (abs(flabel) .eq. 0.0) then
               nchar=1
             else
               nchar=log10(abs(flabel))+1
             end if
c           allow for a minus sign.
             if(flabel.lt.0.0)nchar=nchar+1
c           convert to character format here
             write(lab2,210)nint(flabel)
 210         format(i20)
		if(nway .eq. 3)xplot = xstart - fractn*xchsiz
             if (nway .eq. 0) then
               xplot=xstart+(i-1)*dist
             else if (nway .eq. 1) then
               yplot=ystart+(i-1)*dist
             else if (nway .eq. 2) then
		xplot = xstart + (i-1)*dist
             ELSE IF (NWAY .EQ. 3) THEN
               YPLOT=YSTART - 2.0*YCHSIZ/7.0 + (I-1)*DIST
             END IF
	     if(nway.eq.2)then
		call style(xplot, yplot, lab1(21-nchar), nchar, 0)
	     else if(nway.eq.3)then
		call style(xplot, yplot, lab1(21-nchar), nchar, 1)
	     else
             	call txtm(xplot,yplot,lab1(21-nchar),nchar)
	     end if
 290       continue
         else if (af .eq. '*') then
           if (dist .gt. 0.0) then
             call decade(first, first+step*ns, step, form, nchar, formt)
           else
             call decade(first+step*ns, first, step, form, nchar, formt)
           end if
           if (nway .eq. 3) xplot=xstart - (0.5+real(nchar))*xchsiz
		if(nway .eq. 3)xplot = xstart - fractn*xchsiz
           do 390 i=1, ns+1
             flabel=first + real(i-1)*step
             if (nway .eq. 0) then
               xplot=xstart+(i-1)*dist
             else if (nway .eq. 1) then
               yplot=ystart+(i-1)*dist
             else if (nway .eq. 2) then
		xplot = xstart + (i-1)*dist
             else if (nway .eq. 3) then
               yplot=ystart - 2.0*ychsiz/7.0 + (i-1)*dist
             end if
             if (form .eq. 0) then
               write (lab2,formt, err = 910) nint(flabel)
             else
               write (lab2, formt, err = 910) flabel
             end if
	     if(nway.eq.2)then
		call style(xplot, yplot, lab1(21-nchar), nchar, 0)
	     else if(nway.eq.3)then
		call style(xplot, yplot, lab1(21-nchar), nchar, 1)
	      else
                call txtm(xplot,yplot,lab1(21-nchar),nchar)
	      end if
 390       continue
         else
           write(tstring, *)'format type', af, 'unknown.'
	   call toarkt(tstring)
           return
         end if
               
c       tell the default axis labelling where the numbers are.
         if (nway.eq.1 .or. nway.eq.3)  then
           yxpos=xplot-xchsiz
           yypos=ystart+0.5*dist*real(ns)
         else if (nway.eq.0 .or. nway.eq.2) then
           xxpos=xstart+0.5*dist*real(ns)
           xypos=yplot-ychsiz
         end if
               
         return
               
 910     continue
         write(tstring, *)'ERROR ATTEMPTING TO WRITE AXIS LABEL', FLABEL
	 call toarkt(tstring)
         call arktext('TO STRING.')
              
         END
            
            
            
         subroutine lnlbvl(fstart,fstep)
                 
	include "axecom"
         start=fstart
         step=fstep
         istrt=istrt+2
                      
         end
            
         subroutine txangm(alpha)      
c       specifies rotation of characters and symbols.

	call psangle(alpha)
         end
            
         subroutine linsc2(xmin,xmax,n,xminp,xmaxp,np,dist)
	include "comfil"
                 
C       XMIN    MINIMUM OF DATA VALUES
C       XMAX    MAXIMUM OF DATA VALUES
C       N       APPROXIMATE NUMBER OF DIVISIONS DESIRED
C       XMINP   RETURNED NEW MINIMUM VALUE
C       XMAXP   RETURNED NEW MAXIMUM VALUE
C       NP      RETURNED NUMBER OF DIVISIONS
                                            
         DIMENSION VINT(4),SQR(3)
         DATA VINT/1.0,2.0,5.0,10.0/
         DATA SQR/1.414214,3.162278,7.071068/
         DATA DEL/0.00002/
                          
         IF(XMAX.EQ.XMIN) THEN
           call errmes(0)
c           call arktext( '* PLOTTING PACKAGE S/R LINSC2.  ERROR IN CALL
           write(tstring, *) '* MAX VALUE = MIN VALUE =', XMAX
	   call toarkt(tstring)
           return
         ELSE IF (XMIN .GT. XMAX) THEN
          call errmes(0)
          call arktext( '* PLOTTING PACKAGE S/R LINSC2.  ERROR IN CALL')
          write(tstring, *) '* XMAX < XMIN. XMAX =', XMAX, 'XMIN =', XMI
	  call toarkt(tstring)
          return
         end if
               
         if (n .eq. 0) then
        call arktext( '* WARNING FROM PLOTTING PACKAGE S/R LINSC2.')
           call arktext( '* N=0. SETTING N EQUAL TO 1.')
           NG=1
         else
           ng=n
         end if
         fn=float(ng)
         a=(xmax-xmin)/fn
         al=alog10(a)
         nal=al
         if(a.lt.1.0) nal=nal-1
         tnal=10.0**nal
         b=a/tnal
                 
         do 10 i=1,3
           if(b.lt.sqr(i)) go to 20
 10      continue
         i=4
            
 20      dist=vint(i)*tnal
                          
         fm1=xmin/dist
         m1=fm1
         if(fm1.lt.0.0) m1=m1-1
         if(abs(float(m1)+1.0-fm1).lt.del) m1=m1+1
         xminp=dist*float(m1)
                             
         fm2=xmax/dist
         m2=fm2+1.0
         if(fm2.lt.-1.0) m2=m2-1
         if(abs(fm2+1.0-float(m2)).lt.del) m2=m2-1
         xmaxp=dist*float(m2)
                             
         np=m2-m1
                 
         end
            
         subroutine decade( minz, maxz, dist, form, nchar, formt )
                 
c minz, maxz and dist are the max and min labels, and the difference
c between each label.
c zn=figures after decimal place.
c nchar=total number of chars in the number.
c form=0 => "i" format
c form=1 => "f" format
c form=2 => "e" format
c formt is returned, and is used like "write(string,formt) number "
                                                                   
         REAL MINZ, MAXZ, DIST
         INTEGER FORM, NCHAR, ZN
         CHARACTER*10 FORMT
         REAL BIG, TEMP
c eswtch should help to decide whether we use 'F'
c or 'E' format
	data eswtch/0.001/
	
                       
         BIG = MAX( ABS( MINZ ), ABS( MAXZ ) )
         IF(BIG.GT.eswtch .AND. BIG.LE.99999.9) THEN
C         CAN USE SIMPLE FORMAT.
           NCHAR = ABS(LOG10(BIG)) + 1
C         IF IT'S NEGATIVE ALLOW FOR SIGN.
           IF( BIG .EQ. -MINZ ) THEN
             NCHAR = NCHAR + 1
           ELSE IF( MINZ .LT. 0.0 ) THEN
             ZN = INT( LOG10( -MINZ ) ) + 2
             NCHAR = MAX( NCHAR, ZN )
           END IF
           IF( ABS(DIST) .GT. 0.99998) THEN
C           JUST PRINT INTEGERS.
             FORM = 0
C           WRITE THE FORMAT STATEMENT.
             FORMT='(I20)'
           ELSE
C           IT'S REALS FOLKS
             FORM = 1
             TEMP = LOG10( ABS(DIST) )
             IF( MOD( TEMP, 1.0 ) .EQ. 0.0 ) THEN
               ZN = -TEMP
             ELSE
               ZN = -TEMP + 0.99999
             END IF
             NCHAR = NCHAR + ZN + 1
             WRITE( FORMT, 130 ) ZN
 130         FORMAT( '(F20.', I1, ')' )
           END IF
         ELSE
C         ITS E SOMTHING FORMAT.
           FORM = 2
           TEMP = LOG10(ABS ( BIG / DIST ) )
           IF( MOD( TEMP, 1.0 ) .EQ. 0.0 ) THEN
             ZN = LOG10(ABS( BIG / DIST ) ) + 1
           ELSE
             ZN = LOG10(ABS( BIG / DIST ) ) + 2
           END IF
           NCHAR = ZN + 7
           WRITE( FORMT, 160 ) ZN
 160       FORMAT( '(1PE20.', I1, ')' )
         END IF
               
         END
            

         subroutine lnax(id,tiksiz,x,y,dist,nd)
	include "comfil"
                 
C LINEAR AXIS DRAWING ROUTINE.  DRAWS A LINE STARTING AT (X,Y) IN THE
C DIRECTION  DEFINED BY ID WITH A TICK SIZE TIKSIZ ON THE 'INSIDE' OF
C THE AXIS (USE NEGATIVE NUMBERS IF YOU WANT  THEM  ON  THE  OUTSIDE)
C WITH ND INTERVALS OF DIST BETWEEN TICK MARKS.
C ID DEFINES DIRECTION OF AXIS AS :-
C ID=0 +X DIRECTION    TICKS ABOVE AXIS
C ID=1 +Y DIRECTION    TICKS ON LEFT OF AXIS
C ID=2 -X DIRECTION    TICKS BELOW AXIS
C ID=3 -Y DIRECTION    TICKS ON RIGHT OF AXIS
C                         MSSL COMPATABLE VERSION BY MARK WALKER FEB. 1
                                                                       
                                                                       
         if (id.eq.0 .or. id.eq.1) then
           x1=x+real(nd*dist*(1-id))
           y1=y+real(nd*dist*id)
           call movxy(x,y)
           call linxy(x1,y1)
           i=id
           tick=tiksiz
         else if (id.eq.2 .or. id.eq.3) then
           x1=x
           y1=y
           call movxy( x-real(nd*dist*(3-id)), y-real(nd*dist*(id-2)) )
           call linxy(x1,y1)
           i=id-2
           tick=-tiksiz
         else
c           call arktext( 'ERROR IN CALL TO PLOTTING PACKAGE SUBROUTINE'
           write(tstring, *) 'LNAX.  AXIS DIRECTION NUMBER = I =', I
	   call toarkt(tstring)
         end if
               
         do 10 j = 0,nd
           call movxy(x1-real(dist*j*(1-i))-real(((-1)**j)*i*
     +    tick*0.5)-real(tick*i*0.5),y1-real(dist*i*j)-
     +    real(tick*(1-i)*0.5*((-1)**j))+real(tick*(1-i)*0.5))
           call linxy(x1-real(dist*j*(1-i))+real(((-1)**j)*i*
     +    tick*0.5)-real(tick*i*0.5),y1-real(dist*j*i)+real(tick
     +    *(1-i)*((-1)**j))+real(tick*0.5*(1-i)*(1-(-1)**j)))
 10      continue
                 
         end
            
c log axis routines
                   
       subroutine lgax(i,tiksiz,x0,y0,dist,nint)
	
C
C LOG AXIS DRAWING ROUTINE
C DRAWS A LOG AXIS (NO ANOTATION) IN THE DIRECTION DEFINED BY I, WI
C EVERY DECADE DIST APPART.   N IS THE NUMBER OF DECADES.   TIKSIZ
C IS THE SIZE OF THE DECADE TICKS, ALL OTHERS ARE HALF THAT SIZE, A
C ARE AT 2,3,4,5,6,7,8, AND 9 OF VALUES BETWEEN DECADE TICKS.
C
C I DEFINES DIRECTION AS :-
C I=0 +X DIRECTION
C I=1 +Y DIRECTION
C I=2 -X DIRECTION
C I=3 -Y DIRECTION
C TICKS ARE ON 'INSIDE' OF BOX
C WITH AXIS SIDES GIVEN BY I=0,1,2,3.
C
C --- ENTRY POINTS LGXAX AND LGYAX ALLOW AXES IN POSITIVE DIRECTIONS
C --- TO BE DRAWN, ARGUMENT I IS THEN DROPPED; TICKS ARE THEN
C --- ON POSITIVE SIDE OF AXIS.
C
C --- JFEJ 1170 VERSION 14 OCTOBER 1979.
C --- ENTRY POINTS ALSO ADDED.
C
       REAL TCKS(9)
       DATA TCKS/0.3010,0.1761,0.1250,0.0969,0.0792,0.0669,0.0580,
     1 0.0511,0.0458/
C
       J=I-(I/4)*4+1
       TIKSZ=TIKSIZ
       GO TO 20
C
       ENTRY LGXAX(TIKS,X0,Y0,DIST,NINT)
       TIKSZ=TIKS
       J=1
       GO TO 20
C
       ENTRY LGYAX(TIK,X0,Y0,DIST,NINT)
       TIKSZ=-TIK
       J=2
C
   20 XTICK=0.0
       YTICK=0.0
       XINC=0.0
       YINC=0.0
         call ptxtck(j,x0,y0,dist,tiksz,nint)
       goto(1,2,3,4),j
    1 xinc=dist
       ytick=tiksz
       goto5
    2 yinc=dist
       xtick=- tiksz
       goto5
    3 xinc=-dist
       ytick=-tiksz
       goto5
    4 yinc=-dist
       xtick=tiksz
    5 continue
c
       x=x0
       y=y0
       x1=x+xtick
       y1=y+ytick
       call movxy(x1,y1)
       call linxy(x,y)
c
       DO 10 K=1,NINT
C --- THIS PART GENERATES TICKS WITHIN EACH DECADE
       DO 13 M=1,8
       GOTO(6,6,7,7),J
    6 L=M
       GOTO8
    7 L=10-M
    8 X=X+XINC*TCKS(L)
       Y=Y+YINC*TCKS(L)
       CALL LINXY(X,Y)
       X1=X+XTICK*0.5
       Y1=Y+YTICK*0.5
       CALL LINXY(X1,Y1)
       CALL LINXY(X,Y)
   13 CONTINUE
       X=X0+FLOAT(K)*XINC
       Y=Y0+FLOAT(K)*YINC
       CALL LINXY(X,Y)
       X1=X+XTICK
       Y1=Y+YTICK
       CALL LINXY(X1,Y1)
       CALL LINXY(X,Y)
   10 CONTINUE
       END
          
       SUBROUTINE PTXTCK(IDIR,X,Y,DX,TICK,NINT)
C
C   STORES TICK SIZE AND LOCATION INFO
C   H.E.HUCKLE MSSL MAY 80
C
        DIMENSION XL(4),YL(4),DXL(4),NINTL(4),TICKL(4)
        DATA XL,TICKL,YL,DXL/16*0.0/
        DATA NINTL/4*0/

        INDEX=IDIR
        XL(INDEX)=X
        YL(INDEX)=Y
        DXL(INDEX)=DX
        NINTL(INDEX)=NINT
        TICKL(INDEX)=ABS(TICK)

900        return

        ENTRY GTXTCK(IDIR,X,Y,DX,TICK,NINT)

        INDEX=IDIR
        X=XL(INDEX)
        Y=YL(INDEX)
        DX=DXL(INDEX)
        TICK=TICKL(INDEX)
        NINT=NINTL(INDEX)
        GO TO 900

        END
           
c modified version of lgax
c this contains additional arguments - the minimum and maximum values
c to plot. This enables less than a complete decade to be
c plotted which makes it possible to do neater plots
                                                    
       subroutine lgax2(i,tiksiz,x0,y0,dist,nint,minval,maxval)

	real minval, maxval

       REAL TCKS(9)
       DATA TCKS/0.3010,0.1761,0.1250,0.0969,0.0792,0.0669,0.0580,
     1 0.0511,0.0458/
 
       J=I-(I/4)*4+1
       TIKSZ=TIKSIZ
       GO TO 20
 
       ENTRY LGXAX2(TIKS,X0,Y0,DIST,NINT)
       TIKSZ=TIKS
       J=1
       GO TO 20
 
       ENTRY LGYAX2(TIK,X0,Y0,DIST,NINT)
       TIKSZ=-TIK
       J=2
 
   20 XTICK=0.0
       YTICK=0.0
       XINC=0.0
       YINC=0.0
         CALL PTXTCK(J,X0,Y0,DIST,TIKSZ,NINT)
       GOTO(1,2,3,4),J
    1 XINC=DIST
       YTICK=TIKSZ
       GOTO5
    2 YINC=DIST
       XTICK=- TIKSZ
       GOTO5
    3 XINC=-DIST
       YTICK=-TIKSZ
       GOTO5
    4 YINC=-DIST
       XTICK=TIKSZ
    5 CONTINUE

       X=X0
       Y=Y0
       X1=X+XTICK
       Y1=Y+YTICK
       CALL MOVXY(X1,Y1)
	call movlin(i, x, y, minval, maxval)

       DO 10 K=1,NINT
C --- THIS PART GENERATES TICKS WITHIN EACH DECADE
       DO 13 M=1,8
       GOTO(6,6,7,7),J
    6 L=M
       GOTO8
    7 L=10-M
    8 X=X+XINC*TCKS(L)
       Y=Y+YINC*TCKS(L)
	call movlin(i, x, y, minval, maxval)
                                     
                                     
       X1=X+XTICK*0.5
       Y1=Y+YTICK*0.5
	call movlin(i, x1, y1, minval, maxval)
	call movlin(i, x, y, minval, maxval)
   13 CONTINUE
       X=X0+FLOAT(K)*XINC
       Y=Y0+FLOAT(K)*YINC
	call movlin(i, x, y, minval, maxval)
       X1=X+XTICK
       Y1=Y+YTICK
	call movlin(i, x1, y1, minval, maxval)
	call movlin(i, x, y, minval, maxval)
   10 CONTINUE
11	continue
       END


c similar to the routine above but all marks are the same
c length to enable a "grid" to be plotted.
c These two routines should be combined at some point.

       SUBROUTINE LGAXG(I,TIKSIZ,X0,Y0,DIST,NINT,minval,maxval)
	
	real minval, maxval
                    
       REAL TCKS(9)
       DATA TCKS/0.3010,0.1761,0.1250,0.0969,0.0792,0.0669,0.0580,
     1 0.0511,0.0458/
 
       J=I-(I/4)*4+1
       TIKSZ=TIKSIZ
 
 
   20 XTICK=0.0
       YTICK=0.0
       XINC=0.0
       YINC=0.0
         CALL PTXTCK(J,X0,Y0,DIST,TIKSZ,NINT)
       GOTO(1,2,3,4),J
    1 XINC=DIST
       YTICK=TIKSZ
       GOTO5
    2 YINC=DIST
       XTICK=- TIKSZ
       GOTO5
    3 XINC=-DIST
       YTICK=-TIKSZ
       GOTO5
    4 YINC=-DIST
       XTICK=TIKSZ
    5 CONTINUE

       X=X0
       Y=Y0
       X1=X+XTICK
       Y1=Y+YTICK
       CALL MOVXY(X1,Y1)
	call movlin(i, x, y, minval, maxval)

       DO 10 K=1,NINT
C --- THIS PART GENERATES TICKS WITHIN EACH DECADE
       DO 13 M=1,8
       GOTO(6,6,7,7),J
    6 L=M
       GOTO8
    7 L=10-M
    8 X=X+XINC*TCKS(L)
       Y=Y+YINC*TCKS(L)
	call movlin(i, x, y, minval, maxval)
                                     
                                     
       X1=X+XTICK
       Y1=Y+YTICK
	call movxy(x1, y1)
	call movlin(i, x, y, minval, maxval)
   13 CONTINUE
       X=X0+FLOAT(K)*XINC
       Y=Y0+FLOAT(K)*YINC
	call movlin(i, x, y, minval, maxval)
       X1=X+XTICK
       Y1=Y+YTICK
	call movxy(x1, y1)
	call movlin(i, x, y, minval, maxval)
   10 CONTINUE
11	continue
       END
          
          
c if x or y is within allowed bounds draw a line, otherwise move
c to new position
	subroutine movlin(i, x, y, minval, maxval)
	real minval, maxval
	logical xtest
	xmove = x
	ymove = y
	if(i.eq.0.or.i.eq.2)then
		tval = x
		xtest = .true.
	else if(i.eq.1.or.i.eq.3)then
		tval = y
		xtest = .false.
	end if
	if(tval.lt.minval)then
		if(xtest)then
			xmove = minval
		else
			ymove = minval
		end if
		call movxy(xmove, ymove)
	else if(tval.gt.maxval)then
		if(xtest)then
			xmove = maxval
		else
			ymove = maxval
		end if
		call movxy(xmove, ymove)
	else
		call linxy(x,y)
	end if
	
	end
    
    
        subroutine lglbl(nway,xstart,ystart,dist,ns)
                                                    
c       Log axis labels routine.
c       nway specifies direction
c 1) 0 = x direction, 1 = y direction and  (xstart,  ystart)  is
c the postion of the bottom left corner of the left character of
c the label.
c 2) 2 = x direction, 3 = y direction and (xstart, ystart)is the
c the  position  of  the  bottom  left  corner of the axis to be
c labelled.
c dist is the separation between labels on the plot.
c ns is the no. of intervals
                            
c A device independent version that uses smaller characters for  the
c power of 10 if available.                     Tim Naylor May 1987.
                                                                    
        include 'axecom'
        include 'comfil'
	
        character*1 lab1(20)
        character*20 lab2
        equivalence(lab1(1),lab2)
                                 
c       First decide what the first label is.
        if (istrt .eq. 0) then
          call arktext('Error message from plotting package s/r lglbl.')
          call arktext('Neither limit or lnlbvl has been called.')
          return
        else if (istrt .eq. 1) then
          if (nway.eq.0 .or. nway.eq.2) then
            first=xstrt
          else if (nway.eq.1 .or. nway.eq.3) then
            first=ystrt
          end if
          step=dist
        else if (istrt .eq. 2) then
          first=start
          istrt=0
        else if (istrt .eq. 3) then
          first=start
          istrt=1
        end if
              
        do 100 i=1,ns+1
          flabel= first+(i-1)*step
c         See how many figures there are before the decimal point.
          if (nint(flabel) .eq. 0)then
            idecs=1
            nchar=1
          else
            idecs=log10(abs(flabel))+1
            nchar=idecs
            if(flabel.lt.0.0)nchar=nchar+1
          end if
c         Convert to character format here.
          write(lab2,15)nint(flabel)
15        format(i20)
c         Now work out the positions.
          if (nway .eq. 0)then
            xplot=xstart+(i-1)*dist
            yplot=ystart
          else if (nway .eq. 1) then
            yplot=ystart+(i-1)*dist
            xplot=xstart
          else if (nway .eq. 2) then
            xplot=xstart + (i-1)*dist - xchsiz*real(nchar+2)/2.0
            yplot=ystart - 1.3*ychsiz
          else if (nway .eq. 3) then
            yplot=ystart + (i-1)*dist - 2.0*ychsiz/7.0
            xplot=xstart - (0.5+real(nchar+2))*xchsiz
          end if
c         And plot them.
          call txtm(xplot,yplot,'10',2)
c         Calculate where the power should be.
          delx=2.0*xchsiz
          dely=0.5*ychsiz
c         Reduce character size, and write the power.
                                                     

	oldsiz = fsize

	if(fsize.ge.14.0) then
		cs = fsize - 4.
	else
		cs = fsize - 2.
	end if
	call txtset(cs)
          call txtm(xplot+delx,yplot+dely,lab1(21-nchar),nchar)
c Return to old character  size  (remembering  chrszu  has  changed
c xchsiz and ychsiz).
	cs = oldsiz
	call txtset(cs)
100     continue
                
c       Tell the default axis labelling where the numbers are.
        if (nway.eq.1 .or. nway.eq.3)  then
          yxpos=xplot-xchsiz
          yypos=ystart+0.5*dist*real(ns)
        else if (nway.eq.0 .or. nway.eq.2) then
          xxpos=xstart+0.5*dist*real(ns)
          xypos=yplot-ychsiz
        end if
              
110     return
        end
           
           
        subroutine xxxcnt(idir,x0,y0,x1,y1)
	
        dimension iswtch(2,2)
        data iswtch/1,2,2,1/
        data xl,yl/2*10e+20/
        if(idir.eq.-1)go to 800
        ih=0
        if(y1.gt.y0)ih=1
        isw=iswtch(idir+1,ih+1)
        go to (1,2),isw
1        continue
        if(xl.eq.x0.and.yl.eq.y0)go to 11
        call movxy(x0,y0)
11        call linxy(x1,y1)
        xl=x1
        yl=y1
        go to 3
2        continue
        if(xl.eq.x1.and.yl.eq.y1)go to 21
        call movxy(x1,y1)
21        call linxy(x0,y0)
        xl=x0
        yl=y0
3        continue
900        return
800        xl=10e20
        yl=10e20
        go to 900
        end

 
           
          
           
c     contour line generating programme

c     xmin,xmax,ymin,ymax define the rectangle within which
c             the contour plot will be contained
c
c     array is the n by m array of data to be contoured
c
c     value is the vector of length iv of values at which
c             contour lines will be generated
c
c
      subroutine contor(xmin,ymin,xmax,ymax,array,n,m,value,iv)
	
      real array(n,m),value(iv)

        ivv=0
      mm=m-1
      nn=n-1

c     compute x,y increments

      deltx=(xmax-xmin)/float(nn)
      delty=(ymax-ymin)/float(mm)

      do 1 j=1,nn
        idir=j-j/2*2
      x=xmin+(j-1)*deltx

      do 2 i=1,mm
        ii=i
        if(idir.eq.0)ii=mm+1-i
        y=ymin+(ii-1)*delty

c     get data values for a cell

      v1=array(j,ii)
      v2=array(j+1,ii)
      v3=array(j,ii+1)
      v4=array(j+1,ii+1)

c     check each value against data values

        ivv=1-ivv
      do 3 k=1,iv
        kk=k
        if(ivv.eq.1)kk=iv+1-k
      val=value(kk)
      icase=1

c     record outcome of comparisons in icase

      if(val.gt.v1) icase=icase+1
      if(val.gt.v2) icase=icase+2
      if(val.gt.v3) icase=icase+4
      if(val.gt.v4) icase=9-icase

c     treat each case seperately

      go to (3,4,5,6,7,8,9,10),icase

c     do linear interpolation

    4 x0=x+deltx*(val-v1)/(v2-v1)
      y0=y
      x1=x
      y1=y+delty*(val-v1)/(v3-v1)
      go to 11
c
    5 x0=x+deltx*(val-v1)/(v2-v1)
      y0=y
      x1=x+deltx
      y1=y+delty*(val-v2)/(v4-v2)
      go to 11

    6 x0=x
      y0=y+delty*(val-v1)/(v3-v1)
      x1=x+deltx
      y1=y+delty*(val-v2)/(v4-v2)
      go to 11

    7 x0=x
      y0=y+delty*(val-v1)/(v3-v1)
      x1=x+deltx*(val-v3)/(v4-v3)
      y1=y+delty
      go to 11

    8 x0=x+deltx*(val-v1)/(v2-v1)
      y0=y
      x1=x+deltx*(val-v3)/(v4-v3)
      y1=y+delty
      go to 11

    9 x0=x+deltx*(val-v1)/(v2-v1)
      y0=y
      x1=x
      y1=y+delty*(val-v1)/(v3-v1)
        call xxxcnt(idir,x0,y0,x1,y1)

   10 x0=x+deltx*(val-v3)/(v4-v3)
      y0=y+delty
      x1=x+deltx
      y1=y+delty*(val-v2)/(v4-v2)

c     output the line segment

11        continue
        call xxxcnt(idir,x0,y0,x1,y1)

    3 continue

    2 continue

    1 continue
        call xxxcnt(-1,x0,x0,x0,x0)
      end

         
	subroutine errmes(i)
	integer i
	call arktext('Error detected in a plotting subroutine')
	end
    
	subroutine arktext(string)
	character*(*)	string
	call toarkt(string)
	end
    
       SUBROUTINE PLTCLS
c dummy routine
       END
