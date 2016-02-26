C DRAW DASHED LINE
      subroutine dline(x1,y1,x2,y2,fndash,ratio)
	include	'robcom'
C CALCULATE PARAMETERS OF LINE DEFINED BY X1,Y1 X2,Y2
C MAKE SURE Y1 IS LESS THAN Y2
      if(y1.gt.y2)then
        tempx=x1
        tempy=y1
        x1=x2
        y1=y2
        x2=tempx
        y2=tempy
      end if
C Check no. of dashes is at least 1
      if(fndash.lt.1.0)fndash=1.0
      if(x2-x1.eq.0.0)goto 900
      grad=(y2-y1)/(x2-x1)
      fint=y2-grad*x2
      ndash=nint(fndash)
      xstep=(x2-x1)/fndash
      delta=xstep*ratio/(ratio+1.0)
                                   
      do 10 i=1,ndash
        xf=(i-1)*xstep+x1
        yf=grad*xf+fint
        call movxy(xf,yf)
        xs=i*xstep+x1
        xd=xs-delta
        yd=grad*xd+fint
                       
        call linxy(xd,yd)
               
10    continue
              
11    continue
      return
            
C GRADIENT WOULD BE INFINITE
900   continue
      ndash=nint(fndash)
      ystep=(y2-y1)/fndash
      delta=ystep*ratio/(ratio+1.0)
                                   
      do 110 i=1,ndash
        yf=(i-1)*ystep+y1
        xf=x2
        call movxy(xf,yf)
        ys=i*ystep+y1
        yd=ys-delta
                   
        if(yd.le.y2)then
          call linxy(x2,yd)
        else
          call linxy(x2,y2)
          goto 111
        end if
              
110    continue
               
111    continue
      end
         
C REMOVE COMMENTS FROM INSTRUCTIONS
      subroutine remcom(inst)
      character*(*) inst
	parameter(ibang = ichar('!'))
	parameter(ihash = ichar('#'))
	parameter(itab = ichar('	'))
	integer istart, i, imax, itotal
c !! is treated differently
c it's repeat the last instruction like in the C shell
	if(inst.eq.'!!')return           
	imax = len(inst)
      do 10 i=1,imax
	ic = ichar(inst(i:i))
      IF(ic.eq.ibang.or.ic.eq.ihash)then
       do 12 j=i,imax
12     inst(j:j)=' '
c if the whole line started with a comment don't need to do
c anything else
	if(i.eq.1)then
		return
	else
		goto 11
	end if
c convert tabs to spaces
	else if(ic.eq.itab)then
		inst(i:i) = ' '
      end if
10    continue
11    continue
c remove leading whitespace
	do 20 i = 1, imax
		if(inst(i:i).ne.' ')goto 25
20	continue
25	istart = i
	if(istart .eq. 1) return
	itotal = imax - istart
	do 30 i = 1, itotal
		itemp = istart+i-1
		inst(i:i) = inst(itemp:itemp)
30	continue
c	print*,'output string is', inst         
      END
         

c This routine (commented out) should be independent
c of whether you're on an ASCII or EBCDIC machine
c however it's a lot slower than the other routine
c	subroutine LCTOUC(INST)
C Convert from lower to upper case as,
C since ROBOT was originally written on a terrible Fujitsu computer,
C only upper case is allowed in the command names
c      character*(*) INST
c	character UC(26)
c	character lc(26)
c	data UC/'A','B','C','D','E','F','G','H','I','J',
c     +'K','L','M','N','O','P','Q','R','S','T','U','V','W','X',
c     +'Y','Z'/
c	data lc/'a','b','c','d','e','f','g','h','i','j',
c     +'k','l','m','n','o','p','q','r','s','t','u','v','w','x',
c     +'y','z'/
c convert from lower to upper case if required
c	imax = len(inst)
c	do 20 k=1,imax
c	 do 15 j=1,26
c	  if(inst(k:k).eq.lc(j))inst(k:k)=UC(J)
c15	 continue
c20	continue
c	end
    
c this is faster than the above routine
	subroutine lctouc(inst)
	character*(*) inst
	imax = len(inst)
	do 10 i = 1, imax
		j = ichar(inst(i:i))
		if(j.gt.96.and.j.lt.123)inst(i:i) = char(j-32)
10	continue
	end  
    
    
C CALCULATE MOMENTS OF X AND Y DATA
        subroutine moment(x,y,npts)
C DATA ATTRIBUTES: MEAN AND STANDARD DEVIATION
C FOR USE IN RANDOM AND DCODE
	include	'robcom'
        common/atri/ymean,yvar,stime
        double precision stime
        real x(*),y(*)
        double precision xmeand,ymeand,sumxd,sumyd,xvard,yvard
        double precision xp,yp,xskew,yskew,xcurt,ycurt,sx,sy
        xmeand=0.0d0
        ymeand=0.0d0
        sumxd=0.0d0
        sumyd=0.0d0
        xvard=0.0d0
        yvard=0.0d0
        xskew=0.0d0
        yskew=0.0d0
        xcurt=0.0d0
        ycurt=0.0d0
C SUM OF DATA AND MEAN
        do 10 i=1,npts
        sumxd=sumxd+x(i)
        sumyd=sumyd+y(i)
10      continue
        sumx=sumxd
        sumy=sumyd
        write(tstring,*)'Sum of x values is: ',sumx
	call totext(tstring)
        write(tstring,*)'Sum of y values is: ',sumy
	call totext(tstring)
        write(ifil8,*)'Sum of x values is: ',sumx
        write(ifil8,*)'Sum of y values is: ',sumy
	if(npts.le.0)goto 999
        xmeand=sumxd/dble(npts)
        ymeand=sumyd/dble(npts)
        xmean=xmeand
        ymean=ymeand
        write(tstring,*)'Mean of X is: ',xmean
	call totext(tstring)
        write(tstring,*)'Mean of Y is: ',ymean
	call totext(tstring)
        write(ifil8,*)'Mean of X is: ',xmean
        write(ifil8,*)'Mean of Y is: ',ymean
C STANDARD DEVIATION
        do 50 i=1,npts
        xp=abs(xmeand-x(i))
        yp=abs(ymeand-y(i))
        sx=xp*xp
        sy=yp*yp
        xvard=xvard+sx
        yvard=yvard+sy
        sx=sx*xp
        sy=sy*yp
        xskew=xskew+sx
        yskew=yskew+sy
        sx=sx*xp
        sy=sy*yp
        xcurt=xcurt+sx
        ycurt=ycurt+sy
50      continue
	if(npts.le.1)goto 999
        xvard=sqrt(xvard/dble(npts-1))
        yvard=sqrt(yvard/dble(npts-1))
        xvar=xvard
        yvar=yvard
        write(tstring,*)'Standard deviation of x is: ',xvar
	call totext(tstring)
        write(tstring,*)'Standard deviation of y is: ',yvar
	call totext(tstring)
        write(tstring,*)'Standard error of x is: ',xvar/sqrt(real(npts))
	call totext(tstring)
        write(tstring,*)'Standard error of y is: ',yvar/sqrt(real(npts))
	call totext(tstring)
        write(ifil8,*)'Standard deviation of x is: ',xvar
        write(ifil8,*)'Standard deviation of y is: ',yvar
        write(ifil8,*)'Standard error of x is: ',xvar/sqrt(real(npts))
        write(ifil8,*)'Standard error of y is: ',yvar/sqrt(real(npts))
        if(xvar.ne.0.0)then
          xskw=xskew/(npts*xvar**3)
          xcur=xcurt/(npts*xvar**2)-3.
          call xtext('For X values')
          write(tstring,*)'Skewness= ',xskw
	call totext(tstring)
	   write(tstring, *)'Kurtosis= ',xcur
          write(ifil8,*)'For x values'
          write(ifil8,*)'Skewness= ',xskw
	  write(ifil8, *)'Kurtosis= ',xcur
        ELSE
          call xtext('Variance of x zero, so no skewness or kurtosis')
          write(ifil8,*)'Variance of x zero, so no skewness or kurtosis'
        end if
        if(yvar.ne.0.0)then
          yskw=yskew/(npts*yvar**3)
          ycur=ycurt/(npts*yvar**2)-3.
          call xtext('For Y values')
          write(tstring,*)'Skewness= ',yskw
	call totext(tstring)
          write(tstring,*)'Kurtosis= ',ycur
	call totext(tstring)
          write(ifil8,*)'For y values'
          write(ifil8,*)'Skewness= ',yskw
	  write(ifil8,*)'Kurtosis= ',ycur
        ELSE
          call xtext('Variance of y zero, so no skewness or kurtosis')
          write(ifil8,*)'Variance of y zero, so no skewness or kurtosis'
        end if
	return
999	call xtext('Not enough points for full analysis')
        END
           
           
C Sort data (x and y arrays + error arrays at same time)
c Based on CONCEPTS employed in a Numerical Recipes routine
                                                           
                                                           
        subroutine sort4(npts,xa,xb,xc,xd)
	include	'robcom'
        real xa(*),xb(*),xc(*),xd(*)
	integer npts
c are there enough points for a sort?
	if(npts.le.1)then
		write(tstring, *)'ERROR: TOO FEW POINTS (',npts,')'
		call xtext(tstring)
		return
	end if
        k=npts/2+1
        ir=npts
100      continue
           if(k.gt.1)then
             k=k-1
             rxa=xa(k)
             rxb=xb(k)
             rxc=xc(k)
             rxd=xd(k)
           else
             rxa=xa(ir)
             rxb=xb(ir)
             rxc=xc(ir)
             rxd=xd(ir)
             xa(ir)=xa(1)
             xb(ir)=xb(1)
             xc(ir)=xc(1)
             xd(ir)=xd(1)
             ir=ir-1
             if(ir.eq.1)then
                xa(1)=rxa
                xb(1)=rxb
                xc(1)=rxc
                xd(1)=rxd
                return
              end if
            end if
            i=k
            j=k+k
200          if(j.le.ir)then
             if(j.lt.ir)then
              if(xa(j).lt.xa(j+1))j=j+1
            end if
            if(rxa.lt.xa(j))then
              xa(i)=xa(j)
              xb(i)=xb(j)
              xc(i)=xc(j)
              xd(i)=xd(j)
              i=j
              j=j+j
            else
              j=ir+1
            end if
          goto 200
          end if
          xa(i)=rxa
          xb(i)=rxb
          xc(i)=rxc
          xd(i)=rxd
        goto 100
        end
           
C SUM DATA BETWEEN REQUESTED START AND END POINTS
        subroutine sumdat(x,y,npts)
	include	'robcom'
        real x(*),y(*)
        double precision sum
        sum=0.0d0
        do 10 i=1,npts
          sum=sum+y(i)
10      continue
        sums=sum
        call xtext('Sum of y values between limits')
        write(tstring,*)'IS: ',SUMS
	call totext(tstring)
        write(ifil8,*)'Sum of y values between limits'
        write(ifil8,*)'is: ',SUMS
        end
           
C SUM MODEL BETWEEN REQUESTED START AND END POINTS
        subroutine summod(x,y,npts)
	include	'robcom'
	include 'fitcom'
        common/roba/a,sigmaa
        real a(maxpar), sigmaa(maxpar)
        real x(*),y(*)
        double precision sum
        sum=0.0d0
        do 10 i=1,npts
          sum=sum+functn(x,i,a)
10      continue
        sums=sum
        call xtext('Sum of model values at data points between limits')
        write(tstring,*)'is: ',sums
	call totext(tstring)
        write(ifil8,*)
     +      'Sum of model values at data points between limits'
        write(ifil8,*)'is: ',sums
        end
           
C INTEGRATE AREA BENEATH A SET OF DATA POINTS
C FROM BEVINGTON P.273
        subroutine carea(xfit,yfit,nptsf,nterms)
	include	'robcom'
        real xfit(*),yfit(*)
        ar=areab(xfit,yfit,nptsf,nterms)
        write(tstring,*)'Area under curve is: ',ar
	call totext(tstring)
        write(ifil8,*)'Area under curve is: ',ar
        end
           
        function areab(x,y,npts,nterms)
        double precision sum
        real x(*),y(*)
11      sum=0.0d0
        if(npts-nterms)21,21,13
13      neven=2.*(nterms/2)
        idelta=nterms/2-1
        if(nterms-neven)31,31,51
c fit all points with one curve
21      x1=x(1)
        x2=x(npts)
23      call integ(x,y,npts,1,x1,x2,sum)
        goto 71
c even no. of terms
31      x1=x(1)
        j=nterms-idelta
        x2=x(j)
        call integ(x,y,nterms,1,x1,x2,sum)
        i1=npts-nterms+1
        j=i1+idelta
        x1=x(j)
        x2=x(npts)
39      call integ(x,y,nterms,i1,x1,x2,sum)
        if(i1-2)71,71,41
41      imax=i1-1
        do 46 i=2,imax
        j=i+idelta
        x1=x(j)
        x2=x(j+1)
46      call integ(x,y,nterms,i,x1,x2,sum)
        goto 71
c odd number of terms
51      x1=x(1)
        j=nterms-idelta
        x2=(x(j)+x(j-1))/2.
        call integ(x,y,nterms,1,x1,x2,sum)
        i1=npts-nterms+1
        j=i1+idelta
        x1=(x(j)+x(j+1))/2
        x2=x(npts)
59      call integ(x,y,nterms,i1,x1,x2,sum)
        if(i1-2)71,71,61
61      imax=i1-1
        do 66 i=2,imax
        j=i+idelta
        x1=(x(j+1)+x(j))/2.
        x2=(x(j+2)+x(j+1))/2.
66      call integ(x,y,nterms,i,x1,x2,sum)
71      areab=sum
        end
           
C INTEGRATE THE AREA BENEATH TWO DATA POINTS
        subroutine integ(x,y,nterms,i1,x1,x2,sum)
	include	'robcom'
        include 'fitcom'
        double precision xjk,array(maxpar, maxpar),a,denom,deltax,sum
	double precision det
        real x(*),y(*)
c construct square matrix and invert
11      do 17 j=1,nterms
        i=j+i1-1
        deltax=x(i)-x(i1)
        xjk=1.
        do 17 k=1,nterms
        array(j,k)=xjk
17      xjk=xjk*deltax
21      call matinv(array,nterms,det)
        if(det)31,23,31
23      imid=i1+nterms/2
        sum=sum+y(imid)*(x2-x1)
        goto 40
c evaluate coefficients and integrate
31      dx1=x1-x(i1)
        dx2=x2-x(i1)
33      do 39 j=1,nterms
        i=j+i1-1
        a=0.0
        do 37 k=1,nterms
37      a=a+y(i)*array(j,k)
        denom=j
39      sum=sum+(a/denom)*(dx2**j-dx1**j)
40      return
        end
           
C ARITHMETIC ON X, Y OR Z VALUES
C VARIABLE N SPECIFIES ARRAY TO PERFORM ARITHMETIC ON
C 1=X, 2=Y, 3=Z, 4=DELX, 5=DELY
        subroutine ARITH(N,X,Y,Z,DELX,DELY,NPTS)
	include	'robcom'
        real x(*),y(*),z(*)
        real delx(*),dely(*)
        real ain(10)
        integer ifix(10),igrd(10)
        character*2 oper
        character*60 buffer
C READ THE TYPE OF OPERATION TO BE PERFORMED
         call sprompt('Give mathematical operation (+,-,/,*,**,^)')
        call getit(oper,  1)
        if(oper.ne.'*'.and.oper.ne.'**'.and.oper.ne.'/'.and.oper.
     +ne.'+'.and.oper.ne.'-'.and.oper.ne.'^')then
          write(tstring,*)'OPERATION ',OPER,'NOT RECOGNISED'
	call totext(tstring)
          return
        end if
        call sprompt('Give number or array name')
        call getit(buffer,  1)
	call remcom(buffer)
        call lctouc(buffer)
                           
        if(buffer.ne.'X'.and.buffer.ne.'Y'.and.buffer.ne.'Z'
     +.and.buffer.ne.'XERR'.and.buffer.ne.'YERR')then
         call dcode(buffer,ain,ifix,igrd,k)
         bnum=ain(1)
        end if
              
        DO 10 I=1,NPTS
         IF(BUFFER.eq.'X')BNUM=X(I)
         IF(BUFFER.eq.'Y')BNUM=Y(I)
         IF(BUFFER.eq.'Z')BNUM=Z(I)
         IF(BUFFER.eq.'XERR')BNUM=DELX(I)
         IF(BUFFER.eq.'YERR')BNUM=DELY(I)
         IF(N.eq.1)X(I)=ARTH(X(I),BNUM,OPER)
         IF(N.eq.2)Y(I)=ARTH(Y(I),BNUM,OPER)
         IF(N.eq.3)Z(I)=ARTH(Z(I),BNUM,OPER)
         IF(N.eq.4)DELX(I)=ARTH(DELX(I),BNUM,OPER)
         IF(N.eq.5)DELY(I)=ARTH(DELY(I),BNUM,OPER)
C ALSO PERFORM OPERATION ON X AND Y ERRORS FOR MULTIPLY
C AND DIVIDE
         IF(OPER.ne.'*'.and.OPER.ne.'/')GOTO 10
         IF(BUFFER.eq.'X')BNUM=DELX(I)
         IF(BUFFER.eq.'Y')BNUM=DELY(I)
c Use absolute values to make sure errors remain positive
         IF(N.eq.1)DELX(I)=abs(ARTH(DELX(I),BNUM,OPER))
         IF(N.eq.2)DELY(I)=abs(ARTH(DELY(I),BNUM,OPER))
10      continue
        END
           
        function arth(anum,bnum,oper)
	double precision anum2, bnum2, arth2
	include	'robcom'
        character*2 oper
	anum2 = dble(anum)
	bnum2 = dble(bnum)
        if(oper.eq.'+')then
          arth2=anum+bnum
        else if(oper.eq.'-')then
          arth2=anum-bnum
        else if(oper.eq.'/')then
          if(bnum.ne.0.0)arth2=anum/bnum
        else if(oper.eq.'*')then
          arth2=anum*bnum
        else if(oper.eq.'**'.or.oper.eq.'^')then
          arth2=anum**bnum
        else
          call xtext('***WARNING***')
          write(tstring, *)'OPERATION: ',OPER,' NOT RECOGNISED'
	  call totext(tstring)
        end if
	
	arth = real(arth2)	
                    
        end
           
C FUNCTION ON X, Y OR Z VALUES
C TRIGONOMETRIC FUNCTIONS ALL ASSUME DEGREES
        subroutine rfunc(N,X,Y,Z,DELX,DELY,NPTS)
	include	'robcom'
	REAL X(*),Y(*),Z(*)
        REAL DELX(*),DELY(*)
C READ THE TYPE OF FUNCTION TO BE PERFORMED
        call sprompt('Give function to be performed')
        call sprompt('e.g. sqrt,cubert,square,sin,
     +cos,tan,asin,acos,atan,abs,exp,exp10')
        call sprompt('log,loge,factorial,inverse 
     +(trigs assume degrees)')
        call getit(ainbuf,  1)
	call remcom(ainbuf)
	call lctouc(ainbuf)
        do 10 i=1,npts
         if(n.eq.1)then
		call dofunc(x(i), ainbuf)
         else if(n.eq.2)then
		 call dofunc(y(i), ainbuf)
	 else if(n.eq.3)then
		 call dofunc(z(i), ainbuf)
	 end if

10      continue
        end
           
c cube root function
	function cbrt(x)
	cbrt=10**(log10(x)/3.)
	end
    
    
c factorial function for integers
c bevington p. 32
c name change to avoid conflict with isas plot routine
        function factr(n)
        double precision fi,sum
11      factr=1.
        if(n-1)40,40,13
13      if(n-10)21,21,31
c n less than 11
21      do 23 i=2,n
        fi=i
23      factr=factr*fi
        goto 40
c n greater than 10
31      sum=0.
        do 34 i=11,n
        fi=i
34      sum=sum+dlog(fi)
35      factr=3628800.*dexp(sum)
40      return
        end
           
C RESTRICT DATA FITTING LIMITS
        subroutine fitlim(mode,xfit,yfit,xfite,yfite,
     +x,y,delx,dely,npts,nptsf)
	include	'robcom'
       real x(*),y(*),delx(*),dely(*)
       real xfit(*),yfit(*),xfite(*),yfite(*)
      common/limit2/xmin, xmax, ymin, ymax
C MODE=4 IS SET FIT LIMITS EQUAL TO PLOTTING LIMITS BEFORE CALL
C TO LINSC2
       if(mode.ne.4)then
         call sprompt('Give limits')
         call getit(ainbuf,  1)
         call dcode(ainbuf,ain1,ainfix,aingrd,k)
       end if
             
       if(mode.eq.1.or.mode.eq.2)then
         flow=ain1(1)
         fhi=ain1(2)
       else if(mode.eq.3)then
         flowx=ain1(1)
         flowy=ain1(2)
         fhix=ain1(3)
         fhiy=ain1(4)
       else if(mode.eq.4)then
         flowx = min(xmin, xmax)
         flowy = min(ymin, ymax)
         fhix = max(xmax, xmin)
         fhiy = max(ymax, ymin)
       end if
             
       nptsf=0
       j=1
c y limits?
       if(mode.eq.2)goto 100
       if(mode.eq.3.or.mode.eq.4)goto 200
c restrict by x limits
       do 10 i=1,npts
        if(x(i).ge.flow.and.x(i).le.fhi)then
          xfit(j)=x(i)
          xfite(j)=delx(i)
          yfit(j)=y(i)
          yfite(j)=dely(i)
          j=j+1
        end if
10     continue
       nptsf=j-1
       return
c restrict by y limits
100    continue
       do 20 i=1,npts
        if(y(i).ge.flow.and.y(i).le.fhi)then
          xfit(j)=x(i)
          xfite(j)=delx(i)
          yfit(j)=y(i)
          yfite(j)=dely(i)
          j=j+1
        end if
20     continue
       nptsf=j-1
       return
             
c restrict by x and y limits
200    continue
       do 30 i=1,npts
        if(y(i).ge.flowy.and.y(i).le.fhiy.and.
     +x(i).ge.flowx.and.x(i).le.fhix)then
          xfit(j)=x(i)
          xfite(j)=delx(i)
          yfit(j)=y(i)
          yfite(j)=dely(i)
          j=j+1
        end if
30     continue
       nptsf=j-1
       return
             
       end
          
C COPY X AND Y INTO XFIT AND YFIT
       subroutine copyxy(x,y,delx,dely,
     +xfit,yfit,xfite,yfite,npts,nptsf)
	include	'robcom'
      logical radec
      common/rad/radec
       real x(*),y(*),delx(*),dely(*),xfit(*),yfit(*)
       real xfite(*),yfite(*)
         do 10 i=1,npts
           xfit(i)=x(i)
           yfit(i)=y(i)
           xfite(i)=delx(i)
           yfite(i)=dely(i)
10       continue
         nptsf=npts
c as copyxy is always called when new data is read in set flag
c here to show that multiplication of ra by cos(dec) has not yet taken
c place
         radec=.false.
         end
            
            
c reset z array
      subroutine zreset(z,msiz)
      real z(*)
      do 10 i=1,msiz
       z(i)=0.0
10    continue
      end

c returns maximum number of models allowed as 
c defined in fitcom (used by fitter.c)
	integer function getmm()
	include 'fitcom'
	getmm = maxmod
	end



c returns values of fitted parameters from CRVFIT to a
c calling 'C' routine (fitter.c)
c
	subroutine getpar(index, value, error)
	include 'fitcom'
	include	'robcom'
	integer index
	real value, error
        common/robmod/bpoly,norder,fixpar,ifpar,nmodel,mtype
        common/roba/a,sigmaa
        integer ifpar(maxpar)
        real a(maxpar), sigmaa(maxpar)
         character*15 mtype(maxmod)
       real fixpar(maxpar)
        real bpoly(10)
	if(index.gt.maxpar.or.index.lt.1)then
		call xtext('ERROR: Out of range parameter request')
		value = 0
		return
	end if
c it's simple if the parameter was fixed
	if(ifpar(index).eq.1)then
		value = fixpar(index)
c no errors for fixed values
		error = 0
		return
	end if
c otherwise it's a little more complicated
	nf = 1
	do 25 i = 1, index
		if(ifpar(i).eq.1)then
			value = fixpar(i)
		else
			value = a(nf)
			error = sigmaa(nf)
			nf = nf + 1
		end if
25	continue
	end

		




       
c generalised curfit routine
        subroutine crvfit(xfit,yfit,xfite,yfite,imode,yfit2,nptsf)
	include	'robcom'
	include 'fitcom'
c fchi and nloop give convergence conditions
        common/convg/fchi,nloop
        common/robmod/bpoly,norder,fixpar,ifpar,nmodel,mtype
	common/usefun/nuser
        common/roba/a,sigmaa
        common/inrob/inter
        logical inter
        common/robgrd/nogrid,gstar,gend,gstep
        logical nogrid
        character*15 mtype(maxmod)
        real ain(20)
        integer ifix(20),igrd(20),ifpar(maxpar)
        real a(maxpar),deltaa(maxpar),sigmaa(maxpar)
        real fixpar(maxpar)
        real xfit(*),yfit(*),xfite(*),yfite(*),yfit2(*)
        real bpoly(10)
                      
c check whether a weighted fit was requested for a data set
c without errors
	if(imode.eq.1.and.yfite(1).eq.0.0)then
		imode = 0
		call xtext('*Not* weighting by errors')
		call xtext('First error value zero')
	end if
       
        if(NOGRID)then
          continue
        else
          call xtext('chi**2 grid to have start, end and step of')
          write(tstring, *)GSTAR,GEND,GSTEP
	  call totext(tstring)
        end if
              
c igdind is index of parameter to be gridded
        igdind=0
c set fix/float flags to all float
        do 3 i=1,maxpar
3       ifpar(i)=0
        i=1
        npar=1
        nfree=1
4       if(inter)then
          call xtext('Models: Sine,Polynomial,Gauss,Lorentz,Orbit')
          call xtext('Powerlaw,Blackbody,Triangle,Tophat,User')
        end if
5       continue
		call sprompt('Give model name (endmodels or end to finish)')
		write(tstring, *)'Default= ',mtype(i)
	   	call sprompt(tstring)
        call getit(ainbuf,  0)
	if(ainbuf.ne.' ')then
		mtype(i) = ainbuf
	else
		if(INTER)then
			write(tstring, *)'defaulting to ',MTYPE(I)
	   		call totext(tstring)
		end if
	end if
	call savdata(mtype(i))
c convert model name to upper case if required and remove comments
	call remcom(mtype(i))
	call lctouc(mtype(i))
         IF(MTYPE(I).ne.'ENDMODELS'.and.MTYPE(I).ne.'END')then
          IF(MTYPE(I).eq.'GAUSS')then
	call sprompt('Give normalisation, mean and width (sigma)')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+3
          else if(MTYPE(I).eq.'EXPONENTIAL')then
		call sprompt('Give normalisation, start and decay values')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+3
          else if(mtype(i).eq.'POLYNOMIAL')then
           call sprompt('Give coefficients (all on one line)')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           norder=k-1
           write(tstring, *)'Order ',NORDER,' polynomial'
	   call totext(tstring)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+norder+1
          else if(mtype(i).eq.'USER')then
           call sprompt('Give parameters (all on one line)')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           nuser=k
           write(tstring, *)'You typed ',NUSER,' parameters'
	   call totext(tstring)
	   call xtext('Your function =>must<= use that number')
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+nuser
          else if(mtype(i).eq.'LORENTZ')then
           call sprompt('Give normalisation, mean and width')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+3
          else if(mtype(i).eq.'TOPHAT')then
           call sprompt('Give normalisation, start and width')
           call getit(ainbuf,  1)
           CALL DCODE(ainbuf,AIN,IFIX,IGRD,K)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+3
          else if(MTYPE(I).eq.'POWERLAW')then
           call sprompt('Give normalisation and power')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+2
          else if(mtype(i).eq.'SINE')then
           call sprompt('Give amplitude, phase and period')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+3
          else if(MTYPE(I).eq.'TRIANGLE')then
           call sprompt('Give height, start of func. and hwzi')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+3
          else if(mtype(i).eq.'BLACKBODY')then
           call sprompt('Give normalisation and KT')
           call getit(ainbuf,  1)
           CALL DCODE(ainbuf,AIN,IFIX,IGRD,K)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+2
          else if(mtype(i).eq.'ORBIT')then
	   call sprompt('Give amp., phase, period, mean, e, omega')
           call getit(ainbuf,  1)
           call dcode(ainbuf,ain,ifix,igrd,k)
           call asgn(ain,igrd,ifix,a,deltaa,ifpar,fixpar,
     +nfree,npar,k,igdind)
           npar=npar+6
C THE MODEL REQUESTED AIN'T IN THE LIST
	  else
		write(tstring, *)'MODEL ', mtype(j),' IS UNKNOWN'
	   	call totext(tstring)
		if(inter)then
			call xtext('Please try again')
			GOTO 4
		end if
          end if
          I = I + 1
          if(i.le.maxmod) then
		if(npar.le.maxpar)then
		    GOTO 5
		else
		     call xtext('ERROR: too many parameters to fit')
		     return
                end if
	  else
		call xtext('Maximum number of models reached')
	end if
        end if
10      continue
          npar=npar-1
          nmodel=i-1
          nfree=nfree-1
        write(tstring, *)'Fit to be made to ',nptsf,' points'
	call totext(tstring)
        write(tstring, *)'Using ',nmodel,' model(s)'
	call totext(tstring)
        write(ifil8,*)'Fit to be made to ',nptsf,' points'
        write(ifil8,*)'Using ',nmodel,' model(s)'
        do 18 i=1,nmodel
        write(ifil8,*)mtype(i)
      write(tstring, *)mtype(i)
18	call totext(tstring)
        write(tstring, *)'with a total of ',npar,' parameters'
	call totext(tstring)
                      
       if(npar-nfree.ne.1)then
        write(tstring, *)'of which ',nfree,
     +        ' are free and ',npar-nfree,' are fixed'
	call totext(tstring)
       else
        write(tstring, *)'of which ',nfree,
     +' are free and ',npar-nfree,' is fixed'
	call totext(tstring)
       end if
             
        NF=1
        DO 19 I=1,NPAR
        IF(IFPAR(I).eq.1)then
          write(tstring, *)I,FIXPAR(I),'<---FIXED'
	  call totext(tstring)
          IF(I.eq.IGDIND)call xtext('(Free for first fit, then fixed)')
        ELSE
          write(tstring, *)I,A(NF)
	  call totext(tstring)
          NF=NF+1
        end if
19      continue
c if not gridding then make simple call to curfit
        if(NOGRID)then
c if autogrid is needed this call to curfit is done
c with the grid parameter floating
        if(IGDIND.ne.0)then
c change fixed and floating parameters into a
        ifpar(igdind)=0
        nfree=nfree+1
        nf=1
        do 29 i=1,npar
         if(i.eq.igdind)then
         fkeep=fixpar(igdind)
         dkeep=fixpar(igdind)/50.
        do 800 j=nf,maxpar
        fkeep2=a(j)
        dkeep2=deltaa(j)
        a(j)=fkeep
        deltaa(j)=dkeep
        FKEEP=FKEEP2
        DKEEP=DKEEP2
800     continue
        GOTO 31
        end if
        IF(IFPAR(I).eq.0)then
          NF=NF+1
        ELSE
          continue
        end if
29      continue
        end if
31      continue
C LETS GO!
        if(nptsf-nfree.lt.0.or.nfree.eq.0)then
          call xtext('WARNING: not enough free parameters for fit')
	  write(tstring,*)'No. of points = ',nptsf
	  call xtext(tstring)
	  write(tstring, *)'No. of free parameters =', nfree
	  call xtext(tstring)
          GOTO 22
        end if
        call xtext('Convergence conditions are:')
        write(tstring, *)'Fractional change in CHI**2 = ',FCHI
	call totext(tstring)
        write(tstring, *)'No. of times CURFIT is called = ',NLOOP
	call totext(tstring)
        CHI1=9.0E30
        FLAMDA=1E-03
        DO 20 I=1,NLOOP
        CALL CURFIT(XFIT,YFIT,YFITE,NPTSF,NFREE,IMODE,A,DELTAA,
     +SIGMAA,FLAMDA,YFIT2,CHISQR)
       write(tstring, *)'Iteration ',I,' Chi**2= ',CHISQR,'
     +		flamda= ',FLAMDA
	call totext(tstring)
       write(ifil8,*)'Iteration ',I,' Chisqr= ',CHISQR,' Flambda= ',FLAMDA
c lets also escape here if chi squared was zero (can't get any less!)
	if(chisqr.eq.0.0)goto 21
       DELCHI=(CHI1-CHISQR)/CHISQR
       IF(ABS(DELCHI).LT.FCHI)GOTO 21
       CHI1=CHISQR
20      continue
	call xtext('No more iterations made because maximum number')
	call xtext('of iterations exceeded - use the command "CONVERGENCE"')
	call xtext('to change this')
	GOTO 22
21      continue
	call xtext('No more calls to curfit because fractional')
	write(tstring, *)'change in Chi**2 <', FCHI
	call totext(tstring)
                      
22      call xtext('Final parameters are: ')
        write(ifil8,*)'Final parameters are: '
        NF=1
        DO 25 I=1,NPAR
        IF(IFPAR(I).eq.1)then
          write(tstring, *)I,FIXPAR(I),'<---FIXED'
	  call totext(tstring)
          write(ifil8,*)I,FIXPAR(I),'<---FIXED'
        ELSE
          write(tstring, *)I,A(NF),' +/- ',SIGMAA(NF)
	  call totext(tstring)
          write(ifil8,*)I,A(NF),' +/- ',SIGMAA(NF)
          IF(I.eq.IGDIND)then
            call xtext('(To be gridded later)')
            write(ifil8,*)'(To be gridded later)'
            if(nogrid)then
              gstar=a(nf)-3.*sigmaa(nf)
              gend=a(nf)+3.*sigmaa(nf)
              gstep=sigmaa(nf)/5.
              best=a(nf)
            end if
          end if
          NF=NF+1
        end if
25      continue
C IF IGDIND SET NON-ZERO DO GRID
        IF(IGDIND.ne.0)then
C CHANGE FIXED AND FLOATING PARAMETERS INTO A
C BACK TO PARAMETER(IGDIND) FIXED
        ifpar(igdind)=1
        nfree=nfree-1
        nf=1
        do 39 i=1,npar
         if(i.eq.igdind)then
        do 850 j=nf,maxpar
        a(j)=a(j+1)
        deltaa(j)=deltaa(j+1)
850     continue
        end if
        if(ifpar(i).eq.0)then
          nf=nf+1
        else
          continue
        end if
39      continue
        end if
        end if
              
        if(igdind.eq.0)then
          return
        else
C LETS DO THE CHISQUARE GRID
        call xtext('PARAMETER VALUE    RED CHI**2   CHI**2')
        ntgrd=nint((gend-gstar)/gstep)+1
        do 870 j=1,ntgrd
         fixpar(igdind)=(j-1)*gstep+gstar
        chi1=9.0e30
        flamda=1e-03
        do 220 i=1,nloop
        call curfit(xfit,yfit,yfite,nptsf,nfree,imode,a,deltaa,
     +sigmaa,flamda,yfit2,chisqr)
       delchi=(chi1-chisqr)/chisqr
       if(abs(delchi).lt.fchi)goto 221
       chi1=chisqr
220      continue
221      continue
       write(tstring, *)j,fixpar(igdind),chisqr,chisqr*(nptsf-nfree)
	call totext(tstring)
       write(ifil8,*)fixpar(igdind),chisqr,chisqr*(nptsf-nfree)
870      continue
c restore best value for gridded parameter
         fixpar(igdind)=best
       end if
        return
c900     call xtext('ERROR WHILST READING MODEL NAMES IN CURFIT')
c        return
c901     call xtext('ERROR DETECTED WHILST READING MODEL PARAMETERS')
c        write(tstring, *)'FOR MODEL NUMBER ',I,' TYPE ',MTYPE(I)
c	call totext(tstring)
        end
           
C ASSIGN PARAMETERS TO FIX AND FREE ARRAYS AND INCREMENT NFREE AND
C NPAR AS REQUIRED
C DELTAA IS SET TO A/50. THIS MUST BE RESET IN MAIN PROGRAM
C IF NOT APPROPRIATE
        subroutine asgn(ain,igrd,ifix,a,deltaa,
     +ifpar,fixpar,nfree,npar,k,igdind)
	include	'robcom'
	include	'fitcom'
        real ain(*),a(*),deltaa(*),fixpar(*)
        integer ifpar(*),ifix(*),igrd(*)
        inc=0
        do 10 i=1,k
        if(ifix(i).eq.1.or.igrd(i).eq.1)then
          if((npar+inc).gt.maxpar)then
		call xtext("Error: Too many parameters to fit")
		return
	  end if
          fixpar(npar+inc)=ain(i)
          ifpar(npar+inc)=1
                           
          if(igrd(i).eq.1)then
            igdind=npar+inc
          end if
                
          inc=inc+1
        else
	  if(nfree.gt.maxpar)then
		call xtext("ERROR: Too many parameters to fit")
		return
	  end if
          a(nfree)=ain(i)
          deltaa(nfree)=a(nfree)/50.
c fudge for zero derivatives
          if(deltaa(nfree).eq.0.0)deltaa(nfree)=0.01
          nfree=nfree+1
          inc=inc+1
        end if
10      continue
        end
           
c copy array 2 (integer) to array 1 (real) for k values
        subroutine asar(ar1,ar2,k)
        integer ar1(*)
        real ar2(*)
        do 10 i=1,k
          ar1(i)=nint(ar2(i))
10      continue
        end
           
c the sum of designated models
c the free parameters are stored in array a
c the fixed ones in array fixpar, ifpar contains flags to show
c whether a parameter is fixed=1 or floating
        function functn(x,i,a)
	include 'fitcom'
        real x(*),a(*),bpoly(10)
        real a1(maxpar)
        common/robmod/bpoly,norder,fixpar,ifpar,nmodel,mtype
	common/usefun/nuser
c ismod not equal to zero implies use only specific model number
c ismod
        common/robspm/ismod
        real fixpar(maxpar)
        integer ifpar(maxpar)
        character*15 mtype(maxmod)
        functn=0.0
                  
c load fixed and floating parameters into a1
        nf=1
        do 8 j=1,maxpar
          if(ifpar(j).eq.1)then
            a1(j)=fixpar(j)
          else
            a1(j)=a(nf)
            nf=nf+1
          end if
          if(ifpar(j).eq.1)then
          else
          end if
8       continue
                
        npar=1
        do 10 j=1,nmodel
          IF(MTYPE(J).eq.'GAUSS')then
           ADD=GAUSS(A1(NPAR),A1(NPAR+1),A1(NPAR+2),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+3
          else if(MTYPE(J).eq.'EXPONENTIAL')then
           ADD=EXPON(A1(NPAR),A1(NPAR+1),A1(NPAR+2),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+3
          else if(MTYPE(J).eq.'POLYNOMIAL')then
           DO 30 INC=0,NORDER
30         BPOLY(INC+1)=A1(NPAR+INC)
           ADD=FPOL(X(I),BPOLY,NORDER+1)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+NORDER+1
          else if(MTYPE(J).eq.'USER')then
	    ADD=USER(A1(NPAR),X(I))
	    IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
	    FUNCTN=FUNCTN+ADD
	    NPAR=NPAR+NUSER
          else if(MTYPE(J).eq.'LORENTZ')then
           ADD=PLOREN(A1(NPAR),A1(NPAR+1),A1(NPAR+2),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+3
          else if(MTYPE(J).eq.'TOPHAT')then
           ADD=TOPHAT(A1(NPAR),A1(NPAR+1),A1(NPAR+2),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+3
          else if(MTYPE(J).eq.'POWERLAW')then
           ADD=POWL(A1(NPAR),A1(NPAR+1),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+2
          else if(MTYPE(J).eq.'SINE')then
           ADD=SINE(A1(NPAR),A1(NPAR+1),A1(NPAR+2),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+3
          else if(MTYPE(J).eq.'TRIANGLE')then
           ADD=FUNCTN+TRIAN(A1(NPAR),A1(NPAR+1),A1(NPAR+2),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+3
          else if(MTYPE(J).eq.'BLACKBODY')then
           ADD=BBODY(A1(NPAR),A1(NPAR+1),X,I)
           IF(ISMOD.ne.0.and.ISMOD.ne.J)ADD=0.0
           FUNCTN=FUNCTN+ADD
           NPAR=NPAR+2
          else if(mtype(j).eq.'ORBIT')then
           ADD=ORBIT(A1(NPAR),A1(NPAR+1),A1(NPAR+2),A1(NPAR+3),
     +A1(NPAR+4),A1(NPAR+5),X,I)
           if(ismod.ne.0.and.ismod.ne.j)add=0.0
           functn=functn+add
           npar=npar+6
          end if
10        continue
                                 
          end
             
C ALLOW FITS WITH COMBINATIONS OF
C GAUSSIAN, LORENTZIAN, POLYNOMIAL, POWER LAW, BLACK BODY
C SINE WAVE, TRIANGLE, TOP HAT FN
C GAUSSIAN, EXPONENTIAL
        function gauss(a1,a2,a3,x,i)
c gaussian
c a1=normalization
c a2=mean, a3=standard deviation
	double precision z, t1, t2, t3
        real x(*)
        gauss=0.0
	t1 = x(i)
	t2 = a2
	t3 = a3
        z = (t1 - t2)/t3
        z2=z*z
        if(z2-50.)16,20,20
16      gauss=a1*exp(-z2/2.)
20      return
        end
           
c exponential
        function expon(a1,a2,a3,x,i)
c a1=normalization, a2=start time, a3=decay constant
        real x(*)
	double precision time, t1, t2
	t1 = x(i)
	t2 = a2
        time = t1 - t2
        if(time.lt.0.0)then
c before start of exponential
          expon = 0.0
          return
        else
          expon = a1*exp(-time/a3)
        end if
        end
           
c lorentzian function
        function ploren(a1,a2,a3,x,i)
c a1=normalization, a2=mean, a3=width
        real x(*)
        div=(x(i)-a2)*(x(i)-a2)+a3*a3*0.250000000
        ploren=a1*a3*a3*0.25/div
        end

c top hat
        function tophat(a1,a2,a3,x,i)
c a1=height,a2=start,a3=width
        real x(*)
        if(x(i).gt.a2.and.x(i).lt.a2+a3)then
          tophat=a1
        else
          tophat=0.0
        end if
        end

c power law
        function powl(a1,a2,x,i)
        real x(*)
        powl=a1*x(i)**a2
        end
           
c Sine wave
        function sine(a1,a2,a3,x,i)
        real x(*)
        data twopi/6.2831853/
c a1=amplitude,a2=phase,a3=period
        sine=a1*sin(twopi*(x(i)-a2)/a3)
        end
           
c Triangle
        function trian(a1,a2,a3,x,i)
c a1=height,a2=start,a3=half width at zero
        real x(*)
        if(x(i).lt.a2.or.x(i).gt.a2+a3+a3)then
c not in triangle
          trian=0.0
          return
        end if
        if(x(i).lt.a2+a3)then
c ascending part
          trian=a1*(x(i)-a2)/a3
          return
        end if
c got to be descending bit
          trian=a1*(1.-(x(i)-a2-a3)/a3)
        end
           
c Black body
        function bbody(a1,a2,x,i)
c a1=normalization, a2=kt in units of x
        real x(*)
        bbody=a1*x(i)*x(i)*x(i)/(exp(x(i)/a2)-1.)
        end
           
c binary orbit (velocity)
        function orbit(a1,a2,a3,a4,a5,a6,x,i)
c a1=amplitude,a2=phase,a3=period,a4=mean,a5=e,a6=w
        real x(*)
        data twopi/6.2831853/
c omega in radians
        w=a6*twopi/360.
c get mean anomaly
        am=(twopi/a3)*(x(i)-a2)
c from mean anomaly to eccentric anomaly (from routine of n. kawai)
        ae=am
        do 10 j=1,6
        dedm=1-a5*cos(ae)
        am0=ae-a5*sin(ae)
        de=(am-am0)/dedm
10      ae=ae+de
c to true anomaly
        te=tan(ae*0.5)
        tv=te*sqrt((1+a5)/(1-a5))
        v=atan(tv)*2.0
        orbit=a4+a1*(a5*cos(w)+cos(v+w))
        end
           
c decode text string including numbers in brackets
c into recognised numbers, bracketed numbers are fixed
c those bracketed by square brackets are to be gridded
c set elements of ifix to zero

	subroutine zifix(ifix, n)
	integer ifix(*)
	integer n
	integer i

	do 10 i = 1, n
		ifix(i) = 0
10	continue

	end
	




c numbers, also allow xmax etc. as variables
c k = number of numbers read
      subroutine dcodeo(buff,a,ifix,igrd,k)
	include	'robcom'
      real a(*)
      integer ifix(*),igrd(*)
      logical lb,ab
      character*(*) buff
                       
	nb=len(buff)
c lb=left bracket (; ab=angle bracket <
      lb=.false.
      ab=.false.
c remove comments from buffer
      call remcom(buff)
c and convert to upper case
	call lctouc(buff)
c find space, comma or left bracket
c after first non-blank, non-bracket, non-comma
      k=1
      i=0
      do 10 ind=1,nb
      i=i+1
      if(i.gt.nb)goto 11
       if(buff(i:i).eq.'(')lb=.true.
       if(buff(i:i).eq.')')lb=.false.
       if(buff(i:i).eq.'<')ab=.true.
       if(buff(i:i).eq.'>')ab=.false.
       if(buff(i:i).ne.' '.and.buff(i:i).ne.')'.and.buff(i:i)
     +.ne.'('.and. buff(i:i).ne.','.and.buff(i:i).ne.'<'.and.buff(i:i)
     +.ne.'>'.and.buff(i:i).ne.'	')then
         do 20 j=i+1,nb
         if(buff(j:j).eq.' '.or.buff(j:j).eq.','.or.buff(j:j).eq.
     +'('.or.buff(j:j).eq.')'.or.buff(j:j).eq.'<'.or.buff(j:j).eq.
     +'>'.or.buff(j:j).eq.'	')then
c get number
c      print*,'part of buffer=',buff(i:j-1)
       call numb(buff,i,j-1,a,k)
c was there a bracket before the number to show it should be fixed?
       if(lb)then
         ifix(k)=1
       else
         ifix(k)=0
       end if
       if(ab)then
         igrd(k)=1
       else
         igrd(k)=0
       end if
             
       k=k+1
       i=j
       if(buff(j:j).eq.')')lb=.false.
       if(buff(j:j).eq.'>')ab=.false.
       goto 10
       end if
20     continue
       end if
10     continue
11     continue
       k=k-1
       end
C DECODE TEXT STRING INCLUDING NUMBERS IN BRACKETS
C INTO RECOGNISED NUMBERS, BRACKETED NUMBERS ARE FIXED
C THOSE BRACKETED BY SQUARE BRACKETS ARE TO BE GRIDDED
C NUMBERS, ALSO ALLOW XMAX ETC. AS VARIABLES
C K = NUMBER OF NUMBERS READ
      subroutine dcode(buff,a,ifix,igrd,k)
	include	'robcom'
      real a(*)
      integer ifix(*),igrd(*)
      logical lb,ab
      character*(*) buff
	parameter(ilb = ichar('('))
	parameter(irb = ichar(')'))
	parameter(ilab = ichar('<'))
	parameter(irab = ichar('>'))
	parameter(icomma = ichar(','))
	parameter(itab = ichar('	'))
	parameter(ispace = ichar(' '))
                       
	nb=len(buff)
c lb=left bracket (; ab=angle bracket <
      lb=.false.
      ab=.false.
c remove comments from buffer
      call remcom(buff)
c and convert to upper case
	call lctouc(buff)
c find space, comma or left bracket
c after first non-blank, non-bracket, non-comma
      k=1
      i=0
      do 10 ind=1,nb
      i=i+1
      if(i.gt.nb)goto 11
	ic = ichar(buff(i:i))
       if(ic.eq.ilb)then
		LB=.TRUE.
       else if(ic.eq.irb)then
		LB=.FALSE.
       else if(ic.eq.ilab)then
		AB=.TRUE.
       else if(ic.eq.irab)then
		AB=.FALSE.
	end if
       IF(ic.ne.ispace.and.ic.ne.irb.and.ic
     +.ne.ilb.and. ic.ne.icomma.and.ic.ne.ilab.and.ic
     +.ne.irab.and.ic.ne.itab)then
         do 20 j=i+1,nb
	  ic = ichar(buff(j:j))
         if(ic.eq.ispace.or.ic.eq.icomma.or.ic.eq.
     +ilb.or.ic.eq.irb.or.ic.eq.ilab.or.ic.eq.
     +irab.or.ic.eq.itab)then
c get number
       call numb(buff,i,j-1,a,k)
c was there a bracket before the number to show it should be fixed?
       if(lb)then
         ifix(k)=1
       else
         ifix(k)=0
       end if
       if(ab)then
         igrd(k)=1
       else
         igrd(k)=0
       end if
             
       k=k+1
       i=j
	if(ic.eq.irb)then
		lb=.false.
	else if(ic.eq.irab)then
		ab=.false.
	end if
       goto 10
       end if
20     continue
       end if
10     continue
11     continue
       k=k-1
       end
          
c test whether a character is a number of a character
c ASCII specific
	logical function isnumb(char)
	character char
	integer k
	parameter(plus = ichar('+'))
	parameter(nine = ichar('9'))

	isnumb = .TRUE.

	k = ichar(char)
	if(k.gt.nine.or.k.lt.plus) then
		isnumb = .FALSE.
	end if


	end





c 'parse' input values (in FORTRAN!)
c the default is to try and see if FORTRAN can just read input
c as is. If this fails then other things such as checking for
c operators, functions etc. happens. 
c In previous versions numbers such as 1e02 (i.e. 100) gave
c problems because the e was interpreted as a symbol

       subroutine numb(buff,i1,i2,a,n)
	include	'robcom'
	real a(*), temp, temp2
	character*(*) buff
	parameter(iplus = ichar('+'))
	parameter(iminus = ichar('-'))
	parameter(idiv = ichar('/'))
	parameter(istar = ichar('*'))
	parameter(iexp = ichar('^'))
	parameter(ie = ichar('E'))
	character*10 fname
	logical EXPO, func
	logical isnumb

	logical ipm, isie

	EXPO = .FALSE.


	ipm = .FALSE.
	isie = .FALSE.

c do a check for a '*' or a '/'
	do 30 i = i1, i2
		ic = ichar(buff(i:i))
		if(ic .eq. istar
     +			.or. ic .eq. idiv
     +			.or. ic .eq. iexp) goto 50
c check for a plus or minus
		if(ic .eq. iplus .or. ic .eq. iminus)ipm = .TRUE.
		if(ic .eq. ie)isie = .TRUE.
30	continue

c only allow plus or minus if there's an "e" in the expression
	if(ipm .and. .not.(isie))goto 50
	

c does this make sense as a FORTRAN number as it is?
c first do a check to see if it starts with a number or
c a +/- to avoid DEC compiler reading "e" as 0.
	if(isnumb(buff(i1:i1)))then
c first do a read with an 'f' specifier. This will give
c an error for more circumstances than a free form read
		read(buff(i1:i2), '(f)', err = 50)a(n)
c but, it may give the wrong value!
c So, read again free format
		read(buff(i1:i2), *, err = 50)a(n)
		return
	end if

50	continue

c see if there's a function here to be taken care of
	call ckfunc(buff, i1, i2, func, fname, imin, imax)

c is it a simple number inside an operator?
	if(func)then
		if(isnumb(buff(imin:imin)))then
		   read(buff(imin:imax), *, err = 100)a(n)
		   call dofunc(a(n), fname)
		   return
100		   continue
		end if
	end if

c do terribly crude check for operators
	do 20 i = imin+1, imax-1
		ic = ichar(buff(i:i))
		if(ic.eq.iplus.or.ic.eq.iminus.or.
     +			ic.eq.idiv.or.ic.eq.istar.or.ic.eq.iexp)then
     		call cknam(buff, imin, i-1, temp)
c we have to beware of exponentiation looking like a multiply!
		if(buff(i+1:i+1).eq.'*')then
		  call cknam(buff, i+2, imax, temp2)
		  expo = .true.
		else
		  call cknam(buff, i+1, imax,temp2)
		endif
		if(expo)then
			a(n) = temp ** temp2
			if(func)call dofunc(a(n), fname)
			return
		end if
		if(ic.eq.iplus)then
			a(n) = temp + temp2
		else if(ic.eq.iminus)then
			a(n) = temp - temp2
		else if(ic.eq.istar)then
			a(n) = temp * temp2
		else if(ic.eq.idiv)then
			a(n) = temp / temp2
		else if(ic.eq.iexp)then
			a(n) = temp ** temp2
		end if

		if(func)call dofunc(a(n), fname)

		return
		end if
20	continue
	call cknam(buff, imin, imax, temp)
	a(n) = temp
	if(func)call dofunc(a(n), fname)
	end

	subroutine cknam(buff, i1, i2, a)
	include 'robcom'
        include 'msizcom'
c ichar values of '9', '+' and 'e' (may be used in the future if
c we decide to allow exponential notation
	parameter(nine = ichar('9'))
	parameter(plus = ichar('+'))
	parameter(iminus = ichar('-'))
	parameter(echar = ichar('e'))
      common/limit2/xmin, xmax, ymin, ymax
      common/limits/xminp, xmaxp, yminp, ymaxp
	common/curpos/xcurse,ycurse
c data attributes: mean and standard deviation
c for use in random and dcode
        common/atri/ymean,yvar,stime
c user defined variables
c stime is integer start time of hayshida format input
c file in m.j.d.
       character*(*) buff
       double precision stime
c "a" is here a single variable, not an array
	real a
! set to true if string starts with a minus sign
	logical minus
	save pi,twopi,c,e,h,fk
      data pi,twopi,c,e,h,fk/3.1415927,6.283185,2.9979e8,1.6021e-19,
     +6.6256e-34,1.3805e-23/

	imin = i1
	minus = .FALSE.

	ic = ichar(buff(imin:imin))

	if(ic.eq.iminus)then
		minus = .TRUE.
		imin = imin + 1
c there's no point in a preceding '+' sign but some people
c might use it
	else if(ic.eq.plus)then
		imin = imin + 1
	end if
c check first for user specified name
	do 10 i = 1, nouvar
		if(buff(imin:i2).eq.uservn(i))then
			a = uservv(i)
			if(minus) a = -a
			return
		end if
10	continue
c if ichar isn't in the right range skip this block of if 
c tests (i.e. it has to start with an upper case letter)

	if(ichar(buff(imin:imin)).gt.58)then
	IF(buff(imin:i2).eq.'LOOP')then
		a = vloop
	else if(buff(imin:i2).eq.'XMAX')then
		a=xmax
	else if(buff(imin:i2).eq.'XMIN')then
		a=xmin
	else if(buff(imin:i2).eq.'YMAX')then
		a=ymax
	else if(buff(imin:i2).eq.'YMIN')then
		a=ymin
	else if(buff(imin:i2).eq.'XMAXP')then
		a=xmaxp
	else if(buff(imin:i2).eq.'XMINP')then
		a=xminp
	else if(buff(imin:i2).eq.'YMAXP')then
		a=ymaxp
	else if(buff(imin:i2).eq.'YMINP')then
		a=yminp
	else if(buff(imin:i2).eq.'A1')then
		a=auser1
       else if(buff(imin:i2).eq.'A2')then
         a=auser2
       else if(buff(imin:i2).eq.'A3')then
         a=auser3
       else if(buff(imin:i2).eq.'A4')then
         a=auser4
       else if(buff(imin:i2).eq.'A5')then
         a=auser5
       else if(buff(imin:i2).eq.'YMINP')then
         a=YMINP
       else if(buff(imin:i2).eq.'MEAN')then
         a=YMEAN
       else if(buff(imin:i2).eq.'NUMBERPOINTS')then
         a=npts
       else if(buff(imin:i2).eq.'SIGMA')then
         a=YVAR
       else if(buff(imin:i2).eq.'STIME')then
         a=STIME
       else if(buff(imin:i2).eq.'PI')then
         a=PI
       else if(buff(imin:i2).eq.'TWOPI')then
         a=TWOPI
       else if(buff(imin:i2).eq.'C')then
         a=C
       else if(buff(imin:i2).eq.'E')then
         a=E
       else if(buff(imin:i2).eq.'H')then
         a=H
       else if(buff(imin:i2).eq.'K')then
         a=FK
       else if(buff(imin:i2).eq.'XCURSOR')then
         a=XCURSE
       else if(buff(imin:i2).eq.'YCURSOR')then
         a=YCURSE
	else
		call xtext('ERROR:')
		write(tstring, *)'THE VARIABLE NAMED ', buff(imin:i2)
		call xtext(tstring)
		call xtext('DOESN''T EXIST')
	end if
C SHOULD BE ORDINARY NUMBER
       ELSE
c check there is there are any non-numeric characters
c in the string - not guaranteed to get everything
c this should make the f2c output a little more rugged
15	continue
c	 do 20 i = i1, i2
c		k = ichar(buff(i:i))
c		if(k.gt.nine.or.k.lt.plus) goto 900
c20	continue
c for some reason on a DECstation a straight forward read doesn't work
c therefore copy to another variable first
c         READ(BUFF(Imin:i2),*,err=900, END=900)A
	  tstring = buff(imin:i2)
	  read(tstring, *, err=900, end=900)a
       end if
	if(minus) a = -a
       return
900	call xtext('ERROR IN S/R NUMB')
	write(tstring, *)'CULPRIT IS: ', buff(i1:i2)
	call xtext(tstring)
	a = 0.
	end
    
	subroutine ckfunc(buff, i1, i2, func, fname,
     +			imin, imax)
c see if there's a function in the expression
c also modifies imin and imax to strip away function from
c the argument
	character*(*) buff, fname
	integer i1, i2, imin, imax
	logical func
	
c default is that there is no function
	func = .false.
	
	imin = i1
	imax = i2
c we assume it's only a function if the string ends
c with a "}"	
	if(buff(i2:i2).eq.'}')then
		 imax = i2 - 1
	else
		return
	end if

	do 10 i = i1, imax
	if(buff(i:i).eq.'{')then
		ilen = i - i1
		if(ilen.gt.0)then
			func = .TRUE.
			fname = buff(imin:i-1)
		end if
		imin = i + 1
		return
	end if
10	continue


	end


c apply function called 'fname' to the variable 'a'
	subroutine dofunc(a, fname)
	real a
	character*(*)fname
	include 'msizcom'
	data dtr/0.01745329/
	data rtd/57.2957878/
	
	if(fname.eq.'SIN'.or.fname.eq.'SINE')then
		a = sin(a*dtr)
	else if(fname.eq.'COS'.or.fname.eq.'COSINE')then
		a = cos(a*dtr)
	else if(fname.eq.'TAN')then
		a = tan(a*dtr)
	else if(fname.eq.'SQRT')then
		if(a.ge.0.0)then
			a = sqrt(a)
		else
			call xtext('Negative number, no square root')
		end if
	else if(fname.eq.'SQUARE')then
		a = a*a
	else if(fname.eq.'CBRT')then
		a = cbrt(a)
	else if(fname.eq.'CUBE')then
		a = a*a*a
	else if(fname.eq.'LOG'.or.fname.eq.'LOG10')then
		if(a.gt.0.0) a = log10(a)
	else if(fname.eq.'LOGE')then
		if(a.gt.0.0) a = log(a)
	else if(fname.eq.'EXP')then
		a = exp(a)
	else if(fname.eq.'EXP10')then
		a = 10**a
	else if(fname.eq.'FACTORIAL')then
		a = factr(nint(a))
	else if(fname.eq.'INVERSE')then
		if(a.ne.0.0) a = 1./a
	else if(fname.eq.'ASINE'.or.fname.eq.'ASIN')then
		a = asin(a)*rtd
	else if(fname.eq.'ACOS')then
		a = asin(a)*rtd
	else if(fname.eq.'ATAN')then
		a = asin(a)*rtd
c miscellaneous
	else if(fname.eq.'ABS')then
		a = abs(a)
c hyberbolic functions
	else if(fname.eq.'SINH')then
		a = sinh(a*dtr)
	else if(fname.eq.'COSH')then
		a = cosh(a*dtr)
	else if(fname.eq.'TANH')then
		a = tanh(a*dtr)
c access data arrays
	else if(fname.eq.'X' .or. fname .eq. 'Y' .or.
     +		fname .eq. 'XERR' .or. fname .eq. 'YERR')then
		a = arrfun(fname, a)
c Results of fitting
	else if(fname.eq.'FITVALUE')then
		call getpar(nint(a), value, error)
		a = value
	else if(fname.eq.'FITERROR')then
		call getpar(nint(a), value, error)
		a = error
c C accessed arithmetic routines
	else if(fname.eq.'ROUND')then
		a = crint(a)
	else if(fname.eq.'CEIL')then
		call cceil(a)
	else if(fname.eq.'FLOOR')then
		call cfloor(a)
c C Bessel functions
	else if(fname.eq.'J0')then
		call cj0(a)
	else if(fname.eq.'J1')then
		call cj1(a)

	else
		call xtext('Unrecognised function')
		call xtext(fname)
	end if
	end

	function arrfun(fname, a)
	include 'msizcom'
	include 'robcom'
	character*(*) fname
	real a

	if(a .lt. 1 .or. a .gt. npts)then
		write(tstring, *)'Attempt to use element',nint(a)
		call xtext(tstring)
		write(tstring, *)'Only',npts,' elements in array'
		call xtext(tstring)
		arrfun = 0.0
		return
	end if

	if(fname .eq. 'X')then
		arrfun = x(nint(a))
	else if(fname.eq.'Y')then
		arrfun = y(nint(a))
	else if(fname.eq.'XERR')then
		arrfun = delx(nint(a))
	else if(fname.eq.'YERR')then
		arrfun = dely(nint(a))
	else
		print*,'Error in function arrfun'
		print*,'Function = ',fname
	end if

	end
	
C Version of dcode which should be faster but with less options
                                                               
      subroutine dcode2(buff,a,ifix,igrd,k)
	include	'robcom'
      real a(*)
      integer ifix(*),igrd(*)
      character*(*) buff
	parameter(icomma = ichar(','))
	parameter(itab = ichar('	'))
	parameter(ispace = ichar(' '))
      nb=nnl(buff) + 1
           
c find space, comma
c after first non-blank, non-comma
      k=1
      i=0
      do 10 ind=1,nb
      i=i+1
      IF(I.GT.NB)GOTO 11
        ic = ichar(buff(i:i))             
       IF(ic.ne.ispace.and.ic.ne.icomma
     +.and.ic.ne.itab)then
         DO 20 J=I+1,NB
	ic = ichar(buff(j:j))
        IF(ic.eq.ispace.OR.ic.eq.icomma
     +.OR.ic.eq.itab)then
c         IF(BUFF(J:J).eq.' ')then
C GET NUMBER
c (does removing subroutine call help to speed this up at all?)
c       CALL NUMB2(BUFF,I,J-1,A,K)
	read(buff(i:j-1), *, err = 999)a(k)
                                 
       K=K+1
       I=J
       GOTO 10
       end if
20     continue
       end if
10     continue
11     continue
       K=K-1
	return
999	continue
	call xtext('ERROR READING INPUT')
	write(tstring, *)'Culprit is:', buff(i:j-1)
	call xtext(tstring)
       end
          

c numb2 - cut-down version of numb to go with dcode2
       subroutine NUMB2(BUFF,I1,I2,A,N)
	include	'robcom'
       character*(*) BUFF
       REAL A(*)
c this had better be an ordinary number or we're going to get an error
         read(buff(i1:i2),*, err = 10)a(n)
	return
10	call xtext('ERROR IN READING NUMBER IN S/R NUMB2')
	write(tstring, *)'CULPRIT IS: ', buff(i1:i2)
       END
          
C RESCALE DATA LIMITS
      subroutine rscale(x,y,delx,dely,msiz,npts,ntickx,nticky,ifail,
     +npx,npy,distx,disty,fiplot,fjplot,fmplot,fnplot,sxh,syh,sxl,syl)
	include	'robcom'
	common/limits/xminp, xmaxp, yminp, ymaxp
	common/limit2/xmin, xmax, ymin, ymax
	common/echoc/echo
	common/minors/distxm, distym, mmr
	common/test/xtmin, xtmax, ytmin, ytmax
	logical echo
	real x(*),y(*),delx(*),dely(*)
c find data limits
      ifail=0
      call dmax(x,y,dely,delx,xmax,ymax,xmin,ymin,npts)
      if(echo)then
        call xtext('Data limits are (xmin,xmax,ymin,ymax)')
        write(tstring, *)xmin, xmax, ymin, ymax
	call totext(tstring)
        call xtext('Allowing 5% extra range for plotting')
      end if
      RX=(XMAX-XMIN)/20.
      RY=(YMAX-YMIN)/20.
C TEST LIMITS BEFORE OXHEART PACKAGE HAS A CHANCE TO CRASH
      IF(XMAX.LE.XMIN.OR.YMAX.LE.YMIN)then
        call xtext('** ERROR IN DATA LIMITS!!!')
c fudge the limits
	xminp = xmin
	yminp = ymin
	xmaxp = xmax
	ymaxp = ymax
	npx = ntickx
	npy = nticky
	distx = (xmax - xmin)/npx
	disty = (ymax - ymin)/npy
c see if that fixed it
	if(distx.eq.0.0.or.disty.eq.0.0)then
		call xtext('**xmin = xmax or ymin = ymax**')
c fudge a call
		call limit(20.,20.,90.,90.,0.0,0.0,100.0,100.0)
		ifail = 1
		return
	end if
      else
c use linsc2 to get sensible limits for plotting
c minor axes
	distxm = distx/float(mmr)
	call linsc2(xmin-rx,xmax+rx, ntickx*mmr,xminp,xmaxp,npxm,distxm)
	call linsc2(ymin-ry,ymax+ry, nticky*mmr,yminp,ymaxp,npym,distym)
c major axes
        call linsc2(xmin-rx,xmax+rx,ntickx,xminp,xmaxp,npx,distx)
        call linsc2(ymin-ry,ymax+ry,nticky,yminp,ymaxp,npy,disty)
        IF(ECHO)then
          call xtext('Plot scaled to: (xminp,xmaxp,yminp,ymaxp)')
          write(tstring, *)xminp, xmaxp, yminp, ymaxp
	  call totext(tstring)
        end if
      end if
            
c define plot limits
      sxh2=(sxh-sxl)*fiplot/fmplot+sxl
      sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
      syh2=(syh-syl)*fjplot/fnplot+syl
      syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl

c allow for gaps
	call gap(sxl2, syl2, sxh2, syh2)
      call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)

c limits for testing if a point is inside the area or not
c (for reversed axes xminp, for example, need not be smaller
c than xmaxp

	xtmin = min(xmaxp, xminp)
	xtmax = max(xmaxp, xminp)
	ytmin = min(ymaxp, yminp)
	ytmax = max(ymaxp, yminp)

      end
         

	subroutine gap(sxl2, syl2, sxh2, syh2)
	include 'robcom'
	xr = (sxh2 - sxl2)
	yr = (syh2 - syl2)

	sxh2 = sxh2 - xgap * xr
	sxl2 = sxl2 + xgap * xr

	syh2 = syh2 - ygap * yr
	syl2 = syl2 + ygap * yr

	end



c replace y array with random numbers with same mean and standard
c deviation with gaussian distribution
      subroutine random(x,y,npts)
	include	'robcom'
        common/atri/ymean,yvar,stime
        double precision stime
      real x(*),y(*)
c enought data?
	if(npts.le.2)then
		write(tstring,*)'ERROR: TOO FEW POINTS'
		call xtext(tstring)
		return
	end if

c calculate mean and sigma
        call moment(x,y,npts)
c get seed for random number using process id!
c	idum=float(itemp/10000.)
	do 10 i=1,npts
	y(i)=ymean+yvar*gasd(idum)
10	continue
	end
         
         
c three d version
        subroutine xccnt(idir,x0,y0,x1,y1, val, threed)
	
	real val
	logical threed

        dimension iswtch(2,2)
        data iswtch/1,2,2,1/
        data xl,yl/2*10e+20/
        if(idir.eq.-1)go to 800
        ih=0
        if(y1.gt.y0)ih=1
        isw=iswtch(idir+1,ih+1)
        go to (1,2),isw
1        continue
        IF(XL.eq.X0.and.YL.eq.Y0)GO TO 11
	call rotate3(x0, y0, val, x3, y3, threed)
        call movxy(X3,Y3)
	call rotate3(x1, y1, val, x3, y3, threed)
        call linxy(X3,Y3)
11	continue
        XL=X1
        YL=Y1
        GO TO 3
2        continue
        IF(XL.eq.X1.and.YL.eq.Y1)GO TO 21
	call rotate3(x1, y1, val, x3, y3, threed)
        call movxy(X3,Y3)
	call rotate3(x0, y0, val, x3, y3, threed)
        call linxy(X3,Y3)
21	continue
        XL=X0
        YL=Y0
3        continue
900        RETURN
800        XL=10E20
        YL=10E20
        GO TO 900
        END
          
c contour program but with possibility of colour coding or
c "3D" plotting

      subroutine colcon(xmin,ymin,xmax,ymax,array,n,m,value,iv, 
     +		ccode, threed, zscale, zmin)
	
      real array(n,m),value(iv)
	logical ccode, threed
	real zscale, zmin

        ivv=0
      mm=m-1
      nn=n-1

C     COMPUTE X,Y INCREMENTS

      deltx=(xmax-xmin)/float(nn)
      delty=(ymax-ymin)/float(mm)

      do 1 j=1,nn
        idir=j-j/2*2
      x=xmin+(j-1)*deltx

      do 2 i=1,mm
        ii=i
        if(idir.eq.0)ii=mm+1-i
        y=ymin+(ii-1)*delty

C     GET DATA VALUES FOR A CELL

      V1=ARRAY(J,II)
      V2=ARRAY(J+1,II)
      V3=ARRAY(J,II+1)
      V4=ARRAY(J+1,II+1)

C     CHECK EACH VALUE AGAINST DATA VALUES

        IVV=1-IVV
      DO 3 K=1,IV
        KK=K
        IF(IVV.eq.1)KK=IV+1-K
      VAL=VALUE(KK)
      ICASE=1

C     RECORD OUTCOME OF COMPARISONS IN ICASE

      IF(VAL.GT.V1) ICASE=ICASE+1
      IF(VAL.GT.V2) ICASE=ICASE+2
      IF(VAL.GT.V3) ICASE=ICASE+4
      IF(VAL.GT.V4) ICASE=9-ICASE

C     TREAT EACH CASE SEPERATELY

      GO TO (3,4,5,6,7,8,9,10),ICASE

C     DO LINEAR INTERPOLATION

    4 X0=X+DELTX*(VAL-V1)/(V2-V1)
      Y0=Y
      X1=X
      Y1=Y+DELTY*(VAL-V1)/(V3-V1)
      GO TO 11

    5 X0=X+DELTX*(VAL-V1)/(V2-V1)
      Y0=Y
      X1=X+DELTX
      Y1=Y+DELTY*(VAL-V2)/(V4-V2)
      GO TO 11

    6 X0=X
      Y0=Y+DELTY*(VAL-V1)/(V3-V1)
      X1=X+DELTX
      Y1=Y+DELTY*(VAL-V2)/(V4-V2)
      GO TO 11

    7 X0=X
      Y0=Y+DELTY*(VAL-V1)/(V3-V1)
      X1=X+DELTX*(VAL-V3)/(V4-V3)
      Y1=Y+DELTY
      GO TO 11

    8 X0=X+DELTX*(VAL-V1)/(V2-V1)
      Y0=Y
      X1=X+DELTX*(VAL-V3)/(V4-V3)
      Y1=Y+DELTY
      GO TO 11

    9 X0=X+DELTX*(VAL-V1)/(V2-V1)
      Y0=Y
      X1=X
      Y1=Y+DELTY*(VAL-V1)/(V3-V1)
        CALL XXXCNT(IDIR,X0,Y0,X1,Y1)


   10 X0=X+DELTX*(VAL-V3)/(V4-V3)
      Y0=Y+DELTY
      X1=X+DELTX
      Y1=Y+DELTY*(VAL-V2)/(V4-V2)

C     OUTPUT THE LINE SEGMENT

11        continue
	if(CCODE)call datcol(val, zmin, zscale)
        CALL XCCNT(IDIR,X0,Y0,X1,Y1, val, threed)

    3 continue

    2 continue

    1 continue
	if(CCODE)call datcol(val, zmin, zscale)
        CALL XCCNT(-1,X0,X0,X0,X0, val, threed)

      END


C CONTOUR REGION DEFINED BY DATA LIMITS
C FOR NOW ASSUME
C THAT THE NO. OF X POINTS IS EVEN
      subroutine CONT(X,Y,ZA,NPTS, ccode, threed)
	include	'robcom'
      common/KONTOR/VALUE,NVAL,NSVAL
      REAL X(*),Y(*),ZA(*)
      REAL VALUE(10),ARRAY(2,2)
      LOGICAL NSVAL
	logical ccode, threed
C      DATA NVAL,NSVAL/10,.TRUE./
C FIND MAXIMUM AND MINIMUM FOR DEFAULT CONTOUR LEVELS
        FMAX=ZA(1)
        FMIN=ZA(1)
        DO 10 I=2,NPTS
          IF(ZA(I).GT.FMAX)FMAX=ZA(I)
          IF(ZA(I).LT.FMIN)FMIN=ZA(I)
10      continue
      IF(NSVAL)then
C CONTOUR VALUES
        STEP=(FMAX-FMIN)/REAL(NVAL-1)
        DO 20 I=1,NVAL
          VALUE(I)=FMIN+(I-1)*STEP
20      continue
        end if

	zscale = fmax - fmin
              
C DOES X OR Y VARY FASTEST?
      IF(X(1).ne.X(2))then
C X VARIES FASTEST
C LOAD ARRAY VALUES AND CONTOUR
      DO 30 I=1,NPTS-2
      IF(Y(I+1).ne.Y(I))GOTO 30
      XMIN=X(I)
      XMAX=X(I+1)
      YMIN=Y(I)
      ARRAY(1,1)=ZA(I)
      ARRAY(2,1)=ZA(I+1)
C FIND NEXT VALUES
        DO 35 J=I+2,NPTS-1
          IF(X(J).eq.XMIN)then
            YMAX=Y(J)
            ARRAY(1,2)=ZA(J)
            ARRAY(2,2)=ZA(J+1)
            GOTO 36
          end if
35     continue
        GOTO 30
36      continue
	if(ccode.or.threed)then              
      		CALL COLCON(XMIN,YMIN,XMAX,YMAX,ARRAY,2,2,
     +			VALUE,NVAL, ccode, threed, zscale, fmin)
	else 
		call contor(XMIN,YMIN,XMAX,YMAX,ARRAY,2,2,
     +			VALUE,NVAL)
	end if
30    continue
        ELSE
C Y VARIES FASTEST
C LOAD ARRAY VALUES AND CONTOUR
      DO 130 I=1,NPTS-2
      IF(X(I+1).ne.X(I))GOTO 130
      YMIN=Y(I)
      YMAX=Y(I+1)
      XMIN=X(I)
      ARRAY(1,1)=ZA(I)
      ARRAY(1,2)=ZA(I+1)
C FIND NEXT VALUES
        DO 135 J=I+2,NPTS-1
          IF(Y(J).eq.YMIN)then
            XMAX=X(J)
            ARRAY(2,1)=ZA(J)
            ARRAY(2,2)=ZA(J+1)
            GOTO 136
          end if
135     continue
        GOTO 130
136     continue
                
	if(ccode.or.threed)then              
      		CALL COLCON(XMIN,YMIN,XMAX,YMAX,ARRAY,2,2,
     +			VALUE,NVAL, ccode, threed, zscale, fmin)
	else 
		call contor(XMIN,YMIN,XMAX,YMAX,ARRAY,2,2,
     +			VALUE,NVAL)
	end if
130   continue
      end if
      END
         
c set contour values by hand instead of defaults
      subroutine conval()
	include	'robcom'
      common/kontor/value,nval,nsval
      real value(10)
      logical nsval
        call sprompt('Give contour levels (all on one line)')
        call sprompt('(Blank line returns to automatic mode)')
      call getit(ainbuf,  1)
      call dcode(ainbuf,value,ainfix,aingrd,nval)
      if(nval.eq.0)then
        nsval=.true.
        nval=10
        call xtext('Returning to automatic contour values')
      else
        nsval=.false.
        write(tstring, *)'Read ',nval,' contour levels'
	call totext(tstring)
        write(tstring, *)value
	call totext(tstring)
      end if
      end
         
      subroutine spline(x,y,yfit,npts,msiz,u)
	include	'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
      real x(*),y(*),yfit(*),u(*)
	if(npts.lt.2)then
		call xtext('not enough points for a spline')
		return
	end if
      call nrspln(x,y,npts,0.0,0.0,yfit,u)
      xstep=(xmaxp-xminp)/500.
      call splnt(x,y,yfit,npts,xminp,yplot)
      if(yplot.gt.ymaxp)yplot=ymaxp
      call movxy(xminp,yplot)
                             
      do 20 i=2,500
        xplot=xminp+(i-1)*xstep
        call splnt(x,y,yfit,npts,xplot,yplot)
	if(yplot.gt.ymaxp)yplot=ymaxp
	if(yplot.lt.yminp)yplot=yminp
        call linxy(xplot,yplot)
20    continue
      end
         
c write right justified title
      subroutine rtit(x1,y1,text)
      common/chsiz/xchsiz,ychsiz,size
	character*(*) text
	logical hasbs
c presume there's no back slash control characters in the file
	hasbs = .FALSE.

	n = nnl(text)
	call getlen(text, n, iret, hasbs, xlen)
	if(hasbs)then
             xstart=x1-xlen
             call txtm(xstart,y1,text,n)
	else
	     call style(x1, y1, text, n, 1)
	end if
      end
         
c centered x or y title (specified by direc)
      subroutine ctit(pos,lab,direc)
	include	'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
      common/chsiz/xchsiz,ychsiz,size
      character*(*) lab
      character direc

	logical hasbs
c presume there's no back slash control characters in the file
	
	hasbs = .FALSE.

	imax = nnl(lab)
	call getlen(lab, imax, iret, hasbs, xlen)
      if(direc.eq.'X'.or.direc.eq.'x')then
c check for back slash controls
c if we find them then centring will only be approximate at
c best


	 centre = (xmaxp-xminp)/2. + xminp
	if(hasbs)then
        	start=(xmaxp-xminp)/2.+xminp-xlen/2.
        	call txtm(start,pos,lab,imax)
	else
	 call style(centre, pos, lab, imax, 0) 
	end if

      else if(direc.eq.'Y'.or.direc.eq.'y')then
        start=(ymaxp-yminp)/2.+yminp-real(iret)*ychsiz*4./(7.*2.)
        call txtm(pos,start,lab,imax)


      else
        call xtext('***WARNING***')
        call xtext('DIRECTION FOR S/R CTIT WAS NOT X OR Y!!!')
      end if
      end


c check whether character string to be plotted has control
c sequences in it. If so what's the "real" length of the string
	subroutine getlen(lab, imax, ilen, hasbs, xlen)
	character*(*) lab
	logical hasbs
        common/chsiz/xchsiz,ychsiz,size

	ilen = 0
	xlen = 0

	j = 1
	do 10 k = 1, imax
	if (j.gt.imax)goto 11
	ilen = ilen + 1
		if(lab(j:j).eq.'\\')then
		    hasbs = .TRUE.
		    if(lab(j+1:j+1).eq.'-'.or.
     +			lab(j+1:j+1).eq.'+')then
		         j = j + 2
		else if(lab(j+1:j+1).eq.'{')then
c special character - find matching }
			do 20 i = j+2, imax
			    if(lab(i:i).eq.'}')goto 21
20			continue
21			continue
c add a token increment
			xlen = xlen + xchsiz
			j = i + 1
c back space
		    else if(lab(j+1:j+2).eq.'bs')then
			j = j + 3
			ilen = ilen -1
			xlen = xlen - xchsiz
c colours
		    else if(lab(j+1:j+3).eq.'red')then
			j = j + 4
		    else if(lab(j+1:j+5).eq.'green')then
			j = j + 6
		    else if(lab(j+1:j+4).eq.'blue')then
			j = j + 5
		    else if(lab(j+1:j+5).eq.'white')then
			j = j + 6
		    else if(lab(j+1:j+5).eq.'black')then
			j = j + 6
		else
c assume it's a default (two letter code)
			j = j + 3
		end if
	else
		call chchk(lab(j:j), xinc)
		xlen = xlen + xinc
		j = j + 1
	end if
10	continue
11	continue

		end


         
         
C HELP - direct users to the help button
         subroutine help()
	   call xtext('To get help on a button or menu item')
	   call xtext('place the cursor over the selected item and')
	   call xtext('press the <Help> key.')
	   call xtext('If this doesn''t work, make sure the file')
	   call xtext('robot.info is in a place specified by')
	   call xtext('the HELPPATH environment variable.')
           call xtext('To load a robot file (e.g. one of the demos)')
	   call xtext('press the button marked "File".')
         end
            
C PRINT VALUES OF DATA LIMITS, PEN COLOUR, MAXIMUM NO. OF POINTS
C ALLOWED....
      subroutine values(npts,msiz)
	include	'robcom'
      common/atri/ymean,yvar,stime
      common/echoc/echo
      logical echo
      common/convg/fchi,nloop
      common/limits/xminp, xmaxp, yminp, ymaxp
	common/curpos/xcurse,ycurse
c common block chsiz is from the mssl plotting package and gives
c character size in x and y directions
      common/limit2/xmin, xmax, ymin, ymax
	double precision stime
      call xtext('Values of robot parameters are:')
      write(tstring, *)'No. of points=',npts
	call totext(tstring)
	write(tstring, *)' (Max allowed is ',msiz,')'
	call totext(tstring)
      write(tstring, *)'Data limits are:',XMIN,YMIN,XMAX,YMAX
	call totext(tstring)
      write(tstring, *)'Plot limits are:',XMINP,YMINP,XMAXP,YMAXP
	call totext(tstring)
      write(tstring, *)'Data mean and s. dev. are: ',YMEAN,YVAR
	call xtext('The variables xcursor and ycursor are set to:')
	write(tstring, *)XCURSE, YCURSE
	call totext(tstring)
      write(tstring, *)'Stime= ',stime
	call totext(tstring)
      IF(ECHO)call xtext('Echo is ON')
      IF(.NOT.ECHO)call xtext('Echo is off')
      END
         
c grid for ra/dec plots
      subroutine radecg(fiplot,fjplot,fmplot,fnplot,sxl,syl,sxh,syh)
	include	'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
      data dtr/0.0174532/
      xminp=-180.
      xmaxp=180.
      yminp=-140.
      ymaxp=140.
      if(xmaxp-xminp.ne.0.0.and.ymaxp-yminp.ne.0.0)then
        sxh2=(sxh-sxl)*fiplot/fmplot+sxl
        sxl2=(sxh-sxl)*(fiplot-1.0)/fmplot+sxl
        syh2=(syh-syl)*fjplot/fnplot+syl
        syl2=(syh-syl)*(fjplot-1.0)/fnplot+syl
	call gap(sxl2, syl2, sxh2, syh2)
        call limit(sxl2,syl2,sxh2,syh2,xminp,yminp,xmaxp,ymaxp)
      end if
c now for ra lines
      do 10 ira=-180,180,30
        x=real(ira)*cos(-90.*dtr)
        call movxy(x,-90.)
        do 20 idec=-89,90,1
          x=real(ira)*cos(real(idec)*dtr)
          call linxy(x,real(idec))
20      continue
10      continue
c dec lines
      do 30 idec=-90,90,30
        call movxy(-180.*cos(real(idec)*dtr),real(idec))
        call linxy(180.*cos(real(idec)*dtr),real(idec))
30    continue
        end
           
c plot ra/dec points
      subroutine radecp(x,y,dely,npts,pmode,isymb)
	include	'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
      real x(*),y(*),dely(*)
      character*(*) pmode
      integer isymb
c if dely is zero draw standard size point, otherwise scale
c with size of dely
c first find range of dely
      fmin=dely(1)
      fmax=dely(1)
      do 10 i=2,npts
        if(dely(i).lt.fmin)fmin=dely(i)
        if(dely(i).gt.fmax)fmax=dely(i)
10    continue

      scale=1.
      if(fmax.ne.fmin)scale=3./(fmax-fmin)
      xsize=(xmaxp-xminp)/100.
      ysize=(ymaxp-yminp)/100.



      do 20 i=1,npts

       if(pmode .eq. 'NICE')then

          call symbol(x(i),y(i), isymb)

	else

          xp=(xsize-fmin)*scale
          yp=(ysize-fmin)*scale
          if(dely(i).ne.0.0)then
            xp=xp*dely(i)
            yp=yp*dely(i)
          end if
          call movxy(x(i)-xp,y(i))
          call linxy(x(i)+xp,y(i))
          call movxy(x(i),y(i)+yp)
          call linxy(x(i),y(i)-yp)
	end if
20    continue

      end
         
c miltiply ra by cos dec for plot
      subroutine raconv(x,y,npts)
	include	'robcom'
      real x(*),y(*)
      data dtr/0.0174532/
      do 10 i=1,npts
c get data into -180 to +180 range
        if(x(i).gt.180.)x(i)=360.-x(i)
c multiply by -1 to get positive on left, negative on right
        x(i)=x(i)*(-1.0)
        x(i)=x(i)*cos(y(i)*dtr)
10    continue
      end

c convert ra/dec to galactic coordinates
        subroutine eqtgal(xdata,ydata,npts)
	include	'robcom'
      logical radec
      common/rad/radec
      real xdata(*),ydata(*)
        data c1,c2/27.4,192.25/
        twopi=atan(1.0)*8.
        dtr=twopi/360.0
        c1=c1*dtr
        c2=c2*dtr
                 
        do 10 i=1,npts
        ra=xdata(i)
        dec=ydata(i)
                    
        if(radec)ra=(-1.0)*ra/cos(dec*dtr)
                                          
        dec=dec*dtr
        ra=ra*dtr
                 
        sinb=cos(dec)*cos(c1)*cos(ra-c2)+sin(dec)*sin(c1)
        beta=asin(sinb)
        y=sin(dec)-sin(beta)*sin(c1)
        x=cos(dec)*sin(ra-c2)*cos(c1)
        flam=atan2(y,x)
                       
        flam=flam/dtr
        beta=beta/dtr
                     
        flam=flam+33.0
        if(flam.lt.0.0)flam=flam+360.0
                                      
        xdata(i)=flam
        ydata(i)=beta
        if(radec)xdata(i)=flam*cos(beta*dtr)
10      continue
        end
           
           
           
           
c curfit from bevington
       subroutine curfit(x,y,sigmay,npts,nterms,mode,a,deltaa,
     +sigmaa,flamda,yfit,chisqr)
	include	'robcom'
	include	'fitcom'
       real x(*),y(*),sigmay(*),a(*),deltaa(*),sigmaa(*),
     +yfit(*)
C      DIMENSION WEIGHT(100),ALPHA(10,10),BETA(10),DERIV(10),
C    +ARRAY(10,10),B(10)
C MODIFIED SO THAT WEIGHT IS NOT AN ARRAY BUT IS CALCULATED EACH TIME
C TAKES MORE TIME BUT THERE ARE HENCE NO LIMITATIONS ON ARRAY SIZES
       double precision alpha(maxpar,maxpar),beta(maxpar),deriv(maxpar),
     +array(maxpar,maxpar),b(maxpar)
	real bsing(maxpar)
	double precision det
c maximum no. of times for curfit to loop if chi**2 increase
	data nmax/10/
c maximum value of flamda allowed
	data flmax/1.0e20/
	itried = 0
	
 
11     nfree=npts-nterms
       if(nfree)13,13,20
13     chisqr=0.0
       goto 110
20     continue



c evaluate alpha and beta matrices
c
31     do 34 j=1,nterms
       beta(j)=0.0d0
       do 34 k=1,j
34     alpha(j,k)=0.0d0
41     do 50 i=1,npts
c new section
       if(mode.eq.-1)then
         weight=1./abs(y(i))
       else if(mode.eq.1)then
         weight=1./(sigmay(i)**2)
       else if(mode.eq.0)then
         weight=1.
       else
         write(tstring, *)'undefined weighting mode= ',mode
	 call totext(tstring)
         return
       end if
C END OF NEW SECTION
       call fderiv(x,i,a,deltaa,nterms,deriv)
       do 46 j=1,nterms
       beta(j)=beta(j)+weight*(y(i)-functn(x,i,a))*deriv(j)
       do 46 k=1,j
46     alpha(j,k)=alpha(j,k)+weight*deriv(j)*deriv(k)
50     continue
51     do 53 j=1,nterms
       do 53 k=1,j
53     alpha(k,j)=alpha(j,k)

C EVAL CHI2 AT START

61     do 62 i=1,npts
62     yfit(i)=functn(x,i,a)
63     chisq1=fchisq(y,sigmay,npts,nfree,mode,yfit)

C INVERT MATRIX

71     continue
               
c check whether chi**2 increased too many times
	if (itried .gt. nmax .or. flamda .gt. flmax)goto 999
                                                     
	do 74 j=1,nterms
       do 73 k=1,nterms
73     array(j,k)=alpha(j,k)/sqrt(alpha(j,j)*alpha(k,k))
74     array(j,j)=1.+flamda
80     call matinv(array,nterms,det)
81     do 84 j=1,nterms
       b(j) = a(j)
       do 84 k=1,nterms
84     b(j) = b(j)+beta(k)*array(j,k)/sqrt(alpha(j,j)*alpha(k,k))

	do 85 k = 1, nterms
	bsing(k) = b(k)
85	continue

C IF INCREASE CHI2 TRY AGAIN

91     do 92 i=1,npts
92     yfit(i)=functn(x,i,bsing)
93     chisqr=fchisq(y,sigmay,npts,nfree,mode,yfit)
	itried = itried + 1
       if(chisq1-chisqr)95,101,101
95     flamda=10.*flamda
       goto 71
c eval parameters and errors
101    do 103  j=1,nterms
       a(j) = b(j)
103    sigmaa(j)=sqrt(array(j,j)/alpha(j,j))
       flamda=flamda/10.
110    return
c chi**2 increased too many times
999	continue
	call xtext('WARNING - CURFIT IN TROUBLE')
	call xtext('Emergency Exit')
	return
       end
          
       subroutine matinv(array,norder,det)
c invert symmetric matrix and calculate det
	include 'fitcom'
       double precision array(maxpar, maxpar), amax, save
	double precision det
       integer ik(maxpar),jk(maxpar)
10     det=1.0d0
11     do 100 k=1,norder

c find largest element
       amax=0.
21     do 30 i=k,norder
       do 30 j=k,norder
23     if(dabs(amax)-dabs(array(i,j)))24,24,30
24     amax=array(i,j)
       ik(k)=i
       jk(k)=j
30     continue

c interchange rows and columns
31     if(amax)41,32,41
32     det=0.0d0
       goto 140
41     i=ik(k)
       if(i-k)21,51,43
43     do 50 j=1,norder
       save=array(k,j)
       array(k,j)=array(i,j)
50     array(i,j)=-save
51     j=jk(k)
       if(j-k)21,61,53
53     do 60 i=1,norder
       save=array(i,k)
       array(i,k)=array(i,j)
60     array(i,j)=-save

c accumulate elements
61     do 70 i=1,norder
       if(i-k)63,70,63
63     array(i,k)=-array(i,k)/amax
70     continue
71     do 80 i=1,norder
       do 80 j=1,norder
       if(i-k)74,80,74
74     if(j-k)75,80,75
75     array(i,j)=array(i,j)+array(i,k)*array(k,j)
80     continue
81     do 90 j=1,norder
       if(j-k)83,90,83
83     array(k,j)=array(k,j)/amax
90     continue
       array(k,k)=1./amax
100    det=det*amax

c restore matrix order
101    do 130 l=1,norder
       k=norder-l+1
       j=ik(k)
       if(j-k)111,111,105
105    do 110 i=1,norder
       save=array(i,k)
       array(i,k)=-array(i,j)
110    array(i,j)=save
111    i=jk(k)
       if(i-k)130,130,113
113    do 120 j=1,norder
       save=array(k,j)
       array(k,j)=-array(i,j)
120    array(i,j)=save
130    continue
140    return
       end

        function fchisq(y,sigmay,npts,nfree,mode,yfit)
        double precision chisq, weight
        dimension y(*), sigmay(*), yfit(*)
11      chisq = 0.0d0
12      if(nfree)13,13,20
13      fchisq = 0.0
        goto 40
20      do 30 i=1, npts
21      if(mode)22,27,29
22      if(y(i))25,27,23
23      weight = 1./y(i)
        goto 30
25      weight = 1./(-y(i))
        goto 30
27      weight = 1.0d0
        goto 30
29      weight=1./(sigmay(i)*sigmay(i))
30      chisq = chisq+weight*(y(i)-yfit(i))**2
c divide by no. of degrees of freedom
31      free = nfree
32      fchisq = chisq/free
40      return
        end
           
        subroutine fderiv(x,i,a,deltaa,nterms,deriv)
        dimension x(*),a(*),deltaa(*)
	double precision deriv(*)
11      do 18 j=1,nterms
        aj=a(j)
        delta=deltaa(j)
        a(j)=aj+delta
        yfit=functn(x,i,a)
        a(j)=aj-delta
        deriv(j)=(yfit-functn(x,i,a))/(2.*delta)
18      a(j)=aj
        return
        end
          
