c get starting position for axis labelling
	subroutine getmin(start, min, dist)
	real start, min, dist

10	continue

	if(start .lt. min)then
		start = start + dist
		goto 10
	end if

	end



c do a filled plot
c assume sorted points
	subroutine fplot(npts, x, y, ifstyle, fillvalue)
	integer npts, ifstyle
	real x(*), y(*)
	real xp, yp
	common/test/xtmin, xtmax, ytmin, ytmax
        common/limits/xminp, xmaxp, yminp, ymaxp
	common/limit2/xmin, xmax, ymin, ymax
	real ytofill

	iloop = npts - 1

	if(ifstyle .eq. 0)then
		iloop = npts-2
	else if(ifstyle .eq. 1)then
		ytofill = 0.0
	else if(ifstyle .eq. 2)then
		ytofill = yminp
	else if(ifstyle .eq. 3)then
		ytofill = ymaxp
	else if(ifstyle .eq. 4)then
		ytofill = fillvalue
	else if(ifstyle .eq. 5)then
		ytofill = ymin
	else if(ifstyle .eq. 6)then
		ytofill = ymax
	else
		call xtext('ERROR: INVALID OPTION IN FILL PLOT')
	end if


	do 1000 i = 1, iloop




	if(ifstyle .eq. 0)then

		if(x(i+2) .lt. xtmin) goto 1000
		if(x(i) .gt. xtmax) goto 2000

		xp = MAX(x(i), xtmin)
		xp = MIN(xp, xtmax)
		yp = MAX(y(i), ytmin)
		yp = MIN(yp, ytmax)
		call arkpoly1(xp, yp)

		xp = MAX(x(i+1), xtmin)
		xp = MIN(xp, xtmax)
		yp = MAX(y(i+1), ytmin)
		yp = MIN(yp, ytmax)
		call arkpoly(xp, yp)
		xp = MAX(x(i+2), xtmin)
		xp = MIN(xp, xtmax)
		yp = MAX(y(i+2), ytmin)
		yp = MIN(yp, ytmax)
		call arkpoly(xp, yp)
		call arkpoly2()
		
	else if(ifstyle .gt. 0)then
		
		if(x(i+1) .lt. xtmin) goto 1000
		if(x(i) .gt. xtmax) goto 2000

		xp = MAX(x(i), xtmin)
		xp = MIN(xp, xtmax)
		yp = MAX(ytofill, ytmin)
		yp = MIN(yp, ytmax)
		call arkpoly1(xp, yp)

		yp = MAX(y(i), ytmin)
		yp = MIN(yp, ytmax)
		call arkpoly(xp, yp)

		xp = MAX(x(i+1), xtmin)
		xp = MIN(xp, xtmax)
		yp = MAX(y(i+1), ytmin)
		yp = MIN(yp, ytmax)
		call arkpoly(xp, yp)

		yp = MAX(ytofill, ytmin)
		yp = MIN(ytofill, ytmax)
		call arkpoly(xp, yp)
		call arkpoly2()
	end if


1000	continue

2000	continue

	end







	subroutine sbarplt(npts, xp, yp, xerr, yerr)
	real xp(*), yp(*), xerr(*), yerr(*)
	include 'robcom'
c do a 'stacked bar' plot
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
		yb = yt
		yt = MIN(yt + yerr(i), ytmax)
		call boxm(xl, yb, xr, yt)
		
1000	continue
	end


	subroutine pieplot(npts, xp, yp, xerr, yerr)
	real xp(*), yp(*), xerr(*), yerr(*)
	include 'robcom'
	common/limits/xminp,xmaxp,yminp,ymaxp

	double precision sum, sum2
	xcent = (xminp + xmaxp)/2.
	ycent = (yminp + ymaxp)/2.
	radius = (xmaxp - xminp)*0.4

c do a "pie" plot - usese only values in "Y" array

c first sum values
	sum = 0.0d0
	do 10 i = 1, npts
		sum = sum + yp(i)
10	continue
c normalize
	sum = sum/360.



c now plot some arcs
	angle1 = 0.0
	sum2 = yp(1)
	do 20 i = 2, npts
		angle2 = sum2 / sum
		sum2 = sum2 + yp(i)
		ifill = i - 2*(i/2)
		call arc(xcent, ycent, radius, angle1, angle2, ifill)
		angle1 = angle2
20	continue
	angle2 = 360.
	call arc(xcent, ycent, radius, angle1, angle2, ifill)
	end

		
    



      subroutine erbar(npt,xpt,ypt,dxm,dxp,dym,dyp,id)
	include 'robcom'
c draws error bars
c original by brian vaughan
c if id=1 draw horizontal bar at middle
c if id=2 draw at top and bottom
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
      integer*4 npt,ido
      real*4 xpt(*),ypt(*),dxm(*),dxp(*),dym(*),dyp(*)
      real*4 x,y,xlt,xrt,ybt,ytp

	iplot = 0
	iskip = 0
      do 1002 ido = 1, npt
        x = xpt(ido)
        y = ypt(ido)
        xrt = x + dxp(ido)
	if(xrt.lt.xtmin)goto 1002
        xlt = x - dxm(ido)
	if(xlt.gt.xtmax)goto 1002
        ybt = y - dym(ido)
	if(ybt.gt.ytmax) goto 1002
        ytp = y + dyp(ido)
	if(ytp.lt.ytmin)goto 1002
                          
        if(y.gt.ytmax)y=ytmax
        if(y.lt.ytmin)y=ytmin
        if(x.gt.xtmax)x=xtmax
        if(x.lt.xtmin)x=xtmin
        if(ytp.gt.ytmax)ytp=ytmax
        if(ytp.lt.ytmin)ytp=ytmin
        if(ybt.lt.ytmin)ybt=ytmin
        if(ybt.gt.ytmax)ybt=ytmax
        if(xrt.lt.xtmin)xrt=xtmin
        if(xrt.gt.xtmax)xrt=xtmax
        if(xlt.lt.xtmin)xlt=xtmin
        if(xlt.gt.xtmax)xlt=xtmax
                                 
        call movxy(x,ybt)
        call linxy(x,ytp)
        if(id.eq.1)then
          call movxy(xlt,y)
          call linxy(xrt,y)
        else if(id.eq.2)then
          call movxy(xlt,ytp)
          call linxy(xrt,ytp)
          call movxy(xlt,ybt)
          call linxy(xrt,ybt)
        end if
              
1002	continue
      end
         
      subroutine erbox(npt,xpt,ypt,dxm,dxp,dym,dyp)

      integer*4 npt,ido
      real*4 xpt(npt),ypt(npt),dxm(npt),dxp(npt),dym(npt),dyp(npt)
      real*4 x,y,xlt,xrt,ybt,ytp
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax

      do 1002 ido = 1, npt
        x = xpt(ido)
        y = ypt(ido)
        xrt = x + dxp(ido)
	if(xrt.lt.xtmin)goto 1002
        xlt = x - dxm(ido)
	if(xlt.gt.xtmax)goto 1002
        ybt = y - dym(ido)
	if(ybt.gt.ytmax) goto 1002
        ytp = y + dyp(ido)
	if(ytp.lt.ytmin)goto 1002
                          
        if(ytp.gt.ytmax)ytp=ytmax
        if(ytp.lt.ytmin)ytp=ytmin
        if(ybt.lt.ytmin)ybt=ytmin
        if(ybt.gt.ytmax)ybt=ytmax
        if(xrt.lt.xtmin)xrt=xtmin
        if(xrt.gt.xtmax)xrt=xtmax
        if(xlt.lt.xtmin)xlt=xtmin
        if(xlt.gt.xtmax)xlt=xtmax
        call movxy(xlt,ybt)
        call linxy(xrt,ybt)
        call linxy(xrt,ytp)
        call linxy(xlt,ytp)
        call linxy(xlt,ybt)
 1002 continue
      end
         
      subroutine erboxf(npt,xpt,ypt,dxm,dxp,dym,dyp)
c  filled error box
      integer*4 npt,ido
      real*4 xpt(npt),ypt(npt),dxm(npt),dxp(npt),dym(npt),dyp(npt)
      real*4 x,y,xlt,xrt,ybt,ytp
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax

      do 1002 ido = 1, npt
        x = xpt(ido)
        y = ypt(ido)
        xrt = x + dxp(ido)
	if(xrt.lt.xtmin)goto 1002
        xlt = x - dxm(ido)
	if(xlt.gt.xtmax)goto 1002
        ybt = y - dym(ido)
	if(ybt.gt.ytmax) goto 1002
        ytp = y + dyp(ido)
	if(ytp.lt.ytmin)goto 1002
                          
        if(ytp.gt.ytmax)ytp=ytmax
        if(ytp.lt.ytmin)ytp=ytmin
        if(ybt.lt.ytmin)ybt=ytmin
        if(ybt.gt.ytmax)ybt=ytmax
        if(xrt.lt.xtmin)xrt=xtmin
        if(xrt.gt.xtmax)xrt=xtmax
        if(xlt.lt.xtmin)xlt=xtmin
        if(xlt.gt.xtmax)xlt=xtmax
	call fbox(xlt, ybt, xrt, ytp)
                              
 1002 continue
      end
         
         
      subroutine erdia(npt,xpt,ypt,dxm,dxp,dym,dyp)

      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
      integer*4 npt,ido
      real*4 xpt(npt),ypt(npt),dxm(npt),dxp(npt),dym(npt),dyp(npt)
      real*4 x,y,xlt,xrt,ybt,ytp

      do 1002 ido = 1, npt
        x = xpt(ido)
        y = ypt(ido)
        xrt = x + dxp(ido)
	if(xrt.lt.xtmin)goto 1002
        xlt = x - dxm(ido)
	if(xlt.gt.xtmax)goto 1002
        ybt = y - dym(ido)
	if(ybt.gt.ytmax) goto 1002
        ytp = y + dyp(ido)
	if(ytp.lt.ytmin)goto 1002
                          
        if(y.gt.ytmax)y=ytmax
        if(y.lt.ytmin)y=ytmin
        if(x.gt.xtmax)x=xtmax
        if(x.lt.xtmin)x=xtmin
        if(ytp.gt.ytmax)ytp=ytmax
        if(ytp.lt.ytmin)ytp=ytmin
        if(ybt.lt.ytmin)ybt=ytmin
        if(ybt.gt.ytmax)ybt=ytmax
        if(xrt.lt.xtmin)xrt=xtmin
        if(xrt.gt.xtmax)xrt=xtmax
        if(xlt.lt.xtmin)xlt=xtmin
        if(xlt.gt.xtmax)xlt=xtmax
        call movxy(x,ybt)
        call linxy(xrt,y)
        call linxy(x,ytp)
        call linxy(xlt,y)
        call linxy(x,ybt)
1002  continue
      end

      subroutine erell(npt,xpt,ypt,dxm,dxp,dym,dyp)

      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
      integer*4 npt,ido
      real*4 xpt(npt),ypt(npt),dxm(npt),dxp(npt),dym(npt),dyp(npt)
      real*4 x,y,xlt,xrt,ybt,ytp

      do 1002 ido = 1, npt
        x = xpt(ido)
        y = ypt(ido)
        xrt = x + dxp(ido)
	if(xrt.lt.xtmin)goto 1002
        xlt = x - dxm(ido)
	if(xlt.gt.xtmax)goto 1002
        ybt = y - dym(ido)
	if(ybt.gt.ytmax) goto 1002
        ytp = y + dyp(ido)
	if(ytp.lt.ytmin)goto 1002
                          
        if(y.gt.ytmax)y=ytmax
        if(y.lt.ytmin)y=ytmin
        if(x.gt.xtmax)x=xtmax
        if(x.lt.xtmin)x=xtmin
        if(ytp.gt.ytmax)ytp=ytmax
        if(ytp.lt.ytmin)ytp=ytmin
        if(ybt.lt.ytmin)ybt=ytmin
        if(ybt.gt.ytmax)ybt=ytmax
        if(xrt.lt.xtmin)xrt=xtmin
        if(xrt.gt.xtmax)xrt=xtmax
        if(xlt.lt.xtmin)xlt=xtmin
        if(xlt.gt.xtmax)xlt=xtmax
	call elipse(x, y, xrt, ytp)
1002  continue
      end
         
         
      subroutine ercrs(npt,xpt,ypt,dxm,dxp,dym,dyp)

      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
      integer*4 npt,ido
      real*4 xpt(npt),ypt(npt),dxm(npt),dxp(npt),dym(npt),dyp(npt)
      real*4 x,y,xlt,xrt,ybt,ytp

      do 1002 ido = 1, npt
        x = xpt(ido)
        y = ypt(ido)
        xrt = x + dxp(ido)
	if(xrt.lt.xtmin)goto 1002
        xlt = x - dxm(ido)
	if(xlt.gt.xtmax)goto 1002
        ybt = y - dym(ido)
	if(ybt.gt.ytmax) goto 1002
        ytp = y + dyp(ido)
	if(ytp.lt.ytmin)goto 1002
                          
        if(y.gt.ytmax)y=ytmax
        if(y.lt.ytmin)y=ytmin
        if(x.gt.xtmax)x=xtmax
        if(x.lt.xtmin)x=xtmin
        if(ytp.gt.ytmax)ytp=ytmax
        if(ytp.lt.ytmin)ytp=ytmin
        if(ybt.lt.ytmin)ybt=ytmin
        if(ybt.gt.ytmax)ybt=ytmax
        if(xrt.lt.xtmin)xrt=xtmin
        if(xrt.gt.xtmax)xrt=xtmax
        if(xlt.lt.xtmin)xlt=xtmin
        if(xlt.gt.xtmax)xlt=xtmax
        call movxy(xlt,ybt)
        call linxy(x,y)
        call linxy(xlt,ytp)
        call movxy(xrt,ytp)
        call linxy(x,y)
        call linxy(xrt,ybt)
                           
1002  continue
      end
         
      subroutine smooth(y,npts,ntime,msize)
	include 'robcom'
c smooth data by convolving with triangle, end points treated
c slightly differently
      real y(msize)
                   
      if(npts.le.2)then
         write(tstring,'(a, i, a)')
     +       '***There are only ',NPTS,'in the data set***'
	call totext(tstring)
        call xtext('I will not smooth that few data points')
        return
      end if
            
      do 100 j=1,ntime
                      
        old=y(1)
        y(1)=old*0.666666+y(2)*0.333333
                                       
        do 10 i=2,npts-1
          fnow=y(i)
          y(i)=old*0.25+fnow*0.5+y(i+1)*0.25
          old=fnow
10      continue
        y(npts)=y(npts)*0.6666666+old*0.333333
100     continue
                
      end
         
c rebin data
        subroutine rebin(x,y,delx,dely,npts)
	include 'robcom'
        real x(*),y(*),delx(*),dely(*)
c read the rebinning fact0r
        call sprompt('Give rebinning factor')
        call getit(ainbuf,  1)
        call dcode(ainbuf,ain1,ainfix,aingrd,k)
	fn = ain1(1)
        n=nint(fn)
        i=1
        do 10 ind=1,npts
          iup=min(npts,i+n-1)
          dely(ind)=dely(i)*dely(i)
          y(ind)=y(i)
          do 20 j=i+1,iup
            y(ind)=y(ind)+y(j)
            dely(ind)=dely(ind)+dely(j)*dely(j)
20        continue
          y(ind)=y(ind)/real(iup-i+1)
          dely(ind)=sqrt(dely(ind))/real(iup-i+1)
          delx(ind)=(x(iup)+delx(iup)-x(i)+delx(i))/2.0
          x(ind)=(x(i)+x(iup))/2.0
          i=i+n
          if(i.gt.npts)goto 30
10        continue
30        continue
                  
          npts=ind
                  
                  
                  
        end
           
           
      subroutine linfit(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
	include 'robcom'
      real x(*),y(*),sigmay(*)
                              
c check whether a weighted fit was requested for a data set
c without errors
	if(mode.eq.1.and.sigmay(1).eq.0.0)then
		mode = 0
		call xtext('*Not* weighting by errors')
		call xtext('First error value zero')
	end if
c doo the real work
	call fitlin(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
                                                      
C PRINT RESULTS
      call xtext('Results of straight line fit are:')
      write(tstring,*)'Gradient= '
	call addrealt(b)
	call addtextt(' +/- ')
	call addrealt(sigmab)
	call totext(tstring)
      write(tstring,*)'Intercept= '
		call addrealt(a)
		call addtextt(' +/- ')
		call addrealt(sigmaa)
	call totext(tstring)
      write(tstring,*)'Linear correlation coefficient='
		call addrealt(r)
	call totext(tstring)
      write(ifil8,*)'Results of straight line fit are:'
      write(ifil8,*)'Gradient= ',b,' +/- ',sigmab
      write(ifil8,*)'Intercept= ',a,' +/- ',sigmaa
      write(ifil8,*)'Linear correlation coefficient=',r
c for small numbers of points evaluate significance of
c correlation coefficient (only for small numbers to avoid
c the bevington routine crashing
      if(npts.le.100)then
        prob=pcorre(r,npts)
        call xtext('Probability of getting this level of correlation')
        call xtext('or greater')
        write(tstring,*)
     +      'with an uncorrelated data set= '
		call addrealt(PROB*100.)
		call addtextt('%')
	call totext(tstring)
        write(ifil8,*)'Probability of getting this level of correlation'
        write(ifil8,*)'or greater'
        write(ifil8,*)'with an uncorrelated data set= ',PROB*100.,'%'
       end if
	end
    
	subroutine fitlin(X,Y,SIGMAY,NPTS,MODE,A,SIGMAA,B,SIGMAB,R)
C FROM BEVINGTON P. 105
      double precision SUM,SUMX,SUMY,SUMX2,SUMXY,SUMY2
      double precision XI,YI,WEIGHT,DELTA,VARNCE
      real x(*),y(*),sigmay(*)
                              
c accumulate weighted sums
11    sum=0.0
      sumx=0.0
      sumy=0.0
      sumx2=0.0
      sumxy=0.0
      sumy2=0.0
21    do 50 i=1,npts
      xi=x(i)
      yi=y(i)
      if (mode)31,36,38
31    if(yi)34,36,32
32    weight=1./yi
      goto 41
34    weight=1./(-yi)
      goto 41
36    weight=1.0
      goto 41
38    weight=1./(sigmay(i)*sigmay(i))
41    sum=sum+weight
      sumx=sumx+weight*xi
      sumy=sumy+weight*yi
      sumx2=sumx2+weight*xi*xi
      sumxy=sumxy+weight*xi*yi
      sumy2=sumy2+weight*yi*yi
50    continue
c calculate coefficients and standard deviations
51    delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
53    b=(sumxy*sum-sumx*sumy)/delta
61    if(mode)62,64,62
62    varnce=1.0
      goto 67
64    c=npts-2
      varnce=(sumy2+a*a*sum+b*b*sumx2
     +-2.*(a*sumy+b*sumxy-a*b*sumx))/c
67    sigmaa=dsqrt(varnce*sumx2/delta)
68    sigmab=dsqrt(varnce*sum/delta)
71    r=(sum*sumxy-sumx*sumy)/
     +dsqrt(delta*(sum*sumy2-sumy*sumy))
      end
         
c significance of correlation coefficient
c bevington p.125
      function pcorre(r,npts)
      double precision r2,term,sum,fi,fnum,denom
11    nfree=npts-2
      if(nfree)13,13,15
13    pcorre=0.0
      goto 60
15    r2=r*r
      if(1.-r2)13,13,17
17    neven=2*(nfree/2)
      if(nfree-neven)21,21,41
c no. of degrees of freedom is even
21    imax=(nfree-2)/2
      free=nfree
23    term=abs(r)
      sum=term
      if(imax)60,26,31
26    pcorre=1.-term
      goto 60
31    do 36 i=1,imax
      fi=i
      fnum=imax-i+1
      denom=2*i+1
      term=-term*r2*fnum/fi
36    sum=sum+term/denom
      pcorre=1.128379167*(gamma((free+1.)/2.)/gamma(free/2.))
      pcorre=1.-pcorre*sum
      goto 60
c number of degrees of freedom is odd
41    imax=(nfree-3)/2
42    term=abs(r)*dsqrt(1.-r2)
43    sum=datan(r2/term)
      if(imax)57,45,51
45    sum=sum+term
      goto 57
51    sum=sum+term
52    do 56 i=1,imax
      fnum=2*i
      denom=2*i+1
      term=term*(1.-r2)*fnum/denom
56    sum=sum+term
57    pcorre=1.-0.6366197724*sum
60    return
      end
         
c gamma function of integers and half integers
c bevington p. 126
      function gamma(x)
      double precision prod,sum,fi
c integerize argument
11    n=x-0.25
      xn=n
13    if(x-xn-0.75)31,31,21
c argument is integer
21    gamma=factr(n)
      goto 60
c argument is half integer
31    prod=1.77245385
      if(n)44,44,33
33    if(n-10)41,41,51
41    do 43 i=1,n
      fi=i
43    prod=prod*(fi-0.5)
44    gamma=prod
      goto 60
51    sum=0.0
      do 54 i=11,n
      fi=i
54    sum=sum+dlog(fi-0.5)
55    gamma=prod*639383.8623*dexp(sum)
60    return
      end
         
c subtract polynomial fit from data
      subroutine subpol(x,y,npts,apoly,nterms)
      real x(*),y(*),apoly(*)
      do 10 i=1,npts
        y(i)=y(i)-fpol(x(i),apoly,nterms)
10    continue
      end
         
         
	subroutine pltcrd
c plot fitted function as a dashed line
	call dodash()
	call pltcur(80)
	call nodash()
	end
    
    
    
c plot results of general function fit
      subroutine pltcur(nstep)
	integer nstep
c argumnet is no. of steps to use for plotting model
c use a lot for a smoth continuous line
c and less for a dashed line
	include 'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
	include 'fitcom'
        common/robmod/bpoly,norder,fixpar,ifpar,nmodel,mtype
        common/roba/a,sigmaa
        common/robspm/ismod
        real fixpar(maxpar)
        integer ifpar(maxpar)
        character*15 mtype(maxmod)
        real a(maxpar), sigmaa(maxpar)
        real bpoly(10),xplot(1)
      if(nmodel.eq.0)then
        call xtext('CURFIT HAS NOT YET BEEN CALLED, NOT PLOTTING')
        return
      end if
      step=(xmaxp-xminp)/float(nstep)
      n = 0
9     do 10 i=n,nstep
        xplot(1)=xminp+real(i)*step
        yplot=functn(xplot,1,a)
        if(yplot.lt.ytmax.and.yplot.gt.ytmin)then
          call movxy(xplot(1),yplot)
          goto 11
        end if
10    continue
	goto 21
11    continue
      do 20 j=i,nstep
        xplot(1)=xminp+real(j)*step
        yplot=functn(xplot,1,a)
        if(yplot.lt.ytmax.and.yplot.gt.ytmin)then
          call linxy(xplot(1),yplot)
        else
          n=j
          if(n.ne.nstep)goto 9
        end if
20	continue
21	continue
      end
         
c plot results of function fit with
c others set to zero
c 	----	as DASHED LINE
	subroutine pltcmd()
	call dodash()
	call pltcrm(80)
	call nodash()
	end
    
    
    
    
    
    
c plot results of general function fit
c individual models with others set equal to zero
      subroutine pltcrm(nstep)
	include 'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
	include 'fitcom'
        common/robmod/bpoly,norder,fixpar,ifpar,nmodel,mtype
        common/roba/a,sigmaa
c ismod should be 0 in general, otherwise the function "functn"
c will only use the specific model number "ismod"
        common/robspm/ismod
        real fixpar(maxpar)
        integer ifpar(maxpar)
        character*15 mtype(maxmod)
        real a(maxpar), sigmaa(maxpar)
      real bpoly(10),xplot(1)
c no. of model steps to use
c which models are to be plotted?
3      continue
	call sprompt('Give models to be plotted (e.g. 2, 4:7, ALL)')
        call getit(ainbuf,  1)
	call lctouc(ainbuf)
c N.B. the reply to this question is written out at the end of the
c routine after error checks etc.
                                 
      if(ainbuf.eq.'ALL')then
        istart=1
        iend=nmodel
        goto 5
      end if
            
c check for a colon in the middle
       do 6 i=1,60
         if(ainbuf(i:i).eq.':')then
           read(ainbuf(1:i-1),*, err = 900)istart
           read(ainbuf(i+1:60),*, err = 900)iend
           goto 5
         end if
6      continue
               
c so its just the one model to plot
       read(ainbuf,*, err = 900)istart
       iend=istart
                  
5	continue
c check models with requested numbers really exist
       if(iend.gt.nmodel)then
         call xtext('***WARNING***')
         call xtext('REQUESTED END MODEL TOO HIGH')
         write(tstring,*)'RESET TO',NMODEL
	call totext(tstring)
         iend=nmodel
       end if
       if(istart.gt.nmodel)then
         call xtext('***WARNING***')
         call xtext('REQUESTED START MODEL TOO HIGH')
         write(tstring,*)'RESET TO',NMODEL
	call totext(tstring)
         istart=nmodel
       end if
             
	if(iend.lt.1)then
		call xtext('***WARNING***')
		call xtext('Requested end model too low')
		call xtext('reset to 1')
		iend = 1
	end if
       
	if(istart.lt.1)then
		call xtext('***WARNING***')
		call xtext('Requested start model too low')
		call xtext('reset to 1')
		istart = 1
	end if
       
       
       
      call xtext('Plotting individual models numbers:')
       write(tstring,*)istart,' to ',iend
	call totext(tstring)
      step=(xmaxp-xminp)/float(nstep)
      do 500 ismod=istart,iend
      n=0
9     do 10 i=n,nstep
        xplot(1)=xminp+real(i)*step
        yplot=functn(xplot,1,a)
        if(yplot.lt.ytmax.and.yplot.gt.ytmin)then
          call movxy(xplot(1),yplot)
          goto 11
        end if
10    continue
      goto 500
11    continue
      do 20 j=i,nstep
        xplot(1)=xminp+real(j)*step
        yplot=functn(xplot,1,a)
        if(yplot.lt.ytmax.and.yplot.gt.ytmin)then
          call linxy(xplot(1),yplot)
        else
          n=j
          goto 9
        end if
20    continue
500   continue
      ismod=0
             
                                
	return
       
900	call xtext('Error reading that input')
       
       
      end
         
         
         
c subtract general function fit
        subroutine subcur(x,y,npts,id)
	include 'robcom'
c maximum number of individual models that can be added is maxmod
c total number of allowed parameters is maxpar
	include 'fitcom'
        common/roba/a,sigmaa
        real a(maxpar), sigmaa(maxpar)
      real x(*),y(*)
                    
      if(id.eq.1)then
        do 20 i=1,npts
          y(i)=y(i)+functn(x,i,a)
20      continue
      else
        do 120 i=1,npts
          y(i)=y(i)-functn(x,i,a)
120      continue
      end if
      end
         
c plot results of polynomial fit
      subroutine pltpol(apoly,nterms)
	include 'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
      real apoly(*)
      step=(xmaxp-xminp)/200.0
      n=0
9     do 10 i=n,200
        xplot=xminp+real(i)*step
        yplot=fpol(xplot,apoly,nterms)
        if(yplot.lt.ytmax.and.yplot.gt.ytmin)then
          call movxy(xplot,yplot)
          goto 11
        end if
10    continue
      goto 21
11    continue
      do 20 j=i,200
        xplot=xminp+real(j)*step
        yplot=fpol(xplot,apoly,nterms)
        if(yplot.lt.ytmax.and.yplot.gt.ytmin)then
          call linxy(xplot,yplot)
        else
          n=j
          goto 9
        end if
20    continue
21	continue
      end
         
      function fpol(xplot, apoly, nterms)
      real apoly(*)
      double precision dpol


      dpol = apoly(nterms)

      do 10 i = nterms-1, 1, -1
      	dpol = dpol*xplot + apoly(i)
10    continue

      fpol = dpol

      end
         
c plot results of straight line fit
      subroutine pltfit(fint,grad)
	include 'robcom'
      common/limits/xminp,xmaxp,yminp,ymaxp
	common/test/xtmin, xtmax, ytmin, ytmax
c find start point
      yplot=fint+grad*xminp
      xplot=xminp
      if(yplot.lt.yminp)then
       xplot=(yminp-fint)/grad
       yplot=yminp
      else if(yplot.gt.ytmax)then
       xplot=(ymaxp-fint)/grad
       yplot=ymaxp
      end if
      call movxy(xplot,yplot)
                             
c find end point
      yplot=fint+grad*xmaxp
      xplot=xmaxp
      if(yplot.gt.ytmax)then
       xplot=(ymaxp-fint)/grad
       yplot=ymaxp
      end if
      if(yplot.lt.ytmin)then
       xplot=(yminp-fint)/grad
       yplot=yminp
      end if
      call linxy(xplot,yplot)
      end
         
c fold data without binning
c folds data in fit arrays and then overwrites data set
      subroutine fold(x,y,delx,dely,npts,nptsf,xfit,yfit,xfite,yfite,
     +period,epoch)
	include 'robcom'
      real x(*),y(*),delx(*),dely(*),xfit(*),yfit(*),xfite(*),yfite(*)
                                                                      
      do 100 i=1,nptsf
        time=(xfit(i)-epoch)/period
        inp=int(time)
        time=time-inp
        if(time.lt.0.0)time=time+1.0
        xfit(i)=time
        x(i)=xfit(i)
        y(i)=yfit(i)
        dely(i)=yfite(i)
        xfite(i)=xfite(i)/period
        delx(i)=xfite(i)
100   continue
      npts=nptsf
                
      end
         
c fold data and put into bins
      subroutine bfold(x,y,delx,dely,npts,period,epoch,nbins,chisqr,io)
	include 'robcom'
      parameter (maxbin=100)
      real x(*),y(*),delx(*),dely(*)
      double precision bin(maxbin),berr(maxbin)
      double precision asum,bsum
      double precision dnbin(maxbin)
      double precision temp(maxbin)
      double precision tempv
      real iperiod
      real stepdef

c check values are not totally stupid
	if(nbins .le. 1)then
          write(tstring,*)'Silly number of bins for folding=',nbins
	  call xtext(tstring)
	end if

c zero bins
      do 50 i=1,nbins
      	bin(i)=0.0d0
      	berr(i)=0.0d0
      	dnbin(i)=0.0d0
50    continue

c inverse of period for later use
	iperiod = 1.0/period

	stepdef = period/real(nbins)
              
      do 100 i=1,npts
c modification to allow for time bins bigger than bin size for folding
        test=2.0*delx(i)*iperiod
        ntry=test*nbins+1
        step=0.0
        if(ntry.gt.1)step=stepdef
        start=x(i)-delx(i)
        do 80 j=1,ntry
          time=(j-1)*step+start
          time=(time-epoch)*iperiod
          inp=int(time)
          time=time-inp
          if(time.lt.0.0)time=time+1.0
          index=int(time*real(nbins))+1
          timed=1.0+time*nbins
c check for fractional bin values at ends
c          if(ntry.eq.1)goto 79
          if(ntry.eq.1)then
             bin(index)=bin(index)+dble(y(i))
             dnbin(index)=dnbin(index)+1.0d0
	     goto 80
	  end if

          if(j.eq.1)then
            fractn=timed-index
          else if(j.eq.ntry)then
            fractn=1.-timed+index
          end if
                
79        bin(index)=bin(index)+dble(y(i)*fractn)
          dnbin(index)=dnbin(index)+fractn
80      continue
100   continue
      do 150 i=1,nbins
      if(dnbin(i).ne.0.0)bin(i)=bin(i)/dnbin(i)
150   continue
c standard deviation
      do 200 i=1,npts
        test=2.0*delx(i)*iperiod
        ntry=test*nbins+1
        step=0.0
        if(ntry.gt.1)step=stepdef
        start=x(i)-delx(i)
        do 190 j=1,ntry
          time=(j-1)*step+start
          time=(time-epoch)*iperiod
          inp=int(time)
          time=time-inp
          if(time.lt.0.0)time=time+1.0
          index=int(time*real(nbins))+1
	  tempv = y(i) - bin(index)
          berr(index)=berr(index)+(tempv*tempv)
190   continue
200   continue
      do 250 i=1,nbins
         if(dnbin(i).le.1.0)goto 250
         berr(i)=sqrt(berr(i)/((dnbin(i)-1.0d0)))
c standard error
         berr(i)=berr(i)/sqrt((dnbin(i)))
250   continue
              
c get chi square for constancy of folded curve
c only use bins with a least four points in them
c first weighted mean
      asum=0.0d0
      bsum=0.0d0
      do 260 i=1,nbins
       if(dnbin(i).ge.4.0.and.berr(i).ne.0.0)then
	 temp(i) = 1.0/(berr(i) * berr(i))
c         asum=asum+bin(i)/(berr(i)*berr(i))
c         bsum=bsum+1.0/(berr(i)*berr(i))
         asum = asum + bin(i)*temp(i)
         bsum = bsum + temp(i)
       end if
260   continue
      if(bsum.ne.0.0)ave=asum/bsum
c now chi**2 itself
      chisqr=0.0
      icount=-1
      do 270 i=1,nbins
       if(dnbin(i).ge.4.and.berr(i).ne.0.0)then
	 tempv = ave - bin(i)
         chisqr=chisqr+(tempv)*(tempv)*temp(i)
         icount=icount+1
       end if
270   continue
c reduce it
      if(icount.gt.0)chisqr=chisqr/real(icount)
                                                 
      if(io.eq.1)then
c overwrite x and y arrays
        do 300 i=1,nbins
          x(i)=(real(i)-0.5)/real(nbins)
          delx(i)=.5/real(nbins)
          y(i)=bin(i)
          if(dnbin(i).ge.1.0)then
           dely(i)=berr(i)
          else
           dely(i)=0.0
          end if
300     continue
        npts=nbins
      end if
            
      end
         
c find best period by folding data into bins
        subroutine pfind(x,y,xe,ye,nptsf,bp,nbins)
	include 'robcom'
        real x(*),y(*),xe(*),ye(*)
        call sprompt('Give start, stop, step and no. of bins')
        call getit(ainbuf,  1)
        call dcode(ainbuf,ain1,ainfix,aingrd,k)
	start = ain1(1)
	stop = ain1(2)
	fnbin = ain1(3)
        nbins=nint(fnbin)
        itimes=nint((stop-start)/step)
        period=start
        call bfold(x,y,xe,ye,nptsf,period,epoch,nbins,chisqr,0)
        bp=period
        cmax=chisqr
        do 100 i=1,itimes
        period=start+real(i)*step
        call bfold(x,y,xe,ye,nptsf,period,epoch,nbins,chisqr,0)
          if(chisqr.gt.cmax)then
            cmax=chisqr
            bp=period
          end if
100     continue
        write(tstring,*)'Largest value of chi**2= ',cmax
	call totext(tstring)
	call write8(tstring)
        write(tstring,*)'found at a period of ',bp
	call totext(tstring)
	call write8(tstring)
        end
           
c find best period by folding data into bins
c and plot the results
        subroutine pgram(x,y,xe,ye,nptsf,bp,nbins,
     +sxh2,sxl2,syh2,syl2,start,stop,step,fnbin, wk2, msiz)
	include 'robcom'
        real x(*),y(*),xe(*),ye(*)
	real wk2(*)
	logical sizeok


     
        nbins=nint(fnbin)


	if(nbins.le.1)then
		call xtext('ERROR:')
		write(tstring, *)'ONLY',nbins,' BIN(S) FOR FOLDING'
		call xtext(tstring)
		return
	end if
	if(start.ge.stop)then
		call xtext('ERROR: START VALUE > END VALUE')
		return
	end if

	write(ifil8, *)'Periodogram:'
	write(ifil8, *)'start =',start,'stop =',stop,'step =',step


        itimes=nint((stop-start)/step)
	if(itimes.le.1)then
	   write(tstring, *)'WARNING: NO. OF PERIODS ONLY', itimes
	   call xtext(tstring)
	end if

	if(itimes.le.msiz)then
		sizeok = .TRUE.
	else
		sizeok = .FALSE.
	end if


        period=start
        call bfold(x,y,xe,ye,nptsf,period,epoch,nbins,chisqr,0)
        bp=period
        cmax=chisqr
        do 100 i=1,itimes
          period=start+real(i)*step
          call bfold(x,y,xe,ye,nptsf,period,epoch,nbins,chisqr,0)
	  if(sizeok)wk2(i) = chisqr
          if(chisqr.gt.cmax)then
            cmax=chisqr
            bp=period
          end if
100     continue
c repeat if loop size greater than array size and plot this time
	call limit(sxl2,syl2,sxh2,syh2,start,0.0,stop,cmax*1.2)
	period=start
	call bfold(x,y,xe,ye,nptsf,period,epoch,nbins,chisqr,0)
	call movxy(period,chisqr)
                                 
	do 200 i=1,itimes
          period=start+real(i)*step
	  if(sizeok)then
	    chisqr = wk2(i)
	  else
            call bfold(x,y,xe,ye,nptsf,period,epoch,nbins,chisqr,0)
	  end if
          call linxy(period,chisqr)
200     continue
        cpmax=1.2*cmax
	call boxm(start,0.0,stop,cpmax)
	tstring = '\\{chi}\\up2'
	call txangm(90.)
	call txtm(start-(stop-start)/15.,cpmax/2.,tstring,60)
	write(tstring,*)cmax
	call txtm(start-(stop-start)/20.,cmax,tstring,60)
	tstring='0'
	call txtm(start-(stop-start)/20.,0.0,tstring,60)
c draw line at chi**2=1 and label
        if(cpmax.gt.1.0)then
          call dline(start,1.0,stop,1.0,20.,1.0)
          tstring='1'
          call txtm(start-(stop-start)/20.,1.0,tstring,nnl(tstring))
        end if
              
        call txangm(0.)
        tstring='Trial period'
        call ctext(start+(stop-start)/2.,
     +		-cpmax/12.,tstring)
        write(tstring,*)stop
        call ctext(stop,-cpmax/15.,tstring)
        write(tstring,*)start
        call ctext(start,-cmax/15.,tstring)
        write(tstring,*)'Best period = ',bp
        call txtm(start,cpmax,tstring,nnl(tstring))
	write(tstring, *)'Step = ', step
	call style(stop, cpmax, tstring, nnl(tstring), 1)
	write(tstring, *)'Largest value of chisqr= ',cmax
	call xtext(tstring)
        call write8(tstring)
	write(tstring, *)'Found at a period of ',bp
	call xtext(tstring)
        call write8(tstring)
        end

c read hayshida format file
c and convert to simple time, data value format
      subroutine hfil(x,y,delx,dely,npts,msiz,title,xlab,ylab,
     +xfit,yfit,xfite,yfite,nptsf,imt,itmod)
	include 'robcom'
c imt is a flag which specifies whether the plot is x-ray data
c vs. time (imt=1) or x-ray data vs. x-ray data (imt=2)
      common/atri/ymean,yvar,stime
      common/echoc/echo
      logical echo
c set stime to integer start time in m.j.d. so it can be used
c in arithmetic routines
      real x(*),y(*),delx(*),dely(*),xfit(*),yfit(*),xfite(*),yfite(*)
      character*(*) title, xlab,ylab
      character*157 bigbuf
      double precision   xx(10),mmjd1,mmjd2,stime
      integer  l,i,sft,num,jdum1,jdum2,j
c the number of data values in the file per line
	integer numval
c arrays for read routine
      real ain(20)
      integer ifix(20),igrd(20)
      integer bs1x(10),bs2x(10)
      integer bs1y(10),bs2y(10)
                               
      common/timefl/bs1x,bs2x,bs1y,bs2y,isf,idb
c read timing file name
	call sprompt('Give name of Hayashida format file')
	write(tstring,*)'<default = ',title,'>'
	call sprompt(tstring)
        call getit(ainbuf,  1)
	if(ainbuf.eq.' ')ainbuf=title
      IF(ECHO)then
		write(tstring,*)'Reading Hayashida format file ',pfnam
		call totext(tstring)
	end if
      title=ainbuf
      open(unit=42,file=ainbuf, status = 'old', err=901)
                                                       
c read first line
      read(42,'(a)',end=888,err=902)bigbuf
c     print*,bigbuf
      call hcode(bigbuf,ain,ifix,igrd,k)
	numval = k - 8
            read(bigbuf,*,err=903)
     &        l,num,i,sft,mmjd1,mmjd2,jdum1,jdum2,(xx(j),j=1,numval)
      stime=int(mmjd1)
      if(imt.eq.1)then
        x(1)=real((mmjd2+mmjd1)/2.0d0-stime)
        delx(1)=real((mmjd2-mmjd1)/2.0d0)
      else if(imt.eq.2)then
        divx=0.0
        addx=0.0
        do 5 j=1,numval
          if(bs1x(j).eq.1)addx=addx+xx(j)
          if(bs2x(j).eq.1)divx=divx+xx(j)
5       continue
        x(1)=addx
        if(divx.ne.0.0)x(1)=x(1)/divx
      end if
            
      divy=0.0
      addy=0.0
      do 10 j=1,numval
        if(bs1y(j).eq.1)addy=addy+xx(j)
        if(bs2y(j).eq.1)divy=divy+xx(j)
10    continue
      y(1)=addy
      if(divy.ne.0.0)y(1)=y(1)/divy
c read errors
      read(42,'(a)',end=888)bigbuf
c      call hcode(bigbuf,ain,ifix,igrd,k)
            read(bigbuf,*)
     &        l,num,i,sft,mmjd1,mmjd2,jdum1,jdum2,(xx(j),j=1,numval)
      sigadd=0.0
      sigdiv=0.0
      do 11 j=1,numval
        if(bs1y(j).eq.1)sigadd=sigadd+xx(j)*xx(j)
        if(bs2y(j).eq.1)sigdiv=sigdiv+xx(j)*xx(j)
11    continue
      if(divy.ne.0.0)sigdiv=sigdiv/(divy*divy)
      err=sigadd/(addy*addy)+sigdiv
      dely(1)=abs(y(1)*sqrt(err))
c errors on x if x-ray data
      if(imt.eq.2)then
        sigadd=0.0
        sigdiv=0.0
        do 13 j=1,numval
          if(bs1x(j).eq.1)sigadd=sigadd+xx(j)*xx(j)
          if(bs2x(j).eq.1)sigdiv=sigdiv+xx(j)*xx(j)
13      continue
        if(divx.ne.0.0)sigdiv=sigdiv/(divx*divx)
        err=sigadd/(addx*addx)+sigdiv
        delx(1)=abs(x(1)*sqrt(err))
      end if
            
      ind=2
c read data
20    read(42,'(a)',end=888)bigbuf
            read(bigbuf,*)
     &        l,num,i,sft,mmjd1,mmjd2,jdum1,jdum2,(xx(j),j=1,numval)
      if(imt.eq.1)then
        x(ind)=real((mmjd2+mmjd1)/2.0d0-stime)
        delx(ind)=real((mmjd2-mmjd1)/2.0d0)
      else if(imt.eq.2)then
        divx=0.0
        addx=0.0
        do 15 j=1,numval
          if(bs1x(j).eq.1)addx=addx+xx(j)
          if(bs2x(j).eq.1)divx=divx+xx(j)
15      continue
                
                
        x(ind)=addx
        if(divx.ne.0.0)x(ind)=x(ind)/divx
      end if
            
      addy=0.0
      divy=0.0
      do 40 j=1,numval
        if(bs1y(j).eq.1)addy=addy+xx(j)
        if(bs2y(j).eq.1)divy=divy+xx(j)
40    continue
      y(ind)=addy
      if(divy.ne.0.0)y(ind)=y(ind)/divy
c read errors
      read(42,'(a)',end=888)bigbuf
c      call hcode(bigbuf,ain,ifix,igrd,k)
            read(bigbuf,*)
     &        l,num,i,sft,mmjd1,mmjd2,jdum1,jdum2,(xx(j),j=1,numval)
c error on y
      sigadd=0.0
      sigdiv=0.0
      do 41 j=1,numval
        if(bs1y(j).eq.1)sigadd=sigadd+xx(j)*xx(j)
        if(bs2y(j).eq.1)sigdiv=sigdiv+xx(j)*xx(j)
41    continue
      if(divy.ne.0.0)sigdiv=sigdiv/(divy*divy)
      err=sigadd/(addy*addy)+sigdiv
      dely(ind)=abs(y(ind)*sqrt(err))
c errors on x if needed
      if(imt.eq.2)then
        sigadd=0.0
        sigdiv=0.0
        do 43 j=1,numval
          if(bs1x(j).eq.1)sigadd=sigadd+xx(j)*xx(j)
          if(bs2x(j).eq.1)sigdiv=sigdiv+xx(j)*xx(j)
43      continue
        if(divx.ne.0.0)sigdiv=sigdiv/(divx*divx)
        err=sigadd/(addx*addx)+sigdiv
        delx(ind)=x(ind)*sqrt(err)
      end if
            
      ind=ind+1
c     print*,'ind=',ind
      goto 20
888   continue
      npts=ind-1
      if(echo)then
		write(tstring,*)npts,'points read by HFIL'
		call totext(tstring)
	end if
      close(unit=42)
c construct x and y labels
      call maklab(xlab,ylab,numval,imt)
      if(imt.eq.1)write(xlab,*)'M.J.D. (-',int(stime),')'
                                                         
      return
901   call xtext('ERROR OPENING HAYSHIDA FORMAT FILE')
      return
902   call xtext('ERROR READING DATA FROM HAYASHIDA FORMAT FILE')
      return
903   call xtext('ERROR DECODING DATA FROM HAYASHIDA FORMAT FILE')
      end
         
c construct label for timing file or hayshida file plot
      subroutine maklab(xlab,ylab,nc,imt)
	include 'robcom'
      common/timefl/bs1x,bs2x,bs1y,bs2y,isf,idb
      integer bs1x(10),bs2x(10)
      integer bs1y(10),bs2y(10)
      character*(*) xlab,ylab
c construct y label
c parts that are added
      ylab='Energy Bands'
      inc=13
      ylab(inc+1:inc+1)='('
      do 5 i=1,nc
        if(bs1y(i).eq.1)then
         write(ylab(2+inc:2+inc),999)i
         inc=inc+1
         if(i.eq.10)inc=inc+1
         goto 6
        end if
5     continue
6     continue
      do 7 j=i+1,nc
        if(bs1y(j).eq.1)then
         write(ylab(2+inc:60),997)'+'
         write(ylab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
        if(bs1y(j).eq.-1)then
         write(ylab(2+inc:60),997)'-'
         write(ylab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
7     continue
      write(ylab(2+inc:60),997)')'
      inc=inc+1
c parts that are divided
      idiv=0
      do 15 i=1,nc
        if(bs2y(i).eq.1)then
         write(ylab(2+inc:60),998)'/(',i
         inc=inc+3
         if(i.eq.10)inc=inc+1
         idiv=1
         goto 16
        end if
15     continue
16     continue
      do 17 j=i+1,nc
        if(bs2y(j).eq.1)then
         write(ylab(2+inc:60),997)'+'
         write(ylab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
        if(bs2y(j).eq.-1)then
         write(ylab(2+inc:60),997)'-'
         write(ylab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
17     continue
               
997	format(a1)
998	format(a2,i1)
999	format(i1)
              
      if(idiv.eq.1)write(ylab(2+inc:60),997)')'
c if plot is not vs. x-ray data then we can go back to calling routine
      if(imt.ne.2)return
                        
c construct x label
c parts that are added
      xlab='Energy Bands'
      inc=13
      xlab(inc+1:inc+1)='('
      do 105 i=1,nc
        if(bs1x(i).eq.1)then
         write(xlab(2+inc:60),999)i
         inc=inc+1
         if(i.eq.10)inc=inc+1
         goto 106
        end if
105   continue
106   continue
      do 107 j=i+1,nc
        if(bs1x(j).eq.1)then
         write(xlab(2+inc:60),997)'+'
         write(xlab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
        if(bs1x(j).eq.-1)then
         write(xlab(2+inc:60),997)'-'
         write(xlab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
107   continue
      write(xlab(2+inc:60),997)')'
      inc=inc+1
c parts that are divided
      idiv=0
      do 115 i=1,nc
        if(bs2x(i).eq.1)then
         write(xlab(2+inc:60),998)'/(',i
         inc=inc+3
         if(i.eq.10)inc=inc+1
         idiv=1
         goto 116
        end if
115    continue
116    continue
      do 117 j=i+1,nc
        if(bs2x(j).eq.1)then
         write(xlab(2+inc:60),997)'+'
         write(xlab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
        if(bs2x(j).eq.-1)then
         write(xlab(2+inc:60),997)'-'
         write(xlab(2+inc+1:60),999)j
         inc=inc+2
         if(j.eq.10)inc=inc+1
        end if
117    continue
      if(idiv.eq.1)write(xlab(2+inc:60),997)')'
      end
         
c cross correlation function with another input file
      subroutine ccf(xfit,yfit,nptsf,crud,
     +xt,yt,buff,pfnam)
	include 'robcom'
      real xfit(*),yfit(*)
      real xt(*),yt(*)
      character*(*) crud,buff,pfnam
	crud=buff
      open(unit=42,file=crud,status='old',err=900)
c assumed to have standard three lines of text at top of file
      read(42,'(a)',err=900)buff
      read(42,'(a)',err=900)buff
      read(42,'(a)',err=900)buff
      nptst=0
5     read(42,'(a)',end=50,err=900)ainbuf
      call dcode(ainbuf,ain1,ainfix,aingrd,k)
      nptst=nptst+1
      xt(nptst)=ain1(1)
      yt(nptst)=ain1(2)
      goto 5
50    continue
      close(unit=42)
c check size of data files the same
	if(nptsf.ne.nptst)then
		call xtext('ERROR: data files different sizes')
		return
	end if
      call pfit(yfit,yt,nptsf,xt,buff,crud,pfnam)
      return
900   call xtext('***I/O ERROR IN CCF***')
      end
         
c modified from the (woo/corbet) program "pulcor"
      subroutine pfit(yfit,yt,nptsf,y,buff,crud,pfnam)
	include 'robcom'
c centroid ccf using icen bins
      parameter (icen=3)
      real yfit(*),yt(*),y(*)
      character*(*) crud,pfnam,buff
                                       
                                       
      double precision vsum
      double precision ssum
      real sphase

c subtract mean from yfit and put into y
      vsum=0.0d0
      ssum=0.0d0
      do 10 i=1,nptsf
        vsum=vsum+yfit(i)
        ssum=ssum+yt(i)
10    continue
      vsum=vsum/dble(nptsf)
      ssum=ssum/dble(nptsf)
      do 12 i=1,nptsf
        y(i)=yfit(i)-vsum
        yt(i)=yt(i)-ssum
        yt(i+nptsf)=yt(i)
12    continue
              
              
      ssum=-1.0d+20
      imax=-1
      ccf=-99.99
      do 500 i=1,nptsf
      vsum=0.0
        do 400 j=1,nptsf
          vsum=vsum+y(j)*yt(i+j-1)
400     continue
        if(vsum.gt.ssum) then
          ssum=vsum
          imax=i
        endif
500   continue
c normalise cross correlation function
         anorm=0.0
         bnorm=0.0
         cnorm=0.0
      do 600 i=1,nptsf
         anorm=anorm+y(i)*y(i)
         bnorm=bnorm+yt(imax+i-1)*yt(imax+i-1)
         cnorm=cnorm+y(i)*yt(imax+i-1)
600   continue
      ccf=ssum/(sqrt(anorm)*sqrt(bnorm))
      write(tstring,*)'CCF maximum at shift of ',IMAX-1, 'bins'
	call totext(tstring)
        call write8(tstring)
        write(tstring,*)'CCF = ',CCF
	call totext(tstring)
        call write8(tstring)
c centroid cross correlation function
        sum1=0.0
        sum2=0.0
        vmin=ssum
c find  minimum ccf in this range
        do 550 i=imax-icen/2,imax+icen/2
        ind=i
        if(ind.gt.nptsf)ind=ind-nptsf
        if(ind.le.0)ind=nptsf+ind
        vsum=0.0
        do 560 j=1,nptsf
          vsum=vsum+y(j)*yt(ind+j-1)
560     continue
          if(vsum.lt.vmin)vmin=vsum
550     continue
c now centroid
        do 570 i=imax-icen/2,imax+icen/2
        ind=i
        if(ind.gt.nptsf)ind=ind-nptsf
        if(ind.le.0)ind=nptsf+ind
        vsum=0.0
        do 580 j=1,nptsf
          vsum=vsum+y(j)*yt(ind+j-1)
580     continue
        vsum=vsum-vmin
        phs=real(i-1)
        sum1=sum1+vsum*phs
        sum2=sum2+vsum
570     continue
        sphase=sum1/sum2
        if(sphase.gt.real(nptsf))sphase=sphase-real(nptsf)
        if(sphase.lt.0.0)sphase=sphase+real(nptsf)
	write(tstring, *)'After centroiding obtain phase= ',sphase
        call xtext(tstring)
	call write8(tstring)
        buff='N'
        call sprompt('Plot CCF? (Y/N)')
        call getit(buff,  1)
	call remcom(buff)
	call lctouc(buff)
        if(buff(1:1).eq.'Y')then
        call limit(20.,20.,90.,90.,0.0,-1.0,real(nptsf),1.0)
        call dline(0.0,0.0,real(nptsf),0.0,10.0,2.)
        call boxm(0.0,-1.0,real(nptsf),1.0)
        call txtm(0.0,1.0,pfnam,60)
        call txtm(0.0,real(nptsf)/2.,crud,60)
        call movxy(0.0,0.0)
      do 510 i=1,nptsf
      vsum=0.0
         anorm=0.0
         bnorm=0.0
         cnorm=0.0
      do 610 j=1,nptsf
         anorm=anorm+y(j)*y(j)
         bnorm=bnorm+yt(i+j-1)*yt(i+j-1)
         cnorm=cnorm+y(j)*yt(i+j-1)
610   continue
         ccf=cnorm/(sqrt(anorm)*sqrt(bnorm))
	 if(i .eq. 1)then
             call movxy(real(i) - 1., ccf)
	 else
             call linxy(real(i) - 1., ccf)
	 end if
510   continue
      end if
      end
         
         
c hcode is like dcode but reads larger buffer
      subroutine hcode(buff,a,ifix,igrd,k)
	include 'robcom'
      real a(*)
      integer ifix(*),igrd(*)
      character*(*) buff
      nb=157
c remove comments from buffer
c      call remcom(buff)
c find space, comma or left bracket
c after first non-blank, non-bracket, non-comma
      k=1
      i=0
      do 10 ind=1,nb
      i=i+1
      if(i.gt.nb)goto 11
	if(buff(i:i).ne.' ')then
         do 20 j=i+1,nb
	   if(buff(j:j).eq.' ')then
c get number
       call hnumb(buff,i,j-1,a,k)
c was there a bracket before the number to show it should be fixed?
         ifix(k)=0
         igrd(k)=0
              
       k=k+1
       i=j
       goto 10
       end if
20     continue
       end if
10     continue
11     continue
       k=k-1
       end
          
c hnumb is like numb but for hayshida files with no allowed
c key words etc.
       subroutine hnumb(buff,i1,i2,a,n)
       character*157 buff
       real a(*)
                
       read(buff(i1:i2),*, err = 20)a(n)
	return
20	call xtext('ERRROR: reading in HNUMB:')
	call xtext(buff)
       end
          
c fit polynomial to the data
c from bevington p.141
      subroutine polfit(x,y,sigmay,npts,nterms,mode,a,chisqr)
	include 'robcom'
      double precision sumx(19),sumy(10),xterm,yterm,array(10,10),chisq
      real x(*),y(*),sigmay(*),a(*)
	double precision determ
	double precision xi, yi, weight
c check whether a weighted fit was requested for a data set
c without errors
	if(mode.eq.1.and.sigmay(1).eq.0.0)then
		mode = 0
		call xtext('*Not* weighting by errors')
		call xtext('First error value zero')
	end if
       
       
	chisq = 0.0d0
              
c accumulate weighted sums
11    nmax=2*nterms-1
      do 13 n=1,nmax
13    sumx(n)=0.0d0
      do 15 j=1,nterms
15    sumy(j)=0.0d0
      chisq=0.0d0
21    do 50 i=1,npts
      xi=x(i)
      yi=y(i)
31    if(mode)32,37,39
32    if(yi)35,37,33
33    weight=1./yi
      goto 41
35    weight=1./(-yi)
      goto 41
37    weight=1.
      goto 41
39    weight=1./(sigmay(i)*sigmay(i))
41    xterm=weight
      do 44 n=1,nmax
      sumx(n)=sumx(n)+xterm
44    xterm=xterm*xi
45    yterm=weight*yi
      do 48 n=1,nterms
      sumy(n)=sumy(n)+yterm
48    yterm=yterm*xi
49    chisq=chisq+weight*yi*yi
50    continue
c construct matrices and calculate coefficients
51    do 54 j=1,nterms
      do 54 k=1,nterms
      n=j+k-1
54    array(j,k)=sumx(n)
      delta=determ(array,nterms)
      if(delta)61,57,61
57    chisq=0.0
      do 59 j=1,nterms
59    a(j)=0.0
	call xtext('WARNING')
	call xtext('Polfit failed. Maybe too high an order?')
      goto 80
61    do 70 l=1,nterms
62    do 66 j=1,nterms
      do 65 k=1,nterms
      n=j+k-1
65    array(j,k)=sumx(n)
66    array(j,l)=sumy(j)
70    a(l)=determ(array,nterms)/delta
c calculate chi square
71    do 75 j=1,nterms
      chisq=chisq-2.*a(j)*sumy(j)
      do 75 k=1,nterms
      n=j+k-1
75    chisq=chisq+a(j)*a(k)*sumx(n)
76    free=npts-nterms
77    chisqr=chisq/free
80    continue
      end
         
      double precision function determ(array,norder)
c calculate the determinant of a square matrix
c from bevington p.294
      double precision array(10,10),save
10    determ=1.0d0
11    do 50 k=1,norder
c interchange columns if diagonal element is zero
      if(array(k,k))41,21,41
21    do 23 j=k,norder
      if(array(k,j))31,23,31
23    continue
      determ=0.0d0
      goto 60
31    do 34 i=k,norder
      save=array(i,j)
      array(i,j)=array(i,k)
34    array(i,k)=save
      determ=-determ
c subtract row k from lower rows to get diagonal matrix
41    determ=determ*array(k,k)
	if(k - norder) 43, 50, 50
43    k1=k+1
      do 46 i=k1,norder
      do 46 j=k1,norder
46    array(i,j)=array(i,j)-array(i,k)*array(k,j)/array(k,k)
50    continue
60    return
      end
         
         
	subroutine xtext(string)
	character*(*)	string
	call totext(string)
	end
    
	subroutine totext(string)
	character*(*) string
	character*125 ostr
	ostr = string
c add NULL to help C
	ostr(nnl(ostr)+1:nnl(ostr)+1) = char(0)
	call totext2(ostr)
	end

	subroutine sprompt(string)
	character*(*) string
	character*125 ostr
	ostr = string
c add NULL to help C
	ostr(nnl(ostr)+1:nnl(ostr)+1) = char(0)
	call sprompt2(ostr)
	end

c prepare name for sending to C routine
	subroutine aalias(useral1, noual)
	character*(*) useral1(*)
	character*125 ostr
c new menu
	call nalmen()
	do 10 i = 1, noual
		ostr = useral1(i)
c add NULL to help C
		ostr(nnl(ostr)+1:nnl(ostr)+1) = char(0)
		call aalias2(ostr)
10	continue
c attach to button
	call atalmen()
	end

c set data colour
	subroutine sdatcol(r, g, b)
	include 'robcom'
	real r, g, b
	rgbd(1) = nint(r)
	rgbd(2) = nint(g)
	rgbd(3) = nint(b)
	end


c set colour for everything
	subroutine scol(r, g, b)
	include 'robcom'
	real r, g, b
	rgb(1) = nint(r)
	rgb(2) = nint(g)
	rgb(3) = nint(b)
	rgbd(1) = rgb(1)
	rgbd(2) = rgb(2)
	rgbd(3) = rgb(3)
	call farkc(rgb(1), rgb(2), rgb(3))
	end

c let external C routine know what the colours are
	subroutine colget(colour, value)
	include 'robcom'
	integer colour, value

	if(colour.ge.1.and.colour.le.3)then
		value = rgb(colour)
	else if(colour.ge.4.and.colour.le.6)then
		value = rgbd(colour-3)
	else
		write(tstring, *)'ERROR: colour in colget =',colour
		call xtext(tstring)
	end if

	end 


c output an array one element at a time- some compilers seem to have
c trouble outputting a large amount of data to an internal file
	subroutine oarray(array, n)
	real array(*)
	include 'robcom'
	do 10 i = 1, n
		write(tstring, *)array(i)
		call xtext(tstring)
10	continue
	end
