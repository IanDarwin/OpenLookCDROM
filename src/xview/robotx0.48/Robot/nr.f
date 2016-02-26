C Based on algorithms from the book on
C Numerical Recipes by Press et al.
c Most of these routines have been rewritten
c somewhat and are unlikely to work as the
c original routines did.
                                                    
                                                    
                                                    
C KENDALL'S NON-PARAMETRIC TEST OF CORRELATION
c based on Numerical Recipes P. 493
        subroutine kendl(xfit, yfit, n)
	include 'robcom'
        real xfit(*), yfit(*)

c square root of two
	parameter (srt = 1.4142136)

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return
        n1 = 0
        n2 = 0
        is = 0
        do 120 j = 1, n - 1
          do 110 k = j + 1, n
            aa1 = xfit(j) - xfit(k)
            aa2 = yfit(j) - yfit(k)
            a12 = aa1 * aa2
            if(a12 .ne. 0.0)then
              n1 = n1 + 1
              n2 = n2 + 1
              if(a12 .gt. 0.0)then
                is = is + 1
              else
                is = is - 1
              end if
            else
              if(aa1 .ne. 0.0) n1 = n1 + 1
              if(aa2 .ne. 0.0) n2 = n2 + 1
            end if
110        continue
120      continue
        t = float(is) / sqrt(float(n1) * float(n2))
        var = (4. * n + 10.) / (9. * n * (n - 1.))
        z = t / sqrt(var)
        prob = erfrc(abs(z)/srt)
        write(tstring,*)'Kendall tau statistic = ',t
	call totext(tstring)
        write(tstring,*)'Probability= ',prob
	call totext(tstring)
        write(ifil8,*)'Kendall tau statistic = ',t
        write(ifil8,*)'Probability= ',prob
        end
           
C ERROR FUNCTION
c based on Numerical Recipes P.164
        function erfrc(xin)
	include 'robcom'
	real xin
	parameter (HALF = 0.5)

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return

        y = abs(xin)
        t1 = 1. / (1. + HALF * y)
        erfrc = t1*exp(-y*y - 1.26551223 + t1 *
     +    (1.00002368 + t1 * (.37409196 +
     +    t1 * (.09678418 + t1 * (-.18628806 + t1 *
     +    (.27886807 + t1 * (-1.13520398 +
     +    t1 * (1.48851587 + t1 * (-.82215223 + t1 *.17087277)))))))))
        if(xin .lt. 0.0) erfrc = 2.0 - erfrc
        end

C MEDIAN OF DATA ARRAY
C based on Numerical Recipes P. 461
        subroutine mdian(x, n)
	include	'robcom'
        real x(*)
        parameter(HUGE = 1.0e30, afct = 1.5, amp = 1.5)
	parameter (HALF = 0.5)

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return

        a = HALF * (x(1) + x(n))
        esp = abs(x(n) - x(1))
        ap = HUGE
        am = -HUGE
10      sum = 0.0
        sumx = 0.0
        np = 0
        nm = 0
        xp = HUGE
        xm = -HUGE
        do 110 j = 1, n
          xx = x(j)
          if(xx .ne. a)then
            if(xx .gt. a)then
            np = np + 1
            if(xx .lt. xp)xp = xx
          else if(xx .lt. a)then
            nm = nm+1
            if(xx .gt. xm)xm = xx
          end if
          dum = 1. / (esp + abs(xx - a))
          sum = sum + dum
          sumx = sumx + xx * dum
        end if
110      continue
        if((np - nm) .ge. 2)then
          am = a
          aa = xp + max(0., sumx/sum-a) * amp
          if(aa .gt. ap)aa = HALF*(a+ap)
          esp = afct*abs(aa - a)
          a = aa
          goto 10
        else if((nm - np) .ge. 2)then
          ap = a
          aa = xm + min(0.,sumx/sum-a)*amp
          if(aa .lt. am)aa = HALF * (a + am)
          esp = afct * abs(aa - a)
          a = aa
          goto 10
        else
          if(mod(n,2).eq.0)then
            if(np.eq.nm)then
              xmed = HALF*(xp+xm)
            else if(np.gt.nm)then
              xmed = HALF*(a+xp)
            else
              xmed = HALF*(xm+a)
            end if
          else
            if(np.eq.nm)then
              xmed = a
            else if(np.gt.nm)then
              xmed = xp
            else
              xmed = xm
            end if
          end if
         end if
         write(tstring,*)'Median = ',xmed
	call totext(tstring)
         write(ifil8,*)'Median = ',xmed
         end
            
C GAUSSIAN RANDOM NUMBER GENERATOR
C based on  Numerical Recipes P. 203
      function gasd(idum)
	include 'robcom'
      data ist/0/

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return

      if(ist.eq.0)then
10      val1 = 2. * ran1(idum) - 1.
        val2 = 2. * ran1(idum) - 1.
        rad = val1 * val1 + val2 * val2
        if(rad .ge. 1.0)goto 10
        fact = sqrt(-2. * log(rad) / rad)
        gst = val1 * fact
        gasd = val2 * fact
        ist = 1
      else
        gasd = gst
        ist = 0
      end if
      end

c Numerical recipes one didn't work (dunno why)
	function ran1(idum)
	integer*4 idum
	ran1 = rann(idum)
	end
	
	
C RAN1: RETURNS UNIFORM RANDOM DEVIATE BETWEEN 0 AND 1
C based on Numerical Recipes P.196
      function rann(idum)
	integer idum
	include 'robcom'
      real xa(97)

      integer m1, m2, m3
      integer ia1, ia2, ia3
      real rm1, rm2

      parameter(m1 = 259200, ia1 = 7141, ik1 = 54773, rm1 = 1./m1)
      parameter(m2 = 134456, ia2 = 8121, ik2 = 28411, rm2 = 1./m2)
      parameter(m3 = 243000, ia3 = 4561, ik3 = 51349)

      data ifrc/0/

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return

      if(idum .lt. 0 .or. ifrc .eq. 0)then
        ifrc = 1
        jx1 = mod(ik1-idum,m1)
        jx1 = mod(ia1*jx1+ik1,m1)
        jx2 = mod(jx1,m2)
        jx1 = mod(ia1*jx1+ik1,m1)
        jx3 = mod(jx1,m3)
        do 110 j = 1,97
          jx1 = mod(ia1*jx1+ik1,m1)
          jx2 = mod(ia2*jx2+ik2,m2)
          xa(j) = (float(jx1) + float(jx2) * rm2) * rm1
110      continue
        idum = 1
      end if
      jx1 = mod(ia1 * jx1 + ik1,m1)
      jx2 = mod(ia2 * jx2 + ik2,m2)
      jx3 = mod(ia3 * jx3 + ik3,m3)
      j = 1 + (97 * jx3) / m3
      rann = xa(j)
      xa(j) = (float(jx1) + float(jx2) * rm2) * rm1
      end

C CUBIC SPLINE ROUTINE
c  based on Numerical Recipes P.88
      subroutine nrspln(x, y, n, yp1, ypn, yout, u)
	include	'robcom'
      real x(*), y(*), yout(*), u(*)
	parameter (HUGE = 0.99e+30)
	parameter (HALF = 0.5)

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return

      if(yp1 .gt. HUGE)then
        yout(1) = 0.0
        u(1) = 0.0
      else
        yout(1) = -HALF
        u(1) = (3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      end if
      do 110 i = 2, n - 1
        sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
        p = sig*yout(i-1) + 2.
        yout(i) = (sig-1.)/p
        u(i) = (6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))
     +/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
110    continue

      if(ypn .gt. HUGE)then
        pn = 0.0
        un = 0.0
      else
        pn = HALF
        un = (3.0/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      end if

      yout(n) = (un - pn * u(n - 1))/(pn * yout(n - 1) + 1.0)

      do 120 index = n - 1, 1, -1
        yout(index) = yout(index) * yout(index + 1) + u(index)
120    continue

      end
         
C INTERPOLATE RESULTS OF SPLINE FIT
c based on Numerical Recipes P.89
      subroutine splnt(xin, yin, yin2, n, x, y)
	include	'robcom'
      real xin(*), yin(*), yin2(*)
      integer lo, hi

c routines only to be used inside Robot
c you may NOT remove this line unless the rest of the subroutine is deleted too!
	if(.not.(robot)) return

      lo = 1
      hi = n

10    if((hi-lo) .gt. 1)then
        kt = (hi + lo) / 2
        if(xin(kt) .gt. x)then
          hi = kt
        else
          lo = kt
        end if
        goto 10
      end if

      h = xin(hi) - xin(lo)
      if(h .eq. 0.0)then
        call xtext('ERROR: BAD XA INPUT DETECTED IN S/R SPLINT')
        return
      end if

      a = (xin(hi) - x) / h
      b = (x - xin(lo)) / h
      y = a * yin(lo) + b * yin(hi) +
     +		((a * a * a - a) * yin2(lo) + 
     +		(b  *b * b - b) * yin2(hi)) * 
     +		(h * h)/6.0

      end
         
