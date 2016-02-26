      SUBROUTINE LBFINI
C set demo=0 for working version and set demo=1 for demo version
C                                               Levitin 15.11.90
$define demo=0
C
$if (demo .eq. 1)
        CHARACTER*8    demoname
        COMMON/DEMO/   demoname
$endif
$if (demo .eq. 0)
        CALL Compil
$endif
$if (demo .eq. 1)
        OPEN (1,FILE='demo.dat')
        READ (1,'(A8)') demoname
        CLOSE (1)
$endif
        CALL Locbif
$if (demo .eq. 0)
        CALL Compte
$endif
      STOP
      END
