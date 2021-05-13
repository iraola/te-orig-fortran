C               Tennessee Eastman Process Control Test Problem
C
C                    James J. Downs and Ernest F. Vogel
C
C                  Process and Control Systems Engineering
C                        Tennessee Eastman Company
C                              P.O. Box 511
C                          Kingsport, TN  37662
C
C
C  Reference:
C    "A Plant-Wide Industrial Process Control Problem"
C    Presented at the AIChE 1990 Annual Meeting
C    Industrial Challenge Problems in Process Control, Paper #24a
C    Chicago, Illinois, November 14, 1990
C
C    "A Plant-Wide Industrial Process Control Problem"
C    Computers and Chemical Engineering, Vol. 17, No. 3, pp. 245-255
C    (1993).
C
C
C  Main program for demonstrating application of the Tennessee Eastman
C  Process Control Test Problem
C
C
C=============================================================================
C
C
C  MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   DISTURBANCE VECTOR COMMON BLOCK
C
      INTEGER IDV
      COMMON/DVEC/ IDV(20)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD, DELTAT
      COMMON/CTRL/ SETPT, GAIN, TAUI, ERROLD, DELTAT
C
C  Local Variables
C
      INTEGER I, NN, NPTS, TEST4
C
      DOUBLE PRECISION TIME, YY(50), YP(50)
C
C  Set the number of differential equations (states).  The process has 50
C  states.  If the user wishes to integrate additional states, NN must be
C  increased by the number of additional differential equations.
C
      NN = 50
C
C  Set the number of points to simulate
C
      NPTS = 14400
C
C  Integrator Step Size:  1 Second Converted to Hours
C
      DELTAT = 1. / 3600.
C
C  Initialize Process
C  (Sets TIME to zero)
C
      CALL TEINIT(NN,TIME,YY,YP)
C
C  Set Controller Parameters
C  Make a Stripper Level Set Point Change of +15%
C
      SETPT = XMEAS(15) + 15.0
      GAIN = 2.0
      TAUI = 5.0
      ERROLD = 0.0
C
C  Initialize manipulated variables
C
      XMV(1) = 63.053 + 0.
      XMV(2) = 53.980 + 0.
      XMV(3) = 24.644 + 0.
      XMV(4) = 61.302 + 0.
      XMV(5) = 22.210 + 0.
      XMV(6) = 40.064 + 0.
      XMV(7) = 38.100 + 0.
      XMV(8) = 46.534 + 0.
      XMV(9) = 47.446 + 0.
      XMV(10)= 41.106 + 0.
      XMV(11)= 18.114 + 0.
C
C  Set all Disturbance Flags to OFF
C
      DO 100 I = 1, 20
          IDV(I) = 0
 100  CONTINUE
C
      OPEN(UNIT=32,FILE='TE_data_inc.dat',STATUS='old')
      OPEN(UNIT=12,FILE='TE_data_mv1.dat',STATUS='old')
      OPEN(UNIT=13,FILE='TE_data_mv2.dat',STATUS='old')
      OPEN(UNIT=14,FILE='TE_data_mv3.dat',STATUS='old')
      OPEN(UNIT=21,FILE='TE_data_me01.dat',STATUS='old')
      OPEN(UNIT=22,FILE='TE_data_me02.dat',STATUS='old')
      OPEN(UNIT=23,FILE='TE_data_me03.dat',STATUS='old')
      OPEN(UNIT=24,FILE='TE_data_me04.dat',STATUS='old')
      OPEN(UNIT=25,FILE='TE_data_me05.dat',STATUS='old')
      OPEN(UNIT=26,FILE='TE_data_me06.dat',STATUS='old')
      OPEN(UNIT=27,FILE='TE_data_me07.dat',STATUS='old')
      OPEN(UNIT=28,FILE='TE_data_me08.dat',STATUS='old')
      OPEN(UNIT=29,FILE='TE_data_me09.dat',STATUS='old')
      OPEN(UNIT=30,FILE='TE_data_me10.dat',STATUS='old')
      OPEN(UNIT=31,FILE='TE_data_me11.dat',STATUS='old')
C  Simulation Loop
C
      DO 1000 I = 1, NPTS
C
C          CALL CONTRL
C
        TEST4=MOD(I,180)
        IF (TEST4.EQ.0) THEN
          CALL OUTPUT
          WRITE(32,111) I
 111  	  FORMAT(1X,I6)
        ENDIF
C
        CALL INTGTR(NN,TIME,DELTAT,YY,YP)
C
 1000 CONTINUE
C
      PRINT *, 'Simulation is done. '
C
      CLOSE(UNIT=32)
      CLOSE(UNIT=12)
      CLOSE(UNIT=13)
      CLOSE(UNIT=14)
      CLOSE(UNIT=21)
      CLOSE(UNIT=22)
      CLOSE(UNIT=23)
      CLOSE(UNIT=24)
      CLOSE(UNIT=25)
      CLOSE(UNIT=26)
      CLOSE(UNIT=27)
      CLOSE(UNIT=28)
      CLOSE(UNIT=29)
      CLOSE(UNIT=30)
      CLOSE(UNIT=31)
      STOP
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD, DELTAT
      COMMON/CTRL/ SETPT, GAIN, TAUI, ERROLD, DELTAT
C
      DOUBLE PRECISION ERR, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR = SETPT - XMEAS(15)
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN * ( ( ERR - ERROLD ) + ERR * DELTAT * 60. / TAUI )
C
      XMV(8) = XMV(8) - DXMV
C
      ERROLD = ERR
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE OUTPUT
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
      WRITE(12,100) XMV(1), XMV(2), XMV(3), XMV(4)
    	WRITE(13,100) XMV(5), XMV(6), XMV(7), XMV(8)
    	WRITE(14,100) XMV(9), XMV(10), XMV(11), XMV(12)
    	WRITE(21,100) XMEAS(1), XMEAS(2), XMEAS(3), XMEAS(4)
    	WRITE(22,100) XMEAS(5), XMEAS(6), XMEAS(7), XMEAS(8)
    	WRITE(23,100) XMEAS(9), XMEAS(10), XMEAS(11), XMEAS(12)
     	WRITE(24,100) XMEAS(13), XMEAS(14), XMEAS(15), XMEAS(16)
    	WRITE(25,100) XMEAS(17), XMEAS(18), XMEAS(19), XMEAS(20)
    	WRITE(26,100) XMEAS(21), XMEAS(22), XMEAS(23), XMEAS(24)
    	WRITE(27,100) XMEAS(25), XMEAS(26), XMEAS(27), XMEAS(28)
    	WRITE(28,100) XMEAS(29), XMEAS(30), XMEAS(31), XMEAS(32)
    	WRITE(29,100) XMEAS(33), XMEAS(34), XMEAS(35), XMEAS(36)
    	WRITE(30,100) XMEAS(37), XMEAS(38), XMEAS(39), XMEAS(40)
    	WRITE(31,300) XMEAS(41)
 100  FORMAT(1X,E13.5,2X,E13.5,2X,E13.5,2X,E13.5)
 200  FORMAT(1X,E13.5,2X,E13.5,2X,E13.5)
 300  FORMAT(1X,E13.5)
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE INTGTR(NN,TIME,DELTAT,YY,YP)
C
C  Euler Integration Algorithm
C
C
      INTEGER I, NN
C
      DOUBLE PRECISION TIME, DELTAT, YY(NN), YP(NN)
C
      CALL TEFUNC(NN,TIME,YY,YP)
C
      TIME = TIME + DELTAT
C
      DO 100 I = 1, NN
C
          YY(I) = YY(I) + YP(I) * DELTAT
C
 100  CONTINUE
C
      RETURN
      END
