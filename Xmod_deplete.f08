module mDeplete
contains

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE INITAL - TO INITIALIZE VARIABLES              *
!     *                                                               *
!     *****************************************************************
      SUBROUTINE INITAL(TEST1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1), &
        QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC, &
        ICOUNT,TFACTR

      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          PCM_P(I,J) = 0.0
        ENDDO
      ENDDO

      DO I=1,IFILEN,1
        FV_F(I)=0.0
        GV_K(I)=0.0
        PCM_P(I,I)=1.0
      ENDDO
      TEST1=IFILEN

      RETURN
      END SUBROUTINE INITAL


!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE KALMAN - FORTRAN IMPLEMENTATION OF THE        *
!     *                          RECURSIVE KALMAN FILTER              *
!     *                                                               *
!     *****************************************************************
      SUBROUTINE KALMAN(TEST1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1), &
        QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC, &
        ICOUNT,TFACTR
      REAL TSCALR,TVECT(IMP1),TMAT(IMP1,IMP1)

!     UPDATE THE PREDICTED COVARIANCE MATRIX (1st UPDATE)

      DO I=1,IFILEN,1
        PCM_P(I,I) = PCM_P(I,I) + QCOEFF
      ENDDO

!     UPDATE THE GAIN VECTOR

      TSCALR = 0.0
      TEST0 = 0.0
      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          TSCALR = TSCALR + DV_X(I)*PCM_P(I,J)*DV_X(J)
          TEST0 = TEST0 + ABS(PCM_P(I,J))
        ENDDO
      ENDDO
      TEST1 = TEST1 + IFILEN*(QCOEFF)

!      WRITE(*,*)'ICOUNT,TSCALR,T0,T1',ICOUNT,TSCALR,TEST0,TEST1
      IF(TEST0.GT.(TFACTR*TEST1))THEN
        IF(QCOEFF.GT.1.E-10)QCOEFF=QCOEFF*(REAL(IFILEN)/TEST0)
        IF(RCOEFF.LT.1.E+10)RCOEFF=RCOEFF*(TEST0/REAL(IFILEN))
        TEST1=TEST0
      END IF

      TSCALR = TSCALR + RCOEFF
      DO I=1,IFILEN,1
        GV_K(I) = 0.0
        DO J=1,IFILEN,1
          GV_K(I) =  GV_K(I) + PCM_P(I,J)*DV_X(J)/TSCALR
        ENDDO
      ENDDO

!     UPDATE THE PREDICTED COVARIANCE MATRIX (2nd UPDATE)

      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          TMAT(I,J) =  GV_K(I)*DV_X(J)
        ENDDO
      ENDDO

      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          DO K=1,IFILEN,1
            PCM_P(I,J) =  PCM_P(I,J) - PCM_P(I,K)*TMAT(K,J)
          ENDDO
        ENDDO
      ENDDO

!     UPDATE THE FILTER VECTOR

      PRED_X = 0.0
      DO I=1,IFILEN,1
        PRED_X = PRED_X + FV_F(I)*DV_X(I)
      ENDDO

      DO I=1,IFILEN,1
        FV_F(I) =  FV_F(I) + GV_K(I)*(TRUE_X-PRED_X)
      ENDDO

      RETURN
      END SUBROUTINE KALMAN

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE OUTPUT - TO PRINT RESULTS TO THE OUTPUT FILE  *
!     *                                                               *
!     *****************************************************************
      SUBROUTINE OUTPUT(IOUT,IPLT1,IPLT2,IPLT3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1), &
        QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC, &
        ICOUNT,TFACTR
      ERROR = TRUE_X-PRED_X

      WRITE(IOUT,10)ICOUNT,TRUE_X,PRED_X,ERROR,QCOEFF,RCOEFF
      WRITE(*,10)ICOUNT,TRUE_X,PRED_X,ERROR,QCOEFF,RCOEFF
      WRITE(IPLT1,10)ICOUNT,TRUE_X
      WRITE(IPLT2,10)ICOUNT,PRED_X
      WRITE(IPLT3,10)ICOUNT,ERROR
 10   FORMAT(2X,I5,2X,E12.5,2X,E12.5,2X,E12.5,2X,E12.5,2X,E12.5)

  999 RETURN
      END SUBROUTINE OUTPUT

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE READIN - TO READ AND ECHO-PRINT INPUT DATA    *
!     *                                                               *
!     *****************************************************************
      SUBROUTINE READIN(IFLAG,IIN,IOUT,IPLT1,IPLT2,IPLT3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50,IP=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1), &
        QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC, &
        ICOUNT,TFACTR
      DIMENSION BUFFER(IP)
      CHARACTER TITLE*80
      SAVE

      IF(IFLAG.EQ.1)THEN       ! READ AND ECHO INITIAL INPUT DATA !
        READ(IIN,'(A80)') TITLE
        WRITE(IOUT,'(A80)') TITLE
        READ(IIN,*)QCOEFF,RCOEFF,IFILEN,IPREDP,BASEL,TFACTR
        IFILEN = MIN(IFILEN,IMP1)
        IFILEN = MAX(IFILEN,5)
        IPREDP = MIN(IPREDP,IP+1)
        IPREDP = MAX(IPREDP,1)
        TFACTR = MIN(TFACTR,1.E+10)
        TFACTR = MAX(TFACTR,1.01)
        WRITE(IOUT,1000)QCOEFF,RCOEFF,IFILEN,IPREDP,BASEL,TFACTR
 1000   FORMAT(/,5X,'THE VALUE OF Q IS:'   ,2X,E12.5,/,5X, &
                'THE VALUE OF R IS:'       ,2X,E12.5,/,5X, &
                'THE FILTER LENGTH IS:'    ,2X,I5,/,5X, &
                'THE PREDICTION LENGTH IS:',2X,I5,/,5X, &
                'THE BASELINE VALUE IS:   ',2X,E12.5,/,5X, &
                'THE TEST FACTOR VALUE IS:',2X,E12.5,//)

        DO I=1,IFILEN,1
        DV_X(I) = BASEL
        ENDDO

        IF(IPREDP.GT.1)THEN
        DO I=1,IPREDP-1,1
        BUFFER(I) = BASEL
        ENDDO
        END IF

        TRUE_X = BASEL
        ICOUNT=0
        WRITE(IOUT,*)'     I      TRUE_X        PRED_X        ERROR            Q             R   '
        WRITE(*,*)   '     I      TRUE_X        PRED_X        ERROR            Q             R   '

      ELSE IF(IFLAG.EQ.2)THEN   ! READ TIME HISTORY DATA !
        DO I=1,IFILEN-1,1
        DV_X(I)=DV_X(I+1)
        ENDDO

        IF(IPREDP.EQ.1)THEN
          DV_X(IFILEN)=TRUE_X
        ELSE
          DV_X(IFILEN)=BUFFER(1)
          DO I=1,IPREDP-2,1
          BUFFER(I)=BUFFER(I+1)
          ENDDO
          BUFFER(IPREDP-1)=TRUE_X
        END IF

        READ(IIN,*,END=999,ERR=999)TRUE_X
        ICOUNT=ICOUNT+1
      END IF

      RETURN

  999 STOP
      END SUBROUTINE READIN

end module mDeplete
