module mKalman

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
          PCM_P(I,J) = 0.0D0
        ENDDO
      ENDDO

      DO I=1,IFILEN,1
        FV_F(I)=0.0D0
        GV_K(I)=0.D0
        PCM_P(I,I)=1.0D0
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
    COMMON/FILTER/ PCM_P ( IMP1, IMP1 ), GV_K ( IMP1 ), FV_F ( IMP1 ), DV_X ( IMP1 ), &
                   QCOEFF, RCOEFF, PRED_X, TRUE_X, IFILEN, BASEL, QA, QB, QC, RA, RB, RC, ICOUNT,TFACTR
      REAL TSCALR, TMAT(IMP1,IMP1)

!     UPDATE THE PREDICTED COVARIANCE MATRIX (1st UPDATE)

      DO I=1,IFILEN,1
        PCM_P(I,I) = PCM_P(I,I) + QCOEFF
      ENDDO

!     UPDATE THE GAIN VECTOR

      TSCALR = 0.0
      TEST0 = 0.0D0
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
        GV_K(I) = 0.0D0
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

      PRED_X = 0.0D0
      DO I=1,IFILEN,1
        PRED_X = PRED_X + FV_F(I)*DV_X(I)
      ENDDO

      DO I=1,IFILEN,1
        FV_F(I) =  FV_F(I) + GV_K(I)*(TRUE_X-PRED_X)
      ENDDO

      RETURN
      END SUBROUTINE KALMAN

end module mKalman
