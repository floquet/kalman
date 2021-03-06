module mIO

contains

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE OUTPUT - TO PRINT RESULTS TO THE OUTPUT FILE  *
!     *                                                               *
!     *****************************************************************
    SUBROUTINE OUTPUT(IOUT,IPLT1,IPLT2,IPLT3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
    COMMON/FILTER/ PCM_P ( IMP1, IMP1 ), & ! rank 2
                   GV_K ( IMP1 ), FV_F ( IMP1 ), DV_X ( IMP1 ), & ! rank 1
                   QCOEFF, RCOEFF, PRED_X, TRUE_X, IFILEN, IPREDP, BASEL, QA, QB, QC, RA, RB, RC, ICOUNT, TFACTR ! rank 0
      ERROR = TRUE_X-PRED_X

      WRITE(IOUT,10) ICOUNT, TRUE_X, PRED_X, ERROR, QCOEFF, RCOEFF
      WRITE(*,10)    ICOUNT, TRUE_X, PRED_X, ERROR, QCOEFF, RCOEFF
      WRITE(IPLT1,10)ICOUNT, TRUE_X
      WRITE(IPLT2,10)ICOUNT, PRED_X
      WRITE(IPLT3,10)ICOUNT, ERROR
 10   FORMAT(2X,I5,2X,E12.5,2X,E12.5,2X,E12.5,2X,E12.5,2X,E12.5)

  !999 RETURN
      END SUBROUTINE OUTPUT

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE READIN - TO READ AND ECHO-PRINT INPUT DATA    *
!     *                                                               *
!     *****************************************************************

    SUBROUTINE READIN1(IIN,IOUT,IPLT1,IPLT2,IPLT3)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    PARAMETER (IMP1=50,IP=50)
    COMMON/FILTER/ PCM_P ( IMP1, IMP1 ), & ! rank 2
                   GV_K ( IMP1 ), FV_F ( IMP1 ), DV_X ( IMP1 ), & ! rank 1
                   QCOEFF, RCOEFF, PRED_X, TRUE_X, IFILEN, IPREDP, BASEL, QA, QB, QC, RA, RB, RC, ICOUNT, TFACTR ! rank 0
    DIMENSION BUFFER(IP)
    CHARACTER TITLE*80
    SAVE

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

        write ( *, * ) 'IPREDP = ', IPREDP
        IF(IPREDP.GT.1)THEN
            DO I=1,IPREDP-1,1
                BUFFER(I) = BASEL
            ENDDO
        END IF

        TRUE_X = BASEL
        ICOUNT=0
        WRITE(IOUT,*)'     I      TRUE_X        PRED_X        ERROR            Q             R   '
        WRITE(*,*)   '     I      TRUE_X        PRED_X        ERROR            Q             R   '

    END SUBROUTINE READIN1

    SUBROUTINE READIN2(IIN,IOUT,IPLT1,IPLT2,IPLT3)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    PARAMETER (IMP1=50,IP=50)
    COMMON/FILTER/ PCM_P ( IMP1, IMP1 ), & ! rank 2
                   GV_K ( IMP1 ), FV_F ( IMP1 ), DV_X ( IMP1 ), & ! rank 1
                   QCOEFF, RCOEFF, PRED_X, TRUE_X, IFILEN, IPREDP, BASEL, QA, QB, QC, RA, RB, RC, ICOUNT, TFACTR ! rank 0
    DIMENSION BUFFER(IP)
    SAVE

        DO I=1,IFILEN-1,1
            DV_X(I)=DV_X(I+1)
        ENDDO

        !write ( *, * ) 'pretest IPREDP = ', IPREDP
        IF(IPREDP.EQ.1)THEN
          DV_X(IFILEN)=TRUE_X
        ELSE
        !write ( *, * ) 'IPREDP is now = ', IPREDP
          DV_X(IFILEN)=BUFFER(1)
          DO I=1,IPREDP-2,1
              BUFFER(I)=BUFFER(I+1)
          ENDDO
          BUFFER(IPREDP-1)=TRUE_X
        END IF

        READ(IIN,*,END=999,ERR=999)TRUE_X
        write ( *, * ) 'TRUE_X = ', TRUE_X
        ICOUNT=ICOUNT+1
        write ( *, * ) 'ICOUNT = ', ICOUNT

        return
    999 STOP

    END SUBROUTINE READIN2

end module mIO
