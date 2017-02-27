!     *****************************************************************
!     *                                                               *
!     *      PROGRAM FORSEE - A FILTERING AND PREDICTION CODE FOR     *
!     *                       DISCRETE TIME-HISTORY DATA              *
!     *                                                               *
!     *****************************************************************
program forsee

    use mCharacterPositions,    only : dcharp
    use mKalman,                only : INITAL, KALMAN
    use mIO,                    only : OUTPUT, READIN1, READIN2

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (IMP1=50)
    COMMON/FILTER/ PCM_P ( IMP1, IMP1 ), & ! rank 2
                   GV_K ( IMP1 ), FV_F ( IMP1 ), DV_X ( IMP1 ), & ! rank 1
                   QCOEFF, RCOEFF, PRED_X, TRUE_X, IFILEN, IPREDP, BASEL, QA, QB, QC, RA, RB, RC, ICOUNT,TFACTR ! rank 0
    CHARACTER FNAME*30
    DATA IIN,IOUT,IPLT1,IPLT2,IPLT3/1,2,8,9,10/

        !  ASSIGN FILENAMES AND READ INITIAL INPUT DATA

        WRITE(*,*)'INPUT FILENAME IS? (w/o .inp extension)'
        READ(*,'(A30)') FNAME
        CALL DCHARP(FNAME,30,K1,K2)

        OPEN ( IIN,   FILE = FNAME(K1:K2) // '.INP' )
        OPEN ( IOUT,  FILE = FNAME(K1:K2) // '.OUT', STATUS = 'UNKNOWN')
        OPEN ( IPLT1, FILE = FNAME(K1:K2) // '.T',   STATUS = 'UNKNOWN')
        OPEN ( IPLT2, FILE = FNAME(K1:K2) // '.P',   STATUS = 'UNKNOWN')
        OPEN ( IPLT3, FILE = FNAME(K1:K2) // '.E',   STATUS = 'UNKNOWN')

        CALL READIN1 (IIN,IOUT,IPLT1,IPLT2,IPLT3)

        ! INITIALIZE VARIABLES

        CALL INITAL(TEST1)

        ! READ NEXT DATA POINT, COMPUTE PREDICTION, AND PRINT OUTPUT!
!1      CALL READIN2 (IIN,IOUT,IPLT1,IPLT2,IPLT3)
        do k = 1, 5
            CALL READIN2 (IIN,IOUT,IPLT1,IPLT2,IPLT3)
            write ( *, * ) 'calling Kalman with TEST1 = ', TEST1
            CALL KALMAN(TEST1)
            CALL OUTPUT(IOUT,IPLT1,IPLT2,IPLT3)
        end do
!        GOTO 1

    stop

END program forsee
