!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mKalmanData

    use mCommandLine,                   only : harvest_command_line_sub
    use mConstants,                     only : one, zero, stdout, fmt_generic
    use mIOHandles,                     only : io_handles
    use mSetPrecision,                  only : ip, rp

    implicit none

    ! parameters
    integer, parameter :: imp1 = 50, imp2 = 50
    character ( len = * ), parameter :: error_fatal = 'Fatal error in module "mKalmanData"...'

    ! variables
    integer        :: io_handle = stdout, record
    integer ( ip ) :: k_numDataPoints

    ! pointers
    real ( rp ), pointer :: pcmp_flattened ( : ), pcmp_diagonal ( : )

    type :: KalmanData
        ! rank 2
        real ( rp ), allocatable :: pcm_p ( : , : ), tmat ( : , : )
        ! rank 1
        real ( rp ), allocatable :: dv_x ( : ), gv_k ( : ), fv_f ( : )
        ! rank 0
        real ( rp ) :: q, r, baseline, TestFactor
        real ( rp ) :: t_scalar, test0, test1, rLengthFilter
        real ( rp ) :: pred_x, true_x

        integer        :: LengthFilter, LengthPrediction
        integer ( ip ) :: numDataPoints

        character ( len = 128 ) :: title
    contains
        private
        procedure, public :: allocator       =>  allocator_sub
        procedure, public :: analyze_data    =>  analyze_data_sub
        procedure, public :: initialize_data =>  initialize_data_sub
        procedure, public :: get_data        =>  get_data_sub
    end type KalmanData

    private :: allocator_sub
    private :: analyze_data_sub
    private :: initialize_data_sub
    private :: get_data_sub
    !private :: set_interval_sub

    interface

        module subroutine allocator_sub ( me )
            class ( KalmanData ), target :: me
        end subroutine allocator_sub

        module subroutine get_data_sub ( me )
            class ( KalmanData ), target :: me
        end subroutine get_data_sub

    end interface

contains
    ! analyze_data_sub
    ! initialize_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    !     ***********************************************************************
    !     *                                                                     *
    !     *      SUBROUTINE analyze_data - FORTRAN IMPLEMENTATION OF THE        *
    !     *                                RECURSIVE KALMAN FILTER              *
    !     *                                                                     *
    !     ***********************************************************************

    subroutine analyze_data_sub ( me )

        class ( KalmanData ), target :: me

            call initialize_data_sub ( me )

            pcmp_diagonal ( : ) = pcmp_diagonal ( : ) + me % q  !  UPDATE THE PREDICTED COVARIANCE MATRIX (1st UPDATE) [110]
            me % t_scalar = dot_product ( me % dv_x ( : ), matmul ( me % pcm_p ( : , : ), me % dv_x ( : ) ) )  ! UPDATE THE GAIN VECTOR  [113]  x*Ax
            me % test0 = sum ( sum ( abs ( me % pcm_p ( : , : ) ), 1 ) )  !  [120]
            me % test1 = me % test1 + me % q * me % rLengthFilter  !  [123]

            ! scaling operations on line [126]
            if ( me % test0 > me % TestFactor * me % test1 ) then
                if ( me % q > 10.0_rp ** (-10) ) me % q = me % q * me % rLengthFilter / me % test0
                if ( me % r > 10.0_rp ** (+10) ) me % r = me % r * me % test0         / me % rLengthFilter
                me % test1 = me % test0
            end if

            ! line [132]
            me % t_scalar = me % t_scalar + me % r
            me % gv_k ( : ) = matmul ( me % pcm_p  ( : , : ), me % dv_x ( : ) ) / me % t_scalar

            ! create a rank 0 matrix tmat
            me % tmat  ( : , : ) = dot_product ( me % gv_k, me % dv_x ( : ) ) ! UPDATE THE PREDICTED COVARIANCE MATRIX (2nd UPDATE)  [140]
            me % pcm_p ( : , : ) = me % pcm_p ( : , : ) - matmul ( me % pcm_p ( : , : ), me % tmat ( : , : ) )  ! PCM_P(I,J) =  PCM_P(I,J) - PCM_P(I,K)*TMAT(K,J) [151]

            ! update the filter vector
            me % pred_x = dot_product ( me % fv_f, me % dv_x )
            me % fv_f ( : ) = me % gv_k  ( : ) * ( me % true_x - me % pred_x ) ! [164]

    end subroutine analyze_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    !     **************************************************************************
    !     *                                                                        *
    !     *      SUBROUTINE initialize_data - TO INITIALIZE VARIABLES              *
    !     *                                                                        *
    !     **************************************************************************

    subroutine initialize_data_sub ( me )

        class ( KalmanData ), target :: me

            ! rank 2
            me % pcm_p ( : , : ) = zero ! needed? populated at allocation time [78]
            pcmp_diagonal ( : )  = one  ! [85]

            ! rank 1
            me % fv_f = zero  !  [83]
            me % gv_k = zero  !  [84]

            ! rank 0
            me % test1 = me % rLengthFilter  !  [87]

    end subroutine initialize_data_sub

end module mKalmanData
