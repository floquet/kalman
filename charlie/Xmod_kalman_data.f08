!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mKalmanData

    use mCommandLine,                   only : harvest_command_line_sub
    use mConstants,                     only : one, zero, stdout, fmt_generic
    use mIOHandles,                     only : io_handles
    use mSetPrecision,                  only : ip, rp

    implicit none

    ! parameters
    integer ( ip ), parameter :: imp1 = 50, imp2 = 50
    character ( len = * ), parameter :: error_fatal = 'Fatal error in module "mKalmanData"...'

    ! variables
    integer                 :: io_handle = stdout, record
    integer ( ip ), private :: j, k ! iterators
    integer ( ip )          :: k_numDataPoints

    logical :: echo_print = .true.

    ! pointers
    real ( rp ), pointer :: pcmp_flattened ( : ), pcmp_diagonal ( : )

    type :: KalmanData
        ! rank 2
        real ( rp ), allocatable :: pcm_p ( : , : ), tmat ( : , : )
        ! rank 1
        real ( rp ), allocatable :: dv_x ( : ), gv_k ( : ), fv_f ( : ), buffer ( : )
        ! rank 0
        real ( rp ) :: q, r, baseline, TestFactor
        real ( rp ) :: t_scalar, test0, test1, rLengthFilter
        real ( rp ) :: pred_x, true_x, error_x

        integer ( ip ) :: LengthFilter, LengthPrediction
        integer ( ip ) :: numDataPoints
        integer ( ip ) :: index

        character ( len = 128 ) :: title

        type ( io_handles ) :: myIO

    contains
        private
        !procedure, public :: allocator       =>  allocator_sub
        procedure, public :: analyze_data    =>  analyze_data_sub
        !procedure, public :: initialize_data =>  initialize_data_sub
        !procedure, public :: get_all_data    =>  get_all_data_sub
        !procedure, public :: set_interval    =>  set_interval_sub
    end type KalmanData

    private :: allocator_sub
    private :: analyze_data_sub
    private :: initialize_data_sub
    private :: kalman_sub
    private :: get_all_data_sub
    private :: set_interval_sub

    interface

        module subroutine allocator_sub ( me )
            class ( KalmanData ), target :: me
        end subroutine allocator_sub

        module subroutine echo_data_sub ( me, io_write )
            class ( KalmanData ), target :: me
            integer, intent ( in ) :: io_write
        end subroutine echo_data_sub

        module subroutine first_and_last_sub ( me, io_write )
            class ( KalmanData ), target :: me
            integer, intent ( in ) :: io_write
        end subroutine first_and_last_sub

        module subroutine get_all_data_sub ( me )
            class ( KalmanData ), target :: me
        end subroutine get_all_data_sub

        module subroutine output_sub ( me ) ! [175]
            class ( KalmanData ), target :: me
        end subroutine output_sub

        module subroutine write_header_sub ( me )
            class ( KalmanData ), target :: me
        end subroutine write_header_sub

    end interface

contains
    ! analyze_data_sub
    ! initialize_data_sub
    ! set_interval_sub
    ! kalman_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine analyze_data_sub ( me )

        class ( KalmanData ), target :: me

            call get_all_data_sub    ( me )  ! [25]
            me % dv_x ( 1 : me % LengthFilter ) = me % baseline  ! [236]
            if ( me % LengthPrediction > 1 ) then  ! [239]
                do k = 1, me % LengthPrediction - 1
                    me % buffer ( k ) = me % baseline
                end do
            endif
            me % true_x = me % baseline  ! [245]
            call set_interval_sub    ( me )  ! [212]

            if ( echo_print ) call echo_data_sub      ( me, io_write = stdout )
            if ( echo_print ) call first_and_last_sub ( me, io_write = stdout )

            call write_header_sub    ( me )
            call initialize_data_sub ( me )  ! [29]

            do k = 1, 2 !me % numDataPoints  ! [260]
                me % index = me % index + 1
                me % dv_x ( me % LengthFilter ) = me % true_x  ! [261]

                if ( me % LengthPrediction > 1 ) then
                    me % dv_x ( me % LengthFilter ) = me % buffer ( 1 )
                    do j = 1, me % LengthPrediction - 2
                        me % buffer ( j ) = me % buffer ( j + 1 )  ! [265]
                    end do
                    me % buffer ( me % LengthPrediction - 1 ) = me % true_x  ! [267]
                end if

                call kalman_sub ( me )
                call output_sub ( me )
            end do

    end subroutine analyze_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    !     ***********************************************************************
    !     *                                                                     *
    !     *      SUBROUTINE analyze_data - FORTRAN IMPLEMENTATION OF THE        *
    !     *                                RECURSIVE KALMAN FILTER              *
    !     *                                                                     *
    !     ***********************************************************************

    subroutine kalman_sub ( me )

        class ( KalmanData ), target :: me

            write ( stdout, fmt_generic ) 'index  = ', me % index
            write ( stdout, fmt_generic ) 'true_x = ', me % true_x

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
            me % gv_k ( : ) = matmul ( me % pcm_p ( : , : ), me % dv_x ( : ) ) / me % t_scalar

            ! create a rank 1 matrix tmat
            me % tmat  ( : , : ) = dot_product ( me % gv_k, me % dv_x ( : ) ) ! UPDATE THE PREDICTED COVARIANCE MATRIX (2nd UPDATE)  [140]
            me % pcm_p ( : , : ) = me % pcm_p ( : , : ) - matmul ( me % pcm_p ( : , : ), me % tmat ( : , : ) )  ! PCM_P(I,J) =  PCM_P(I,J) - PCM_P(I,K)*TMAT(K,J) [151]

            ! update the filter vector
            me % pred_x = dot_product ( me % fv_f, me % dv_x )
            me % fv_f ( : ) = me % gv_k  ( : ) * ( me % true_x - me % pred_x ) ! [164]

    end subroutine kalman_sub

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
            me % fv_f ( : ) = zero  !  [83]
            me % gv_k ( : ) = zero  !  [84]
            !me % dv_x = me % baseline ! [236]
            !if ( me % LengthPrediction > 1 ) me % buffer ( : ) = me % baseline  ! [241]

            ! rank 0
            me % test1  = me % rLengthFilter  !  [87]
            !me % true_x = me % baseline  ! [245]

            me % index = 0_ip

    end subroutine initialize_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine set_interval_sub ( me )  ! [212]

        class ( KalmanData ), target :: me

            ! LengthFilter \in [ 5, imp1 ]
            me %  LengthFilter =  min ( me % LengthFilter, imp1 ) ! enforce upper bound
            me %  LengthFilter =  max ( me % LengthFilter, 5_ip ) ! enforce lower bound
            me % rLengthFilter = real ( me % LengthFilter, rp )   ! saves on keyboarding

            ! LengthPrediction \in [ 1, imp2 ]
            me % LengthPrediction =  min ( me % LengthPrediction, imp2 + 1_ip ) ! enforce upper bound
            me % LengthPrediction =  max ( me % LengthPrediction, 1_ip )  ! enforce lower bound

            ! TestFactor \in [ 1.01, 10 ** 10 ]
            me % TestFactor =  min ( me % TestFactor, 10.0_rp ** 10 ) ! enforce upper bound
            me % TestFactor =  max ( me % TestFactor, 1.01_rp )       ! enforce lower bound

                ! mod_kalman_data.f08:96:54:
                !              me % TestFactor =  min ( me % TestFactor, 10_rp ** 10 ) ! enforce upper bound
                ! Error: 'a2' argument of 'min' intrinsic at (1) must be REAL(8)

    end subroutine set_interval_sub

end module mKalmanData
