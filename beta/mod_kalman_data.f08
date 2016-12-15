!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mKalmanData

    use, intrinsic :: iso_fortran_env,  only : iostat_end
    use mFileHandling,                  only : stdout, find_IU_info
    use mIOHandles,                     only : io_handles
    use mSetPrecision,                  only : ip, rp

    implicit none

    ! parameters
    real ( rp ) :: zero = 0.0_rp, one = 1.0_rp
    integer, parameter :: imp1 = 50, imp2 = 50

    ! variables
    integer        :: io_handle = stdout, io_status, record!, k
    integer ( ip ) :: k_numDataPoints

    ! pointers
    real ( rp ), pointer :: pcmp_flattened ( : ), pcmp_diagonal ( : )

    character ( len = 256 ) :: io_msg

    type :: KalmanData
        ! rank 2
        real ( rp ), allocatable :: pcm_p ( : , : ), tmat ( : , : )
        ! rank 1
        real ( rp ), allocatable :: dv_x ( : ), gv_k ( : ), fv_f ( : )
        ! rank 0
        real ( rp ) :: q, r, baseline, TestFactor
        real ( rp ) :: t_scalar, test0, test1, rLengthFilter

        integer        :: LengthFilter, LengthPrediction
        integer ( ip ) :: numDataPoints

        character ( len = 128 ) :: title
    contains
        private
        procedure, public :: read_file_type_inp  =>  read_file_type_inp_sub
        procedure, public :: analyze_data        =>  analyze_data_sub
    end type KalmanData

    private :: read_file_type_inp_sub, allocator_sub, set_interval_sub, initialize_data_sub

contains

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine analyze_data_sub ( me )

        class ( KalmanData ), target :: me

            !write ( unit = io_handle, fmt = '( "inside analyze_data_sub ", g0, "." )' ) trim ( me % title )
            call initialize_data_sub ( me )
            pcmp_diagonal ( : ) = pcmp_diagonal ( : ) + me % q  !  UPDATE THE PREDICTED COVARIANCE MATRIX (1st UPDATE) [110]
            me % t_scalar = dot_product ( me % dv_x, matmul ( me % pcm_p, me % dv_x ) )  ! UPDATE THE GAIN VECTOR  [113]  x*Ax
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
            me % gv_k = matmul ( me % pcm_p, me % dv_x ) / me % t_scalar

            print *, 'ping'
            me % tmat = dot_product ( me % gv_k, me % dv_x ) ! UPDATE THE PREDICTED COVARIANCE MATRIX (2nd UPDATE)  [140]
            print *, 'pong'
            !PCM_P(I,J) =  PCM_P(I,J) - PCM_P(I,K)*TMAT(K,J)

    end subroutine analyze_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

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

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine kalman_filter_sub ( me )

        class ( KalmanData ), target :: me

            call set_interval_sub ( me )

    end subroutine kalman_filter_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine set_interval_sub ( me )

        class ( KalmanData ), target :: me

            ! LengthFilter \in [ 5, imp1 ]
            me % LengthFilter =  min ( me % LengthFilter, imp1 ) ! enforce upper bound
            me % LengthFilter =  max ( me % LengthFilter, 5 )    ! enforce lower bound
            me % rLengthFilter = real ( me % LengthFilter, rp )  ! saves on keyboqrding

            ! LengthPrediction \in [ 1, imp2 ]
            me % LengthPrediction =  min ( me % LengthPrediction, imp2 ) ! enforce upper bound
            me % LengthPrediction =  max ( me % LengthPrediction, 1 )    ! enforce lower bound

            ! TestFactor \in [ 1.01, 10 ** 10 ]
            me % TestFactor =  min ( me % TestFactor, 10.0_rp ** 10 ) ! enforce upper bound
            me % TestFactor =  max ( me % TestFactor, 1.01_rp )       ! enforce lower bound

                ! mod_kalman_data.f08:96:54:
                !              me % TestFactor =  min ( me % TestFactor, 10_rp ** 10 ) ! enforce upper bound
                ! Error: 'a2' argument of 'min' intrinsic at (1) must be REAL(8)

    end subroutine set_interval_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine read_file_type_inp_sub ( me, myIO )

        class ( KalmanData ), target :: me

        ! slot arguments
        type ( io_handles ), intent ( in ) :: myIO

        ! local variables

            ! read data from the *.inp file
            read ( unit = myIO % inp, fmt = '( A )', iostat = io_status, iomsg = io_msg  ) me % title
            if ( io_status /= 0 ) then
                write ( io_handle, 100 ) 'READ', io_status, trim ( io_msg )
                call find_IU_info ( myIO % inp )
                stop '! ! !  Fatal error'
            end if

            write ( unit = io_handle, fmt = '( "Reading data for ", g0, "." )' ) trim ( me % title )

            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % q, me % r

            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % LengthFilter, me % LengthPrediction

            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % baseline, me % TestFactor

            ! measure the length of the data, allocate memory, then read the data
            read ( unit = myIO % inp, iostat = io_status, iomsg = io_msg  ) ! skip comment line

            ! count data points
            me % numDataPoints = -1 ! damn trailing blank line in data file
            count_data_points : do
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
                if ( is_iostat_end ( io_status ) ) exit count_data_points
                me % numDataPoints = me % numDataPoints + 1
            end do count_data_points

            call allocator_sub ( me )

            ! read data points
            rewind ( myIO % inp )
            advance_pointer : do k_numDataPoints = 1, 8
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
            end do advance_pointer

            read_data_points : do k_numDataPoints = 1, me % numDataPoints
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg ) me % dv_x ( k_numDataPoints )
                !write ( * , '( g0, ": ", g0 )' ) k_numDataPoints, me % dv_x ( k_numDataPoints )
            end do read_data_points

            print *, 'data point ( 1 ) = ', me % dv_x ( 1 )
            print *, 'last data point ( ', me % numDataPoints,' ) = ', me % dv_x ( me % numDataPoints )

        return

        100 format ( 'I/O failure during ', A, /, 'iostatus = ', g0, /, 'iomsg = ', g0, '.', / )

    end subroutine read_file_type_inp_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine allocator_sub ( me )

        class ( KalmanData ), target :: me

        integer :: stat
        character ( len = 512 ) :: errmsg

            ! rank 2
            allocate ( me % pcm_p ( 1 : me % numDataPoints, 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) '', 'pcm_p'
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % pcm_p ( : , : ) = zero ! populate

            allocate ( me % pcm_p ( 1 : me % numDataPoints, 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) '', 'pcm_p'
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % pcm_p ( : , : ) = zero ! populate

            ! pointer to diagonal elements
            ! Metcalf, Cohen, Reid, figure 20.5, p. 364
            pcmp_flattened ( 1 : me % numDataPoints * me % numDataPoints ) => me % pcm_p ( : , : )
            pcmp_diagonal  => pcmp_flattened ( :: me % numDataPoints + 1 )

            ! rank 1
            allocate ( me % dv_x ( 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) '', 'dv_x'
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % dv_x ( : ) = zero ! populate

            allocate ( me % gv_k ( 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) '', 'gv_k'
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % gv_k ( : ) = zero ! populate

            allocate ( me % fv_f ( 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) '', 'fv_f'
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % fv_f ( : ) = zero ! populate

        100 format ( 'Mortal error during ', g0, 'allocation of ', g0, '...' )  ! allocation or deallocation
        110 format ( 'requested size is ', g0, ' elements' )
        120 format ( 'errmsg = ', g0, '.' )
        130 format ( 'stat = ', g0 )

    end subroutine allocator_sub

end module mKalmanData
