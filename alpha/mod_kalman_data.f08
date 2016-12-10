!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mKalmanData

    use, intrinsic :: iso_fortran_env,  only : iostat_end
    use mFileHandling,                  only : stdout, find_IU_info
    use mIOHandles,                     only : io_handles
    use mSetPrecision,                  only : ip, rp

    implicit none

    ! parameters
    integer, parameter :: imp1 = 50, imp2 = 50

    ! variables
    integer :: io_handle = stdout, io_status, record, k

    character ( len = 256 ) :: io_msg

    type :: KalmanData
        ! rank 2
        real ( rp ), allocatable :: pcm_p ( : , : )
        ! rank 1
        real ( rp ), allocatable :: dv_x ( : )
        ! rank 0
        real ( rp ) :: q, r, baseline, TestFactor
        real ( rp ) :: x
        integer ( ip ) :: LengthFilter, LengthPrediction
        integer ( ip ) :: numDataPoints
        character ( len = 128 ) :: title
    contains
        private
        procedure, public :: read_file_type_inp  =>  read_file_type_inp_sub
    end type KalmanData

    private :: read_file_type_inp_sub, allocator_sub

contains

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine read_file_type_inp_sub ( me, myIO )

        class ( KalmanData ), target :: me

        ! slot arguments
        type ( io_handles ), intent ( in ) :: myIO

        ! local variables
        character ( len = 128 ) :: LineNumber

            ! read data from the *.inp file
            read ( unit = myIO % inp, fmt = '( A )', iostat = io_status, iomsg = io_msg  ) me % title
                if ( io_status /= 0 ) then
                write ( io_handle, 100 ) 'READ', io_status, trim ( io_msg )
                call find_IU_info ( myIO % inp )
                stop '! ! !  Fatal error'
            end if

            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % q, me % r

            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % LengthFilter, me % LengthPrediction

            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % baseline, me % TestFactor

            print *, 'q, r = ', me % q, me % r
            print *, 'LengthFilter, LengthPrediction = ', me % LengthFilter, me % LengthPrediction
            print *, 'baseline, TestFactor = ', me % baseline, me % TestFactor

            ! measure the length of the data, allocate memory, then read the data
            !read ( myIO % inp, fmt = *, position = LineNumber, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            !read ( unit = myIO % inp, rec = record, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( unit = myIO % inp, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            !print *, 'we are now at line number ', record, '.'

            ! count data points
            me % numDataPoints = 0 ! damn trailing blank line in data file
            count_data_points : do
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
                if ( is_iostat_end ( io_status ) ) exit count_data_points
                me % numDataPoints = me % numDataPoints + 1
            end do count_data_points

            call allocator_sub ( me )

            ! read data points
            rewind ( myIO % inp )
            advance_pointer : do k = 1, 8
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
            end do advance_pointer
            read_data_points : do k = 1, me % numDataPoints - 1
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg ) me % dv_x ( k )
            end do read_data_points

            print *, 'data point ( 1 ) = ', me % dv_x ( 1 )
            print *, 'last data point ( ', me % numDataPoints,' ) = ', me % dv_x ( me % numDataPoints )

        return

        100 format ( 'I/O failure during ', A, /, 'iostatus = ', g0, /, 'iomsg = ', g0, '.', / )
        200 format ( F20.9 )

    end subroutine read_file_type_inp_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine allocator_sub ( me )

        class ( KalmanData ), target :: me

        integer :: stat
        character ( len = 512 ) :: errmsg

            ! rank 2
            allocate ( me % pcm_p ( 1 : me % numDataPoints, 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) ''
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % pcm_p ( : , : )= 0.0_rp ! populate

            ! rank 1
            allocate ( me % dv_x ( 1 : me % numDataPoints ), stat = stat, errmsg = errmsg )
            if ( stat /= 0 ) then
                write ( io_handle, 100 ) ''
                write ( io_handle, 110 ) me % numDataPoints
                write ( io_handle, 120 ) trim ( errmsg )
                write ( io_handle, 130 ) stat
                flush ( io_handle )
                stop '!  !  !  fatal program error during allocation'
            end if
            me % dv_x ( : ) = 0.0_rp ! populate

        100 format ( 'Mortal error during ', g0, 'allocation...' )  ! allocation or deallocation
        110 format ( 'requested size is ', g0, ' elements' )
        120 format ( 'errmsg = ', g0, '.' )
        130 format ( 'stat = ', g0 )

    end subroutine allocator_sub

end module mKalmanData
