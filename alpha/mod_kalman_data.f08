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
    integer :: io_handle = stdout, numDataPoints, io_status, record

    character ( len = 256 ) :: io_msg

    type :: KalmanData
        character ( len = 128 ) :: title
        real    ( rp ) :: q, r, baseline, TestFactor
        integer ( ip ) :: LengthFilter, LengthPrediction
    contains
        private
        procedure, public :: read_file_type_inp  =>  read_file_type_inp_sub
    end type KalmanData

    private :: read_file_type_inp_sub

contains

    subroutine read_file_type_inp_sub ( me, myIO )

        class ( KalmanData ), target :: me

        ! slot arguments
        type ( io_handles ), intent ( in ) :: myIO

        ! local variables
        character ( len = 128 ) :: LineNumber

            ! read data from the *.inp file
            read ( myIO % inp, fmt = '( A )', iostat = io_status, iomsg = io_msg  ) me % title
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
            read ( myIO % inp, rec = record, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            print *, 'we are now at line number ', record, '.'
            ! count data points
            numDataPoints = 1
            count_data_points : do
                read ( myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
                if ( is_iostat_end ( io_status ) ) exit count_data_points
                numDataPoints = numDataPoints + 1
            end do count_data_points

            print *, 'I counted ', numDataPoints, ' records'


        return

        100 format ( 'I/O failure during ', A, /, 'iostatus = ', g0, /, 'iomsg = ', g0, '.', / )

    end subroutine read_file_type_inp_sub

end module mKalmanData
