!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mKalmanData

    use mFileHandling,                  only : stdout, find_IU_info
    use mIOHandles,                     only : io_handles
    use mSetPrecision,                  only : ip, rp

    implicit none

    integer, parameter :: imp1 = 50, ip = 50
    integer :: io_handle = stdout, io_status

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

        return

        100 format ( 'I/O failure during ', A, /, 'iostatus = ', g0, /, 'iomsg = ', g0, '.', / )

    end subroutine read_file_type_inp_sub

end module mKalmanData
