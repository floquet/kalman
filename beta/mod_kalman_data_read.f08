!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
submodule ( mKalmanData ) smKalmanDataRead

    use, intrinsic :: iso_fortran_env,  only : iostat_end

    use mFileHandling,                  only : find_IU_info
    use mIOHandles,                     only : io_handles

    integer :: io_status
    character ( len = 256 ) :: io_msg

contains

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

            call me % allocator_sub ( me )

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

end submodule mKalmanDataRead
