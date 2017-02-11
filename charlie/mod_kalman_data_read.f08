!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
submodule ( mKalmanData ) smKalmanDataRead

    use, intrinsic :: iso_fortran_env,  only : iostat_end

    use mFileHandling,                  only : safeopen_readonly, find_IU_info

    integer :: io_status
    character ( len = 256 )          :: io_msg
    character ( len = * ), parameter :: PathData = '../input_data/'

contains
    ! get_data_sub
    ! read_file_type_inp_sub
    ! open_data_set_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine get_data_sub ( me )

        class ( KalmanData ), target :: me

        ! local variables
        type ( io_handles ) :: myIOHandles

            call open_data_set_sub      ( myIO = myIOHandles )
            call read_file_type_inp_sub ( me, myIO = myIOHandles )
            call echo_data_sub          ( me, io_write = stdout )
            call first_and_last_sub     ( me, io_write = stdout )

    end subroutine get_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine read_file_type_inp_sub ( me, myIO )

        class ( KalmanData ), target :: me

        ! slot arguments
        type ( io_handles ), intent ( in ) :: myIO

            ! read data from the *.inp file
            read ( unit = myIO % inp, fmt = '( A )', iostat = io_status, iomsg = io_msg  ) me % title
            if ( io_status /= 0 ) then
                write ( io_handle, 100 ) 'READ', io_status, trim ( io_msg )
                call find_IU_info ( myIO % inp )
                stop error_fatal
            end if

            write ( unit = io_handle, fmt = '( "Reading data for ", g0, "." )' ) trim ( me % title )

            read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % q, me % r

            read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % LengthFilter, me % LengthPrediction

            read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) ! skip comment line
            read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg  ) me % baseline, me % TestFactor

            ! measure the length of the data, allocate memory, then read the data
            read ( unit = myIO % inp, iostat = io_status, iomsg = io_msg  ) ! skip comment line

            ! count data points
            me % numDataPoints = -1 ! damn trailing blank line in data file
            count_data_points : do
                read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
                if ( is_iostat_end ( io_status ) ) exit count_data_points
                me % numDataPoints = me % numDataPoints + 1
            end do count_data_points

            call allocator_sub ( me )

            ! read data points
            rewind ( myIO % inp )
            advance_pointer : do k_numDataPoints = 1, 8
                read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg )
            end do advance_pointer

            read_data_points : do k_numDataPoints = 1, me % numDataPoints
                read ( unit = myIO % inp, fmt = *, iostat = io_status, iomsg = io_msg ) me % dv_x ( k_numDataPoints )
            end do read_data_points

        return

        100 format ( 'I/O failure during ', A, /, 'iostatus = ', g0, /, 'iomsg = ', g0, '.', / )

    end subroutine read_file_type_inp_sub

    !   @   @   @   @  @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine open_data_set_sub ( myIO )  ! open file handle to inout and output files

        ! slot arguments
        type ( io_handles ), intent ( out ) :: myIO

        ! local variables
        integer :: FileNameLen
        character ( len = 128 ) :: FileNameStem
        character ( len = 256 ) :: FullName

            call harvest_command_line_sub ( FileNameStem = FileNameStem, LenCommandArgument  = FileNameLen )
            FullName = PathData // FileNameStem ( 1 : FileNameLen )

            myIO % inp = safeopen_readonly ( trim ( FullName ) // '.inp' )
            myIO % out = safeopen_readonly ( trim ( FullName ) // '.out' )
            myIO % t   = safeopen_readonly ( trim ( FullName ) // '.t' )
            myIO % p   = safeopen_readonly ( trim ( FullName ) // '.p' )
            myIO % e   = safeopen_readonly ( trim ( FullName ) // '.e' )

    end subroutine open_data_set_sub

end submodule smKalmanDataRead
