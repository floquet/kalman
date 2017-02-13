!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
submodule ( mKalmanData ) smKalmanDataWrite

    use mConstants,                     only : fmt_generic

contains
    ! echo_data_sub
    ! first_and_last_sub
    ! output_sub
    ! write_header_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine write_header_sub ( me )

        class ( KalmanData ), target :: me

        character ( len = * ), parameter :: head = '      I      TRUE_X        PRED_X        ERROR', &
                                            tail = '             Q             R'

            write ( unit = me % myIO % out, fmt = fmt_generic ) me % title
            write ( unit = me % myIO % out, fmt = fmt_generic ) ''
            call echo_data_sub ( me, me % myIO % out )
            write ( unit = me % myIO % out, fmt = fmt_generic ) ''
            write ( unit = me % myIO % out, fmt = fmt_generic ) head, tail  ! [247]


    end subroutine write_header_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    !     ************************************************************
    !     *                                                          *
    !     *      SUBROUTINE echo_data_sub - ECHO-PRINT INPUT DATA    *
    !     *                                                          *
    !     ************************************************************

    ! ECHO INITIAL INPUT DATA
    subroutine echo_data_sub ( me, io_write )

        class ( KalmanData ), target :: me

        ! local variables
        integer, intent ( in ) :: io_write
        character ( len = * ), parameter :: tab = '    '

            write ( unit = io_write, fmt = fmt_generic ) tab, 'THE VALUE OF Q IS:        ', me % q  ! [218]
            write ( unit = io_write, fmt = fmt_generic ) tab, 'THE VALUE OF R IS:        ', me % r
            write ( unit = io_write, fmt = fmt_generic ) tab, 'THE FILTER LENGTH IS:     ', me % LengthFilter
            write ( unit = io_write, fmt = fmt_generic ) tab, 'THE PREDICTION LENGTH IS: ', me % LengthPrediction
            write ( unit = io_write, fmt = fmt_generic ) tab, 'THE BASELINE VALUE IS:    ', me % baseline
            write ( unit = io_write, fmt = fmt_generic ) tab, 'THE TEST FACTOR VALUE IS: ', me % TestFactor

    end subroutine echo_data_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine first_and_last_sub ( me, io_write ) ! print first and last entry in data

        class ( KalmanData ), target :: me

        ! local variables
        integer, intent ( in ) :: io_write

            write ( unit = io_write, fmt = fmt_generic ) ''
            write ( unit = io_write, fmt = fmt_generic ) 'Peek at data stream: '
            write ( unit = stdout,   fmt = fmt_generic ) 'data point ( 1 ) = ', me % dv_x ( 1 )
            write ( unit = stdout,   fmt = fmt_generic ) 'last data point ( ', me % numDataPoints,' ) = ', &
                                                          me % dv_x ( me % numDataPoints )
            write ( unit = io_write, fmt = fmt_generic ) ''

    end subroutine first_and_last_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE OUTPUT - TO PRINT RESULTS TO THE OUTPUT FILE  *
!     *                                                               *
!     *****************************************************************

    subroutine output_sub ( me ) ! [175]

        class ( KalmanData ), target :: me

            me % error_x = me % true_x - me % pred_x  ! [181]

            ! if ( echo_print ) then
            !     write ( unit = stdout,      fmt = 100 ) me % index, me % true_x, me % pred_x, me % error_x, me % q, me % r  ! [184]
            ! end if
            write ( unit = me % myIO % out, fmt = 100 ) me % index, me % true_x, me % pred_x, me % error_x, me % q, me % r  ! [183]
            write ( unit = me % myIO % t,   fmt = 100 ) me % index, me % true_x  ! [185]
            write ( unit = me % myIO % p,   fmt = 100 ) me % index, me % pred_x  ! [186]
            write ( unit = me % myIO % e,   fmt = 100 ) me % index, me % error_x ! [186]

        100 format ( 2X, I5, 2X, E12.5, 2X, E12.5, 2X, E12.5, 2X, E12.5, 2X, E12.5 )

    end subroutine output_sub

end submodule smKalmanDataWrite
