!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
submodule ( mKalmanData ) smKalmanDataWrite

    use mConstants,                     only : fmt_generic

contains
    ! get_data_sub
    ! read_file_type_inp_sub
    ! open_data_set_sub

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

            write ( unit = io_write, fmt = fmt_generic ) ''
            write ( unit = io_write, fmt = fmt_generic ) 'Echo print: '
            write ( unit = io_write, fmt = fmt_generic ) 'THE VALUE OF Q IS:        ', me % q
            write ( unit = io_write, fmt = fmt_generic ) 'THE VALUE OF R IS:        ', me % r
            write ( unit = io_write, fmt = fmt_generic ) 'THE FILTER LENGTH IS:     ', me % LengthFilter
            write ( unit = io_write, fmt = fmt_generic ) 'THE PREDICTION LENGTH IS: ', me % LengthPrediction
            write ( unit = io_write, fmt = fmt_generic ) 'THE BASELINE VALUE IS:    ', me % baseline
            write ( unit = io_write, fmt = fmt_generic ) 'THE TEST FACTOR VALUE IS: ', me % TestFactor

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

    end subroutine first_and_last_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

!     *****************************************************************
!     *                                                               *
!     *      SUBROUTINE OUTPUT - TO PRINT RESULTS TO THE OUTPUT FILE  *
!     *                                                               *
!     *****************************************************************

    subroutine output_sub ( me, io_write, myIO ) ! [175]

        class ( KalmanData ), target :: me

        ! local variables
        integer,            intent ( in ) :: io_write
        type (io_handles ), intent ( in ) :: myIO

            me % error_x = me % true_x - me % pred_x

            write ( unit = stdout,   fmt = 100 ) me % q, me % q, me % LengthFilter, me % baseline, me % TestFactor
            write ( unit = io_write, fmt = 100 ) me % q, me % q, me % LengthFilter, me % baseline, me % TestFactor

            write ( unit = myIO % t ) me % true_x
            write ( unit = myIO % p ) me % pred_x
            write ( unit = myIO % e ) me % error_x

        100 format ( 2X, I5, 2X, E12.5, 2X, E12.5, 2X, E12.5, 2X, E12.5, 2X, E12.5 )

    end subroutine output_sub

end submodule smKalmanDataWrite
