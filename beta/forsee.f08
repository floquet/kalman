!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                                                                                           *
!     *      PROGRAM FORSEE - A FILTERING AND PREDICTION CODE FOR DISCRETE TIME-HISTORY DATA                                      *
!     *                                                                                                                           *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

program foresee

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options
    use mKalmanData,                    only : KalmanData
    use mInputOutput,                   only : get_data
    use mTimeStamp,                     only : timestamp

    implicit none

    ! rank 0
    real ( rp ) :: cpu_time_start = zero, cpu_time_stop = zero, cpu_time_elapsed = zero

    type ( KalmanData ) :: dataHAA

        call cpu_time ( cpu_time_start )

            write ( *, "( /, 'Running FORESEE ...' )" )

            call get_data ( myData = dataHAA )
            call dataHAA % analyze_data ( )

        call cpu_time ( cpu_time_stop  )
        cpu_time_elapsed = cpu_time_stop - cpu_time_start

        write ( stdout, * )
        write ( stdout, fmt_generic ) 'cpu seconds: ', cpu_time_elapsed
        write ( stdout, fmt_generic ) 'timestamp: ', timestamp ( )

        write ( *, '( /, "Fortran compiler version: ", g0 )' )       compiler_version ()
        write ( *, '( /, "Fortran compilation options: ", g0, / )' ) compiler_options ()

    stop '#  #  # successful completion for program foresee . . .'

end program foresee

    ! Hendrik D. Carleton
    ! His 41-year career as a geophysicist included ten years with the Cities Service Oil Company (CITGO).
    ! He was involved in the oil industry's conversion of seismic exploration crews from the single-coverage to the (computerized)
    ! common-depth-point method, and field operations over a wide variety of land and water prospects. His final CITGO assignment
    ! was as the Pacific Region Staff Geophysicist. With the U.S. Army, his major contributions were in the areas of improving
    ! barrier munitions, designing digital filters to support protective structures research, and reorganizing the topographic
    ! battalions of the U. S. Army, Europe. He is retired in the grade of Colonel, A.U.S.


! dantopa@Muntz-Szasz.local:beta $ date
! Mon Dec 12 15:02:13 CST 2016
! dantopa@Muntz-Szasz.local:beta $ pwd
! /Users/dantopa/Documents/GitHub_desktop/kalman/beta
! dantopa@Muntz-Szasz.local:beta $ make
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_io_handles.o mod_io_handles.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_kalman_data.o mod_kalman_data.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_input_output.o mod_input_output.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o foresee.o foresee.f08
! gfortran -g -o foresee foresee.o mod_file_handling.o mod_input_output.o mod_io_handles.o mod_kalman_data.o mod_set_precision.o
! dantopa@Muntz-Szasz.local:beta $ ./foresee haa
!
! Running FORESEE ...
! Reading data for HAWAIAN AIRLINES - 1998 DAILY CLOSES.
!  q, r =    1.0040000000000000       0.20499999999999999
!  LengthFilter, LengthPrediction =            4           1
!  baseline, TestFactor =    0.0000000000000000        9.9999999999999995E-008
!  data point ( 1 ) =    3.6875000000000000
!  last data point (                   224  ) =    3.2500000000000000
!
! Program received signal SIGSEGV: Segmentation fault - invalid memory reference.
!
! Backtrace for this error:
! #0  0x10e3f7715
! #1  0x10e3f6efa
! #2  0x7fffdcd8ebb9
! #3  0x10e3e8ead
! #4  0x10e3e41b4
! #5  0x10e3e8ef5
! Segmentation fault: 11
