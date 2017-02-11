!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                                                                                           *
!     *      PROGRAM FORSEE - A FILTERING AND PREDICTION CODE FOR DISCRETE TIME-HISTORY DATA                                      *
!     *                                                                                                                           *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

program foresee

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options

    use mConstants,                     only : zero, stdout, fmt_generic
    use mKalmanData,                    only : KalmanData
    use mSetPrecision,                  only : rp
    use mTimeStamp,                     only : timestamp

    implicit none

    ! rank 0
    real ( rp ) :: cpu_time_start = zero, cpu_time_stop = zero, cpu_time_elapsed = zero

    type ( KalmanData ) :: dataHAA

        call cpu_time ( cpu_time_start )

            write ( *, "( /, 'Running FORESEE ...' )" )

            ! HAA Hawaiian Airlines
            call dataHAA % get_data ( )
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


! dantopa@Muntz-Szasz.local:charlie $ date
! Sat Feb 11 15:46:47 CST 2017

! dantopa@Muntz-Szasz.local:charlie $ pwd
! /Users/dantopa/Documents/GitHub_desktop/kalman/charlie

! dantopa@Muntz-Szasz.local:charlie $ make
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_constants.o mod_constants.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_command_line.o mod_command_line.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_io_handles.o mod_io_handles.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_kalman_data.o mod_kalman_data.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_kalman_data_allocate.o mod_kalman_data_allocate.f08
! mod_kalman_data_allocate.f08:2:23:
!
!  submodule ( mKalmanData ) smKalmanDataAllocate
!                        1
! Warning: USE statement at (1) has no ONLY qualifier [-Wuse-without-only]
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_kalman_data_read.o mod_kalman_data_read.f08
! mod_kalman_data_read.f08:2:23:
!
!  submodule ( mKalmanData ) smKalmanDataRead
!                        1
! Warning: USE statement at (1) has no ONLY qualifier [-Wuse-without-only]
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_time_stamp.o mod_time_stamp.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o forsee.o forsee.f08
! gfortran -g -o forsee forsee.o mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_set_precision.o mod_time_stamp.o

! dantopa@Muntz-Szasz.local:charlie $ ./forsee haa
!
! Running FORESEE ...
! Reading data for HAWAIAN AIRLINES - 1998 DAILY CLOSES.
! data point ( 1 ) = 3.6875000000000000
! last data point ( 224 ) = 3.2500000000000000
!
! cpu seconds: 0.49699999999999996E-002
! timestamp: 2017-02-11  15:47:07  UCT-0600
!
! Fortran compiler version: GCC version 7.0.1 20170205 (experimental)
!
! Fortran compilation options: -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.12.4 -mtune=core2 -auxbase-strip forsee.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal,invalid,zero -fbacktrace -fcheck=bounds -fmax-errors=5
!
! STOP #  #  # successful completion for program foresee . . .
