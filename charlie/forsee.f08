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


! dantopa@Riesz-Fischer:charlie $ date
! Sun Feb 12 22:46:00 CST 2017

! dantopa@Riesz-Fischer:charlie $ pwd
! /Users/dantopa/Documents/GitHub_desktop/kalman/charlie

! dantopa@Riesz-Fischer:charlie $ echo $gflags
! -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5

! dantopa@Riesz-Fischer:charlie $ make debug
! PROGRAM  = forsee
! PRG_OBJ  = forsee.o
! SRCS     = forsee.f08 mod_command_line.f08 mod_constants.f08 mod_file_handling.f08 mod_io_handles.f08 mod_kalman_data.f08 mod_kalman_data_allocate.f08 mod_kalman_data_read.f08 mod_kalman_data_write.f08 mod_set_precision.f08 mod_time_stamp.f08
! OBJS     = forsee.o mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_kalman_data_write.o mod_set_precision.o mod_time_stamp.o
! MODS     = mod_command_line.f08 mod_constants.f08 mod_file_handling.f08 mod_io_handles.f08 mod_kalman_data.f08 mod_kalman_data_allocate.f08 mod_kalman_data_read.f08 mod_kalman_data_write.f08 mod_set_precision.f08 mod_time_stamp.f08
! MOD_OBJS = mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_kalman_data_write.o mod_set_precision.o mod_time_stamp.o

! dantopa@Riesz-Fischer:charlie $ gcc --version
! Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
! Apple LLVM version 8.0.0 (clang-800.0.42.1)
! Target: x86_64-apple-darwin15.6.0
! Thread model: posix
! InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin

! dantopa@Riesz-Fischer:charlie $ gfortran --version
! GNU Fortran (GCC) 6.1.0
! Copyright (C) 2016 Free Software Foundation, Inc.
! This is free software; see the source for copying conditions.  There is NO
! warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!

! dantopa@Riesz-Fischer:charlie $ make
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_set_precision.o mod_set_precision.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_command_line.o mod_command_line.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_io_handles.o mod_io_handles.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data.o mod_kalman_data.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data_allocate.o mod_kalman_data_allocate.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data_read.o mod_kalman_data_read.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data_write.o mod_kalman_data_write.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_time_stamp.o mod_time_stamp.f08
! gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o forsee.o forsee.f08
! gfortran -g -o forsee forsee.o mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_kalman_data_write.o mod_set_precision.o mod_time_stamp.o

! dantopa@Riesz-Fischer:charlie $ ./forsee haa
!
! Running FORESEE ...
! Reading data for HAWAIAN AIRLINES - 1998 DAILY CLOSES.
!     THE VALUE OF Q IS:        1.0040000000000000
!     THE VALUE OF R IS:        0.20499999999999999
!     THE FILTER LENGTH IS:     4
!     THE PREDICTION LENGTH IS: 1
!     THE BASELINE VALUE IS:    0.0000000000000000
!     THE TEST FACTOR VALUE IS: 0.99999999999999995E-007
!
! Peek at data stream:
! data point ( 1 ) = 3.6875000000000000
! last data point ( 224 ) = 3.2500000000000000
!   32767   0.00000E+00   0.00000E+00   0.00000E+00   0.11183E-01   0.20500E+00
!
! cpu seconds: 0.16338999999999999E-001
! timestamp: 2017-02-12  22:46:46  UCT-0600
!
! Fortran compiler version: GCC version 6.1.0
!
! Fortran compilation options: -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -auxbase-strip forsee.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -ffpe-trap=denormal,invalid,zero -fbacktrace -fcheck=bounds -fmax-errors=5
!
! STOP #  #  # successful completion for program foresee . . .
