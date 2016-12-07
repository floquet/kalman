!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                                                                                           *
!     *      PROGRAM FORSEE - A FILTERING AND PREDICTION CODE FOR DISCRETE TIME-HISTORY DATA                                      *
!     *                                                                                                                           *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

program foresee

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_option
    use mInputOutput,                   only : start_the_show

    implicit none

        write ( *, "( /, 'Running FORESEE ...' )" )

        call start_the_show ( )

        write ( *, '( /, "Fortran compiler version: ", g0 )' ) compiler_version ()
        write ( *, '(    "Fortran compilation options: ", g0, / )' ) compiler_option ()

    stop '#  #  # successful completion for program foresee . . .'

end program foresee

    ! Hendrik D. Carleton
    ! His 41-year career as a geophysicist included ten years with the Cities Service Oil Company (CITGO).
    ! He was involved in the oil industry's conversion of seismic exploration crews from the single-coverage to the (computerized)
    ! common-depth-point method, and field operations over a wide variety of land and water prospects. His final CITGO assignment
    ! was as the Pacific Region Staff Geophysicist. With the U.S. Army, his major contributions were in the areas of improving
    ! barrier munitions, designing digital filters to support protective structures research, and reorganizing the topographic
    ! battalions of the U. S. Army, Europe. He is retired in the grade of Colonel, A.U.S.


! Wed Dec  7 14:41:21 CST 2016
! rditldmt@ITLDMT-MD-O2034:alpha $ pwd
! /Users/rditldmt/Documents/GitHub Desktop/kalman/alpha
! rditldmt@ITLDMT-MD-O2034:alpha $ make
! mpifort -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! mpifort -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_io_handles.o mod_io_handles.f08
! mpifort -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_input_output.o mod_input_output.f08
! mpifort -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o foresee.o foresee.f08
! mpifort -g -o foresee foresee.o mod_file_handling.o mod_input_output.o mod_io_handles.o
! rditldmt@ITLDMT-MD-O2034:alpha $ ./foresee haa
!
! Running FORESEE ...
! harvesting command line...
! The command line argument is haa.
! File descriptor unit    -10 is opened in mode READ at position ASIS.
! File descriptor unit    -10 is in the range of values allowed by the compiler.
! File descriptor unit    -10 is named ../input_data/haa.inp.
! File descriptor unit    -11 is opened in mode READ at position ASIS.
! File descriptor unit    -11 is in the range of values allowed by the compiler.
! File descriptor unit    -11 is named ../input_data/haa.out.
! File descriptor unit    -12 is opened in mode READ at position ASIS.
! File descriptor unit    -12 is in the range of values allowed by the compiler.
! File descriptor unit    -12 is named ../input_data/haa.t.
! File descriptor unit    -13 is opened in mode READ at position ASIS.
! File descriptor unit    -13 is in the range of values allowed by the compiler.
! File descriptor unit    -13 is named ../input_data/haa.p.
! File descriptor unit    -14 is opened in mode READ at position ASIS.
! File descriptor unit    -14 is in the range of values allowed by the compiler.
! File descriptor unit    -14 is named ../input_data/haa.e.
! STOP #  #  # successful completion for program foresee . . .

! rditldmt@ITLDMT-MD-O2034:alpha $ gfortran --version
! GNU Fortran (MacPorts gcc7 7-20161127_0) 7.0.0 20161127 (experimental)
! Copyright (C) 2016 Free Software Foundation, Inc.
! This is free software; see the source for copying conditions.  There is NO
! warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
