# Find all source files, create a list of corresponding object files
PROGRAM = forsee

EXT = f08
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.$(EXT))
OBJS=$(patsubst %.$(EXT),%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.$(EXT))
MOD_OBJS=$(patsubst %.$(EXT),%.o,$(MODS))

# Compiler/Linker settings
# gfortran - gnu (execution: ./a.out) (flags: gflags in ~/.bashrc)
# mpif90 - gnu, MPI Mac Desktop (execution: mpirun -np 4 ./a.out)
# FC = mpifort
FC = gfortran
FLFLAGS = -g
FCFLAGS = -c ${gflags}
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .$(EXT) .o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
$(OBJS) : %.o : %.$(EXT)
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

debug:
	@echo "${bold}PROGRAM${normal}  = $(PROGRAM)"
	@echo "${bold}PRG_OBJ${normal}  = $(PRG_OBJ)"
	@echo "${bold}SRCS${normal}     = $(SRCS)"
	@echo "${bold}OBJS${normal}     = $(OBJS)"
	@echo "${bold}MODS${normal}     = $(MODS)"
	@echo "${bold}MOD_OBJS${normal} = $(MOD_OBJS)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod *.smod *.o *.dSYM

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# module interdependencies
mod_command_line.o        : mod_constants.o
mod_constants.o           : mod_set_precision.o
mod_file_handling.o       :
mod_io_handles.o          :
mod_command_line.o        : mod_constants.o
mod_kalman_data.o         : mod_command_line.o mod_constants.o mod_io_handles.o mod_set_precision.o mod_file_handling.o
mod_kalman_write.o        : mod_constants.o
mod_set_precision.o       :
mod_time_stamp.o          :

# dantopa@Riesz-Fischer:charlie $ date
# Sun Feb 12 22:46:00 CST 2017

# dantopa@Riesz-Fischer:charlie $ pwd
# /Users/dantopa/Documents/GitHub_desktop/kalman/charlie

# dantopa@Riesz-Fischer:charlie $ echo $gflags
# -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5

# dantopa@Riesz-Fischer:charlie $ make debug
# PROGRAM  = forsee
# PRG_OBJ  = forsee.o
# SRCS     = forsee.f08 mod_command_line.f08 mod_constants.f08 mod_file_handling.f08 mod_io_handles.f08 mod_kalman_data.f08 mod_kalman_data_allocate.f08 mod_kalman_data_read.f08 mod_kalman_data_write.f08 mod_set_precision.f08 mod_time_stamp.f08
# OBJS     = forsee.o mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_kalman_data_write.o mod_set_precision.o mod_time_stamp.o
# MODS     = mod_command_line.f08 mod_constants.f08 mod_file_handling.f08 mod_io_handles.f08 mod_kalman_data.f08 mod_kalman_data_allocate.f08 mod_kalman_data_read.f08 mod_kalman_data_write.f08 mod_set_precision.f08 mod_time_stamp.f08
# MOD_OBJS = mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_kalman_data_write.o mod_set_precision.o mod_time_stamp.o

# dantopa@Riesz-Fischer:charlie $ gcc --version
# Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
# Apple LLVM version 8.0.0 (clang-800.0.42.1)
# Target: x86_64-apple-darwin15.6.0
# Thread model: posix
# InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin

# dantopa@Riesz-Fischer:charlie $ gfortran --version
# GNU Fortran (GCC) 6.1.0
# Copyright (C) 2016 Free Software Foundation, Inc.
# This is free software; see the source for copying conditions.  There is NO
# warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

# dantopa@Riesz-Fischer:charlie $ make
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_set_precision.o mod_set_precision.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_command_line.o mod_command_line.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_io_handles.o mod_io_handles.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data.o mod_kalman_data.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data_allocate.o mod_kalman_data_allocate.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data_read.o mod_kalman_data_read.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_kalman_data_write.o mod_kalman_data_write.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_time_stamp.o mod_time_stamp.f08
# gfortran -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o forsee.o forsee.f08
# gfortran -g -o forsee forsee.o mod_command_line.o mod_constants.o mod_file_handling.o mod_io_handles.o mod_kalman_data.o mod_kalman_data_allocate.o mod_kalman_data_read.o mod_kalman_data_write.o mod_set_precision.o mod_time_stamp.o

# dantopa@Riesz-Fischer:charlie $ ./forsee haa
#
# timestamp: 2017-02-12  22:46:46  UCT-0600
#
# Fortran compiler version: GCC version 6.1.0
#
# Fortran compilation options: -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -auxbase-strip forsee.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -ffpe-trap=denormal,invalid,zero -fbacktrace -fcheck=bounds -fmax-errors=5
#
# STOP #  #  # successful completion for program foresee . . .
