# Find all source files, create a list of corresponding object files
PROGRAM = foresee

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
	rm -f *.mod *.smod *.o

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# module interdependencies
mod_file_handling.o       :
mod_io_handles.o          :
mod_input_output.o        : mod_file_handling.o mod_io_handles.o mod_kalman_data.o
mod_kalman_data.o         : mod_file_handling.o mod_io_handles.o mod_set_precision.o
mod_set_precision.o       :


# rditldmt@ITLDMT-MD-O2034:alpha $ date
# Wed Dec  7 14:40:27 CST 2016
# rditldmt@ITLDMT-MD-O2034:alpha $ pwd
# /Users/rditldmt/Documents/GitHub Desktop/kalman/alpha
# rditldmt@ITLDMT-MD-O2034:alpha $ make debug
# PROGRAM  = foresee
# PRG_OBJ  = foresee.o
# SRCS     = foresee.f08 mod_file_handling.f08 mod_input_output.f08 mod_io_handles.f08
# OBJS     = foresee.o mod_file_handling.o mod_input_output.o mod_io_handles.o
# MODS     = mod_file_handling.f08 mod_input_output.f08 mod_io_handles.f08
# MOD_OBJS = mod_file_handling.o mod_input_output.o mod_io_handles.o
# rditldmt@ITLDMT-MD-O2034:alpha $ echo $gflags
# -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only
#
