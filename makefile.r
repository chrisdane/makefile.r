#!/usr/bin/env Rscript

graphics.off(); rm(list=ls())
library("tools") # for file_ext

args <- commandArgs(trailingOnly=T)
#print(args)
if (length(args) == 0) {
    targets <- "default"
    dry <- F
} else if (length(args) == 1) {
    if (any(args == c("-m", "--dry"))) {
        targets <- "default"
        dry <- T
    } else {
        targets <- "default"
        dry <- F
    }
} else if (length(args) > 1) {
    if (any(args[1] == c("-m", "--dry"))) {
        dry <- T
        targets <- args[2:length(args)]
    } else {
        targets <- args
        dry <- F
    }
}

# arguments for this 'make' script
#dry <- T
verbose <- F

message("###################################################################")
message("   make", ifelse(dry, " --dry ", " [hint: -m or --dry for dry run] "), 
        paste0(targets, collapse=" "))
message("   https://www.dkrz.de/up/systems/mistral/programming#section-2")
message("###################################################################")
#stop("asd")

# netcdf-C and -Fortran library versions:
netcdf_c_version <- "4.3.2" # there is only one netcdf_c module: netcdf_c/4.3.2-gcc48
gcc_version <- "48"
netcdf_f_version <- "4.4.2" # there is no module 'netcdf_f'. took version from link above 

# C and Fortran compiler and needed modules to load them
CC <- "mpiicc"
FC <- "mpiifort"
LD <- FC
needed_modules <- c("intelmpi", # for mpiifort 
                    "intel") # for ifort

# objects to build
OBJECTS <- c("modules.o", "modules_rotate_grid.o", "psolve.o",
             "comm.o", "read_parameters.o", "oce_read_mesh.o",
             "gen_partitioning.F90", "mesh_setup.o", "boundary_condition.o",
             "array.o", "matrix.o", "rhs.o", "inout.o", "solve.o", 
             "cal_strfcn.o")

# binary name
EXE <- "stream.x"

# compiler options
FOPT <- c("-r8", "-i4", "-O3", "-no-prec-div", "-no-prec-sqrt",
          "-fast-transcendentals", "-xHost", "-ip", "-fp-model", 
          "precise") #-traceback -check noarg_temp_created #-g -debug -check all
COPT <- c("-O3", "-DMPI2", "-Iinclude", "-xHost")
CPP_DEFS <- "-DPARMS"

# Definition of pARMS include and library
PARMS_DIR <- "../lib/parms"
PARMS_INC <- c(paste0("-I", PARMS_DIR, "/include"), 
               paste0("-I", PARMS_DIR, "/src/include"))
PARMS_DEFS <- c("-DPARMS", "-DUSE_MPI", "-DREAL=double", "-DDBL", "-DHAS_BLAS",
                "-DVOID_POINTER_SIZE_8", "-DSGI")
LIB_PARMS <- c(paste0("-L", PARMS_DIR, "/lib"),
               "-lparms")
LIBS <- "-mkl"

# libraries for linking C program to netCDF (no module load necessary)
nc_c_libs <- system(paste0("/sw/rhel6-x64/netcdf/netcdf_c-", netcdf_c_version, 
                           "-gcc", gcc_version, "/bin/nc-config --libs"), intern=T)

# Fortran netCDF include files (no module load necessary)
nc_f_inc <- system(paste0("/sw/rhel6-x64/netcdf/netcdf_fortran-", netcdf_f_version, 
                          "-intel14/bin/nf-config --fflags"), intern=T)

# libraries for linking Fortran program to netCDF (no module load necessary)
nc_f_libs <- system(paste0("/sw/rhel6-x64/netcdf/netcdf_fortran-", netcdf_f_version, 
                           "-intel14/bin/nf-config --flibs"), intern=T)


## make
# paste all commands together in one very long command just as in a real makefile
cmd_list <- list()

# 1) load needed modules
# both of the following two ways work!
if (F) { 
    module <- "/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash" # $ type module
    cmd_list[[1]] <- paste0("eval `", module, " purge`")
    cmd_list[[2]] <- paste0("eval `", module, " load ", paste0(needed_modules, collapse=" "), "`")
    cmd_list[[3]] <- paste0("eval `", module, " list`")
} else if (T) {
    cmd_list[[1]] <- paste0("source /etc/bashrc") # to define module
    cmd_list[[2]] <- paste0("module purge")
    cmd_list[[3]] <- paste0("module load ", paste0(needed_modules, collapse=" "))
    cmd_list[[4]] <- paste0("module list")
}

# 2) define dependencies
# one entry in cmd_list per command
cnt <- length(cmd_list)
for (obji in 1:length(OBJECTS)) {

    object_file <- OBJECTS[obji]
    if (verbose) message(obji, ": ", object_file, " ...")

    # to do here: check if file is older or different than actual
    if (F) {


    # (re-)building object nesessary
    } else if (T) {

        # get filename and tye 
        object_filename <- file_path_sans_ext(object_file)
        # file source file of .o object
        source_files <- system(paste0("ls ", object_filename, ".*"), intern=T)
        if (any(source_files == object_file)) {
            source_files <- source_files[-which(source_files == object_file)]
        }

        if (length(source_files) > 0) {
            sourcefile_types <- file_ext(source_files)

            if (verbose) {
                message("   object_file: ", object_file)
                message("   object_filename: ", object_filename)
                message("   source_files: ", paste0(source_files, collapse=" "))
                message("   sourcefile_types: ", paste0(sourcefile_types, collapse=" "))
                message("   ***********")
            }

            for (sourcefilei in 1:length(source_files)) {

                # .F90 target
                if (sourcefile_types[sourcefilei] == "F90") {
                    cnt <- cnt + 1
                    cmd <- paste0(FC, " ",
                                  paste0(FOPT, collapse=" "), " ", paste0(CPP_DEFS, collapse=" "), " ",
                                  paste0(nc_f_inc, collapse=" "), " ", paste0(PARMS_INC, collapse=" "),
                                  " -c ", object_filename, ".", sourcefile_types[sourcefilei])
                    cmd_list[[cnt]] <- cmd
                
                # .c target
                } else if (sourcefile_types[sourcefilei] == "c") {
                    cnt <- cnt + 1
                    cmd <- paste0(CC, " ",
                                  paste0(COPT, collapse=" "), " ", paste0(CPP_DEFS, collapse=" "), " ",
                                  paste0(PARMS_DEFS, collapse=" "), " " ,paste0(PARMS_INC, collapse=" "),
                                  " -c ", object_filename, ".", sourcefile_types[sourcefilei])
                    cmd_list[[cnt]] <- cmd
               
                # not defined
                } else {
                    # nothing to do
                }

            } # for sourcefilei source_files

        } # if object dependency object is wanted (e.g. when a file.F90 is in 'OBJECTS', do nothing)

    } # run dependency rule if object changed 

} # for obji objects

# 3) define target
# one entry in cmd_list per command
# to do here: check if file is older or different than actual
if (F) {

} else if (T) {
    cnt <- length(cmd_list)
    if (any(targets == "default")) {
        cmd_list[[cnt + 1]] <- paste0("rm -f ", EXE)
        cmd_list[[cnt + 2]] <- paste0(LD, " ", paste0(FOPT, collapse=" "), " -o ", EXE, " ", 
                                      paste0(OBJECTS, collapse=" "), " ", paste0(LIB_PARMS, collapse=" "), " ",
                                      paste0(nc_f_libs, collapse=" "), " ", paste0(LIBS, collapse=" "))
        cmd_list[[cnt + 3]] <- paste0("cp -pf ", EXE, " ../bin/.")
    }
}

## run all commands
message("")
message("Run the following commands in one shell:")
nchar <- nchar(length(cmd_list))
for (cmdi in 1:length(cmd_list)) {
    message("   ", sprintf(paste0("%", nchar, "i"), cmdi), "/", length(cmd_list), ": ", cmd_list[[cmdi]])
}
message("")
if (dry) {
    message("This is a dry run. Stop here.")
} else {
    message(" ... processing ...")
    cmd_all <- paste0(unlist(cmd_list), collapse=" && ")
    system(cmd_all)
    message("")
    message("finish")
}
message("")
