# makefile.r

Writing makefiles with minimal hard-coded version numbers and without the annoying `make` syntax. See also the [MakefileR package of Müller (2016)](https://cran.r-project.org/web/packages/MakefileR/vignettes/demo.html).

# How to
Output of the command
```
$ ./makefile.r -m # or --dry
```

```
###################################################################
   make --dry default
   https://www.dkrz.de/up/systems/mistral/programming#section-2
###################################################################

Run the following commands in one shell:
    1/21: source /etc/bashrc
    2/21: module purge
    3/21: module load intelmpi intel
    4/21: module list
    5/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c modules.F90
    6/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c modules_rotate_grid.F90
    7/21: mpiicc -O3 -DMPI2 -Iinclude -xHost -DPARMS -DPARMS -DUSE_MPI -DREAL=double -DDBL -DHAS_BLAS -DVOID_POINTER_SIZE_8 -DSGI -I../lib/parms/include -I../lib/parms/src/include -c psolve.c
    8/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c comm.F90
    9/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c read_parameters.F90
   10/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c oce_read_mesh.F90
   11/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c mesh_setup.F90
   12/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c boundary_condition.F90
   13/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c array.F90
   14/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c matrix.F90
   15/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c rhs.F90
   16/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c inout.F90
   17/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c solve.F90
   18/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -DPARMS -I/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/include -I../lib/parms/include -I../lib/parms/src/include -c cal_strfcn.F90
   19/21: rm -f stream.x
   20/21: mpiifort -r8 -i4 -O3 -no-prec-div -no-prec-sqrt -fast-transcendentals -xHost -ip -fp-model precise -o stream.x modules.o modules_rotate_grid.o psolve.o comm.o read_parameters.o oce_read_mesh.o gen_partitioning.F90 mesh_setup.o boundary_condition.o array.o matrix.o rhs.o inout.o solve.o cal_strfcn.o -L../lib/parms/lib -lparms -L/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/lib -lnetcdff -Wl,-rpath,/sw/rhel6-x64/netcdf/netcdf_fortran-4.4.2-intel14/lib -L/sw/rhel6-x64/netcdf/netcdf_c-4.3.2-gcc48/lib -Wl,-rpath,/sw/rhel6-x64/netcdf/netcdf_c-4.3.2-gcc48/lib -L/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib -Wl,-rpath,/sw/rhel6-x64/hdf5/hdf5-1.8.14-threadsafe-gcc48/lib -L/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib -Wl,-rpath,/sw/rhel6-x64/sys/libaec-0.3.2-gcc48/lib -lnetcdf -lhdf5_hl -lhdf5 -lsz -lcurl -lz -lnetcdf -mkl
   21/21: cp -pf stream.x ../bin/.

This is a dry run. Stop here.

```
