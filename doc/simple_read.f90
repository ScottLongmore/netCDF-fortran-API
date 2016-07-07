program simple_read
  use netcdf
  implicit none

  ! This is the name of the data file we will read. 
  character (len = *), parameter :: FILE_NAME = "simple_xy.nc"
  integer, parameter :: NX = 6, NY = 12 ! 2D Grid Dimensions
  integer, dimension(:,:), allocatable :: data_in ! Input variable
  integer :: ncid, varid ! netcdf and variable ID

  allocate(data_in(NY, NX)) ! Memory Allocation

  nf90_open(FILE_NAME, NF90_NOWRITE, ncid) ! Open netCDF file

  nf90_inq_varid(ncid, "data", varid) ! Inquire variable ID

  nf90_get_var(ncid, varid, data_in) ! Read data

  nf90_close(ncid)  ! Close netCDF file

end program simple_read
