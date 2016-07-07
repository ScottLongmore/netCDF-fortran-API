program simpler_read
  use netcdfai
  implicit none

  ! This is the name of the data file we will read. 
  character (len = *), parameter :: FILE_NAME = "simple_xy.nc"
  integer, dimension(:,:), allocatable :: data_in ! input variable
  type(nf90aiMeta) :: meta ! netCDFai meta object

  call nf90ai_new_meta(meta, FILE_NAME) ! Read meta 

  nf90ai_get_var(meta, "data", data_in) ! Read data

end program simpler_read
