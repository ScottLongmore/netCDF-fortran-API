      ! netCDF FORTRAN 90 Application/Abstraction Interface (nf90ai)
      ! netcdf - netcdfai
      ! nf90_  - nf90ai
      !
      program nf90ai_example
        use netcdfai 
        implicit none

        type(nf90ai_meta) :: meta 
        character(len=4) :: varName='Temp'
        real, pointer :: var(:,:,:)

        ! Read netcdf file meta information 
        nf90ai_new_meta(meta,filename)

        ! "ncdump" print 
        nf90ai_dump(meta)

        ! Get a variable
        nf90ai_get_var(meta,varName,var)


      end program nf90ai_example

        




