     ! $Id: nf90api_test_data.f90,v 0.1  2012/05/XX 12:00:00 longmore Exp $
     
     module netcdfai_test_data 
       use netcdf
       implicit none

       character (len = NF90_MAX_NAME), parameter :: filename = &
         'hursat.nc'
       character (len = NF90_MAX_NAME), parameter :: filename2 = &
         'hursat2.nc'

       character(len=NF90_MAX_NAME), parameter :: &
          var1Name = 'WindSpd', &
          var2Name = 'lat', &
          var3Name = 'VSCHN', &
          var4Name = 'IRWVP'

       character(len=NF90_MAX_NAME), parameter :: &
          att1Name = 'valid_range', &
          att2Name = 'actual_range', &
          att3Name = 'scale_factor', &
          att4Name = 'add_offset'

       character(len=NF90_MAX_NAME), parameter :: &
          gatt1Name = 'TC_name'

       real,pointer :: var1Ref
       real,pointer :: var2Ref(:)
       real,pointer :: var3Ref(:,:)
       real,pointer :: var4Ref(:,:)

     end module netcdfai_test_data
