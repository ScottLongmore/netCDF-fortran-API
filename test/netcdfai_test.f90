     ! $Id: nf90ai_test.f90,v 0.1  2012/05/XX 12:00:00 longmore Exp $
     
     module netcdfai_test 
       use fruit
       use netcdf
       use netcdfai
       use netcdfai_test_data
       implicit none

       ! test variables
       type(nf90aiMeta) :: testMeta,testMeta2
       integer :: status

       contains

       ! Master Test Routine
       subroutine test_nf90ai

         ! Test new_nf90ai_new_meta
         !call test_nf90ai_new_meta

         ! Test nf90ai_ncdump 
         !call test_nf90ai_ncdump

         ! Test get_var
         !call test_nf90ai_get_var

         ! Test get_var
         !call test_nf90ai_get_var_att

         ! Test Multiple netcdf files
         call test_multiple_netcdf

       end subroutine test_nf90ai

       ! Test new_nf90object routine
       subroutine test_nf90ai_new_meta

         call nf90ai_new_meta(testMeta,filename)

       end subroutine test_nf90ai_new_meta

       ! Test print_nf90object routine
       subroutine test_nf90ai_ncdump

         call nf90ai_ncdump(testMeta)

       end subroutine test_nf90ai_ncdump

       ! Test get_var routine
       subroutine test_nf90ai_get_var

         !status = nf90ai_get__var(testMeta,var1Name,var1Ref)
 
         !status = nf90ai_get_var(testMeta,var2Name,var2Ref)

         status = nf90ai_get_var(testMeta,var3Name,var3Ref)

       end subroutine test_nf90ai_get_var

       ! Test get_var routine
       subroutine test_nf90ai_get_var_att
         real,pointer :: values(:)
         character(len=NF90AI_MAX_CHAR_SIZE) :: valueString
         integer :: i
          
         !status = nf90ai_get_var_att(testMeta,var1Name,att1Name,values)
         !print *, 'Stat,Size: ',status,size(values)
         !print *, (values(i),i=1,size(values))

         status = nf90ai_get_var_att(testMeta,NF90AI_GLOBAL_VAR_NAME, &
                                     gatt1Name,valueString)
         !print *, trim(gatt1Name)
         !print *, 'Stat: ',status
         !print *, trim(valueString)

       end subroutine test_nf90ai_get_var_att

       ! Test multiple netcdf files  
       subroutine test_multiple_netcdf

         print *, 'Reading file 1: ',trim(filename)
         call nf90ai_new_meta(testMeta,filename)

         print *, 'Reading file 2: ',trim(filename2)
         call nf90ai_new_meta(testMeta,filename2)

       end subroutine test_multiple_netcdf

     end module netcdfai_test 
