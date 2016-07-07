     module netcdfai_meta
       use netcdf
       use netcdfai_common
       use netcdfai_att
       use netcdfai_var

       implicit none

       contains

       subroutine nf90ai_new_meta(self,filename)

         type(nf90aiMeta) :: self 
         character (len = *),intent(in) :: filename
         integer :: dimIdx, varIdx, attIdx

         self%filename=filename

         call nf90ai_check( &
           nf90_open(self%filename, NF90_NOWRITE, self%ncid) &
         )
         call nf90ai_check( &
           nf90_inquire(self%ncid, self%nDims, self%nVars, &
             self%nGlobalAtts, self%unlimDimId &
           ) &
         )

         ! Get/set dimension names and sizes
         allocate(self%dimNames(self%nDims))
         allocate(self%dimSizes(self%nDims))
         do dimIdx=1,self%nDims
            call nf90ai_check( & 
              nf90_inquire_dimension(ncid = self%ncid, &
                dimid = dimIdx, &
                name = self%dimNames(dimIdx), &
                len = self%dimSizes(dimIdx) &
              ) & 
            )
         end do

         ! Get/set variable names
         allocate(self%vars(self%nVars))
         do varIdx=1,self%nVars
           call nf90ai_new_var(self%ncid,varIdx,self%vars(varIdx)) 
         end do

         allocate(self%gatts(self%nGlobalAtts))
         do attIdx = 1,self%nGlobalAtts
           call nf90ai_new_att( &
             self%ncid,NF90_GLOBAL,attIdx,self%gatts(attIdx) &
           ) 
         end do
         
         call nf90ai_check( nf90_close(self%ncid) )

       end subroutine nf90ai_new_meta

     end module netcdfai_meta
