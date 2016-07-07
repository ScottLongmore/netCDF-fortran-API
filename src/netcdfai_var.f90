     module netcdfai_var
       use netcdf
       use netcdfai_common
       use netcdfai_att
       implicit none

       interface nf90ai_get_var
         module procedure nf90ai_get_var_0D,nf90ai_get_var_1D, &
                          nf90ai_get_var_2D,nf90ai_get_var_3D, &
                          nf90ai_get_var_4D
       end interface nf90ai_get_var
 
       interface nf90ai_get_var_att
         module procedure nf90ai_get_var_att_text,nf90ai_get_var_att_1D
       end interface nf90ai_get_var_att

       contains

       subroutine nf90ai_new_var(ncid,varid,var)
         integer, intent(in) :: ncid,varid
         type(nf90aiVar), intent(inout) :: var
         integer :: attIdx

         allocate(var%dimids(NF90_MAX_VAR_DIMS))
         call nf90ai_check( &
           nf90_inquire_variable( &
             ncid = ncid, &
             varid = varid, &
             name = var%name, &
             xtype = var%xtype, &
             ndims = var%ndims,  &
             dimids =var%dimids, &
             natts= var%natts &
           ) &
         )

          allocate(var%atts(var%natts))
          do attIdx=1,var%natts
            call nf90ai_new_att(ncid,varid,attIdx,var%atts(attIdx))
          end do

       end subroutine nf90ai_new_var

       function nf90ai_get_var_0D(self,varName,value) result(returnVal)
         ! Dummy Arguments 
         type(nf90aiMeta),intent(inout) :: self
         character(len=*),intent(in) :: varName
         real,intent(inout):: value 
         integer :: returnVal

         ! Local Variables
         integer :: varIdx
         integer :: i

         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           
           if(self%vars(varIdx)%nDims==0) then
             !returnVal =  nf90_open(self%filename, NF90_NOWRITE, self%ncid)
             call nf90ai_check( &
               nf90_open(self%filename, NF90_NOWRITE, self%ncid) &
             )
             !returnVal = nf90_get_var(self%ncid,varIdx,value)
             !print *, 'get_var returnVal: ',returnVal
             call nf90ai_check( &
               nf90_get_var(self%ncid,varIdx,value) &
             )
             !call nf90ai_check( nf90_close(self%ncid) )
             returnVal = nf90_close(self%ncid)
             !print *, 'close returnVal: ',returnVal
             !print *, trim(self%vars(varIdx)%name), &
             !         self%vars(varIdx)%nDims, &
             !         varIdx,values
           else
             returnVal=-999
           endif
         else
           returnVal=-999 
         endif

       end function nf90ai_get_var_0D

       function nf90ai_get_var_1D(self,varName,values) result(returnVal)
         ! Dummy Arguments 
         type(nf90aiMeta),intent(inout) :: self
         character(len=*),intent(in) :: varName
         real,intent(inout),pointer :: values(:) 
         integer :: returnVal

         ! Local Variables
         integer :: varIdx,dimSize
         integer :: i
 
         nullify(values)
 
         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           
           if(self%vars(varIdx)%nDims==1) then
             !returnVal = nf90_open(self%filename, NF90_NOWRITE, self%ncid) 
             !print *, 'get_var_1D: open:',returnVal
             call nf90ai_check( &
               nf90_open(self%filename, NF90_NOWRITE, self%ncid) &
             )
             dimSize=self%dimSizes(self%vars(varIdx)%dimids(1))
             allocate(values(dimSize))
             call nf90ai_check( &
               nf90_get_var(self%ncid,varIdx,values) &
             )
             !returnVal =  nf90_get_var(self%ncid,varIdx,values) 
             !print *, 'get_var_1D: get_var:',returnVal
             !call nf90ai_check( nf90_close(self%ncid) )
             returnVal = nf90_close(self%ncid)
             !print *, 'get_var_1D: get_var:',returnVal
             !print *, trim(self%vars(varIdx)%name), &
             !         self%vars(varIdx)%nDims, &
             !         varIdx,dimSize
             !print *, (values(i),i=1,dimSize)
           else
             returnVal=-999 
           endif
         else
           returnVal=-999 
         endif

       end function nf90ai_get_var_1D

       function nf90ai_get_var_2D(self,varName,values) result(returnVal)
         ! Dummy Arguments 
         type(nf90aiMeta),intent(inout) :: self
         character(len=*),intent(in) :: varName
         real,intent(inout),pointer :: values(:,:) 
         integer :: returnVal

         ! Local Variables
         integer :: varIdx,dimSize1,dimSize2
         integer :: i,j

         nullify(values)

         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           
           if(self%vars(varIdx)%nDims==2) then
             !returnVal = nf90_open(self%filename, NF90_NOWRITE, self%ncid)
             !print *, 'get_var_2D: open: ',returnVal
             call nf90ai_check( &
               nf90_open(self%filename, NF90_NOWRITE, self%ncid) &
             )
             dimSize1=self%dimSizes(self%vars(varIdx)%dimids(1))
             dimSize2=self%dimSizes(self%vars(varIdx)%dimids(2))
             allocate(values(dimSize1,dimSize2))
             !returnVal = nf90_get_var(self%ncid,varIdx,values)
             !print *, 'get_var_2D: get_var: ',returnVal
             call nf90ai_check( &
               nf90_get_var(self%ncid,varIdx,values) &
             )
             !call nf90ai_check( nf90_close(self%ncid) )
             returnVal = nf90_close(self%ncid)
             !print *, 'get_var_2D: close: ',returnVal
             !print *, trim(self%vars(varIdx)%name), &
             !         self%vars(varIdx)%nDims, &
             !         varIdx,dimSize1,dimSize2
             !print *, ((values(i,j),j=1,dimSize2),i=1,dimSize1)
           else
             returnVal=-999 
           endif
         else
           returnVal=-999 
         endif

       end function nf90ai_get_var_2D

       function nf90ai_get_var_3D(self,varName,values) result(returnVal)
         ! Dummy Arguments 
         type(nf90aiMeta),intent(inout) :: self
         character(len=*),intent(in) :: varName
         real,intent(inout),pointer :: values(:,:,:)
         integer :: returnVal

         ! Local Variables
         integer :: varIdx,dimSize1,dimSize2,dimSize3
         integer :: i,j,k

         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           
           if(self%vars(varIdx)%nDims==3) then
             call nf90ai_check( &
               nf90_open(self%filename, NF90_NOWRITE, self%ncid) &
             )
             dimSize1=self%dimSizes(self%vars(varIdx)%dimids(1))
             dimSize2=self%dimSizes(self%vars(varIdx)%dimids(2))
             dimSize3=self%dimSizes(self%vars(varIdx)%dimids(3))
             allocate(values(dimSize1,dimSize2,dimSize3))
             call nf90ai_check( &
               nf90_get_var(self%ncid,varIdx,values) &
             )
             call nf90ai_check( nf90_close(self%ncid) )
             !print *, trim(self%vars(varIdx)%name), &
             !         self%vars(varIdx)%nDims, &
             !         varIdx,dimSize1,dimSize2,dimSize3
             !print *, (((values(i,j,k),k=1,dimSize3),j=1,dimSize2),i=1,dimSize1)
           else
             returnVal=-999 
           endif
         else
           returnVal=-999 
         endif

       end function nf90ai_get_var_3D

       function nf90ai_get_var_4D(self,varName,values) result(returnVal)
         ! Dummy Arguments 
         type(nf90aiMeta),intent(inout) :: self
         character(len=*),intent(in) :: varName
         real,intent(inout),pointer :: values(:,:,:,:)
         integer :: returnVal

         ! Local Variables
         integer :: varIdx,dimSize1,dimSize2,dimSize3,dimSize4
         integer :: i,j,k,l

         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           
           if(self%vars(varIdx)%nDims==4) then
             call nf90ai_check( &
               nf90_open(self%filename, NF90_NOWRITE, self%ncid) &
             )
             dimSize1=self%dimSizes(self%vars(varIdx)%dimids(1))
             dimSize2=self%dimSizes(self%vars(varIdx)%dimids(2))
             dimSize3=self%dimSizes(self%vars(varIdx)%dimids(3))
             dimSize4=self%dimSizes(self%vars(varIdx)%dimids(4))
             allocate(values(dimSize1,dimSize2,dimSize3,dimSize4))
             call nf90ai_check( &
               nf90_get_var(self%ncid,varIdx,values) &
             )
             call nf90ai_check( nf90_close(self%ncid) )
             !print *, trim(self%vars(varIdx)%name), &
             !         self%vars(varIdx)%nDims, &
             !         varIdx,dimSize1,dimSize2,dimSize3
             !print *, ((((values(i,j,k,l),l=1,dimSize4),k=1,dimSize3), &
             !             j=1,dimSize2),i=1,dimSize1)
           else
             returnVal=-999 
           endif
         else
           returnVal=-999 
         endif

       end function nf90ai_get_var_4D

       function nf90ai_get_var_index(self,varName,varIdx) result(returnVal)

         type(nf90aiMeta),intent(in) :: self
         character(len=*), intent(in) :: varName
         integer,intent(inout) :: varIdx

         integer :: returnVal
         integer :: i
         
         returnVal=-999
         varIdx=-1
         if(varName .eq. NF90AI_GLOBAL_VAR_NAME) then
           varIdx=0
           returnVal=nf90_noerr
         else
           do i=1,self%nVars
             if (varName .eq. self%vars(i)%name) then
               varIdx=i
               returnVal=nf90_noerr
               exit
             end if
           end do
         endif

       end function nf90ai_get_var_index

       function nf90ai_get_var_att_text(self,varName,attName,valueString) result(returnVal)
         type(nf90aiMeta),intent(in) :: self
         character(len=*),intent(in) :: varName,attName
         character(len=NF90AI_MAX_CHAR_SIZE),intent(inout) :: valueString
 
         integer :: varIdx
         integer :: returnVal

         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           returnVal = nf90ai_get_att(self,varIdx,attName,valueString)
         else
           returnVal = -999
         end if
           
       end function nf90ai_get_var_att_text

       function nf90ai_get_var_att_1D(self,varName,attName,values) result(returnVal)
         type(nf90aiMeta),intent(in) :: self
         character(len=*),intent(in) :: varName,attName
         real,intent(inout),pointer :: values(:)
 
         integer :: varIdx
         integer :: returnVal
  
         nullify(values)
 
         if(nf90ai_get_var_index(self,varName,varIdx) == nf90_noerr) then
           returnVal = nf90ai_get_att(self,varIdx,attName,values)
         else
           returnVal = -999
         end if
           
       end function nf90ai_get_var_att_1D

     end module netcdfai_var
