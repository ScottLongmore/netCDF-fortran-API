     module netcdfai_att
       use netcdf
       use netcdfai_common
       implicit none

       interface nf90ai_get_att
         module procedure nf90ai_get_att_text,nf90ai_get_att_1D
       end interface nf90ai_get_att

       contains

       subroutine nf90ai_new_att(ncid,varid,attnum,att)

         integer, intent(in) :: ncid,varid,attnum
         type(nf90aiAtt), intent(inout) :: att

         character(len=NF90AI_MAX_CHAR_SIZE) :: attBuffer 
         character(len=2) :: set
         integer :: i,iter,bIdx,eIdx

         write(set, '(AA)'), char(0), ' '

         call nf90ai_check( &
           nf90_inq_attname( &
             ncid = ncid, &
             varid = varid, &
             attnum = attnum, &
             name = att%name &
           ) &
         )

         call nf90ai_check( &
           nf90_inquire_attribute( &
              ncid = ncid, &
              varid = varid, &
              name = att%name, &
              xtype = att%xtype, &
              len = att%len &
            ) &
          )

         ! Save attribue value as character if numeric
         if(att%xtype == 2) then 
           call nf90ai_check( &
             nf90_get_att(ncid,varid,att%name,attBuffer) &
           )
           att%valueString=attBuffer(1:scan(attBuffer,set)-1)
         else
           allocate(att%values(att%len))
           call nf90ai_check( &
             nf90_get_att(ncid,varid,att%name,att%values) &
           )

           iter=10
           bIdx=1
           eIdx=iter
           do i=1,att%len
             write(attBuffer(bIdx:eIdx),'(F8.3,", ")') att%values(i) 
             bIdx=bIdx+iter
             eIdx=eIdx+iter
           end do

           !att%valueString=attBuffer(1:scan(attBuffer,set)-1)
           att%valueString=attBuffer(1:eIdx-iter)
           
         endif
 
       end subroutine nf90ai_new_att

       function nf90ai_get_att_text(self,varIdx,attName,valueString) result(status)
         type(nf90aiMeta),intent(in) :: self
         integer,intent(in) :: varIdx
         character(len=*),intent(in) :: attName
         character(len=NF90AI_MAX_CHAR_SIZE),intent(inout) :: valueString

         integer :: attIdx
         integer :: status

         if(nf90ai_get_att_index(self,varIdx,attName,attIdx) == nf90_noerr) then
           if(varIdx==0) then
             valueString=self%gatts(attIdx)%valueString
           else
             valueString=self%vars(varIdx)%atts(attIdx)%valueString
           end if
           !print *, 'ValueString: ',trim(valueString)
           status=nf90_noerr
         else 
           status=-999
         end if

       end function nf90ai_get_att_text
      
       function nf90ai_get_att_1D(self,varIdx,attName,values) result(status)
         type(nf90aiMeta),intent(in) :: self
         integer,intent(in) :: varIdx
         character(len=*),intent(in) :: attName
         real,intent(inout),pointer :: values(:)

         integer :: attIdx
         integer :: status
    
         nullify(values)

         if(nf90ai_get_att_index(self,varIdx,attName,attIdx) == nf90_noerr) then
           if(varIdx==0) then
             !print *, trim(attName),' ',varIdx,' ',attIdx
             allocate(values(self%gatts(attIdx)%len))
             values=self%gatts(attIdx)%values
           else
             ! print *, trim(attName),' ',varIdx,' ',attIdx
             allocate(values(self%vars(varIdx)%atts(attIdx)%len))
             values=self%vars(varIdx)%atts(attIdx)%values
           end if
           status=nf90_noerr
         else 
           status=-999
         end if

       end function nf90ai_get_att_1D

       function nf90ai_get_att_index(self,varIdx,attName,attIdx) result(status)

         type(nf90aiMeta),intent(in) :: self
         integer, intent(in) :: varIdx
         character(len=*), intent(in) :: attName
         integer,intent(inout) :: attIdx

         integer :: i
         integer :: status

         attIdx=-1
         status=-999
         if(varIdx == 0) then
           do i=1,self%nGlobalAtts
             if (attName .eq. self%gatts(i)%name) then
               attIdx=i
               status=nf90_noerr
               exit
             end if
           end do
           return

         else if (varIdx <= self%nVars) then
           do i=1,self%vars(varIdx)%natts
             if (attName .eq. self%vars(varIdx)%atts(i)%name) then
               attIdx=i
               status=nf90_noerr
               exit
             end if
           end do
           return

         else
           attIdx=-1
           status=-999
         endif

       end function nf90ai_get_att_index

     end module netcdfai_att
