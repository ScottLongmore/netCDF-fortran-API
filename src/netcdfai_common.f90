     ! $Id: netcdfai_common.f90,v 0.1 2012/07/26 12:00:00 longmore Exp $
     
     ! netcdfai_common: parameters and common netcdfai routines
     module netcdfai_common
       use netcdf
       
       implicit none

       ! Parameters
       integer, parameter :: NF90AI_MAX_NAME=NF90_MAX_NAME
       integer, parameter :: NF90AI_MAX_CHAR_SIZE = 512
       character(len=6), parameter :: NF90AI_GLOBAL_VAR_NAME = "GLOBAL"

       ! Att type
       type, public :: nf90aiAtt

         character (len = NF90_MAX_NAME) :: name
         integer :: xtype,len
         character(len=NF90AI_MAX_CHAR_SIZE) :: valueString
         real,pointer :: values(:)

       end type nf90aiAtt

       ! Var type
       type, public :: nf90aiVar

         character (len = NF90_MAX_NAME) :: name
         integer :: xtype
         integer :: ndims
         integer, pointer :: dimids(:)
         integer :: natts

         type(nf90aiAtt), pointer :: atts(:)

       end type nf90aiVar

       ! Meta type
       type, public :: nf90aiMeta

         character (len = NF90_MAX_NAME) filename
         integer :: ncid, varid
         integer :: nDims, nVars, nGlobalAtts, unlimDimId

         integer, pointer:: dimSizes(:)
         character (len = NF90_MAX_NAME), pointer :: dimNames(:)
         character (len = NF90_MAX_NAME), pointer :: varNames(:)

         type(nf90aiVar), pointer :: vars(:)

         type(nf90aiAtt), pointer :: gatts(:)

       end type nf90aiMeta

       ! Result type
       type, public :: nf90aiResult
         logical :: returnVal
         character(len=NF90AI_MAX_CHAR_SIZE) :: message
       end type nf90aiResult

       contains

       subroutine nf90ai_check(returnVal)
         integer, intent ( in) :: returnVal

         if(returnVal /= nf90_noerr) then
           print *, 'Error code: ',returnVal
           stop 2
         end if
       end subroutine nf90ai_check

       subroutine nf90ai_ncdump(self)

         type(nf90aiMeta) :: self
         integer :: dimIdx, varIdx, attIdx
         character (len = NF90_MAX_NAME), pointer :: xtypeName, dimList
         integer :: ndims,xtypeSize,xtypeNfields

         print '("netcdf ",A," {")', trim(self%filename)

         print '("dimensions:")'
         do dimIdx = 1,self%nDims
           print '(6X,A," = ",I4," ;")', &
             trim(self%dimNames(dimIdx)), self%dimSizes(dimIdx)
         end do

         print '("variables:")'
         do varIdx = 1,self%nVars
           
           ndims=self%vars(varIdx)%ndims
           allocate(dimList)
           if (ndims >= 2) then
             dimList = "(" // &
               trim(self%dimNames(self%vars(varIdx)%dimids(ndims)))
             do dimIdx = ndims-1,1,-1
               dimList = trim(dimList) // ", " // &
                 trim(self%dimNames(self%vars(varIdx)%dimids(dimIdx)))
             end do
             dimList = trim(dimList) // ')'
           else if (ndims == 1) then
             dimList = "(" //  &
               trim(self%dimNames(self%vars(varIdx)%dimids(1))) // ")"
           else
             dimList = ""
           end if

           allocate(xtypeName)
           call nf90ai_check( &
             nf90_INQ_TYPE(self%ncid,self%vars(varIdx)%xtype, &
                           xtypeName,xtypeSize,xtypeNfields) &
           )
           xtypeName=xtypeName(1:scan(xtypeName,'\0'))
           
           print '(6X,A,X,A,A," ;")',trim(xtypeName), &
                 trim(self%vars(varIdx)%name),trim(dimList)
           deallocate(xtypeName)
           deallocate(dimList)

           do attIdx = 1,self%vars(varIdx)%natts
             print '(12X,A,":",A," = ",A," ;")', &
                    trim(self%vars(varIdx)%name), &
                    trim(self%vars(varIdx)%atts(attIdx)%name),  &
                    trim(self%vars(varIdx)%atts(attIdx)%valueString)
           end do


         end do

         print '("// global attibutes:")'
         do attIdx = 1,self%nGlobalAtts
           print '(12X,":",A," = ",A," ;")', &
                 trim(self%gatts(attIdx)%name), & 
                 trim(self%gatts(attIdx)%valueString)
         end do

       end subroutine nf90ai_ncdump

     end module netcdfai_common
