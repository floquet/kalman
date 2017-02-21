module mCharacterPositions
    implicit none
contains
! **********************************************************************
! *                                                                    *
! *   SUBROUTINE DCHARP - TO DETERMINE THE CHARACTER POSITIONS         *
! *                                                                    *
! **********************************************************************
      subroutine dcharp(cstring,length,k1,k2)
          integer,       intent ( in )  :: length
          integer,       intent ( out ) :: k1, k2
          character*(*), intent ( in )  :: cstring

          integer :: i
      k1=1
      k2=length
      do i=1,length
      if (cstring(i:i).ne.' ') then
          k1=i
          goto 10
        endif
      enddo
   10 do i=length,1,-1
      if (cstring(i:i).ne.' ') then
          k2=i
          goto 20
        endif
      enddo
   20 return
      end subroutine dcharp

end module mCharacterPositions
