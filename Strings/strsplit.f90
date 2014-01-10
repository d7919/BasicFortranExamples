MODULE strsplit_mod
  implicit none
  private
  public :: strsplit
contains
  function strsplit(MsgIn,Splt)
    !This function splits MsgIn on Splt and returns the individual strings in array
    implicit none
    character(len=*),intent(in) :: MsgIn, Splt !Inputs
    integer :: MsgLen, SpltLen
    !Note here we define each substring to have length of msg
    character(len=len(trim(MsgIn))),dimension(:),allocatable :: strsplit
    character(len=len(trim(MsgIn))),dimension(:),allocatable :: strsplit_tmp
    character(len=len(trim(MsgIn))) :: trimMsg
    integer :: nmatch, ind, sec, mat

    MsgLen=len(trim(MsgIn)); SpltLen=len(Splt)
    trimMsg=trim(MsgIn)

    !First count how many matches of Splt we have in MsgIn
    nmatch=count_match(MsgIn,Splt)
    !If no matches then we want to exit
    if(nmatch.le.0) then
       !But we have to make sure function has a return value
       allocate(strsplit(1))
       strsplit(1)=MsgIn
       return
    endif

    !Now allocate tmp array
    allocate(strsplit_tmp(nmatch+1))
    
    !Initialise counts
    ind=0 ; sec=0
    
    !Now loop over string splitting on matches
    do 
       !Get index of first match
       mat=INDEX(trim(MsgIn(ind:)),splt)

       !Increment section index
       sec=sec+1

       !If there is a match save section
       if(mat.ne.0)then
          strsplit_tmp(sec)=MsgIn(ind:ind+mat-2)
       !Else write rest of string and exit loop
       else
          strsplit_tmp(sec)=MsgIn(ind:)
          exit
       endif

       !Increment index
       ind=ind+SpltLen+mat-1

       !If we've reached the end of the string then exit
       if(ind.gt.MsgLen) exit
    enddo

    !Now we want to see if first and/or last part of string is Splt
    mat=0 ; ind=0
    if(MsgIn(1:SpltLen).eq.Splt) then
       mat=mat+1
       ind=ind+1
    endif
    if(trimMsg(MsgLen-SpltLen+1:).eq.Splt)then
       mat=mat+2
    endif

    !Now allocate return array
    allocate(strsplit(nmatch-ind))
    select case(mat)
       case(0)
          strsplit=strsplit_tmp
       case(1)
          strsplit=strsplit_tmp(2:)
       case(2)
          strsplit=strsplit_tmp(:nmatch)
       case(3)
          strsplit=strsplit_tmp(2:nmatch)
    end select
    deallocate(strsplit_tmp)
  end function strsplit

  function count_match(strin,splt)
    implicit none
    character(len=*),intent(in)::strin,splt
    integer :: count_match
    integer :: LenSplt,LenStrin
    integer :: nmat, ind,mat
    LenSplt=len(trim(splt))
    LenStrin=len(trim(strin))

    !Initialise count
    count_match=0
    ind=0

    !Loop over string until no characters left
    do
       !Get index of first match
       mat=INDEX(trim(strin(ind:)),trim(splt))
       !If no matches then we exit
       if(mat.eq.0) return

       !Increment count and index
       count_match=count_match+1
       ind=ind+LenSplt+mat-1

       !Exit if we've reached the end of the string
       if(ind.gt.LenStrin) return
    enddo
  end function count_match

end MODULE strsplit_mod

PROGRAM test_strsplit
  use strsplit_mod, only: strsplit
  implicit none
  character(len=256) :: TestMsg="/AA/A / /A/ /  "
  character :: Splt1='/', Splt2='A', Splt3='l'
  character(len=2) :: Splt4='AA'
  character(len=256),dimension(:),allocatable :: retvals
  integer :: i, nm
  write(6,'("A program to test strsplit")')
  write(6,'("   Splitting : ")')
  write(6,'(A)') trim(TestMsg)

  write(6,'(" ")') 
  write(6,'(40("="))')
  write(6,'("Using split string : ",A,A,A)') "'",Splt1,"'"
  retvals=strsplit(TestMsg,Splt1)
  nm=size(retvals)
  write(6,'("   Num Sub String : ",I0)') nm
  do i=1,nm
     write(6,'("    -- : ",A,A,A)') "'",trim(retvals(i)),"'"
  enddo  
  write(6,'(40("-"))')

  write(6,'(" ")') 
  write(6,'(40("="))')
  write(6,'("Using split string : ",A,A,A)') "'",Splt2,"'"
  retvals=strsplit(TestMsg,Splt2)
  nm=size(retvals)
  write(6,'("   Num Sub String : ",I0)') nm
  do i=1,nm
     write(6,'("    -- : ",A,A,A)') "'",trim(retvals(i)),"'"
  enddo  
  write(6,'(40("-"))')

  write(6,'(" ")') 
  write(6,'(40("="))')
  write(6,'("Using split string : ",A,A,A)') "'",Splt3,"'"
  retvals=strsplit(TestMsg,Splt3)
  nm=size(retvals)
  write(6,'("   Num Sub String : ",I0)') nm
  do i=1,nm
     write(6,'("    -- : ",A,A,A)') "'",trim(retvals(i)),"'"
  enddo  
  write(6,'(40("-"))')

  write(6,'(" ")') 
  write(6,'(40("="))')
  write(6,'("Using split string : ",A,A,A)') "'",Splt4,"'"
  retvals=strsplit(TestMsg,Splt4)
  nm=size(retvals)
  write(6,'("   Num Sub String : ",I0)') nm
  do i=1,nm
     write(6,'("    -- : ",A,A,A)') "'",trim(retvals(i)),"'"
  enddo  
  write(6,'(40("-"))')


end PROGRAM test_strsplit
  
