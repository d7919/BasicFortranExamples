subroutine char2int(cc,ii)
  implicit none
  character,intent(out) :: ii
  character, intent(in) :: cc
  ii=cc
end subroutine char2int

program test
  implicit none
  character :: mychar
  integer :: i
  integer*1:: j
  !Note here we lie about the function
  !This ends up with the function returning the ascii integer for the character
  interface 
     subroutine char2int(cc,ii)
       integer,intent(out)::ii
       character,intent(in)::cc
     end subroutine char2int
  end interface

  !Note the routine only touches the first byte of i and hence
  !you have to be careful with what is in the other bits
  mychar='1'
  call char2int(mychar,i)
  write(6,'("No initialisation.")')
  write(6,'("Converted ",A,A,A," to ",I0)') "'",mychar,"'",i

  i=300
  write(6,'("Initialisation to ",I0)') i
  mychar='1'
  call char2int(mychar,i)
  write(6,'("Converted ",A,A,A," to ",I0)') "'",mychar,"'",i

  i=32000
  write(6,'("Initialisation to ",I0)') i
  mychar='1'
  call char2int(mychar,i)
  write(6,'("Converted ",A,A,A," to ",I0)') "'",mychar,"'",i

  i=128
  write(6,'("Initialisation to ",I0)') i
  mychar='1'
  call char2int(mychar,i)
  write(6,'("Converted ",A,A,A," to ",I0)') "'",mychar,"'",i

  i=0
  mychar='1'
  write(6,'("Initialisation to ",I0)') i
  call char2int(mychar,i)
  write(6,'("Converted ",A,A,A," to ",I0)') "'",mychar,"'",i
end program test
