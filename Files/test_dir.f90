PROGRAM Test
  !A simple program to test if a directory exists and is writable
  implicit none
  integer :: narg,unit=20, ii,ierr
  integer, parameter :: MaxTries=256
  integer, parameter :: MaxStrLen=256
  character(len=MaxStrLen) :: DirName, Arg1
  character(len=MaxStrLen) :: tmp_file_name
  character(len=MaxStrLen) :: tmp_file_name_base="testfile"
  character :: tmp_char
  logical :: debug=.true.,exist

  !First check how many args have been passed
  narg=command_argument_count()
  if(debug)then
     tmp_char=''
     if(narg.ne.1) tmp_char='s'
     write(6,'("Called with ",I0," arg",A,".")') narg,trim(tmp_char)
  endif

  !Now we will exit if no args
  if(narg.eq.0)then
     write(6,'("Must pass the directory name to test.")')
     stop
  endif

  !Now we stash the first arg and assume that's the dir to test
  call get_command_argument(1,DirName)
  if(debug)write(6,'("Testing existence of dir with name ",A)') trim(DirName)

  !Now try to find a file which doesn't already exist
  do ii=1,MaxTries
     !Make file name
     write(tmp_file_name,'(A,"/",A,"_",I0)') trim(DirName),trim(tmp_file_name_base),ii

     !Check if file already exists, if not then exit loop
     inquire(file=tmp_file_name,exist=exist)
     if(.not.exist) exit
  enddo
  if(debug) write(6,'("Using tmp file : ",A)') trim(tmp_file_name)

  !Now try to open file
  open(unit=unit,File=tmp_file_name,iostat=ierr)
  !If open was successful then we can close the file and delete it
  if(ierr.eq.0) close(unit=unit,status='delete')

  !Write outcome
  if(ierr.eq.0)then
     write(6,'("Directory ",A,A,A," is writable.")') "'",trim(DirName),"'"
  else
     write(6,'("Directory ",A,A,A," is not writable.")') "'",trim(DirName),"'"
  endif
END PROGRAM Test
