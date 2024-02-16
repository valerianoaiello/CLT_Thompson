
FUNCTION GenerateCompleteBinaryTree(height) RESULT(treeResult)
  INTEGER, INTENT(IN) :: height
  CHARACTER(len=:), ALLOCATABLE, DIMENSION(:) :: treeResult
  
  ! Caso base: altezza zero
  IF (height == 0) THEN
    ALLOCATE(treeResult(1))
    treeResult(1) = ""
  ! Caso base: altezza uno
  ELSEIF (height == 1) THEN
    ALLOCATE(treeResult(3))
    treeResult = ["0", "1", "2"]
  ! Caso generale
  ELSE
    ALLOCATE(treeResult(3 * SIZE(treeResult(height - 1))))
    treeResult = [TRIM("0" // i) | i = GenerateCompleteBinaryTree(height - 1)] // [TRIM("1" // i) | i = GenerateCompleteBinaryTree(height - 1)] // [TRIM("2" // i) | i = GenerateCompleteBinaryTree(height - 1)]
  END IF

END FUNCTION GenerateCompleteBinaryTree

function generate_sequences(list1, length_of_sequence)
  implicit none
  integer, intent(in) :: length_of_sequence
  integer :: size_list1

  size_list1 = size(list1)
  
  character(LEN=20), dimension(size_list1) :: generate_sequences

  integer :: i, j

  do i = 1, size_list1
    do j = 1

function foo(a) result(b)
    implicit none
    
    real(kind=8), intent(in)    :: a(:,:)
    complex(kind=8)             :: b(size(a,1),size(a,2))
    
    b = exp((0,1)*a)
    
end function foo

PROGRAM MyFortranProgram
  ! Chiamata alla subroutine
  CALL HelloSubroutine
CONTAINS
  ! Definizione della subroutine
  SUBROUTINE HelloSubroutine()
    PRINT *, 'Hello, World!'
  END SUBROUTINE HelloSubroutine
END PROGRAM MyFortranProgram

PROGRAM MyFortranProgram
  ! Chiamata alla subroutine
  CALL HelloSubroutine
CONTAINS
  ! Definizione della subroutine
  SUBROUTINE HelloSubroutine()
    PRINT *, 'Hello, World!'
  END SUBROUTINE HelloSubroutine
END PROGRAM MyFortranProgram


function foo(a) result(b)
    implicit none
    
    real(kind=8), intent(in)    :: a(:,:)
    complex(kind=8)             :: b(size(a,1),size(a,2))
    
    b = exp((0,1)*a)
    
end function foo
!program hello
  ! This is a comment line; it is ignored by the compiler
!  print *, 'Hello, World!'
!end program hello
