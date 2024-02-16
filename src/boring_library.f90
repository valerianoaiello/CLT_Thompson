!
!   boring_library.f90:
!



SUBROUTINE foo()
  IMPLICIT NONE
  PRINT *, 'Hello, World!'
END SUBROUTINE foo



recursive function GenerateCompleteBinaryTreeNew(height) RESULT(treeResult)
!  This function returns the words representing a complete binary tree, producing the labels of its leaves 
!  (which are words composed of the letters {0, 1}). The height of the tree must be at least 1.
  INTEGER, INTENT(IN) :: height
  integer :: i, j
  CHARACTER(len=20), DIMENSION( 2 ** height) :: treeResult
  CHARACTER(len=20), DIMENSION( 2 ** (height-1)) :: treeResult_temp
  

  ! Case : height = 0
  IF (height == 0) THEN
    treeResult(1) = ""
  ! Case : height = 1
  else if (height == 1) then
    i = 1
    treeResult(1) = '0'
    treeResult(2) = '1'
  else
  ! Case : height > 1
    treeResult_temp = GenerateCompleteBinaryTreeNew(height-1)

    do i = 1, 2 ** (height -1) 
      treeResult(i) = '0' // treeResult_temp(i)
    end do
    do i = 1, 2 ** (height -1) 
      treeResult(i+  2** (height-1)) = '1' // treeResult_temp(i)
    end do
  end if
end function GenerateCompleteBinaryTreeNew