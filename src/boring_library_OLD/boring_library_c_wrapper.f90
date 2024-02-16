! This automatically generated Fortran wrapper file allows codes
! written in Fortran to be called directly from C and translates all
! C-style arguments into expected Fortran-style arguments (with
! assumed size, local type declarations, etc.).


SUBROUTINE C_FOO() BIND(C)
  IMPLICIT NONE

  INTERFACE
    SUBROUTINE FOO()
      IMPLICIT NONE
    END SUBROUTINE FOO
  END INTERFACE

  CALL FOO()
END SUBROUTINE C_FOO

