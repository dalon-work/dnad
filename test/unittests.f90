program unittests
use dnad
implicit none

character(:),allocatable :: test_name

! check dnad is compiled correctly
call check(dnad_nderiv == 4)

call assign_dual
call comparison_operators
call addition
call subtraction
call multiplication
call division

contains

subroutine check(condition)
    logical, intent(in) :: condition

    if (.not. condition) then
      write(*,*) test_name, " failed!"
        stop -1
    end if
end subroutine

subroutine assign_dual()
  integer :: i

  test_name = "assign_dual"

  assign_real: block
    type(dual) :: a
    a = 1.0
    call check(a%x == 1.0)
    call check(all(a%dx == 0.0))
  end block assign_real

  assign_integer: block
    type(dual) :: a
    a = 2
    call check(a%x == 2)
    call check(all(a%dx == 0.0))
  end block assign_integer

  assign_dual_: block
    type(dual) :: a,b
    a = 3
    a%dx(3) = 78.5
    b = a
    call check(b%x == 3)
    call check(b%dx(1) == 0)
    call check(b%dx(2) == 0)
    call check(b%dx(3) == 78.5)
    call check(b%dx(4) == 0)
  end block assign_dual_
end subroutine

subroutine comparison_operators()
  type(dual) :: a,b,c
  test_name = "comparison_operators"

  ! assign with a real
  a = 1.0
  b = 2.0
  c = 1.0

  ! dual-on-dual
  call check( a /= b )
  call check( a == c )
  call check( a < b )
  call check( b > a )
  call check( a <= c )
  call check( a >= c )

  ! dual-on-real
  call check( a == 1.0 )
  call check( a /= 2.0 )
  call check( a < 10.0 )
  call check( a > 0.0 )
  call check( a <= 1.0 )
  call check( a >= 1.0 )

  ! dual-on-integer
  call check( a == 1 )
  call check( a /= 2 )
  call check( a < 10 )
  call check( a > 0 )
  call check( a <= 1 )
  call check( a >= 1 )

end subroutine

subroutine addition()
  type(dual) :: a, b
  integer :: i
  test_name = "addition"

  a = 2
  b = 3

  a%dx(1) = 1.0
  b%dx(2) = 1.0

  add_duals: block
    type(dual) :: c
    c = a + b
    call check(c == 5.0)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == 1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block add_duals

  add_real: block
    type(dual) :: c
    c = a + 1.5
    call check(c == 3.5)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10.5 + b
    call check(c == 13.5)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block add_real

  add_integer: block
    type(dual) :: c
    c = a + 20
    call check(c == 22)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10 + b
    call check(c == 13)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block add_integer

  unary_plus: block
    type(dual) :: c
    c = +a
    call check(c == 2.0)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block unary_plus 
end subroutine

subroutine subtraction()
  type(dual) :: a, b
  integer :: i
  test_name = "subtraction"

  a = 2
  b = 3

  a%dx(1) = 1.0
  b%dx(2) = 1.0

  sub_duals: block
    type(dual) :: c
    test_name = "sub_duals"
    c = a - b
    call check(c == -1.0)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == -1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sub_duals

  sub_real: block
    type(dual) :: c
    test_name = "sub_real"
    c = a - 1.5
    call check(c == 0.5)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10.5 - b
    call check(c == 7.5)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == -1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sub_real

  sub_integer: block
    type(dual) :: c
    test_name = "sub_integer"
    c = a - 20
    call check(c == -18)
    call check(c%dx(1) == 1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10 - b
    call check(c == 7)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == -1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sub_integer

  unary_minus: block
    type(dual) :: c
    test_name = "unary_minus"
    c = -a
    call check(c == -2.0)
    call check(c%dx(1) == -1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block unary_minus
end subroutine

subroutine multiplication()
  type(dual) :: a, b
  integer :: i

  a = 2
  b = 3

  a%dx(1) = 1.0
  b%dx(2) = 1.0

  mul_duals: block
    type(dual) :: c
    test_name = "mul_duals"
    c = a * b
    call check(c == 6.0)
    call check(c%dx(1) == 3.0)
    call check(c%dx(2) == 2.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block mul_duals

  mul_real: block
    type(dual) :: c
    test_name = "mul_real"
    c = a * 1.5
    call check(c == 3.0)
    call check(c%dx(1) == 1.5)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10.5 * b
    call check(c == 31.5)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 10.5)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block mul_real

  mul_integer: block
    type(dual) :: c
    test_name = "mul_integer"
    c = a * 20
    call check(c == 40)
    call check(c%dx(1) == 20.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10 * b
    call check(c == 30)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 10.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block mul_integer
end subroutine

subroutine division()
  type(dual) :: a, b
  integer :: i

  a = 2
  b = 4

  a%dx(1) = 1.0
  b%dx(2) = 1.0

  div_duals: block
    type(dual) :: c
    test_name = "div_duals"
    c = a / b
    call check(c == 0.5)
    call check(c%dx(1) == 0.25)
    call check(c%dx(2) == -0.125)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block div_duals

  div_real: block
    type(dual) :: c
    test_name = "div_real"
    c = a / 0.5
    call check(c == 4.0)
    call check(c%dx(1) == 2.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 16.0 / b
    call check(c == 4.0)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == -1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block div_real

  div_integer: block
    type(dual) :: c
    test_name = "div_integer"
    c = a / 4
    call check(c == 0.5)
    call check(c%dx(1) == 0.25)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 40 / b
    call check(c == 10)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == -2.5)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block div_integer
end subroutine

end program unittests
