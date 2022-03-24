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
call power
call trig
call absolute_value
call exp_and_log
call square_root
call linear_algebra

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
    integer :: r
    a = 2
    call check(a%x == 2)
    call check(all(a%dx == 0.0))
    r = a
    call check(r == 2)
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

  call check( 1.0 == a )
  call check( 2.0 /= a )
  call check( 10.0 > a )
  call check( 0.0 < a )
  call check( 1.0 >= a )
  call check( 1.0 <= a )

  ! dual-on-integer
  call check( a == 1 )
  call check( a /= 2 )
  call check( a < 10 )
  call check( a > 0 )
  call check( a <= 1 )
  call check( a >= 1 )

  call check( 1 == a )
  call check( 2 /= a )
  call check( 10 > a )
  call check( 0 < a )
  call check( 1 >= a )
  call check( 1 <= a )

end subroutine

subroutine addition()
  type(dual) :: a, b
  integer :: i
  test_name = "addition"

  a = 2
  b = 3

  a%dx(1) = 2.0
  b%dx(2) = 3.0

  add_duals: block
    type(dual) :: c
    c = a + b
    call check(c == 5.0)
    call check(c%dx(1) == 2.0*1.0)
    call check(c%dx(2) == 3.0*1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block add_duals

  add_real: block
    type(dual) :: c
    c = a + 1.5
    call check(c == 3.5)
    call check(c%dx(1) == 2.0*1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10.5 + b
    call check(c == 13.5)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 3.0*1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block add_real

  add_integer: block
    type(dual) :: c
    c = a + 20
    call check(c == 22)
    call check(c%dx(1) == 2.0*1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10 + b
    call check(c == 13)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 3.0*1.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block add_integer

  unary_plus: block
    type(dual) :: c
    c = +a
    call check(c == 2.0)
    call check(c%dx(1) == 2.0*1.0)
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

  a%dx(1) = 2.0
  b%dx(2) = 3.0

  sub_duals: block
    type(dual) :: c
    test_name = "sub_duals"
    c = a - b
    call check(c == -1.0)
    call check(c%dx(1) == 2.0*1.0)
    call check(c%dx(2) == 3.0*(-1.0))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sub_duals

  sub_real: block
    type(dual) :: c
    test_name = "sub_real"
    c = a - 1.5
    call check(c == 0.5)
    call check(c%dx(1) == 2.0*1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10.5 - b
    call check(c == 7.5)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 3.0*(-1.0))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sub_real

  sub_integer: block
    type(dual) :: c
    test_name = "sub_integer"
    c = a - 20
    call check(c == -18)
    call check(c%dx(1) == 2.0*1.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10 - b
    call check(c == 7)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 3.0*(-1.0))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sub_integer

  unary_minus: block
    type(dual) :: c
    test_name = "unary_minus"
    c = -a
    call check(c == -2.0)
    call check(c%dx(1) == 2.0*(-1.0))
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

  a%dx(1) = 2.0
  b%dx(2) = 7.0

  mul_duals: block
    type(dual) :: c
    test_name = "mul_duals"
    c = a * b
    call check(c == 6.0)
    call check(c%dx(1) == 2.0*3.0)
    call check(c%dx(2) == 7.0*2.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block mul_duals

  mul_real: block
    type(dual) :: c
    test_name = "mul_real"
    c = a * 1.5
    call check(c == 3.0)
    call check(c%dx(1) == 2.0*1.5)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10.5 * b
    call check(c == 31.5)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*10.5)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block mul_real

  mul_integer: block
    type(dual) :: c
    test_name = "mul_integer"
    c = a * 20
    call check(c == 40)
    call check(c%dx(1) == 2.0*20.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 10 * b
    call check(c == 30)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*10.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block mul_integer
end subroutine

subroutine division()
  type(dual) :: a, b
  integer :: i

  a = 2
  b = 4

  a%dx(1) = 2.0
  b%dx(2) = 7.0

  div_duals: block
    type(dual) :: c
    test_name = "div_duals"
    c = a / b
    call check(c == 0.5)
    call check(c%dx(1) == 2.0*0.25)
    call check(c%dx(2) == 7.0*(-0.125))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block div_duals

  div_real: block
    type(dual) :: c
    test_name = "div_real"
    c = a / 0.5
    call check(c == 4.0)
    call check(c%dx(1) == 2.0*2.0)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 16.0 / b
    call check(c == 4.0)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*(-1.0))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block div_real

  div_integer: block
    type(dual) :: c
    test_name = "div_integer"
    c = a / 4
    call check(c == 0.5)
    call check(c%dx(1) == 2.0*0.25)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)

    c = 40 / b
    call check(c == 10)
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*(-2.5))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block div_integer
end subroutine

subroutine power()
  type(dual) :: a, b
  integer :: i

  a = 2
  b = 4

  a%dx(1) = 2.0
  b%dx(2) = 7.0

  pow_duals: block
    type(dual) :: c
    test_name = "pow_duals"
    c = a ** b
    call check(c == 16.0)
    call check(c%dx(1) == 2.0*(4.0*2.0**3.0))
    call check(c%dx(2) == 7.0*(2.0**4.0*log(2.0)))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block pow_duals

  pow_integer: block
    type(dual) :: c
    test_name = "pow_integer"
    c = a ** 8
    call check(c == 256.0)
    call check(c%dx(1) == 2.0*(8*2.0**7))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block pow_integer

  pow_real: block
    type(dual) :: c
    test_name = "pow_real"
    c = a ** 0.5
    call check(c == 2.0**0.5)
    call check(c%dx(1) == 2.0*(0.5*2.0**(-0.5)))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block pow_real

end subroutine

subroutine trig()
  type(dual) :: a, b
  integer :: i

  a = 0.5
  b = 0.25

  a%dx(1) = 2.0
  b%dx(2) = 7.0

  cosine: block
    type(dual) :: c
    test_name = "cosine"
    c = cos(a)
    call check(c == cos(0.5))
    call check(c%dx(1) == 2.0*(-sin(0.5)))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block cosine

  sine: block
    type(dual) :: c
    test_name = "sine"
    c = sin(a)
    call check(c == sin(0.5))
    call check(c%dx(1) == 2.0*cos(0.5))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block sine

  tangent: block
    type(dual) :: c
    test_name = "tangent"
    c = tan(a)
    call check(c == tan(0.5))
    call check(c%dx(1) == 2.0*1.0/cos(0.5)**2)
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block tangent

  arccosine: block
    type(dual) :: c
    test_name = "arccosine"
    c = acos(a)
    call check(c == acos(0.5))
    call check(c%dx(1) == 2.0*(-1.0/sqrt(1.0-0.5**2)))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block arccosine

  arcsine: block
    type(dual) :: c
    test_name = "arcsine"
    c = asin(a)
    call check(c == asin(0.5))
    call check(c%dx(1) == 2.0*1.0/sqrt(1.0-0.5**2))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block arcsine

  arctangent: block
    type(dual) :: c
    test_name = "arctangent"
    c = atan(a)
    call check(c == atan(0.5))
    call check(c%dx(1) == 2.0*1.0/(1.0+0.5**2))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block arctangent

  arctangent2_dual: block
    type(dual) :: c
    test_name = "arctangent2_dual"
    c = atan2(a,b)
    call check(c == atan2(0.5,0.25))
    call check(c%dx(1) == 2.0*0.25/(0.5**2+0.25**2))
    call check(c%dx(2) == 7.0*(-0.5/(0.5**2+0.25**2)))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block arctangent2_dual

  arctangent2_real1: block
    type(dual) :: c
    test_name = "arctangent2_real1"
    c = atan2(a,0.25)
    call check(c == atan2(0.5,0.25))
    call check(c%dx(1) == 2.0*0.25/(0.5**2+0.25**2))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block arctangent2_real1

  arctangent2_real2: block
    type(dual) :: c
    test_name = "arctangent2_real2"
    c = atan2(0.5,b)
    call check(c == atan2(0.5,0.25))
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*(-0.5/(0.5**2+0.25**2)))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block arctangent2_real2

  hyperbolic_cosine: block
    type(dual) :: c
    test_name = "hyperbolic_cosine"
    c = cosh(a)
    call check(c == cosh(0.5))
    call check(c%dx(1) == 2.0*sinh(0.5))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block hyperbolic_cosine

  hyperbolic_sine: block
    type(dual) :: c
    test_name = "hyperbolic_sine"
    c = sinh(a)
    call check(c == sinh(0.5))
    call check(c%dx(1) == 2.0*cosh(0.5))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block hyperbolic_sine

end subroutine

subroutine absolute_value
  type(dual) :: a, b, c
  test_name = "absolute value"

  a = 0.5
  b = -10.5

  a%dx(1) = 2.0
  b%dx(2) = 7.0

  c = abs(a)

  call check(c%x == 0.5)
  call check(c%dx(1) == 2.0*1.0)
  call check(c%dx(2) == 0.0)
  call check(c%dx(3) == 0.0)
  call check(c%dx(4) == 0.0)

  c = abs(b)

  call check(c%x == 10.5)
  call check(c%dx(1) == 0.0)
  call check(c%dx(2) == 7.0*(-1.0))
  call check(c%dx(3) == 0.0)
  call check(c%dx(4) == 0.0)

end subroutine 

subroutine exp_and_log
  type(dual) :: a, b

  a = 0.5
  b = 10.5

  a%dx(1) = 2.0
  b%dx(2) = 7.0

  exponential: block
    type(dual) :: c
    test_name = "exponential"
    c = exp(a)
    call check(c%x == exp(0.5))
    call check(c%dx(1) == 2.0*exp(0.5))
    call check(c%dx(2) == 0.0)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block exponential

  natural_logarithm: block
    type(dual) :: c
    test_name = "natural_logarithm"
    c = log(b)
    call check(c%x == log(10.5))
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*1.0/10.5)
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block natural_logarithm

  base10_logarithm: block
    type(dual) :: c
    test_name = "base10_logarithm"
    c = log10(b)
    call check(c%x == log10(10.5))
    call check(c%dx(1) == 0.0)
    call check(c%dx(2) == 7.0*1.0/(10.5*log(10.0)))
    call check(c%dx(3) == 0.0)
    call check(c%dx(4) == 0.0)
  end block base10_logarithm

end subroutine 

subroutine square_root
  type(dual) :: a, b
  test_name = "square_root"

  a = 16.0

  a%dx(1) = 7.0

  b = sqrt(a)
  call check(b%x == 4.0)
  call check(b%dx(1) == 7.0*(1.0/(2.0*4.0)))
  call check(b%dx(2) == 0.0)
  call check(b%dx(3) == 0.0)
  call check(b%dx(4) == 0.0)
end subroutine 

subroutine linear_algebra
  type(dual), dimension(3,3) :: a, b, c
  integer :: i,j
  test_name = "linear_algebra"

  a = 0.0
  b = 0.0

  do i=1,3
    a(i,i) = 1.0
    a(i,i)%dx(1) = 2.0
    do j=1,3
      b(i,j) = i*3+j
      b(i,j)%dx(2) = 7.0
    end do
  end do

  c = matmul(a,b)

  call check(all(b == c))

  write(*,*) "1,1: ", c(1,1)
  call check(c(1,1)%x == 4.0)
  call check(c(1,1)%dx(1) == 8.0)
  call check(c(1,1)%dx(2) == 7.0)
  call check(c(1,1)%dx(3) == 0.0)
  call check(c(1,1)%dx(4) == 0.0)

  write(*,*) "1,2: ", c(1,2)
  call check(c(1,2)%x == 5.0)
  call check(c(1,2)%dx(1) == 10.0)
  call check(c(1,2)%dx(2) == 7.0)
  call check(c(1,2)%dx(3) == 0.0)
  call check(c(1,2)%dx(4) == 0.0)

  !write(*,*) c(1,3)
  call check(c(1,3)%x == 6.0)
  call check(c(1,3)%dx(1) == 12.0)
  call check(c(1,3)%dx(2) == 7.0)
  call check(c(1,3)%dx(3) == 0.0)
  call check(c(1,3)%dx(4) == 0.0)

  write(*,*) c(2,1)
  call check(c(2,1)%x == 7.0)
  call check(c(2,1)%dx(1) == 14.0)
  call check(c(2,1)%dx(2) == 7.0)
  call check(c(2,1)%dx(3) == 0.0)
  call check(c(2,1)%dx(4) == 0.0)

  write(*,*) c(2,2)
  call check(c(2,2)%x == 5.0)
  call check(c(2,2)%dx(1) == 10.0)
  call check(c(2,2)%dx(2) == 7.0)
  call check(c(2,2)%dx(3) == 0.0)
  call check(c(2,2)%dx(4) == 0.0)

  write(*,*) c(2,3)
  call check(c(2,3)%x == 6.0)
  call check(c(2,3)%dx(1) == 12.0)
  call check(c(2,3)%dx(2) == 7.0)
  call check(c(2,3)%dx(3) == 0.0)
  call check(c(2,3)%dx(4) == 0.0)
end subroutine 

end program unittests
