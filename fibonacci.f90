program main_fibonacci

use, intrinsic :: iso_c_binding
use rewriter_class

implicit none

type(rewriter) :: rw
type(c_funptr) :: f_ptr
procedure(fibonacci), pointer :: f
integer :: r

r = fibonacci( 8 )

print *, "fib(8) = ", r

call rw%init()

call rw%verbose(.true., .true., .true.)
call rw%set_function(c_funloc(fibonacci))

call rw%config_staticpar(0)
call rw%config_parcount(1)

f_ptr = rw%rewrite( 8 )
call c_f_procpointer(f_ptr,f)
print *, "rewritten? ", .not. associated(f,fibonacci)

print *, f( 8 ) ! Expected value is 21

contains

  function fibonacci(n) result(fib)
    implicit none
    integer, intent(in) :: n

    integer :: a, b, temp, i, fib

    if (n <= 0) then
      fib = 0
    else if (n == 1) then
      fib = 1
    else
      a = 0
      b = 1
      do i = 2, n
        temp = a + b
        a = b
        b = temp
      end do
      fib = b
    end if
  end function fibonacci

end program
