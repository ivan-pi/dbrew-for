program main_simple

use, intrinsic :: iso_c_binding
use dbrew_mod

implicit none

abstract interface
function foo_func(i,j)! bind(c)
    import c_int
    integer(c_int) :: i, j
    integer(c_int) :: foo_func
end function
end interface

type(c_ptr) :: rw
type(c_funptr) :: f1, f2
integer(c_int) :: a, b

procedure(foo_func), pointer :: p_f1, p_f2

rw = dbrew_new()

call dbrew_verbose(rw, .true._c_bool, .true._c_bool, .true._c_bool)
call dbrew_set_function(rw, c_funloc(foo))
call dbrew_config_staticpar(rw, 0)
call dbrew_config_parcount(rw, 2)

f1 = dbrew_rewrite(rw, 2, 3)
call c_f_procpointer(f1,p_f1)
print *, p_f1(2, 3)

print '(A,Z0.16)', "Address of foo = ", transfer(c_funloc(foo),1_c_intptr_t)
print '(A,Z0.16)', "Address of f1  = ", transfer(f1,1_c_intptr_t)

f2 = dbrew_rewrite(rw, 5, 3)
call c_f_procpointer(f2,p_f2)
print *, p_f2(5, 3)
print '(A,Z0.16)', "Address of f2  = ", transfer(f2,1_c_intptr_t)

contains

function foo(i,j)
    use, intrinsic :: iso_c_binding
    integer(c_int) :: i, j
    integer(c_int) :: foo
    if (i == 5) then
        foo = 0
    else
        foo = i + j
    end if
end function

end program
