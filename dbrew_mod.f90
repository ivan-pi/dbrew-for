
module dbrew_mod

use, intrinsic :: iso_c_binding
implicit none
public


interface
    function makeDynamic(v) bind(c,name="makeDynamic")
        import c_int64_t
        integer(c_int64_t), value :: v
        integer(c_int64_t) :: makeDynamic
    end function
    function makeStatic(v) bind(c,name="makeStatic")
        import c_int64_t
        integer(c_int64_t), value :: v
        integer(c_int64_t) :: makeStatic
    end function
    function dbrew_new() bind(c)
        import c_ptr
        type(c_ptr) :: dbrew_new
    end function
    subroutine dbrew_free(rw) bind(c)
        import c_ptr
        type(c_ptr), value :: rw
    end subroutine
    subroutine dbrew_set_decoding_capacity(rw,instr_capacity,bb_capacity) bind(c)
        import c_ptr, c_int
        type(c_ptr), value :: rw
        integer(c_int), value :: instr_capacity
        integer(c_int), value :: bb_capacity
    end subroutine
    subroutine dbrew_set_capture_capacity(rw,instr_capacity,bb_capacity,code_capacity) bind(c)
        import c_ptr, c_int
        type(c_ptr), value :: rw
        integer(c_int), value :: instr_capacity
        integer(c_int), value :: bb_capacity
        integer(c_int), value :: code_capacity
    end subroutine

    subroutine dbrew_set_function(rw,f) bind(c)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(c_funptr), value :: f
    end subroutine

    subroutine dbrew_verbose(rw,decode,emu_state,emu_steps) bind(c)
        import c_ptr, c_bool
        type(c_ptr), value :: rw
        logical(c_bool), value :: decode
        logical(c_bool), value :: emu_state
        logical(c_bool), value :: emu_steps
    end subroutine

    subroutine dbrew_optverbose(rw,v) bind(c)
        import c_ptr, c_bool
        type(c_ptr), value :: rw
        logical(c_bool), value :: v
    end subroutine

    subroutine dbrew_printer_showbytes(rw,v) bind(c)
        import c_ptr, c_bool
        type(c_ptr), value :: rw
        logical(c_bool), value :: v
    end subroutine

    function dbrew_decode(rw,f) bind(c)
        import c_ptr, c_int64_t
        type(c_ptr), value :: rw
        integer(c_int64_t) :: f
        type(c_ptr) :: dbrew_decode
    end function

    subroutine dbrew_decode_print(rw,f,count) bind(c)
        import c_ptr, c_int64_t, c_int
        type(c_ptr), value :: rw
        integer(c_int64_t) :: f
        integer(c_int) :: count
    end subroutine

    subroutine dbrew_print_decoded(bb,print_bytes) bind(c)
        import c_ptr, c_bool
        type(c_ptr), value :: bb
        logical(c_bool), value :: print_bytes
    end subroutine

! FIXME: See https://github.com/caps-tum/dbrew/issues/55 for details
!
!    subroutine dbrew_set_stacksize(rw,stacksize) bind(c)
!        import c_ptr, c_int
!        type(c_ptr), value :: rw
!        integer(c_int), value :: stacksize
!    end subroutine

    function dbrew_generated_code(rw) bind(c)
        import c_ptr, c_int64_t
        type(c_ptr), value :: rw
        integer(c_int64_t) :: dbrew_generated_code
    end function
    function dbrew_generated_size(rw) bind(c)
        import c_ptr, c_int64_t
        type(c_ptr), value :: rw
        integer(c_int64_t) :: dbrew_generated_size
    end function

    subroutine dbrew_config_reset(rw) bind(c)
        import c_ptr
        type(c_ptr), value :: rw
    end subroutine

    subroutine dbrew_config_staticpar(rw,static_par_pos) bind(c)
        import c_ptr, c_int
        type(c_ptr), value :: rw
        integer(c_int), value :: static_par_pos
    end subroutine

    subroutine dbrew_config_returnfp(rw) bind(c)
        import c_ptr
        type(c_ptr), value :: rw
    end subroutine

    subroutine dbrew_config_parcount(rw,parcount) bind(c)
        import c_ptr, c_int
        type(c_ptr), value :: rw
        integer(c_int), value :: parcount
    end subroutine

    subroutine dbrew_config_force_unknown(rw,depth) bind(c)
        import c_ptr, c_int
        type(c_ptr), value :: rw
        integer(c_int), value :: depth
    end subroutine

    subroutine dbrew_config_branches_known(rw,b) bind(c)
        import c_ptr, c_bool
        type(c_ptr), value :: rw
        logical(c_bool), value :: b
    end subroutine

    subroutine dbrew_config_function_setname(rw,f,name) bind(c)
        import c_ptr, c_int64_t, c_char
        type(c_ptr), value :: rw
        integer(c_int64_t), value :: f
        character(len=1,kind=c_char), intent(in) :: name(*)
    end subroutine

    subroutine dbrew_config_function_setsize(rw,f,len) bind(c)
        import c_ptr, c_int64_t, c_int
        type(c_ptr), value :: rw
        integer(c_int64_t), value :: f
        integer(c_int), value :: len
    end subroutine

    subroutine dbrew_config_par_setname(rw,par,name) bind(c)
        import c_ptr, c_int, c_char
        type(c_ptr), value :: rw
        integer(c_int), value :: par
        character(len=1,kind=c_char) :: name(*)
    end subroutine

    subroutine dbrew_config_set_memrange(rw,name,is_writable,start,size)  bind(c)
        import c_ptr, c_char, c_bool, c_int64_t, c_int
        type(c_ptr), value :: rw
        character(len=1,kind=c_char) :: name(*)
        logical(c_bool), value :: is_writable
        integer(c_int64_t), value :: start
        integer(c_int), value :: size
    end subroutine

    subroutine dbrew_def_verbose(decode,emu_state,emu_steps) bind(c)
        import c_bool
        logical(c_bool), value :: decode
        logical(c_bool), value :: emu_state
        logical(c_bool), value :: emu_steps
    end subroutine
end interface


abstract interface
    function dbrew_func_R8V8_t(a) bind(c)
        import c_double
        real(c_double), value :: a
        real(c_double) :: dbrew_func_R8V8_t
    end function
    function dbrew_func_R8V8V8_t(a,b) bind(c)
        import c_double
        real(c_double), value :: a, b
        real(c_double) :: dbrew_func_R8V8V8_t
    end function
    function dbrew_func_R8P8_t(a) bind(c)
        import c_double
        real(c_double) :: a(*)
        real(c_double) :: dbrew_func_R8P8_t
    end function
end interface


interface
    function dbrew_set_vectorsize(rw,s) bind(c)
        import c_ptr, c_int
        type(c_ptr), value :: rw
        integer(c_int), value :: s
        integer(c_int) :: dbrew_set_vectorsize
    end function

! FIXME: check if these routines are meant to accept
!        scalars or doubles

    subroutine dbrew_apply4_R8V8(f,ov,iv) bind(c,name="dbrew_apply4_R8V8")
        import c_funptr, c_double
        type(c_funptr), value :: f
        real(c_double) :: ov(*), iv(*)
    end subroutine
    subroutine dbrew_apply4_R8V8V8(f,ov,i1v,i2v) bind(c,name="dbrew_apply4_R8V8V8")
        import c_funptr, c_double
        type(c_funptr), value :: f
        real(c_double) :: ov(*), i1v(*), i2v(*)
    end subroutine
    subroutine dbrew_apply4_R8P8(f,ov,iv) bind(c,name="dbrew_apply4_R8V8")
        import c_funptr, c_double
        type(c_funptr), value :: f
        real(c_double) :: ov(*), iv(*)
    end subroutine

end interface



interface dbrew_rewrite
    function f_dbrew_rewrite_1(rw,x1) bind(c) result(f)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(*) :: x1
        type(c_funptr) :: f
    end function
    function f_dbrew_rewrite_2(rw,x1,x2) bind(c) result(f)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(*) :: x1, x2
        type(c_funptr) :: f
    end function
    function f_dbrew_rewrite_3(rw,x1,x2,x3) bind(c) result(f)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(*) :: x1, x2, x3
        type(c_funptr) :: f
    end function
    function f_dbrew_rewrite_4(rw,x1,x2,x3,x4) bind(c) result(f)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(*) :: x1, x2, x3, x4
        type(c_funptr) :: f
    end function
    function f_dbrew_rewrite_5(rw,x1,x2,x3,x4,x5) bind(c) result(f)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(*) :: x1, x2, x3, x4, x5
        type(c_funptr) :: f
    end function
    function f_dbrew_rewrite_6(rw,x1,x2,x3,x4,x5,x6) bind(c) result(f)
        import c_ptr, c_funptr
        type(c_ptr), value :: rw
        type(*) :: x1, x2, x3, x4, x5, x6
        type(c_funptr) :: f
    end function
end interface


end module
