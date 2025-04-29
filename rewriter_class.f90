module rewriter_class

use, intrinsic :: iso_c_binding
use dbrew_mod

implicit none
private

public :: rewriter

type :: rewriter
    type(c_ptr) :: rw = c_null_ptr
contains

    procedure :: init => rewriter_init
    procedure :: free => rewriter_free

    procedure :: set_decoding_capacity => rewriter_set_decoding_capacity
    procedure :: set_capture_capacity => rewriter_set_capture_capacity
    procedure :: set_function => rewriter_set_function
    procedure :: verbose => rewriter_verbose
    procedure :: optverbose => rewriter_optverbose
    procedure :: printer_showbytes => rewriter_printer_showbytes
    procedure :: decode => rewriter_decode
    procedure :: decode_print => rewriter_decode_print
    procedure :: generated_code => rewriter_generated_code
    procedure :: generated_size => rewriter_generated_size
    procedure :: config_reset => rewriter_config_reset
    procedure :: config_staticpar => rewriter_config_staticpar
    procedure :: config_returnfp => rewriter_config_returnfp
    procedure :: config_parcount => rewriter_config_parcount
    procedure :: config_force_unknown => rewriter_config_force_unknown
    procedure :: config_branches_known => rewriter_config_branches_known
    procedure :: config_function_setname => rewriter_config_function_setname
    procedure :: config_function_setsize => rewriter_config_function_setsize
    procedure :: config_par_setname => rewriter_config_par_setname
    procedure :: config_set_memrange => rewriter_config_set_memrange
    procedure :: set_vectorsize => rewriter_set_vectorsize

    procedure, private :: rewrite_1
    procedure, private :: rewrite_2
    procedure, private :: rewrite_3
    procedure, private :: rewrite_4
    procedure, private :: rewrite_5
    procedure, private :: rewrite_6

    generic :: rewrite => rewrite_1, rewrite_2, rewrite_3, rewrite_4, &
                                  rewrite_5, rewrite_6
end type


contains

subroutine rewriter_init(this)
    class(rewriter), intent(inout) :: this
    if (c_associated(this%rw)) error stop "already initialized"
    this%rw = dbrew_new()
    if (.not. c_associated(this%rw)) error stop "init error"
end subroutine

subroutine rewriter_free(this)
    class(rewriter), intent(inout) :: this
    if (c_associated(this%rw)) then
        call dbrew_free(this%rw)
    end if
end subroutine

! Define type-bound wrappers:

subroutine rewriter_set_decoding_capacity(this, instr_capacity, bb_capacity)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: instr_capacity, bb_capacity
    call dbrew_set_decoding_capacity(this%rw, instr_capacity, bb_capacity)
end subroutine

subroutine rewriter_set_capture_capacity(this, instr_capacity, bb_capacity, code_capacity)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: instr_capacity, bb_capacity, code_capacity
    call dbrew_set_capture_capacity(this%rw, instr_capacity, bb_capacity, code_capacity)
end subroutine

subroutine rewriter_set_function(this, f)
    class(rewriter), intent(inout) :: this
    type(c_funptr), value :: f
    call dbrew_set_function(this%rw, f)
end subroutine

subroutine rewriter_verbose(this, decode, emu_state, emu_steps)
    class(rewriter), intent(inout) :: this
    logical, intent(in) :: decode, emu_state, emu_steps
    call dbrew_verbose(this%rw, &
            logical(decode,c_bool), &
            logical(emu_state,c_bool), &
            logical(emu_steps,c_bool))
end subroutine

subroutine rewriter_optverbose(this, v)
    class(rewriter), intent(inout) :: this
    logical(c_bool), value :: v
    call dbrew_optverbose(this%rw, v)
end subroutine

subroutine rewriter_printer_showbytes(this, v)
    class(rewriter), intent(inout) :: this
    logical(c_bool), value :: v
    call dbrew_printer_showbytes(this%rw, v)
end subroutine

function rewriter_decode(this, f) result(ptr)
    class(rewriter), intent(inout) :: this
    integer(c_int64_t), value :: f
    type(c_ptr) :: ptr
    ptr = dbrew_decode(this%rw, f)
end function

subroutine rewriter_decode_print(this, f, count)
    class(rewriter), intent(inout) :: this
    integer(c_int64_t), value :: f
    integer(c_int), value :: count
    call dbrew_decode_print(this%rw, f, count)
end subroutine

function rewriter_generated_code(this) result(code)
    class(rewriter), intent(inout) :: this
    integer(c_int64_t) :: code
    code = dbrew_generated_code(this%rw)
end function

function rewriter_generated_size(this) result(size)
    class(rewriter), intent(inout) :: this
    integer(c_int64_t) :: size
    size = dbrew_generated_size(this%rw)
end function

subroutine rewriter_config_reset(this)
    class(rewriter), intent(inout) :: this
    call dbrew_config_reset(this%rw)
end subroutine

subroutine rewriter_config_staticpar(this, static_par_pos)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: static_par_pos
    call dbrew_config_staticpar(this%rw, static_par_pos)
end subroutine

subroutine rewriter_config_returnfp(this)
    class(rewriter), intent(inout) :: this
    call dbrew_config_returnfp(this%rw)
end subroutine

subroutine rewriter_config_parcount(this, parcount)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: parcount
    call dbrew_config_parcount(this%rw, parcount)
end subroutine

subroutine rewriter_config_force_unknown(this, depth)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: depth
    call dbrew_config_force_unknown(this%rw, depth)
end subroutine

subroutine rewriter_config_branches_known(this, b)
    class(rewriter), intent(inout) :: this
    logical(c_bool), value :: b
    call dbrew_config_branches_known(this%rw, b)
end subroutine

subroutine rewriter_config_function_setname(this, f, name)
    class(rewriter), intent(inout) :: this
    integer(c_int64_t), value :: f
    character(len=1,kind=c_char), intent(in) :: name(*)
    call dbrew_config_function_setname(this%rw, f, name)
end subroutine

subroutine rewriter_config_function_setsize(this, f, len)
    class(rewriter), intent(inout) :: this
    integer(c_int64_t), value :: f
    integer(c_int), value :: len
    call dbrew_config_function_setsize(this%rw, f, len)
end subroutine

subroutine rewriter_config_par_setname(this, par, name)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: par
    character(len=1,kind=c_char) :: name(*)
    call dbrew_config_par_setname(this%rw, par, name)
end subroutine

subroutine rewriter_config_set_memrange(this, name, is_writable, start, size)
    class(rewriter), intent(inout) :: this
    character(len=1,kind=c_char) :: name(*)
    logical(c_bool), value :: is_writable
    integer(c_int64_t), value :: start
    integer(c_int), value :: size
    call dbrew_config_set_memrange(this%rw, name, is_writable, start, size)
end subroutine

function rewriter_set_vectorsize(this, s) result(status)
    class(rewriter), intent(inout) :: this
    integer(c_int), value :: s
    integer(c_int) :: status
    status = dbrew_set_vectorsize(this%rw, s)
end function


    ! --- REWRITE INTERFACE BINDINGS ---

    function rewrite_1(this, x1) result(f)
        class(rewriter), intent(inout) :: this
        type(*) :: x1
        type(c_funptr) :: f
        f = f_dbrew_rewrite_1(this%rw, x1)
    end function

    function rewrite_2(this, x1, x2) result(f)
        class(rewriter), intent(inout) :: this
        type(*) :: x1, x2
        type(c_funptr) :: f
        f = f_dbrew_rewrite_2(this%rw, x1, x2)
    end function

    function rewrite_3(this, x1, x2, x3) result(f)
        class(rewriter), intent(inout) :: this
        type(*) :: x1, x2, x3
        type(c_funptr) :: f
        f = f_dbrew_rewrite_3(this%rw, x1, x2, x3)
    end function

    function rewrite_4(this, x1, x2, x3, x4) result(f)
        class(rewriter), intent(inout) :: this
        type(*) :: x1, x2, x3, x4
        type(c_funptr) :: f
        f = f_dbrew_rewrite_4(this%rw, x1, x2, x3, x4)
    end function

    function rewrite_5(this, x1, x2, x3, x4, x5) result(f)
        class(rewriter), intent(inout) :: this
        type(*) :: x1, x2, x3, x4, x5
        type(c_funptr) :: f
        f = f_dbrew_rewrite_5(this%rw, x1, x2, x3, x4, x5)
    end function

    function rewrite_6(this, x1, x2, x3, x4, x5, x6) result(f)
        class(rewriter), intent(inout) :: this
        type(*) :: x1, x2, x3, x4, x5, x6
        type(c_funptr) :: f
        f = f_dbrew_rewrite_6(this%rw, x1, x2, x3, x4, x5, x6)
    end function

end module
