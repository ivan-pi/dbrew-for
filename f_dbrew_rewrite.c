// f_dbrew_rewrite.c

#include <stdint.h>
#include <dbrew.h>

// Fortran doesn't have variadic arguments, so we need to specialize
// depending on the number of arguments received.

extern intptr_t
f_dbrew_rewrite_1(Rewriter *rw, void *x1) {
    return dbrew_rewrite(rw, x1);
}

extern intptr_t
f_dbrew_rewrite_2(Rewriter *rw, void *x1, void *x2) {
    return dbrew_rewrite(rw, x1, x2);
}

extern intptr_t
f_dbrew_rewrite_3(Rewriter *rw, void *x1, void *x2, void *x3) {
    return dbrew_rewrite(rw, x1, x2, x3);
}

extern intptr_t
f_dbrew_rewrite_4(Rewriter *rw, void *x1, void *x2, void *x3, void *x4) {
    return dbrew_rewrite(rw, x1, x2, x3, x4);
}

extern intptr_t
f_dbrew_rewrite_5(Rewriter *rw,
                  void *x1,
                  void *x2,
                  void *x3,
                  void *x4,
                  void *x5)
{
    return dbrew_rewrite(rw, x1, x2, x3, x4, x5);
}

extern intptr_t
f_dbrew_rewrite_6(Rewriter *rw,
                  void *x1,
                  void *x2,
                  void *x3,
                  void *x4,
                  void *x5,
                  void *x6)
{
    return dbrew_rewrite(rw, x1, x2, x3, x4, x5, x6);
}
