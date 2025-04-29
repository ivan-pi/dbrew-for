
FC=gfortran
FFLAGS=-g -O1 -Wall -static-libgfortran

CC=gcc-14
CFLAGS=-g -std=gnu99 -I../include

LDFLAGS=-L..
LIBS=-ldbrew

TARGETS := main fibonacci

all: $(TARGETS)

main: dbrew_mod.f90 rewriter_class.f90 main.f90 f_dbrew_rewrite.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

fibonacci: dbrew_mod.f90 rewriter_class.f90 fibonacci.f90 f_dbrew_rewrite.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)


f_dbrew_rewrite.o: f_dbrew_rewrite.c
	$(CC) $(CFLAGS) -c $<

.PHONY: clean
clean:
	$(RM) *.o *.mod $(TARGETS)
