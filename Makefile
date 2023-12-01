all: $(patsubst %.ss,%.exe,$(wildcard day*.ss))

GXCFLAGS=-O

day%.exe: day%.ss day%
	gxc $(GXCFLAGS) -exe -o $@ $<
	strip $@

clean: FRC
	rm -f *.exe

FRC:
