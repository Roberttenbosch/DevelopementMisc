.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = example 

all: ${MODS: %=%.beam} example

example: example.c erl_comm.c example_driver.c
	gcc -o example1 example.c erl_comm.c example_driver.c

clean:
	