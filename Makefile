SRC = main.f90
OBJ_FILES = ${addprefix ./build/, ${addsuffix .o, ${basename ${SRC}}}}

F_FLAGS = -Wall -Wextra -pedantic -std=f2018

release: F_FLAGS += -march=native -O3 -ffast-math -fstack-arrays
release: main

debug: F_FLAGS += -O0 -g -fcheck=all
debug: main

main: ${OBJ_FILES}
	${FC} ${F_FLAGS} -I./build/ -o main ${OBJ_FILES}

build/%.o: %.f90 build
	${FC} ${F_FLAGS} -J./build/ -c -o $@ $<

build:
	mkdir -p build

clean:
	${RM} -r main build/

.PHONY: release debug clean
