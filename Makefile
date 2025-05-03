SRC = precision.f90 quadtree_class.f90 main.f90
OBJ_FILES = ${addprefix ./build/, ${addsuffix .o, ${basename ${SRC}}}}

BASENAME_FC = ${notdir ${FC}}
ifeq (${BASENAME_FC}, gfortran)
	F_FLAGS = -Wall -Wextra -pedantic -Wconversion -Wshadow -std=f2023
	F_FLAGS_RELEASE = -march=native -O3 -ffast-math -fstack-arrays
	F_FLAGS_DEBUG = -O0 -g -fcheck=all -fsanitize=leak
else ifeq (${BASENAME_FC}, ${filter ${BASENAME_FC}, flang flang-new flang-20})
	F_FLAGS = -pedantic -std=f2018
	F_FLAGS_RELEASE = -march=native -mtune=native -O3 -ffast-math
	F_FLAGS_DEBUG = -O0 -g
endif

release: F_FLAGS += ${F_FLAGS_RELEASE}
release: main

debug: F_FLAGS += ${F_FLAGS_DEBUG}
debug: main

main: ${OBJ_FILES}
	${FC} ${F_FLAGS} -I./build/ -o main ${OBJ_FILES}

build/%.o: src/%.f90 build
	${FC} ${F_FLAGS} -I./build/ -J./build/ -c -o $@ $<

build:
	mkdir -p build

clean:
	${RM} -r main build/

.PHONY: release debug clean
