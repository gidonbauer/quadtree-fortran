SRC = precision.f90 quadtree_class.f90
OBJ_FILES = ${addprefix ./build/, ${SRC:.f90=.o}}
LIBRARY = build/libqtree.a

EXAMPLES = example_01.f90 example_02.f90
EXECUTABLES = ${addprefix ./build/, ${basename ${EXAMPLES}}}

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
release: ${EXECUTABLES}

debug: F_FLAGS += ${F_FLAGS_DEBUG}
debug: ${EXECUTABLES}

# Build executables
build/%: examples/%.f90 ${LIBRARY}
	${FC} ${F_FLAGS} -I./build/ -o $@ $< -L./build/ -lqtree

# Build quadtree library
${LIBRARY}: ${OBJ_FILES}
	${AR} -rsc $@ $^

# Build object files for library
build/%.o: src/%.f90 build
	${FC} ${F_FLAGS} -I./build/ -J./build/ -c -o $@ $<

# Create build directory
build:
	mkdir -p build

# Clean up build directory
clean:
	${RM} -r build/

.PHONY: release debug clean
