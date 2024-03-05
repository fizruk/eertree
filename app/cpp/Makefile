CXX = g++
CXXFLAGS = -std=c++11

# Specify the names of your executables
EXECS = msubstr msubstr-direct msubstr-semi-pers queries-direct queries-semi-pers rich-direct rich-semi-pers

.PHONY: all clean build run

all: build

build:
	@if [ ! -d "build" ]; then \
		mkdir build; \
	fi
	@cd build && cmake .. && make

run-msubstr: build
	./build/msubstr-classic
run-msubstr-direct: build
	./build/msubstr-direct
run-msubstr-semi-pers: build
	./build/msubstr-semi-pers
run-queries-direct: build
	./build/queries-direct
run-queries-semi-pers: build
	./build/queries-semi-pers
run-rich-direct: build
	./build/rich-direct
run-rich-semi-pers: build
	./build/rich-semi-pers
clean:
	@rm -rf build

.PHONY: $(EXECS)

$(EXECS): build
	@cd build && make $@
