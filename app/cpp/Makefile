CXX = g++
CXXFLAGS = -std=c++11

# Specify the names of your executables
EXECS = msubstr queries

.PHONY: all clean build run

all: build

build:
	@if [ ! -d "build" ]; then \
		mkdir build; \
	fi
	@cd build && cmake .. && make

run_msubstr:
	make build
	@./build/msubstr
run_queries:
	make build
	@./build/queries

clean:
	@rm -rf build

.PHONY: $(EXECS)

$(EXECS): build
	@cd build && make $@
