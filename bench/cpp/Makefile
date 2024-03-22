CXX = g++
CXXFLAGS = -std=c++11

# todo: super long line, rewrite
EXECS = bench-msubstr bench-msubstr-direct bench-msubstr-semi-pers bench-queries-direct bench-queries-semi-pers bench-rich-simple bench-rich-direct bench-rich-semi-pers bench-simple bench-simple-direct bench-simple-semi-pers bench-simple-binary bench-simple-direct-binary bench-simple-semi-pers-binary

.PHONY: all clean build run

all: build

build:
	@if [ ! -d "build" ]; then \
		mkdir build; \
	fi
	@cd build && cmake .. && make

run: build
	./build/bench-simple-binary --benchmark_format=csv
	@echo
	./build/bench-simple-direct-binary --benchmark_format=csv
	@echo
	./build/bench-simple-semi-pers-binary --benchmark_format=csv
	@echo
	./build/bench-simple --benchmark_format=csv
	@echo
	./build/bench-simple-direct --benchmark_format=csv
	@echo
	./build/bench-simple-semi-pers --benchmark_format=csv
	@echo
	./build/bench-msubstr --benchmark_format=csv
	@echo
	./build/bench-msubstr-direct --benchmark_format=csv
	@echo
	./build/bench-msubstr-semi-pers --benchmark_format=csv
	@echo
	./build/bench-queries-direct --benchmark_format=csv
	@echo
	./build/bench-queries-semi-pers --benchmark_format=csv
	@echo
	./build/bench-rich-simple --benchmark_format=csv
	@echo
	./build/bench-rich-direct --benchmark_format=csv
	@echo
	./build/bench-rich-semi-pers --benchmark_format=csv

# Cleaning
clean:
	rm -rf build
$(EXECS): build
	@cd build && make $@