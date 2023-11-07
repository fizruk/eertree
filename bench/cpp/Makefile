# Cleaning
clean:
	rm -rf build

.PHONY: build

BUILD_ALL_RUN := $(shell if [ -f build/CMakeCache.txt ]; then echo "1"; else echo "0"; fi)

build:
    ifeq ($(BUILD_ALL_RUN),1)
	$(info build/all has been run before. Running partial build...)
	cd build && make
    else 
	$(info build/all has not been run before. Running build/all...)
	$(MAKE) build/all
    endif

build/all: clean
	mkdir -p build
	cd build && cmake .. && make

run: build
	./build/bench1 --benchmark_format=csv
	@echo
	./build/bench2 --benchmark_format=csv
	@echo
	./build/bench3 --benchmark_format=csv
	@echo
	./build/bench4 --benchmark_format=csv
	@echo
	./build/bench5 --benchmark_format=csv