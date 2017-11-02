MAKEFLAGS += -j

TARGET  := bin/$(shell basename `pwd`)

SOURCES := $(shell find src -name *.cc 2>/dev/null)
OBJECTS := $(patsubst src/%.cc,build/%.o,$(SOURCES))

COMPILER := g++
BUILD_FLAGS := -g -O0 -D DEBUG
STDS = -std=gnu++17
WARN = -Wall -Wextra -Wconversion -Werror -Wuninitialized -Wpedantic #-Weffc++
OPTS = -iquote$(shell pwd)/src
# LLVM_CXX = $(shell llvm-config --cxxflags)
# LLVM_LINK = $(shell llvm-config --cxxflags --ldflags --system-libs --libs)

all: $(TARGET)

.PHONY: release
release: BUILD_FLAGS := -O3
release: $(TARGET)

build/%.o: src/%.cc
	@$(COMPILER) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) -c src/$*.cc -o build/$*.o

$(TARGET): $(OBJECTS)
	@echo -n Linking...
	@$(COMPILER) $(LINK_FLAGS) $(OBJECTS) -o $@
	@echo Done.

rebuild:
	@make clean
	@make header
	@make unity

clean:
	@rm -f $(TARGET) $(OBJECTS)

help:
	@echo "TARGET  : $(TARGET)"
	@echo "SOURCES : $(SOURCES)"
	@echo "OBJECTS : $(OBJECTS)"
	@echo "DEPENDS : $(DEPENDS)"
#	@echo "LLVM_CXX : $(LLVM_CXX)"
#	@echo "LLVM_LINK : $(LLVM_LINK)"

wc:
	@wc src/*.* src/*/*.*
