MAKEFLAGS =+ -j

TARGET  := bin/$(shell basename `pwd`)

SOURCES := $(shell find src -name *.cpp 2>/dev/null)
OBJECTS := $(patsubst src/%.cpp,build/%.o,$(SOURCES))
DEPENDS := $(SOURCES:src/%.cpp=build/%.d)

COMPILER := clang++
BUILD_FLAGS := -g -O0 -D DEBUG
STDS = -std=c++11
WARN = -Wall -Wextra -Wconversion -Werror
OPTS = -iquote$(shell pwd)/src

all: $(TARGET)

.PHONY: release
release: BUILD_FLAGS := -O3
release: $(TARGET)

build/%.o: src/%.cpp
	@mkdir -p $(@D)
	@$(COMPILER) -MM $(STDS) $(OPTS) src/$*.cpp -MF build/$*.d
	@$(COMPILER) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) -c src/$*.cpp -o build/$*.o

-include $(DEPENDS)

$(TARGET): $(OBJECTS)
	@$(COMPILER) $(OBJECTS) -o $@

unity:
	@rm -f build/unity.cpp
	@printf '$(patsubst src/%.cpp,#include "%.cpp"\n,$(SOURCES))' > build/unity.cpp
	@$(COMPILER) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) build/unity.cpp -o $(TARGET)

clean:
	@rm -f $(TARGET) $(OBJECTS) $(DEPENDS)

help:
	@echo "TARGET  : $(TARGET)"
	@echo "SOURCES : $(SOURCES)"
	@echo "OBJECTS : $(OBJECTS)"
	@echo "DEPENDS : $(DEPENDS)"
