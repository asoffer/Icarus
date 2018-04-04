MAKEFLAGS += --jobs=4

TARGET  := bin/$(shell basename `pwd`)

SOURCES := $(shell find src -name *.cc 2>/dev/null)
OBJECTS := $(patsubst src/%.cc,build/%.o,$(SOURCES))

COMPILER := g++-7
BUILD_FLAGS := -g -O0 -D DBG -rdynamic
STDS = -std=c++17
WARN = -Wno-unused-but-set-parameter -Wno-unused-variable -Wall -Wextra -Werror -Wuninitialized -Wpedantic -Wno-unused-parameter #-Weffc++
OPTS = -iquote$(shell pwd)/src
LINK_FLAGS = -rdynamic -pthread
LLVM_CXX = -I/usr/lib/llvm/include  -fPIC -fvisibility-inlines-hidden -Werror=date-time -std=c++17 -Wall -W -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wno-missing-field-initializers -pedantic -Wno-long-long -Wno-maybe-uninitialized -Wdelete-non-virtual-dtor -Wno-comment -ffunction-sections -fdata-sections -O3 -fno-exceptions -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
# mild modifications to $(shell /usr/lib/llvm/bin/llvm-config --cxxflags)
# -std=c++11 -> -std=c++17
# removed -fno-rtti so I could use dynamic cast

LLVM_LINK = $(shell /usr/lib/llvm/bin/llvm-config --cxxflags --ldflags --system-libs --libs)

all: $(TARGET)

.PHONY: release
release: BUILD_FLAGS := -O3
release: $(TARGET)

build/%.o: src/%.cc
	@time $(COMPILER) $(LLVM_CXX) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) -c src/$*.cc -o build/$*.o
	@echo "Above time is for: " $< "\n"

$(TARGET): $(OBJECTS)
	@echo -n Linking...
	@$(COMPILER) $(LINK_FLAGS) $(OBJECTS) $(LLVM_LINK) -o $@
	@echo Done.

clean:
	@rm -f $(TARGET) $(OBJECTS)

help:
	@echo "TARGET  : $(TARGET)"
	@echo "SOURCES : $(SOURCES)"
	@echo "OBJECTS : $(OBJECTS)"
	@echo "DEPENDS : $(DEPENDS)"
	@echo "LLVM_CXX : $(LLVM_CXX)"
	@echo "LLVM_LINK : $(LLVM_LINK)"

wc:
	@wc src/*.* src/*/*.*
