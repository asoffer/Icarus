MAKEFLAGS += --jobs=4

TESTS := $(shell find src -name *_test.cc 2>/dev/null)
TEST_TARGETS := $(patsubst src/%.cc,bin/test/%,$(TESTS))

SRCS := $(shell find src -name *.cc ! -name *_test.cc 2>/dev/null)
SRC_OBJS := $(patsubst src/%.cc,build/%.o,$(SRCS))
TARGET := bin/$(shell basename `pwd`)


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
	@mkdir -p `dirname build/%.o`
	@time $(COMPILER) $(LLVM_CXX) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) -c src/$*.cc -o build/$*.o
	@echo "Above time is for: " $< "\n"

$(TARGET): $(SRC_OBJS)
	@echo -n Linking...
	@$(COMPILER) $(LINK_FLAGS) $(SRC_OBJS) $(LLVM_LINK) -o $@
	@echo Done.

test: $(TEST_TARGETS)

bin/test/%: src/%.cc
	@mkdir -p `dirname bin/test/$*.cc`
	@$(COMPILER) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) src/$*.cc -o $@

clean:
	@rm -f $(TARGET) $(SRC_OBJS) $(TEST_TARGETS)

help:
	@echo "      TARGET: $(TARGET)"
	@echo "        SRCS: $(SRCS)"
	@echo "    SRC_OBJS: $(SRC_OBJS)"
	@echo "       TESTS: $(TESTS)"
	@echo "TEST_TARGETS: $(TEST_TARGETS)"

wc:
	@wc src/*.* src/*/*.*
