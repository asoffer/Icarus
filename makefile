MAKEFLAGS += --jobs=10

TESTS := $(shell find src -name *_test.cc 2>/dev/null)
TEST_TARGETS := $(patsubst src/%.cc,bin/test/%,$(TESTS))

SRCS := $(shell find src -name *.cc ! -name *_test.cc 2>/dev/null)
SRC_OBJS := $(patsubst src/%.cc,build/%.o,$(SRCS))
TARGET := bin/$(shell basename `pwd`)

COMPILER := g++
BUILD_FLAGS := -g -O0 -DDBG -rdynamic
STDS = -std=c++17
WARN = -Wno-unused-but-set-parameter -Wno-unused-variable -Wall -Wextra -Werror -Wuninitialized -Wpedantic -Wno-unused-parameter #-Weffc++
OPTS = -iquote$(shell pwd)/src -fno-exceptions
LINK_FLAGS = -rdynamic -pthread -ldl -lstdc++fs


all: $(TARGET)

.PHONY: release
release: BUILD_FLAGS := -O3
release: $(TARGET)

.PHONY: llvm
llvm: LLVM_CXX = -I/usr/lib/llvm/include -fPIC -fvisibility-inlines-hidden -Werror=date-time -std=c++17 -Wall -W -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wno-missing-field-initializers -pedantic -Wno-long-long -Wno-maybe-uninitialized -Wdelete-non-virtual-dtor -Wno-comment -ffunction-sections -fdata-sections -O3 -fno-exceptions -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
llvm: LLVM_LINK = $(shell /usr/lib/llvm/bin/llvm-config --cxxflags --ldflags --system-libs --libs)
llvm: OPTS += -DICARUS_USE_LLVM
# mild modifications to $(shell /usr/lib/llvm/bin/llvm-config --cxxflags)
# -std=c++11 -> -std=c++17
# removed -fno-rtti so I could use dynamic cast
llvm: $(TARGET)

build/%.o: src/%.cc
	@mkdir -p `dirname build/$*.o`
	@time $(COMPILER) $(LLVM_CXX) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) -c src/$*.cc -o build/$*.o
	@echo "Above time is for: " $< "\n"

$(TARGET): $(SRC_OBJS)
	@echo -n Linking...
	@mkdir -p `dirname $(TARGET)`
	@$(COMPILER) $(SRC_OBJS) $(LLVM_LINK) $(LINK_FLAGS) -o $@
	@echo Done.

.PHONY: number_test 
number_test: build/frontend/numbers.o build/frontend/numbers_test.o
	@mkdir -p `dirname bin/test/$*.cc`
	@$(COMPILER) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) src/frontend/numbers.cc src/frontend/numbers_test.cc -o bin/test/frontend/$@

bin/test/%: src/%.cc
	@mkdir -p `dirname bin/test/$*.cc`
	@$(COMPILER) $(STDS) $(OPTS) $(WARN) $(BUILD_FLAGS) src/$*.cc -o $@

.PHONY: unity
unity:
	@cat $(SRCS) > build/unity.cc
	@echo Building...
	@time $(COMPILER) $(LLVM_CXX) $(STDS) $(OPTS) $(WARN) -O3 -c build/unity.cc -o build/unity.o
	@echo Linking...
	@mkdir -p `dirname $(TARGET)`
	@$(COMPILER) $(LINK_FLAGS) build/unity.o $(LLVM_LINK) -o bin/icarus
	@echo DONE

.PHONY: clean
clean:
	@rm -f $(TARGET) $(SRC_OBJS) $(TEST_TARGETS)

.PHONY: help
help:
	@echo "      TARGET: $(TARGET)"
	@echo "        SRCS: $(SRCS)"
	@echo "    SRC_OBJS: $(SRC_OBJS)"
	@echo "       TESTS: $(TESTS)"
	@echo "TEST_TARGETS: $(TEST_TARGETS)"

.PHONY: wc
wc:
	@wc src/*.* src/*/*.*
