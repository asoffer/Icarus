#include "pstr.h"
#include <cstring>
#include <cassert>
#include <cstdlib>

static constexpr size_t max_string_length = 512;
static constexpr size_t page_size         = 4096;
static constexpr size_t malloc_overhead   = 16;

char *pstr::current_block = nullptr;
char *pstr::current_head = nullptr;
size_t pstr::current_space = 0;

pstr::pstr(const char *c_string) {
  size_t input_length = std::strlen(c_string) + 1; // including null-terminator

  assert(input_length <= max_string_length);

  if (input_length > current_space) {
    current_block = (char *)malloc(page_size - malloc_overhead);
    current_head = current_block;
    current_space = page_size - malloc_overhead;
  }

  std::memcpy(current_head, c_string, input_length);
  ptr = current_head;
  current_head += input_length;
  current_space -= input_length;
}

pstr::pstr(const pstr &p) : ptr(p.ptr) {}
