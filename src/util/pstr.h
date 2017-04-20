#ifndef ICARUS_UTIL_PSTR_H
#define ICARUS_UTIL_PSTR_H
#include <string>
// permanent strings
struct pstr {
  char *ptr;

  pstr() : ptr(nullptr) {}
  pstr(const char *c_string);
  pstr(const pstr& p);
  ~pstr() {}

  inline operator std::string() { return std::string(ptr); }
  inline operator const char *() { return ptr; }

  static char *current_block;
  static char *current_head;
  static size_t current_space;
};

#endif // ICARUS_UTIL_PSTR_H
