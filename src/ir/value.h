#ifndef ICARUS_VALUE_H
#define ICARUS_VALUE_H

#include "../base/types.h"
#include "../constants_and_enums.h"
#include <string>

namespace AST {
struct ScopeLiteral;
struct CodeBlock;
} // namespace AST

struct Type;

namespace IR {
struct Block;
struct Func;
struct Loc;
struct Val;
} // namespace IR

namespace IR {
struct Value {
  union {
    IR::Val *as_val;
    IR::Loc *as_loc;
    const char *as_ext_fn;
    char *as_cstr;
    Block *as_block;
    void *as_heap_addr;
    size_t as_global_cstr;
    size_t as_blah;
  };

  ValType flag;

  explicit Value(char *p) : as_cstr(p), flag(ValType::CStr) {}
  explicit Value(Block *b) : as_block(b), flag(ValType::Block) {}

  Value() : flag(ValType::None) {}
  ~Value();
  Value(const Value &v);
  Value &operator=(const Value &v);

  static Value GlobalCStr(size_t n);
  static Value None();
  static Value Error();
  static Value CreateGlobal();
  static Value HeapAddr(void *ptr);
  static Value ExtFn(const char *name);

#define VAL_MACRO(TypeName, type_name, cpp_type)                               \
  static Value TypeName(cpp_type x);
#include "../config/val.conf"
#undef VAL_MACRO

  static Value Reg(size_t n);
  static Value Arg(size_t n);
  static Value StackAddr(size_t n);
  static Value FrameAddr(size_t n);

};

Order ArbitraryOrdering(const Val *lhs, const Val *rhs);
bool operator<(const Value &lhs, const Value &rhs);
bool operator==(const Value &lhs, const Value &rhs);

inline bool operator!=(const Value &lhs, const Value &rhs) {
  return !(lhs == rhs);
}

std::ostream &operator<<(std::ostream &os, const Value &value);
} // namespace IR

#endif //ICARUS_VALUE_H
