#ifndef ICARUS_PRECOMPILED_H
#define ICARUS_PRECOMPILED_H

// Common standard headers
#include <iostream>
#include <string>
#include <map>
#include <set>
#include <vector>
#include <queue>
#include <stack>
#include <sstream>
#include <fstream>
#include <memory>

// TODO Figure out what you need from this.
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "constants_and_enums.h"
#include "base/types.h"
#include "util/pstr.h"

template <typename T, typename... Args> std::unique_ptr<T> make_unique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

namespace llvm {
class Value;
class Function;
} // namespace llvm

namespace AST {
struct Node;
struct Expression;
struct Identifier;
struct TokenNode;
struct Declaration;
struct Binop;
struct Statements;
struct FunctionLiteral;
struct ScopeLiteral;
struct CodeBlock;
} // namespace AST

struct Type;
struct Primitive;
struct Array;
struct Tuple;
struct Pointer;
struct Function;
struct Enum;
struct Struct;
struct ParamStruct;
struct TypeVariable;
struct RangeType;
struct SliceType;
struct Scope_Type;

struct Scope;
struct BlockScope;
struct FnScope;

using NPtrVec = std::vector<AST::Node *>;

namespace IR {
struct Block;
struct LocalStack;
struct StackFrame;
struct Func;
} // namespace IR


inline size_t MoveForwardToAlignment(size_t ptr, size_t alignment) {
  return ((ptr - 1) | (alignment - 1)) + 1;
}

#endif // ICARUS_PRECOMPILED_H
