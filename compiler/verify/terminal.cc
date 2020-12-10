#include <type_traits>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {

type::QualType Compiler::VerifyType(ast::Terminal const *node) {
  type::Type t;
  base::MetaValue mv = node->value().type();
  if (mv == base::meta<int8_t>) {
    t = type::I8;
  } else if (mv == base::meta<int16_t>) {
    t = type::I16;
  } else if (mv == base::meta<int32_t>) {
    t = type::I32;
  } else if (mv == base::meta<int64_t>) {
    t = type::I64;
  } else if (mv == base::meta<uint8_t>) {
    t = type::U8;
  } else if (mv == base::meta<uint16_t>) {
    t = type::U16;
  } else if (mv == base::meta<uint32_t>) {
    t = type::U32;
  } else if (mv == base::meta<uint64_t>) {
    t = type::U64;
  } else if (mv == base::meta<bool>) {
    t = type::Bool;
  } else if (mv == base::meta<float>) {
    t = type::F32;
  } else if (mv == base::meta<double>) {
    t = type::F64;
  } else if (mv == base::meta<ir::Addr>) {
    t = type::NullPtr;
  } else if (mv == base::meta<ir::String>) {
    t = type::ByteView;
  } else if (mv == base::meta<type::Type>) {
    t = type::Type_;
  } else {
    UNREACHABLE(mv.name());
  }
  return context().set_qual_type(node, type::QualType::Constant(t));
}

}  // namespace compiler
