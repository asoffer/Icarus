#include "ast/ast.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/slice.h"

namespace compiler {

absl::Span<type::QualType const> Compiler::VerifyType(ast::Terminal const *node) {
  type::Type t;
  base::MetaValue mv = node->type();
  if (mv == base::meta<ir::Integer>) {
    t = type::Integer;
  } else if (mv == base::meta<bool>) {
    t = type::Bool;
  } else if (mv == base::meta<ir::Char>) {
    t = type::Char;
  } else if (mv == base::meta<float>) {
    t = type::F32;
  } else if (mv == base::meta<double>) {
    t = type::F64;
  } else if (mv == base::meta<ir::addr_t>) {
    t = node->value().get<ir::addr_t>() ? type::Slc(type::Char) : type::NullPtr;
  } else if (mv == base::meta<type::Type>) {
    t = type::Type_;
  } else {
    UNREACHABLE(mv.name());
  }
  return context().set_qual_type(node, type::QualType::Constant(t));
}

bool Compiler::VerifyPatternType(ast::Terminal const *node, type::Type t) {
  return VerifyType(node)[0].type() == t;
}

}  // namespace compiler
