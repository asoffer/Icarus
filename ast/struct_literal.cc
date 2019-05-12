#include "ast/struct_literal.h"

#include <sstream>
#include "ast/declaration.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

namespace ir {
// TODO: The functions here that modify struct fields typically do so by
// modifying the last field, since we always build them in order. This saves us
// from having to pass extra information and thereby bloating all commands. At
// some point we should switch to a buffer-chunk system so that one won't bloat
// another.
Reg CreateStruct(core::Scope const *scope,
                      ast::StructLiteral const *parent);
void CreateStructField(Reg struct_type,
                       RegisterOr<type::Type const *> type);
void SetStructFieldName(Reg struct_type, std::string_view field_name);
void AddHashtagToField(Reg struct_type, ast::Hashtag hashtag);
void AddHashtagToStruct(Reg struct_type, ast::Hashtag hashtag);
Reg FinalizeStruct(Reg r);

Reg ArgumentCache(ast::StructLiteral const *sl);
}  // namespace ir

namespace ast {
std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "struct (";
  for (auto &a : args_) { ss << a.to_string(n) << ", "; }
  ss << ") {\n";
  for (const auto &f : fields_) {
    ss << std::string((n + 1) * 2, ' ') << f.to_string(n) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

}  // namespace ast
