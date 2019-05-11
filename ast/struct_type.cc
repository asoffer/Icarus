#include "ast/struct_type.h"

#include <sstream>
#include "misc/context.h"
#include "type/type.h"

namespace ast {
std::string StructType::to_string(size_t n) const {
  if (args_.empty()) { return "[; struct]"; }
  std::stringstream ss;
  auto iter = args_.begin();
  ss << "[" << (**iter++).to_string(n);
  for (; iter != args_.end(); ++iter) { ss << ", " << (**iter).to_string(n); }
  ss << "; struct]";
  return ss.str();
}

void StructType::DependentDecls(DeclDepGraph *g,
                                Declaration *d) const {
  for (auto &arg : args_) { arg->DependentDecls(g, d); }
}

ir::Results StructType::EmitIr(Context *ctx) { NOT_YET(); }
}  // namespace ast
