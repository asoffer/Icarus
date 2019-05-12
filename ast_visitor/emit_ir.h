#ifndef ICARUS_AST_VISITOR_EMIT_IR_H
#define ICARUS_AST_VISITOR_EMIT_IR_H

#include <vector>

#include "ast/ast_fwd.h"
#include "ir/addr.h"
#include "ir/register.h"
#include "type/typed_value.h"

struct Context;

namespace ir {
struct Results;
}  // namespace ir

namespace ast_visitor {

struct EmitIr {
#define ICARUS_AST_NODE_X(name)                                                \
  ir::Results Val(ast::name const *node, Context *ctx) const;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::Node const *node,
                                            Context *ctx) const { UNREACHABLE(node); }
  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::Access const *node,
                                            Context *ctx) const;
  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::CommaList const *node,
                                            Context *ctx) const;
  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::Identifier const *node,
                                            Context *ctx) const;
  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::Index const *node,
                                            Context *ctx) const;
  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::Unop const *node,
                                            Context *ctx) const;

  void MoveInit(ast::Node const *, type::Typed<ir::Reg> reg,
                Context *ctx) const {
    UNREACHABLE();
  }
  void MoveInit(ast::Expression const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
  void MoveInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
  void MoveInit(ast::CommaList const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
  void MoveInit(ast::Unop const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;

  void CopyInit(ast::Node const *, type::Typed<ir::Reg> reg,
                Context *ctx) const {
    UNREACHABLE();
  }
  void CopyInit(ast::Expression const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
  void CopyInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
  void CopyInit(ast::CommaList const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
  void CopyInit(ast::Unop const *, type::Typed<ir::Reg> reg,
                Context *ctx) const;
};

}  // namespace ast_visitor

#endif  // ICARUS_AST_VISITOR_EMIT_IR_H
