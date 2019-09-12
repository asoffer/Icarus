#ifndef ICARUS_VISITOR_EMIT_IR_H
#define ICARUS_VISITOR_EMIT_IR_H

#include <vector>

#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "ir/addr.h"
#include "ir/reg.h"
#include "ir/register.h"
#include "type/type_fwd.h"
#include "type/typed_value.h"
#include "visitor/deferred_body.h"

struct Context;
struct Module;

namespace ir {
struct Builder;
struct Results;
}  // namespace ir

namespace visitor {

struct EmitIr : public DeferredBody<EmitIr> {
  EmitIr();
  EmitIr(EmitIr const &) = delete;
  EmitIr(EmitIr &&)      = delete;

  // AST-related IR-emission functions
  ir::Results Val(ast::Node const *node, Context *ctx);
#define ICARUS_AST_NODE_X(name)                                                \
  ir::Results Val(ast::name const *node, Context *ctx);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  std::vector<ir::RegOr<ir::Addr>> Ref(ast::Node const *node, Context *ctx) {
    UNREACHABLE(node);
  }
  std::vector<ir::RegOr<ir::Addr>> Ref(ast::Access const *node, Context *ctx);
  std::vector<ir::RegOr<ir::Addr>> Ref(ast::CommaList const *node,
                                       Context *ctx);
  std::vector<ir::RegOr<ir::Addr>> Ref(ast::Identifier const *node,
                                       Context *ctx);
  std::vector<ir::RegOr<ir::Addr>> Ref(ast::Index const *node, Context *ctx);
  std::vector<ir::RegOr<ir::Addr>> Ref(ast::Unop const *node, Context *ctx);

  void MoveInit(ast::Node const *, type::Typed<ir::Reg> reg, Context *ctx) {
    UNREACHABLE();
  }
  void MoveInit(ast::Expression const *, type::Typed<ir::Reg> reg,
                Context *ctx);
  void MoveInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg,
                Context *ctx);
  void MoveInit(ast::CommaList const *, type::Typed<ir::Reg> reg, Context *ctx);
  void MoveInit(ast::Unop const *, type::Typed<ir::Reg> reg, Context *ctx);

  void CopyInit(ast::Node const *, type::Typed<ir::Reg> reg, Context *ctx) {
    UNREACHABLE();
  }
  void CopyInit(ast::Expression const *, type::Typed<ir::Reg> reg,
                Context *ctx);
  void CopyInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg,
                Context *ctx);
  void CopyInit(ast::CommaList const *, type::Typed<ir::Reg> reg, Context *ctx);
  void CopyInit(ast::Unop const *, type::Typed<ir::Reg> reg, Context *ctx);

  // Type-related IR-emission functions
  void DefaultInit(type::Type const *t, ir::Reg, Context *);
  void DefaultInit(type::Array const *t, ir::Reg reg, Context *ctx);
  void DefaultInit(type::Flags const *t, ir::Reg reg, Context *ctx);
  void DefaultInit(type::Pointer const *t, ir::Reg reg, Context *ctx);
  void DefaultInit(type::Primitive const *t, ir::Reg reg, Context *ctx);
  void DefaultInit(type::Struct const *t, ir::Reg reg, Context *ctx);
  void DefaultInit(type::Tuple const *t, ir::Reg reg, Context *ctx);

  void EmitCopyAssign(type::Type const *t, ir::RegOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) {
    UNREACHABLE();
  }

  void CopyInit(type::Type const *from_type, ir::Results const &from_val,
                type::Typed<ir::Reg> to_var, Context *ctx);
  void MoveInit(type::Type const *from_type, ir::Results const &from_val,
                type::Typed<ir::Reg> to_var, Context *ctx);

  ir::Builder &builder() { return bldr_; };

 private:
  ir::Builder &bldr_;
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_EMIT_IR_H
