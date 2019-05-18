#ifndef ICARUS_VISITOR_EMIT_IR_H
#define ICARUS_VISITOR_EMIT_IR_H

#include <vector>

#include "ast/ast_fwd.h"
#include "ir/addr.h"
#include "ir/register.h"
#include "type/type_fwd.h"
#include "type/typed_value.h"

struct Context;

namespace ir {
struct Results;
}  // namespace ir

namespace visitor {

struct EmitIr {
  // AST-related IR-emission functions
#define ICARUS_AST_NODE_X(name)                                                \
  ir::Results Val(ast::name const *node, Context *ctx) const;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  std::vector<ir::RegisterOr<ir::Addr>> Ref(ast::Node const *node,
                                            Context *ctx) const {
    UNREACHABLE(node);
  }
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

  // Type-related IR-emission functions
  void DefaultInit(type::Type const *, ir::Reg, Context *) const {
    UNREACHABLE();
  }
  void DefaultInit(type::Array const *t, ir::Reg reg, Context *ctx) const;
  void DefaultInit(type::Flags const *t, ir::Reg reg, Context *ctx) const;
  void DefaultInit(type::Pointer const *t, ir::Reg reg, Context *ctx) const;
  void DefaultInit(type::Primitive const *t, ir::Reg reg, Context *ctx) const;
  void DefaultInit(type::Struct const *t, ir::Reg reg, Context *ctx) const;
  void DefaultInit(type::Tuple const *t, ir::Reg reg, Context *ctx) const;

  void CopyAssign(type::Type const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const {
    UNREACHABLE();
  }

  void CopyAssign(type::Array const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Enum const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Flags const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Function const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Pointer const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Primitive const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Struct const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Tuple const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void CopyAssign(type::Variant const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;

  void MoveAssign(type::Type const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const {
    UNREACHABLE();
  }
  void MoveAssign(type::Array const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Enum const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Flags const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Function const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Pointer const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Primitive const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Struct const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Tuple const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;
  void MoveAssign(type::Variant const *t, ir::RegisterOr<ir::Addr> to,
                  type::Typed<ir::Results> const &from, Context *ctx) const;

  void Destroy(type::Type const *, ir::Reg, Context *) const { UNREACHABLE(); }
  void Destroy(type::Struct const *t, ir::Reg reg, Context *ctx) const;
  void Destroy(type::Variant const *t, ir::Reg reg, Context *ctx) const;
  void Destroy(type::Tuple const *t, ir::Reg reg, Context *ctx) const;
  void Destroy(type::Array const *t, ir::Reg reg, Context *ctx) const;

  void Print(type::Type const *, ir::Results const &, Context *) const {
    UNREACHABLE();
  }

  void Print(type::Array const *t, ir::Results const &val,
             Context *ctx) const;
  void Print(type::Enum const *t, ir::Results const &val,
             Context *ctx) const;
  void Print(type::Flags const *t, ir::Results const &val,
             Context *ctx) const;
  void Print(type::Pointer const *t, ir::Results const &val,
             Context *ctx) const;
  void Print(type::Primitive const *t, ir::Results const &val,
             Context *ctx) const;
  void Print(type::Tuple const *t, ir::Results const &val,
             Context *ctx) const;
  void Print(type::Variant const *t, ir::Results const &val,
             Context *ctx) const;

  void CopyInit(type::Type const *from_type, ir::Results const &from_val,
                type::Typed<ir::Reg> to_var, Context *ctx) const;
  void MoveInit(type::Type const *from_type, ir::Results const &from_val,
                type::Typed<ir::Reg> to_var, Context *ctx) const;
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_EMIT_IR_H
