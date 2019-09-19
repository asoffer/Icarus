#ifndef ICARUS_VISITOR_TRADITIONAL_COMPILATION_H
#define ICARUS_VISITOR_TRADITIONAL_COMPILATION_H

#include "ir/builder.h"
#include "ir/results.h"
#include "misc/context.h"
#include "type/type_fwd.h"
#include "visitor/verify_result.h"
#include "visitor/deferred_body.h"

struct Module;

namespace visitor {
// TODO: Come up with a better name.
//
// These are the steps in a traditional compiler of verifying types and emitting
// code. They're tied together because they don't necessarily happen in a
// particular order. Certainly for any given AST node we need to verify its type
// before emitting code for it. However, we may need to emit and execute code
// for some nodes to compute a type at compile-time. For this reason these steps
// require the same contextual data and therefore should be placed into a simple
// visitor type.
//
// Note that tying these together has a cost. C++ ties together these steps as
// well as parsing and it has compile-time performance cost as well as language
// semantic restrictions. This was design was not chosen lightly. We believe
// that the primary problem with C++ is that parsing is lumped together with
// type-verification and code generation and that the primary benefits that
// surface from separation are from separating parsing from these two stages
// rather than separating all stages. In time we will see if this belief holds
// water.

struct TraditionalCompilation : public DeferredBody<TraditionalCompilation> {
  TraditionalCompilation(Module *mod);

  Module *module() { return mod_; }
  ir::Builder &builder() { return bldr_; };
  Context &context() { return ctx_; }
  error::Log *error_log() { return &module()->error_log_; }

  ir::Results EmitValue(ast::Node const *node) { UNREACHABLE(node); };
#define ICARUS_AST_NODE_X(name) ir::Results EmitValue(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  VerifyResult VerifyType(ast::Node const *node) { UNREACHABLE(node); };
#define ICARUS_AST_NODE_X(name)                                                \
  VerifyResult VerifyType(ast::name const *node);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  VerifyResult VerifyConcreteFnLit(ast::FunctionLiteral const *node);

  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Node const *node) {
    UNREACHABLE(node);
  }
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Access const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::CommaList const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Identifier const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Index const *node);
  std::vector<ir::RegOr<ir::Addr>> EmitRef(ast::Unop const *node);

  void EmitPrint(type::Type const *, ir::Results const &) { UNREACHABLE(); }
  void EmitPrint(type::Array const *t, ir::Results const &val);
  void EmitPrint(type::Enum const *t, ir::Results const &val);
  void EmitPrint(type::Flags const *t, ir::Results const &val);
  void EmitPrint(type::Pointer const *t, ir::Results const &val);
  void EmitPrint(type::Primitive const *t, ir::Results const &val);
  void EmitPrint(type::Tuple const *t, ir::Results const &val);
  void EmitPrint(type::Variant const *t, ir::Results const &val);

  void EmitDestroy(type::Type const *, ir::Reg) { UNREACHABLE(); }
  void EmitDestroy(type::Struct const *t, ir::Reg reg);
  void EmitDestroy(type::Variant const *t, ir::Reg reg);
  void EmitDestroy(type::Tuple const *t, ir::Reg reg);
  void EmitDestroy(type::Array const *t, ir::Reg reg);

  void EmitCopyAssign(type::Type const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from) {
    UNREACHABLE();
  }
  void EmitCopyAssign(type::Array const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Enum const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Flags const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Function const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Struct const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitCopyAssign(type::Variant const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);

  void EmitMoveAssign(type::Type const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from) {
    UNREACHABLE();
  }
  void EmitMoveAssign(type::Array const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Enum const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Flags const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Function const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Pointer const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Primitive const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Struct const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Tuple const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);
  void EmitMoveAssign(type::Variant const *t, ir::RegOr<ir::Addr> to,
                      type::Typed<ir::Results> const &from);

  void EmitDefaultInit(type::Type const *t, ir::Reg) { UNREACHABLE(); }
  void EmitDefaultInit(type::Array const *t, ir::Reg reg);
  void EmitDefaultInit(type::Flags const *t, ir::Reg reg);
  void EmitDefaultInit(type::Pointer const *t, ir::Reg reg);
  void EmitDefaultInit(type::Primitive const *t, ir::Reg reg);
  void EmitDefaultInit(type::Struct const *t, ir::Reg reg);
  void EmitDefaultInit(type::Tuple const *t, ir::Reg reg);

  void EmitMoveInit(ast::Node const *, type::Typed<ir::Reg> reg) {
    UNREACHABLE();
  }
  void EmitMoveInit(ast::Expression const *, type::Typed<ir::Reg> reg);
  void EmitMoveInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg);
  void EmitMoveInit(ast::CommaList const *, type::Typed<ir::Reg> reg);
  void EmitMoveInit(ast::Unop const *, type::Typed<ir::Reg> reg);

  void EmitMoveInit(type::Type const *from_type, ir::Results const &from_val,
                    type::Typed<ir::Reg> to_var);

  void EmitCopyInit(ast::Node const *, type::Typed<ir::Reg> reg) {
    UNREACHABLE();
  }
  void EmitCopyInit(ast::Expression const *, type::Typed<ir::Reg> reg);
  void EmitCopyInit(ast::ArrayLiteral const *, type::Typed<ir::Reg> reg);
  void EmitCopyInit(ast::CommaList const *, type::Typed<ir::Reg> reg);
  void EmitCopyInit(ast::Unop const *, type::Typed<ir::Reg> reg);

  void EmitCopyInit(type::Type const *from_type, ir::Results const &from_val,
                    type::Typed<ir::Reg> to_var);

 private:
  Module *mod_;
  Context ctx_;
  ir::Builder &bldr_;
};
}  // namespace visitor

#endif  // ICARUS_VISITOR_TRADITIONAL_COMPILATION_H
