#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "base/defer.h"
#include "base/guarded.h"
#include "compiler/dispatch/fn_call_table.h"
#include "compiler/dispatch/scope_table.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/executable_module.h"
#include "diagnostic/consumer/trivial.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/interpretter/evaluate.h"
#include "ir/interpretter/execution_context.h"
#include "ir/jump.h"
#include "ir/struct_field.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg.h"
#include "type/generic_struct.h"
#include "type/jump.h"
#include "type/type.h"
#include "type/typed_value.h"

// TODO: Value emission of any kind could fail even if there weren't type
// errors. Any value emission could be a constant-evaluated function that ends
// up dividing by zero. It's *possible* you that you can cover this stuff with
// property checking, but that's way off in the future.
namespace compiler {

type::QualType VerifyBody(Compiler *compiler, ast::FunctionLiteral const *node,
                          type::Type const *fn_type = nullptr);

namespace {

absl::flat_hash_map<ir::Jump const *, ir::ScopeDef const *> MakeJumpInits(
    Compiler *c, ast::OverloadSet const &os) {
  absl::flat_hash_map<ir::Jump const *, ir::ScopeDef const *> inits;
  DEBUG_LOG("ScopeNode")
  ("Overload set for inits has size ", os.members().size());
  for (ast::Expression const *member : os.members()) {
    DEBUG_LOG("ScopeNode")(member->DebugString());

    auto maybe_def = c->EvaluateAs<ir::ScopeDef *>(member);
    if (not maybe_def) { NOT_YET(); }
    auto *def = *maybe_def;
    DEBUG_LOG("ScopeNode")(def);
    if (def->work_item and *def->work_item) {
      (std::move(*def->work_item))();
      def->work_item = nullptr;
    }
    for (auto const *init : def->start_->after()) {
      bool success = inits.emplace(init, def).second;
      static_cast<void>(success);
      ASSERT(success == true);
    }
  }
  return inits;
}

void AddAdl(ast::OverloadSet *overload_set, std::string_view id,
            type::Type const *t) {
  absl::flat_hash_set<CompiledModule *> modules;
  // TODO t->ExtractDefiningModules(&modules);

  for (auto *mod : modules) {
    auto decls = mod->ExportedDeclarations(id);
    // TODO accessing module from another thread is bad news!
    auto &data = mod->data();
    diagnostic::TrivialConsumer consumer;
    module::FileImporter<LibraryModule> importer;
    for (auto *d : decls) {
      // TODO Wow this is a terrible way to access the type.
      ASSIGN_OR(continue, auto &t,
                Compiler({
                             .builder             = ir::GetBuilder(),
                             .data                = data,
                             .diagnostic_consumer = consumer,
                             .importer            = importer,
                         })
                    .type_of(d));
      // TODO handle this case. I think it's safe to just discard it.
      for (auto const *expr : overload_set->members()) {
        if (d == expr) { return; }
      }

      // TODO const
      overload_set->insert(d);
    }
  }
}

ast::OverloadSet FindOverloads(
    ast::Scope const *scope, std::string_view token,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  ast::OverloadSet os(scope, token);
  for (type::Typed<ir::Value> const &arg : args) {
    AddAdl(&os, token, arg.type());
  };
  DEBUG_LOG("FindOverloads")
  ("Found ", os.members().size(), " overloads for '", token, "'");
  return os;
}

std::optional<ast::OverloadSet> MakeOverloadSet(
    Compiler *c, ast::Expression const *expr,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  if (auto *id = expr->if_as<ast::Identifier>()) {
    return FindOverloads(expr->scope(), id->token(), args);
  } else if (auto *acc = expr->if_as<ast::Access>()) {
    ASSIGN_OR(return std::nullopt,  //
                     auto result, c->VerifyType(acc->operand()));
    if (result.type() == type::Module) {
      auto maybe_mod = c->EvaluateAs<ir::ModuleId>(acc->operand());
      if (not maybe_mod) { NOT_YET(); }
      auto const *mod         = maybe_mod->get<CompiledModule>();
      std::string_view member = acc->member_name();
      ast::OverloadSet os(mod->ExportedDeclarations(member));
      for (type::Typed<ir::Value> const &arg : args) {
        AddAdl(&os, member, arg.type());
      };
      return os;
    }
  }

  ast::OverloadSet os;
  os.insert(expr);
  // TODO ADL for node?
  return os;
}

template <typename NodeType>
base::move_func<void()> *DeferBody(Compiler::PersistentResources resources,
                                   NodeType const *node, type::Type const *t) {
  DEBUG_LOG("DeferBody")(node->DebugString());
  auto [iter, success] = resources.data.deferred_work_.lock()->emplace(
      node, [c = Compiler(resources), node, t]() mutable {
        if constexpr (base::meta<NodeType> ==
                          base::meta<ast::FunctionLiteral> or
                      base::meta<NodeType> ==
                          base::meta<ast::ShortFunctionLiteral>) {
          CompleteBody(&c, node, &t->as<type::Function>());
        } else {
          static_cast<void>(t);
          CompleteBody(&c, node);
        }
      });
  ASSERT(success == true);
  return &iter->second;
}

}  // namespace

ir::Value Compiler::EmitValue(ast::ArgumentType const *node) {
  return ir::Value(ASSERT_NOT_NULL(data().arg_type(node->name())));
}

ir::Value Compiler::EmitValue(ast::Assignment const *node) {
  std::vector<type::Typed<ir::RegOr<ir::Addr>>> lhs_refs;
  lhs_refs.reserve(node->lhs().size());

  // TODO understand the precise semantics you care about here and document
  // them. Must references be computed first?
  for (auto const *l : node->lhs()) {
    lhs_refs.push_back(type::Typed<ir::RegOr<ir::Addr>>(
        EmitRef(l), type::Ptr(ASSERT_NOT_NULL(data().qual_type(l))->type())));
  }

  auto ref_iter = lhs_refs.begin();
  for (auto const *r : node->rhs()) {
    auto from_qt          = *ASSERT_NOT_NULL(data().qual_type(r));
    size_t expansion_size = from_qt.expansion_size();
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> ref_span(&*ref_iter,
                                                                expansion_size);
    EmitAssign(r, ref_span);
    ref_iter += expansion_size;
  }
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::BlockLiteral const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  std::vector<ir::RegOr<ir::Fn>> befores;
  std::vector<ir::RegOr<ir::Jump *>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(EmitValue(decl).get<ir::RegOr<ir::Fn>>());
  }

  for (auto const &decl : node->after()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    afters.push_back(EmitValue(decl).get<ir::RegOr<ir::Jump *>>());
  }

  return ir::Value(builder().MakeBlock(data().add_block(), std::move(befores),
                                       std::move(afters)));
}

ir::Value Compiler::EmitValue(ast::BlockNode const *node) {
  UNREACHABLE("Should be called via Compiler::EmitBlockNode");
}

ir::Value Compiler::EmitValue(ast::BuiltinFn const *node) {
  return ir::Value(node->value());
}

// TODO: Checking if an AST node is a builtin is problematic because something as simple as
// ```
// f ::= bytes
// f(int)
// ```
// breaks.
//
ir::Value EmitBuiltinCall(Compiler *c, ast::BuiltinFn const *callee,
                          core::FnArgs<ast::Expression const *> const &args) {
  switch (callee->value().which()) {
    case ir::BuiltinFn::Which::Foreign: {
      auto maybe_name         = c->EvaluateAs<ir::String>(args[0]);
      auto maybe_foreign_type = c->EvaluateAs<type::Type const *>(args[1]);
      if (not maybe_name) { NOT_YET(); }
      if (not maybe_foreign_type) { NOT_YET(); }

      return ir::Value(
          c->builder().LoadSymbol(*maybe_name, *maybe_foreign_type).get());
    } break;

    case ir::BuiltinFn::Which::Opaque:
      return ir::Value(c->builder().OpaqueType(&c->data().module()));
    case ir::BuiltinFn::Which::Bytes: {
      auto const &fn_type = *ir::BuiltinFn::Bytes().type();
      ir::OutParams outs  = c->builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c->builder().Call(
          ir::Fn{ir::BuiltinFn::Bytes()}, &fn_type,
          {ir::Value(
              c->EmitValue(args[0]).get<ir::RegOr<type::Type const *>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::Alignment: {
      auto const &fn_type = *ir::BuiltinFn::Alignment().type();
      ir::OutParams outs  = c->builder().OutParams(fn_type.output());
      ir::Reg reg         = outs[0];
      c->builder().Call(
          ir::Fn{ir::BuiltinFn::Alignment()}, &fn_type,
          {ir::Value(
              c->EmitValue(args[0]).get<ir::RegOr<type::Type const *>>())},
          std::move(outs));

      return ir::Value(reg);
    } break;

    case ir::BuiltinFn::Which::DebugIr:
      c->builder().DebugIr();
      return ir::Value();
  }
  UNREACHABLE();
}

void Compiler::EmitMoveInit(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
          type::Typed{result, data().qual_type(node)->type()},
          EmitCopyAssignTag{});
  }

  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  // TODO: This is a pretty terrible hack.
  if (auto const *gen_struct_type =
          ASSERT_NOT_NULL(data().qual_type(node->callee()))
              ->type()
              ->if_as<type::GenericStruct>()) {
    type::Type const *s = gen_struct_type->concrete(args);
    Visit(type::Type_, *to[0],
          type::Typed<ir::Value>(ir::Value(s), type::Type_), EmitCopyAssignTag{});
    return;
  }

  auto const &os = data().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u) << "TODO: Support dynamic dispatch.";
  FnCallDispatchTable::EmitMoveInit(this, os.members().front(), args, to);
  // TODO node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}


void Compiler::EmitCopyInit(ast::Call const *node,
                        absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
          type::Typed{result, data().qual_type(node)->type()},
          EmitCopyAssignTag{});
  }

  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  // TODO: This is a pretty terrible hack.
  if (auto const *gen_struct_type =
          ASSERT_NOT_NULL(data().qual_type(node->callee()))
              ->type()
              ->if_as<type::GenericStruct>()) {
    type::Type const *s = gen_struct_type->concrete(args);
    Visit(type::Type_, *to[0],
          type::Typed<ir::Value>(ir::Value(s), type::Type_), EmitCopyAssignTag{});
    return;
  }

  auto const &os = data().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u) << "TODO: Support dynamic dispatch.";
  FnCallDispatchTable::EmitCopyInit(this, os.members().front(), args, to);
  // TODO node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}

void Compiler::EmitAssign(
    ast::Call const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    auto result = EmitBuiltinCall(this, b, node->args());
    if (result.empty()) return;
    Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
          type::Typed{result, data().qual_type(node)->type()},
          EmitCopyAssignTag{});
  }

  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  // TODO: This is a pretty terrible hack.
  if (auto const *gen_struct_type =
          ASSERT_NOT_NULL(data().qual_type(node->callee()))
              ->type()
              ->if_as<type::GenericStruct>()) {
    type::Type const *s = gen_struct_type->concrete(args);
    Visit(type::Type_, *to[0],
          type::Typed<ir::Value>(ir::Value(s), type::Type_),
          EmitCopyAssignTag{});
    return;
  }

  auto const &os = data().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u) << "TODO: Support dynamic dispatch.";
  return FnCallDispatchTable::EmitAssign(this, os.members().front(), args, to);
  // TODO node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}

ir::Value Compiler::EmitValue(ast::Call const *node) {
  if (auto *b = node->callee()->if_as<ast::BuiltinFn>()) {
    return EmitBuiltinCall(this, b, node->args());
  }

  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  // TODO: This is a pretty terrible hack.
  if (auto const *gen_struct_type =
          ASSERT_NOT_NULL(data().qual_type(node->callee()))
              ->type()
              ->if_as<type::GenericStruct>()) {
    type::Type const *s = gen_struct_type->concrete(args);
    return ir::Value(s);
  }

  auto qt = *ASSERT_NOT_NULL(data().qual_type(node));

  auto const &os = data().ViableOverloads(node->callee());
  ASSERT(os.members().size() == 1u) << "TODO: Support dynamic dispatch.";
  switch (qt.expansion_size()) {
    case 0:
      FnCallDispatchTable::EmitMoveInit(this, os.members().front(), args, {});
      return ir::Value();
    case 1: {
      // TODO: It'd be nice to not stack-allocate register-sized values.
      type::Typed<ir::RegOr<ir::Addr>> out(builder().TmpAlloca(qt.type()),
                                           qt.type());
      FnCallDispatchTable::EmitMoveInit(this, os.members().front(), args,
                                        absl::MakeConstSpan(&out, 1));
      return ir::Value(builder().PtrFix(out->reg(), qt.type()));
    }
    default: NOT_YET();
  }
  // TODO node->contains_hashtag(ast::Hashtag(ast::Hashtag::Builtin::Inline)));
}

void Compiler::EmitCopyInit(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  // TODO user-defined-types
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}

void Compiler::EmitMoveInit(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  // TODO user-defined-types
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitMoveAssignTag{});
}

ir::Value Compiler::EmitValue(ast::Cast const *node) {
  // TODO user-defined-types

  auto *to_type = ASSERT_NOT_NULL(type_of(node));
  auto results  = EmitValue(node->expr());
  if (to_type == type::Type_) {
    return ir::Value(results.get<type::Type const *>());
  }
  auto *from_type = type_of(node->expr());
  if (type::IsNumeric(from_type)) {
    if (type::IsIntegral(from_type)) {
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double,
                              ir::EnumVal, ir::FlagsVal>(
          to_type, [&](auto tag) {
            return ir::Value(builder().CastTo<typename decltype(tag)::type>(
                type::Typed(results, from_type)));
          });
    } else {
      return type::ApplyTypes<float, double>(to_type, [&](auto tag) {
        return ir::Value(builder().CastTo<typename decltype(tag)::type>(
            type::Typed(results, from_type)));
      });
    }
  } else {
    NOT_YET();
  }
}

ir::Value Compiler::EmitValue(ast::Declaration const *node) {
  // TODO: The entirety of constant-caching mechanism here is weird and broken
  // and needs a cleanup.
  DEBUG_LOG("EmitValueDeclaration")(node->id());
  if (node->flags() & ast::Declaration::f_IsConst) {
    if (node->module() != &data().module()) {
      // Constant declarations from other modules should already be stored on
      // that module. They must be at the root of the binding tree map,
      // otherwise they would be local to some function/jump/etc. and not be
      // exported.
      return node->module()->as<CompiledModule>().data().Constant(node)->value;
    }

    if (node->flags() & ast::Declaration::f_IsFnParam) {
      auto val = data().LoadConstantParam(node);
      DEBUG_LOG("EmitValueDeclaration")(val);
      return val;
    } else {
      if (auto *constant_value = data().Constant(node)) {
        // TODO: This feels quite hacky.
        if (node->init_val()->is<ast::StructLiteral>()) {
          if (not constant_value->complete and state_.must_complete) {
            DEBUG_LOG("compile-work-queue")
            ("Request work complete-struct: ", node);
            state_.work_queue.Enqueue({
                .kind     = WorkItem::Kind::CompleteStructMembers,
                .node     = node->init_val(),
                .context  = data(),
                .consumer = diag(),
            });
          }
        }
        return constant_value->value;
      }
      auto const *t = type_of(node);

      if (node->IsCustomInitialized()) {
        DEBUG_LOG("EmitValueDeclaration")
        ("Computing slot with ", node->init_val()->DebugString());
        auto maybe_val =
            Evaluate(type::Typed(node->init_val(), t), state_.must_complete);
        if (not maybe_val) {
          // TODO we reserved a slot and haven't cleaned it up. Do we care?
          NOT_YET("Found errors but haven't handled them.",
                  diag().num_consumed());
          return ir::Value();
        }

        DEBUG_LOG("EmitValueDeclaration")("Setting slot = ", *maybe_val);
        data().SetConstant(node, *maybe_val);

        // TODO: This is a struct-speficic hack.
        if (type::Type const **type_val =
                maybe_val->get_if<type::Type const *>()) {
          if (auto const *struct_type = (*type_val)->if_as<type::Struct>()) {
            if (struct_type->completeness() != type::Completeness::Complete) {
              return *maybe_val;
            }
            data().CompleteConstant(node);
          }
        }

        return *maybe_val;

      } else if (node->IsDefaultInitialized()) {
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
      UNREACHABLE(node->DebugString());
    }
  } else {
    if (node->IsUninitialized()) { return ir::Value(); }
    auto *t = type_of(node);
    auto a  = data().addr(node);
    if (node->IsCustomInitialized()) {
      auto to = type::Typed<ir::RegOr<ir::Addr>>(a, type::Ptr(t));
      EmitMoveInit(node->init_val(), absl::MakeConstSpan(&to, 1));
    } else {
      if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
        Visit(t, a, EmitDefaultInitTag{});
      }
    }
    return ir::Value(a);
  }
  UNREACHABLE();
}

ir::Value Compiler::EmitValue(ast::DesignatedInitializer const *node) {
  auto *t    = type_of(node);
  auto alloc = builder().TmpAlloca(t);
  auto typed_alloc = type::Typed<ir::RegOr<ir::Addr>>(
      ir::RegOr<ir::Addr>(alloc), type::Ptr(t));
  EmitMoveInit(node, {typed_alloc});
  return ir::Value(alloc);
}

void Compiler::EmitMoveInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto *t            = type_of(node);
  auto &struct_type  = t->as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().token();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    {
      auto reg = builder().Field(to[0]->reg(), &struct_type, i).get();
      if (field.initial_value.empty()) {
        Visit(field.type, reg, EmitDefaultInitTag{});
      } else {
        Visit(field.type, reg, type::Typed{field.initial_value, field.type},
              EmitCopyAssignTag{});
      }
    }
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    if (assignment->lhs().size() != 1) { NOT_YET(assignment->lhs().size()); }
    if (assignment->rhs().size() != 1) { NOT_YET(assignment->rhs().size()); }

    auto const &id     = assignment->lhs()[0]->as<ast::Identifier>();
    auto const *f      = struct_type.field(id.token());
    size_t field_index = struct_type.index(f->name);
    auto typed_reg = builder().Field(to[0]->reg(), &struct_type, field_index);
    type::Typed<ir::RegOr<ir::Addr>> lhs(*typed_reg, typed_reg.type());
    EmitMoveInit(assignment->rhs()[0], absl::MakeConstSpan(&lhs, 1));
  }
}

void Compiler::EmitCopyInit(
    ast::DesignatedInitializer const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  // TODO actual initialization with these field members.
  auto *t            = type_of(node);
  auto &struct_type  = t->as<type::Struct>();
  auto const &fields = struct_type.fields();
  for (size_t i = 0; i < fields.size(); ++i) {
    auto const &field = fields[i];

    for (auto const *assignment : node->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        std::string_view field_name = expr->as<ast::Identifier>().token();
        // Skip default initialization if we're going to use the designated
        // initializer.
        if (field_name == field.name) { goto next_field; }
      }
    }

    {
      auto reg = builder().Field(to[0]->reg(), &struct_type, i).get();
      if (field.initial_value.empty()) {
        Visit(field.type, reg, EmitDefaultInitTag{});
      } else {
        Visit(field.type, reg, type::Typed{field.initial_value, field.type},
              EmitCopyAssignTag{});
      }
    }
  next_field:;
  }

  for (auto const *assignment : node->assignments()) {
    if (assignment->lhs().size() != 1) { NOT_YET(assignment->lhs().size()); }
    if (assignment->rhs().size() != 1) { NOT_YET(assignment->rhs().size()); }

    auto const &id     = assignment->lhs()[0]->as<ast::Identifier>();
    auto const *f      = struct_type.field(id.token());
    size_t field_index = struct_type.index(f->name);
    auto typed_reg = builder().Field(to[0]->reg(), &struct_type, field_index);
    type::Typed<ir::RegOr<ir::Addr>> lhs(*typed_reg, typed_reg.type());
    EmitCopyInit(assignment->rhs()[0], absl::MakeConstSpan(&lhs, 1));
  }
}

ir::Value Compiler::EmitValue(ast::EnumLiteral const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  using enum_t = ir::EnumVal::underlying_type;
  std::vector<std::string_view> names;
  absl::flat_hash_map<uint64_t, ir::RegOr<enum_t>> specified_values;

  for (auto const *elem : node->elems()) {
    if (auto *id = elem->if_as<ast::Identifier>()) {
      names.push_back(id->token());
    } else if (auto *decl = elem->if_as<ast::Declaration>()) {
      names.push_back(decl->id());
      if (decl->IsCustomInitialized()) {
        specified_values.emplace(
            names.size() - 1,
            EmitValue(decl->init_val()).get<ir::RegOr<enum_t>>());
      }
    } else {
      // TODO: This should be a parse-time failure.
      UNREACHABLE("Require identifier or constant declaration: ",
                  elem->DebugString());
    }
  }

  // TODO: allocate the type upfront so it can be used in incomplete contexts.
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: {
      auto *e = type::Allocate<type::Enum>(&data().module());
      return ir::Value(builder().Enum(e, names, specified_values));
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      auto *f = type::Allocate<type::Flags>(&data().module());
      return ir::Value(builder().Flags(f, names, specified_values));
    } break;
    default: UNREACHABLE();
  }
}

template <typename NodeType>
ir::NativeFn MakeConcreteFromGeneric(
    Compiler *compiler, NodeType const *node,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  ASSERT(node->is_generic() == true);

  // Note: Cannot use structured bindings because the bindings need to be
  // captured in the lambda.

  // TODO: Rather than recompute this we colud store the `Call` node in the
  // dependent context.
  DependentComputedData temp_data(&compiler->data().module());
  Compiler c({
      .builder             = compiler->builder(),
      .data                = temp_data,
      .diagnostic_consumer = compiler->diag(),
      .importer            = compiler->importer(),
  });
  temp_data.parent_ = &compiler->data();
  auto params =
      c.ComputeParamsFromArgs(node, OrderedDependencyNodes(node), args);
  auto find_dependent_result = compiler->data().FindDependent(node, params);
  auto const *fn_type        = find_dependent_result.fn_type;
  auto &data                 = find_dependent_result.data;

  return data.EmplaceNativeFn(node, [&]() {
    ir::NativeFn f(
        &data.fns_, fn_type,
        node->params().Transform([fn_type, i = 0](auto const &d) mutable {
          return type::Typed<ast::Declaration const *>(
              d.get(), fn_type->params()[i++].value.type());
        }));
    f->work_item = DeferBody({.builder             = compiler->builder(),
                              .data                = data,
                              .diagnostic_consumer = compiler->diag(),
                              .importer            = compiler->importer()},
                             node, fn_type);
    return f;
  });
}

ir::Value Compiler::EmitValue(ast::ShortFunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::FnArgs<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); });
    return ir::Value(gen_fn);
  }

  ir::NativeFn ir_func = data().EmplaceNativeFn(node, [&] {
    auto *fn_type = &type_of(node)->as<type::Function>();
    auto f        = AddFunc(
        fn_type,
        node->params().Transform([fn_type, i = 0](auto const &d) mutable {
          return type::Typed<ast::Declaration const *>(
              d.get(), fn_type->params()[i++].value.type());
        }));
    f->work_item = DeferBody(resources_, node, fn_type);
    return f;
  });
  return ir::Value(ir::Fn{ir_func});
}

ir::Value Compiler::EmitValue(ast::FunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::FnArgs<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn { return MakeConcreteFromGeneric(&c, node, args); });
    return ir::Value(gen_fn);
  }

  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  // TODO Use correct constants
  ir::NativeFn ir_func = data().EmplaceNativeFn(node, [&] {
    auto *fn_type = &type_of(node)->as<type::Function>();
    auto f        = AddFunc(
        fn_type,
        node->params().Transform([fn_type, i = 0](auto const &d) mutable {
          return type::Typed<ast::Declaration const *>(
              d.get(), fn_type->params()[i++].value.type());
        }));
    f->work_item = DeferBody(resources_, node, fn_type);
    return f;
  });
  return ir::Value(ir::Fn{ir_func});
}

ir::Value Compiler::EmitValue(ast::FunctionType const *node) {
  std::vector<ir::RegOr<type::Type const *>> param_vals, out_vals;
  param_vals.reserve(node->params().size());
  out_vals.reserve(node->outputs().size());
  for (auto const *p : node->params()) {
    param_vals.push_back(EmitValue(p).get<ir::RegOr<type::Type const *>>());
  }
  for (auto const *o : node->outputs()) {
    out_vals.push_back(EmitValue(o).get<ir::RegOr<type::Type const *>>());
  }

  return ir::Value(builder().Arrow(std::move(param_vals), std::move(out_vals)));
}

void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitMoveInit(
      type::Typed<ir::Value>(EmitValue(node), data().qual_type(node)->type()),
      type::Typed<ir::Reg>(to[0]->reg(), to[0].type()));
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitCopyInit(
      type::Typed<ir::Value>(EmitValue(node), data().qual_type(node)->type()),
      type::Typed<ir::Reg>(to[0]->reg(), to[0].type()));
}

ir::Value Compiler::EmitValue(ast::Identifier const *node) {
  auto decl_span = data().decls(node);
  ASSERT(decl_span.size() != 0u);
  if (decl_span[0]->flags() & ast::Declaration::f_IsConst) {
    return EmitValue(decl_span[0]);
  }
  if (decl_span[0]->flags() & ast::Declaration::f_IsFnParam) {
    auto *t     = type_of(node);
    ir::Reg reg = data().addr(decl_span[0]);
    return (decl_span[0]->flags() & ast::Declaration::f_IsOutput) and
                   not t->is_big()
               ? builder().Load(reg, t)
               : ir::Value(reg);
  } else {
    auto *t   = ASSERT_NOT_NULL(type_of(node));
    auto lval = EmitRef(node);
    if (not lval.is_reg()) { NOT_YET(); }
    return ir::Value(builder().PtrFix(lval.reg(), t));
  }
}

void Compiler::EmitAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}

ir::Value Compiler::EmitValue(ast::Import const *node) {
  auto module_id = data().imported_module(node);
  ASSERT(module_id != ir::ModuleId::Invalid());
  return ir::Value(module_id);
}

void EmitJump(Compiler *c, absl::Span<ast::JumpOption const> options) {
  std::vector<std::string_view> names;
  names.reserve(options.size());

  std::vector<ir::BasicBlock *> blocks;
  blocks.reserve(options.size());

  std::vector<core::FnArgs<type::Typed<ir::Value>>> args;
  args.reserve(options.size());

  auto current_block = c->builder().CurrentBlock();

  for (auto const &opt : options) {
    ir::BasicBlock *block = c->builder().AddBlock();
    blocks.push_back(block);
    names.push_back(opt.block());

    c->builder().CurrentBlock() = block;

    args.push_back(opt.args().Transform([c](auto const &expr) {
      return type::Typed(c->EmitValue(expr.get()), c->type_of(expr.get()));
    }));
  }

  c->builder().CurrentBlock() = current_block;
  c->builder().ChooseJump(std::move(names), std::move(blocks), std::move(args));
}

ir::Value Compiler::EmitValue(ast::ConditionalGoto const *node) {
  auto condition    = EmitValue(node->condition());
  auto *true_block  = builder().AddBlock();
  auto *false_block = builder().AddBlock();
  builder().CondJump(condition.get<ir::RegOr<bool>>(), true_block, false_block);

  builder().CurrentBlock() = true_block;
  EmitJump(this, node->true_options());

  builder().CurrentBlock() = false_block;
  EmitJump(this, node->false_options());

  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::UnconditionalGoto const *node) {
  EmitJump(this, node->options());
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::Label const *node) {
  return ir::Value(/* TODO node->value() */);
}

ir::Value Compiler::EmitValue(ast::Jump const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  return ir::Value(data().add_jump(node, [this, node] {
    auto *jmp_type     = &type_of(node)->as<type::Jump>();
    auto work_item_ptr = DeferBody(resources_, node, jmp_type);

    size_t i    = 0;
    auto params = node->params().Transform([&](auto const &decl) {
      return type::Typed<ast::Declaration const *>(
          decl.get(), jmp_type->params()[i++].value);
    });

    DEBUG_LOG("Jump")("Jump type = ", jmp_type->to_string());
    ir::Jump jmp(jmp_type, std::move(params));
    if (work_item_ptr) { jmp.work_item = work_item_ptr; }
    return jmp;
  }));
}

static std::vector<std::pair<ast::Expression const *, ir::Value>>
EmitValueWithExpand(Compiler *c, base::PtrSpan<ast::Expression const> exprs) {
  // TODO expansion
  std::vector<std::pair<ast::Expression const *, ir::Value>> vals;
  for (auto *expr : exprs) { vals.emplace_back(expr, c->EmitValue(expr)); }
  return vals;
}

ir::Value Compiler::EmitValue(ast::ReturnStmt const *node) {
  auto const &fn_scope =
      *ASSERT_NOT_NULL(node->scope()->Containing<ast::FnScope>());
  auto const &fn_lit = *ASSERT_NOT_NULL(fn_scope.fn_lit_);
  auto const &fn_type =
      ASSERT_NOT_NULL(type_of(&fn_lit))->as<type::Function>();

  // TODO: It's tricky... on a single expression that gets expanded, we could
  // have both small and big types and we would need to handle both setting
  // registers for small types and writing through them for big ones.
  ASSERT(
      node->exprs().size() ==
      fn_type.output().size());  // TODO: For now, assume no actual expansion.
  for (size_t i = 0; i < node->exprs().size(); ++i) {
    auto const *expr     = node->exprs()[i];
    auto const &ret_type = *ASSERT_NOT_NULL(fn_type.output()[i]);
    if (ret_type.is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(builder().GetRet(i, &ret_type)),
          type::Ptr(&ret_type));
      EmitMoveInit(expr, absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      builder().SetRet(i, type::Typed{EmitValue(expr), &ret_type});
    }
  }

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  auto *scope = node->scope();
  while (auto *exec = scope->if_as<ast::ExecScope>()) {
    MakeAllDestructions(this, exec);
    if (not exec->parent or exec->is<ast::FnScope>()) { break; }
    scope = exec->parent;
  }

  builder().ReturnJump();
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::YieldStmt const *node) {
  auto arg_vals = EmitValueWithExpand(this, node->exprs());
  // TODO store this as an exec_scope.
  MakeAllDestructions(this, &node->scope()->as<ast::ExecScope>());
  // TODO pretty sure this is all wrong.

  // Can't return these because we need to pass them up at least through the
  // containing statements this and maybe further if we allow labelling
  // scopes to be yielded to.
  auto &yielded_args = state_.yields.back();

  // TODO one problem with this setup is that we look things up in a context
  // after returning, so the `after` method has access to a different
  // (smaller) collection of bound constants. This can change the meaning of
  // things or at least make them not compile if the `after` function takes
  // a compile-time constant argument.
  for (size_t i = 0; i < arg_vals.size(); ++i) {
    auto qt = qual_type_of(node->exprs()[i]);
    ASSERT(qt.has_value() == true);
    yielded_args.vals.pos_emplace(arg_vals[i].second, *qt);
  }
  yielded_args.label = node->label() ? node->label()->value() : ir::Label{};

  builder().block_termination_state() =
      node->label() ? ir::Builder::BlockTerminationState::kLabeledYield
                    : ir::Builder::BlockTerminationState::kYield;
  return ir::Value();
}

ir::Value Compiler::EmitValue(ast::ScopeLiteral const *node) {
  DEBUG_LOG("ScopeLiteral")(node->state_type());
  type::Type const *state_type = nullptr;
  if (node->state_type()) {
    auto maybe_type = EvaluateAs<type::Type const *>(node->state_type());
    if (not maybe_type) { NOT_YET(); }
    state_type = *maybe_type;
  }

  absl::flat_hash_map<std::string_view, ir::BlockDef *> blocks;
  std::vector<ir::RegOr<ir::Jump *>> inits;
  std::vector<ir::RegOr<ir::Fn>> dones;
  for (auto const &decl : node->decls()) {
    if (decl.id() == "enter") {
      inits.push_back(EmitValue(&decl).get<ir::RegOr<ir::Jump *>>());
    } else if (decl.id() == "exit") {
      dones.push_back(EmitValue(&decl).get<ir::RegOr<ir::Fn>>());
    } else {
      blocks.emplace(decl.id(),
                     EmitValue(&decl).get<ir::RegOr<ir::BlockDef *>>().value());
    }
  }

  return ir::Value(builder().MakeScope(
      data().add_scope(&data().module(), state_type), std::move(inits),
      std::move(dones), std::move(blocks)));
}

ir::Value Compiler::EmitValue(ast::ScopeNode const *node) {
  DEBUG_LOG("ScopeNode")("Emitting IR for ScopeNode");

  // Jump to a new block in case some scope ends up with `goto start()` in order
  // to re-evealuate arguments.
  auto *args_block = builder().AddBlock();
  builder().UncondJump(args_block);
  builder().CurrentBlock() = args_block;

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed(EmitValue(expr), type_of(expr));
  });

  ASSIGN_OR(return ir::Value(),  //
                   auto os, MakeOverloadSet(this, node->name(), args));

  auto inits = MakeJumpInits(this, os);
  DEBUG_LOG("ScopeNode")(inits);

  ASSIGN_OR(
      return ir::Value(),  //
             auto table,
             ScopeDispatchTable::Verify(this, node, std::move(inits), args));

  return table.EmitCall(this, args);
}

struct IncompleteField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "incomplete-field";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Struct field has incomplete type."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

WorkItem::Result Compiler::CompleteStruct(ast::StructLiteral const *node) {
  DEBUG_LOG("struct")
  ("Completing struct-literal emission: ", node,
   " must-complete = ", state_.must_complete);

  type::Struct *s = data().get_struct(node);
  if (s->completeness() == type::Completeness::Complete) {
    DEBUG_LOG("struct")("Already complete, exiting: ", node);
    return WorkItem::Result::Success;
  }

  bool field_error = false;
  for (auto const &field : node->fields()) {
    auto const *qt = data().qual_type(&field);
    if (not qt or qt->type()->completeness() != type::Completeness::Complete) {
      diag().Consume(IncompleteField{.range = field.range()});
      field_error = true;
    }
  }
  if (field_error) { return WorkItem::Result::Failure; }

  ir::CompiledFn fn(type::Func({}, {}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(&fn, &builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    builder().CurrentBlock() = fn.entry();

    std::vector<ir::StructField> fields;
    fields.reserve(node->fields().size());

    std::optional<ir::Fn> dtor, move_assign;
    for (auto const &field : node->fields()) {
      // TODO hashtags.
      if (field.id() == "destroy") {
        // TODO handle potential errors here.
        auto dtor_value = EmitValue(field.init_val());
        if (auto const *dtor_fn = dtor_value.get_if<ir::Fn>()) {
          dtor = *dtor_fn;
        } else {
          NOT_YET("Log an error");
        }
      } else if (field.id() == "assign") {
        // TODO handle potential errors here.
        auto assign_value = EmitValue(field.init_val());
        if (auto const *move_assign_fn = assign_value.get_if<ir::Fn>()) {
          move_assign = *move_assign_fn;
        } else {
          NOT_YET("Log an error");
        }
      } else {
        if (auto *init_val = field.init_val()) {
          // TODO init_val type may not be the same.
          auto *t = ASSERT_NOT_NULL(qual_type_of(init_val)->type());
          fields.emplace_back(field.id(), t, EmitValue(init_val));
        } else {
          fields.emplace_back(
              field.id(),
              EmitValue(field.type_expr()).get<type::Type const *>());
        }
      }
    }
    builder().Struct(s, std::move(fields), move_assign, dtor);
    builder().ReturnJump();
  }

  // TODO: What if execution fails.
  fn.WriteByteCode<interpretter::instruction_set_t>();
  interpretter::Execute(std::move(fn));
  s->complete();
  DEBUG_LOG("struct")
  ("Completed ", node->DebugString(), " which is a struct ", *s, " with ",
   s->fields().size(), " field(s).");
  return WorkItem::Result::Success;
}

ir::Value Compiler::EmitValue(ast::StructLiteral const *node) {
  DEBUG_LOG("struct")
  ("Starting struct-literal emission: ", node,
   state_.must_complete ? " (must complete)" : " (need not complete)");

  if (type::Struct *s = data().get_struct(node)) {
    return ir::Value(static_cast<type::Type const *>(s));
  }

  type::Struct *s = type::Allocate<type::Struct>(
      &data().module(),
      type::Struct::Options{
          .is_copyable = not node->contains_hashtag(
              ast::Hashtag(ast::Hashtag::Builtin::Uncopyable)),
          .is_movable = not node->contains_hashtag(
              ast::Hashtag(ast::Hashtag::Builtin::Immovable)),
      });

  DEBUG_LOG("struct")("Allocating a new struct ", s, " for ", node);
  data().set_struct(node, s);

  // Note: VerifyBody may end up triggering EmitValue calls for member types
  // that depend on this incomplete type. For this reason it is important that
  // we have already allocated the struct so we do not get a double-allocation.
  //
  // The process, as you can see above is to
  // 1. Check if it has already been allocated. Return if it has been.
  // 2. Allocate ourselves.
  // 3. Start body verification.
  // 4. Schedule completion.
  //
  // Notably, steps 1 and 2 must not be separated. Moreover, because body
  // verification could end up calling this function again, we must "set up
  // guards" (i.e., steps 1 and 2) before step 3 runs.
  //
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }

  if (state_.must_complete) {
    DEBUG_LOG("compile-work-queue")("Request work complete struct: ", node);
    state_.work_queue.Enqueue({
        .kind     = WorkItem::Kind::CompleteStructMembers,
        .node     = node,
        .context  = data(),
        .consumer = diag(),
    });
  }
  return ir::Value(static_cast<type::Type const *>(s));
}

ir::Value Compiler::EmitValue(ast::ParameterizedStructLiteral const *node) {
  // TODO: Check the result of body verification.
  if (data().ShouldVerifyBody(node)) { VerifyBody(node); }
  // NOT_YET();
  // TODO: At least right now with the hacky version we don't look at this.

  return ir::Value(data().qual_type(node)->type());
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_span = data().decls(node);
  ASSERT(decl_span.size() == 1u);
  return data().addr(decl_span[0]);
}

}  // namespace compiler
