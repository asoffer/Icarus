#include "compiler/compiler.h"

#include "ast/ast.h"
#include "base/log.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/parse.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/interpretter/evaluate.h"
#include "ir/jump.h"
#include "ir/value/value.h"
#include "type/generic_struct.h"
#include "type/jump.h"

namespace compiler {

WorkItem::Result WorkItem::Process() const {
  module::FileImporter<LibraryModule> importer;
  Compiler c({
      .builder             = ir::GetBuilder(),
      .data                = context,
      .diagnostic_consumer = consumer,
      .importer            = importer,
  });
  switch (kind) {
    case Kind::VerifyBlockBody:
      return c.VerifyBody(&node->as<ast::BlockLiteral>());
    case Kind::VerifyEnumBody:
      return c.VerifyBody(&node->as<ast::EnumLiteral>());
    case Kind::VerifyFunctionBody:
      return c.VerifyBody(&node->as<ast::FunctionLiteral>());
    case Kind::VerifyJumpBody: return c.VerifyBody(&node->as<ast::Jump>());
    case Kind::VerifyScopeBody: NOT_YET();
    case Kind::VerifyStructBody:
      return c.VerifyBody(&node->as<ast::StructLiteral>());
    case Kind::CompleteStructMembers:
      return c.CompleteStruct(&node->as<ast::StructLiteral>());
  }
}

Compiler::Compiler(PersistentResources const &resources)
    : resources_(resources) {}
Compiler Compiler::WithPersistent() const { return Compiler(resources_); }

std::optional<type::QualType> Compiler::qual_type_of(
    ast::Expression const *expr) const {
  if (auto *decl = expr->if_as<ast::Declaration>()) {
    // If the declarations module is the same as this one, we haven't completed
    // compiling it yet and so we need to access it through the compiler.
    // Otherwise, we have finished compiling, so we access it through the
    // module.
    //
    // TODO This could be a TestModule which doesn't have a .type_of(). we
    // really shouldn't need to pay for the check here.
    if (auto const *mod =
            &ASSERT_NOT_NULL(decl->module())->as<CompiledModule>()) {
      if (mod != &data().module()) {
        auto *qt = mod->data().qual_type(decl);
        return qt ? std::optional(*qt) : std::nullopt;
      }
      if (auto *qt = data().qual_type(decl)) { return *qt; }
    }
  }

  if (auto *qt = data().qual_type(expr)) { return *qt; }

  // TODO embedded modules?
  return std::nullopt;
}

type::Type const *Compiler::type_of(ast::Expression const *expr) const {
  return qual_type_of(expr).value_or(type::QualType{}).type();
}

void Compiler::CompleteDeferredBodies() {
  base::move_func<void()> f;
  while (true) {
    {
      auto handle = data().deferred_work_.lock();
      if (handle->empty()) { return; }
      auto nh = handle->extract(handle->begin());
      LOG("CompleteDeferredBodies", "%s", nh.key()->DebugString());
      f = std::move(nh.mapped());
    }
    if (f) { std::move(f)(); }
  }
}

ir::NativeFn Compiler::AddFunc(
    type::Function const *fn_type,
    core::Params<type::Typed<ast::Declaration const *>> params) {
  return ir::NativeFn(&data().fns_, fn_type, std::move(params));
}

static ir::CompiledFn MakeThunk(Compiler &c, ast::Expression const *expr,
                                type::Type const *type) {
  ir::CompiledFn fn(type::Func({}, {ASSERT_NOT_NULL(type)}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(&fn, &c.builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    auto val = c.EmitValue(expr);
    // TODO wrap this up into SetRet(vector)
    std::vector<type::Type const *> extracted_types;
    if (auto *tup = type->if_as<type::Tuple>()) {
      extracted_types = tup->entries_;
    } else {
      extracted_types = {type};
    }

    if (type != type::Void()) { ASSERT(val.empty() == false); }
    // TODO is_big()?

    type::Type const *t = extracted_types[0];
    LOG("MakeThunk", "%s %s", *t, t->is_big() ? "true" : "false");
    if (t->is_big()) {
      // TODO must `r` be holding a register?
      // TODO guaranteed move-elision

      c.EmitMoveInit(
          type::Typed<ir::Value>(val, t),
          type::Typed<ir::Reg>(c.builder().GetRet(0, t), type::Ptr(t)));

    } else if (auto const *gs = t->if_as<type::GenericStruct>()) {
      c.builder().SetRet(0, gs);
    } else {
      c.builder().SetRet(0, type::Typed<ir::Value>(val, t));
    }
    c.builder().ReturnJump();
  }

  ASSERT(fn.work_item == nullptr);
  fn.WriteByteCode<interpretter::instruction_set_t>();

  return fn;
}

base::expected<ir::Value, interpretter::EvaluationFailure> Compiler::Evaluate(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  Compiler c(resources_);
  c.state_.must_complete = must_complete;
  auto result = interpretter::Evaluate(MakeThunk(c, *expr, expr.type()));
  if (not result) { return result; }
  c.CompleteWorkQueue();
  return result;
}

ir::ModuleId Compiler::EvaluateModuleWithCache(ast::Expression const *expr) {
  // TODO: Implement caching behavior.
  auto maybe_mod = EvaluateAs<ir::ModuleId>(expr);
  if (not maybe_mod) {
    diag().Consume(diagnostic::EvaluationFailure{.failure = maybe_mod.error(),
                                                 .range   = expr->range()});
    return ir::ModuleId::Invalid();
  }
  return *maybe_mod;
}

}  // namespace compiler
