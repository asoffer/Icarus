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
#include "ir/compiled_jump.h"
#include "ir/interpretter/evaluate.h"
#include "ir/value/value.h"
#include "type/generic_struct.h"
#include "type/jump.h"

namespace compiler {

WorkItem::Result WorkItem::Process() const {
  module::FileImporter<LibraryModule> importer;
  Compiler c({
      .data                = context,
      .diagnostic_consumer = consumer,
      .importer            = importer,
  });
  switch (kind) {
    case Kind::VerifyEnumBody:
      return c.VerifyBody(&node->as<ast::EnumLiteral>());
    case Kind::VerifyFunctionBody:
      return c.VerifyBody(&node->as<ast::FunctionLiteral>());
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
      if (mod != &context().module()) {
        auto *qt = mod->context().qual_type(decl);
        return qt ? std::optional(*qt) : std::nullopt;
      }
      if (auto *qt = context().qual_type(decl)) { return *qt; }
    }
  }

  if (auto *qt = context().qual_type(expr)) { return *qt; }

  // TODO embedded modules?
  return std::nullopt;
}

type::Type Compiler::type_of(ast::Expression const *expr) const {
  return qual_type_of(expr).value_or(type::QualType{}).type();
}

void Compiler::CompleteDeferredBodies() {
  while (true) {
    if (state_.deferred_work.empty()) { return; }
    for (auto &work : state_.deferred_work) { std::move (*work)(); }
  }
}

ir::NativeFn Compiler::AddFunc(
    type::Function const *fn_type,
    core::Params<type::Typed<ast::Declaration const *>> params) {
  return ir::NativeFn(&context().fns_, fn_type, std::move(params));
}

static ir::CompiledFn MakeThunk(Compiler &c, ast::Expression const *expr,
                                type::Type type) {
  ir::CompiledFn fn(type::Func({}, {type}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    auto val = c.EmitValue(expr);
    // TODO wrap this up into SetRet(vector)
    std::vector<type::Type> extracted_types;
    if (auto *tup = type.if_as<type::Tuple>()) {
      extracted_types = tup->entries_;
    } else {
      extracted_types = {type};
    }

    if (type != type::Void()) { ASSERT(val.empty() == false); }
    // TODO is_big()?

    type::Type t = extracted_types[0];
    LOG("MakeThunk", "%s %s", t, t.get()->is_big() ? "true" : "false");
    if (t.get()->is_big()) {
      // TODO must `r` be holding a register?
      // TODO guaranteed move-elision

      c.EmitMoveInit(
          type::Typed<ir::Value>(val, t),
          type::Typed<ir::Reg>(c.builder().GetRet(0, t), type::Ptr(t)));

    } else if (auto const *gs = t.if_as<type::GenericStruct>()) {
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
  Compiler c             = MakeChild(resources_);
  c.state_.must_complete = must_complete;
  auto result = interpretter::Evaluate(MakeThunk(c, *expr, expr.type()));
  if (not result) { return result; }
  c.CompleteWorkQueue();
  return result;
}

ir::ModuleId Compiler::EvaluateModuleWithCache(ast::Expression const *expr) {
  // TODO: Implement caching behavior.
  if (auto maybe_mod = EvaluateOrDiagnoseAs<ir::ModuleId>(expr)) {
    return *maybe_mod;
  } else {
    return ir::ModuleId::Invalid();
  }
}

Context::InsertSubcontextResult Compiler::Instantiate(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  Context scratchpad = context().ScratchpadSubcontext();
  Compiler c({
      .data                = scratchpad,
      .diagnostic_consumer = diag(),
      .importer            = importer(),
  });
  return context().InsertSubcontext(node, c.ComputeParamsFromArgs(node, args),
                                    std::move(scratchpad));
}

Context::FindSubcontextResult Compiler::FindInstantiation(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  Context scratchpad = context().ScratchpadSubcontext();
  Compiler c({
      .data                = scratchpad,
      .diagnostic_consumer = diag(),
      .importer            = importer(),
  });
  return context().FindSubcontext(node, c.ComputeParamsFromArgs(node, args));
}

}  // namespace compiler
