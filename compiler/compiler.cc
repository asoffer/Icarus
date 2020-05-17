#include "compiler/compiler.h"

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/parse.h"
#include "interpretter/evaluate.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "ir/value/value.h"
#include "type/jump.h"

namespace compiler {

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
      if (mod != data().module()) {
        auto *qt = mod->data().qual_type(decl);
        return qt ? std::optional(*qt) : std::nullopt;
      }
      if (auto *t = data().constants_.type_of(decl)) {
        return type::QualType::Constant(t);
      }
    }
  }

  if (auto *result = data().qual_type(expr)) { return *result; }

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
      DEBUG_LOG("CompleteDeferredBodies")(nh.key()->DebugString());
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

static ir::CompiledFn MakeThunk(Compiler::PersistentResources const &resources,
                                ast::Expression const *expr,
                                type::Type const *type) {
  Compiler c(resources);
  ir::CompiledFn fn(type::Func({}, {ASSERT_NOT_NULL(type)}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(&fn, &resources.builder)) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    resources.builder.CurrentBlock() = fn.entry();

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

    size_t i           = 0;
    auto handle_result = [&](type::Type const *t, ir::Value const &v) {
      DEBUG_LOG("MakeThunk")(*t);
      if (t->is_big()) {
        // TODO must `r` be holding a register?
        // TODO guaranteed move-elision

        c.EmitMoveInit(
            t, v,
            type::Typed<ir::Reg>(resources.builder.GetRet(i, t), type::Ptr(t)));

      } else {
        resources.builder.SetRet(i, type::Typed<ir::Value>(v, t));
      }
    };

    if (auto const *m = val.get_if<ir::MultiValue>()) {
      for (auto const &v : m->span()) {
        handle_result(extracted_types[i], v);
        i++;
      }
    } else {
      handle_result(extracted_types[0], val);
    }

    resources.builder.ReturnJump();
  }

  ASSERT(fn.work_item == nullptr);
  fn.WriteByteCode();

  return fn;
}

base::expected<ir::Value> Compiler::Evaluate(
    type::Typed<ast::Expression const *> expr) {
  return interpretter::Evaluate(MakeThunk(resources_, *expr, expr.type()));
}

}  // namespace compiler
