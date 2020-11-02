#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "compiler/verify/common.h"
#include "type/generic_struct.h"
#include "type/qual_type.h"
#include "type/struct.h"
#include "type/typed_value.h"

namespace compiler {

WorkItem::Result Compiler::VerifyBody(
    ast::ParameterizedStructLiteral const *node) {
  // NOT_YET();
  return WorkItem::Result::Success;
}

type::QualType Compiler::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  auto gen = [node, c = Compiler(resources())](
                 core::FnArgs<type::Typed<ir::Value>> const &args) mutable
      -> std::pair<core::Params<type::QualType>, type::Struct const *> {
    // TODO: Need a version of MakeConcrete that doesn't generate return types
    // because those only make sense for functions.
    auto [params, rets, data, inserted] = c.Instantiate(node, args);
    if (inserted) {
      type::Struct *s = new type::Struct(
          &c.context().module(),
          {.is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
           .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable)});

      LOG("struct", "Allocating a new (parameterized) struct %p for %p", s,
          node);
      c.context().set_struct(node, s);
      for (auto const &field : node->fields()) { c.VerifyType(&field); }

      // TODO: This should actually be behind a must_complete work queue item.
      ir::CompiledFn fn(type::Func({}, {}),
                        core::Params<type::Typed<ast::Declaration const *>>{});
      ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
        // TODO: this is essentially a copy of the body of
        // FunctionLiteral::EmitValue Factor these out together.
        c.builder().CurrentBlock() = fn.entry();

        std::vector<ir::StructField> fields;
        fields.reserve(node->fields().size());

        for (auto const &field : node->fields()) {
          // TODO hashtags, special members.
          if (auto *init_val = field.init_val()) {
            // TODO init_val type may not be the same.
            type::Type t = c.qual_type_of(init_val)->type();
            fields.emplace_back(field.id(), t, c.EmitValue(init_val));
          } else {
            fields.emplace_back(
                field.id(), c.EmitValue(field.type_expr()).get<type::Type>());
          }
        }
        // TODO destructors and assignment
        c.builder().Struct(s, std::move(fields), std::nullopt, std::nullopt);
        c.builder().ReturnJump();
      }

      // TODO: What if execution fails.
      fn.WriteByteCode<interpretter::instruction_set_t>();
      interpretter::Execute(std::move(fn));
      LOG("struct",
          "Completed %s which is a (parameterized) struct %s with %u field(s).",
          node->DebugString(), *s, s->fields().size());
      return std::make_pair(core::Params<type::QualType>{}, s);
    } else {
      return std::make_pair(core::Params<type::QualType>{},
                            data.get_struct(node));
    }
  };

  return context().set_qual_type(
      node, type::QualType::Constant(
                type::Allocate<type::GenericStruct>(std::move(gen))));
}

}  // namespace compiler
