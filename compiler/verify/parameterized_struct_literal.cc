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
  auto gen = [node, instantiation_compiler = Compiler(resources()),
              cg = builder().CurrentGroup()](
                 core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> std::pair<core::Params<type::QualType>, type::Struct *> {
    auto [params, rets_ref, context, inserted] =
        instantiation_compiler.Instantiate(node, args);

    if (inserted) {
      LOG("ParameterizedStructLiteral", "inserted! %s", node->DebugString());
      auto compiler =
          instantiation_compiler.MakeChild(Compiler::PersistentResources{
              .data                = context,
              .diagnostic_consumer = instantiation_compiler.diag(),
              .importer            = instantiation_compiler.importer(),
          });
      compiler.builder().CurrentGroup() = cg;

      type::Struct *s = type::Allocate<type::Struct>(
          &compiler.context().module(),
          type::Struct::Options{.is_copyable = not node->hashtags.contains(
                                    ir::Hashtag::Uncopyable),
                                .is_movable = not node->hashtags.contains(
                                    ir::Hashtag::Immovable)});

      LOG("ParameterizedStructLiteral",
          "Allocating a new (parameterized) struct %p for %p", s, node);
      compiler.context().set_struct(node, s);
      for (auto const &field : node->fields()) { compiler.VerifyType(&field); }

      // TODO: This should actually be behind a must_complete work queue item.
      ir::CompiledFn fn(type::Func({}, {}),
                        core::Params<type::Typed<ast::Declaration const *>>{});
      ICARUS_SCOPE(ir::SetCurrent(fn, compiler.builder())) {
        // TODO: this is essentially a copy of the body of
        // FunctionLiteral::EmitValue Factor these out together.
        compiler.builder().CurrentBlock() = fn.entry();

        std::vector<type::StructInstruction::Field> fields;
        fields.reserve(node->fields().size());

        for (auto const &field : node->fields()) {
          // TODO hashtags, special members.
          if (auto *init_val = field.init_val()) {
            // TODO init_val type may not be the same.
            type::Type t = compiler.qual_type_of(init_val)->type();
            fields.emplace_back(field.id(), t, compiler.EmitValue(init_val));
          } else {
            fields.emplace_back(
                field.id(), compiler.EmitValue(field.type_expr()).get<type::Type>());
          }
        }
        // TODO destructors and assignment
        compiler.current_block()->Append(
            type::StructInstruction{.struct_     = s,
                                    .fields      = std::move(fields),
                                    .assignments = {},
                                    .dtor        = std::nullopt,
                                    .result      = compiler.builder().Reserve()});

        compiler.builder().ReturnJump();
      }

      // TODO: What if execution fails.
      fn.WriteByteCode<interpretter::instruction_set_t>();
      interpretter::Execute(std::move(fn));
      LOG("ParameterizedStructLiteral",
          "Completed %s which is a (parameterized) struct %s with %u field(s).",
          node->DebugString(), *s, s->fields().size());
      // TODO: Hack just assuming parameterized structs are parameterized on
      // exactly one type which is anonymous.
      return std::make_pair(core::Params<type::QualType>{core::AnonymousParam(
                                type::QualType::Constant(type::Type_))},
                            s);
    } else {
      return std::make_pair(core::Params<type::QualType>{core::AnonymousParam(
                                type::QualType::Constant(type::Type_))},
                            context.get_struct(node));
    }
  };

  return context().set_qual_type(
      node, type::QualType::Constant(
                type::Allocate<type::GenericStruct>(std::move(gen))));
}

}  // namespace compiler
