#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "compiler/resources.h"
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

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  auto gen = [node, instantiation_compiler = Compiler(resources()),
              cg = builder().CurrentGroup()](
                 core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> std::pair<core::Params<type::QualType>, type::Struct *> {
    auto [params, rets_ref, context, inserted] =
        instantiation_compiler.Instantiate(node, args);

    if (inserted) {
      LOG("ParameterizedStructLiteral", "inserted! %s", node->DebugString());
      auto compiler = instantiation_compiler.MakeChild(PersistentResources{
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

      auto maybe_fn = StructCompletionFn(compiler, s, node->fields());
      // TODO: Deal with error-case.
      ASSERT(maybe_fn.has_value() == true);
      // TODO: What if execution fails.
      InterpretAtCompileTime(*maybe_fn);
      LOG("ParameterizedStructLiteral",
          "Completed %s which is a (parameterized) struct %s with %u field(s).",
          node->DebugString(), *s, s->fields().size());
      // TODO: Hack just assuming parameterized structs are parameterized on
      // exactly one type which is anonymous.
      return std::make_pair(core::Params<type::QualType>{core::AnonymousParam(
                                type::QualType::Constant(type::Type_))},
                            s);
    } else {
      LOG("ParameterizedStructLiteral", "cached! %s", node->DebugString());
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
