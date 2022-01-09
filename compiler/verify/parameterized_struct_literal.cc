#include "ast/ast.h"
#include "compiler/emit/common.h"
#include "compiler/instantiate.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/generic.h"
#include "type/qual_type.h"
#include "type/struct.h"
#include "type/typed_value.h"

namespace compiler {

bool BodyVerifier::VerifyBody(ast::ParameterizedStructLiteral const *node) {
  // NOT_YET();
  return true;
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ParameterizedStructLiteral const *node) {
  auto *gen_struct = type::Allocate<type::Generic<type::Struct>>();

  auto gen =
      [gen_struct, node, d = data()](
          WorkResources const &wr,
          core::Arguments<type::Typed<ir::CompleteResultRef>> const
              &args) mutable -> type::InstantiatedGeneric<type::Struct> * {
    d.work_resources = wr;
    ASSIGN_OR(return nullptr,  //
                     auto result,
                     Instantiate(CompilationDataReference(&d), node, args));
    auto const &[params, rets_ref, context, inserted] = result;

    if (inserted) {
      LOG("ParameterizedStructLiteral", "inserted! %s", node->DebugString());
      CompilationData data{
          .context = &context, .work_resources = wr, .resources = d.resources};
      auto [t, inserted] =
          context.EmplaceType<type::InstantiatedGeneric<type::Struct>>(
              node, gen_struct, d.resources.module,
              type::Struct::Options{.is_copyable = not node->hashtags.contains(
                                        ir::Hashtag::Uncopyable),
                                    .is_movable = not node->hashtags.contains(
                                        ir::Hashtag::Immovable)});

      auto *s = &const_cast<type::InstantiatedGeneric<type::Struct> &>(
          t.as<type::InstantiatedGeneric<type::Struct>>());
      if (inserted) {
        s->set_arguments(args.Transform([](auto const &a) {
          ir::CompleteResultBuffer buffer;
          buffer.append(*a);
          return type::Typed(buffer, a.type());
        }));
      }

      for (auto const &field : node->fields()) {
        compiler::VerifyType(CompilationDataReference(&data), &field);
      }

      {
        auto maybe_fn = StructDataCompletionFn(CompilationDataReference(&data),
                                               s, node->fields());
        // TODO: Deal with error-case.
        ASSERT(maybe_fn.has_value() == true);
        InterpretAtCompileTime(*maybe_fn);
      }

      {
        auto maybe_fn = StructCompletionFn(CompilationDataReference(&data), s,
                                           node->fields());
        // TODO: Deal with error-case.
        ASSERT(maybe_fn.has_value() == true);
        // TODO: What if execution fails.
        InterpretAtCompileTime(*maybe_fn);
      }

      LOG("ParameterizedStructLiteral",
          "Completed %s which is a (parameterized) struct %s with %u field(s).",
          node->DebugString(), *s, s->fields().size());
      // TODO: Hack just assuming parameterized structs are parameterized on
      // exactly one type which is anonymous.
      return s;
    } else {
      LOG("ParameterizedStructLiteral", "cached! %s", node->DebugString());
      return &const_cast<type::InstantiatedGeneric<type::Struct> &>(
          context.LoadType(node).as<type::InstantiatedGeneric<type::Struct>>());
    }
  };

  gen_struct->set_invocable(std::move(gen));

  return context().set_qual_type(node, type::QualType::Constant(gen_struct));
}

}  // namespace compiler
