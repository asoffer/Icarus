#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/module.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::EnumLiteral const *node,
                            ir::PartialResultBuffer &out) {
  LOG("EnumLiteral", "Starting enum-literal emission: %p on %s", node,
      context().DebugString());

  type::Type t;
  bool inserted;
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: {
      std::tie(t, inserted) =
          context().EmplaceType<type::Enum>(node, resources().module);
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      std::tie(t, inserted) =
          context().EmplaceType<type::Flags>(node, resources().module);
    } break;
  }

  if (inserted) {
    // Strictly speaking this conditional is not needed. Enqueuing the same work
    // item twice will be deduplicated.
    Enqueue({.kind    = WorkItem::Kind::CompleteEnum,
             .node    = node,
             .context = &context()},
            {WorkItem{.kind    = WorkItem::Kind::VerifyEnumBody,
                      .node    = node,
                      .context = &context()}});
  }
  out.append(t);
}

bool Compiler::CompleteEnum(ast::EnumLiteral const *node) {
  LOG("EnumLiteral", "Completing enum-literal emission: %p", node);

  ir::CompiledFn fn(type::Func({}, {}));
  type::Type t = context().LoadType(node);

  set_builder(&fn);
  absl::Cleanup cleanup    = [&] { state().builders.pop_back(); };
  builder().CurrentBlock() = fn.entry();

  std::vector<std::string_view> names(node->enumerators().begin(),
                                      node->enumerators().end());
  absl::flat_hash_map<uint64_t, ir::RegOr<type::Enum::underlying_type>>
      specified_values;

  uint64_t i = 0;
  for (uint64_t i = 0; i < names.size(); ++i) {
    if (auto iter = node->specified_values().find(names[i]);
        iter != node->specified_values().end()) {
      specified_values.emplace(i, EmitWithCastTo<type::Enum::underlying_type>(
                                      t, iter->second.get()));
    }
  }

  // TODO: Find a way around these const casts.
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: {
      current_block()->Append(type::EnumInstruction{
          .type              = &const_cast<type::Enum &>(t.as<type::Enum>()),
          .names_            = std::move(names),
          .specified_values_ = std::move(specified_values),
          .result            = builder().CurrentGroup()->Reserve()});
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      current_block()->Append(type::FlagsInstruction{
          .type              = &const_cast<type::Flags &>(t.as<type::Flags>()),
          .names_            = std::move(names),
          .specified_values_ = std::move(specified_values),
          .result            = builder().CurrentGroup()->Reserve()});
    } break;
    default: UNREACHABLE();
  }

  builder().ReturnJump();

  InterpretAtCompileTime(fn);

  return true;
}

}  // namespace compiler
