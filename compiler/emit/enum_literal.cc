#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::EnumLiteral const *node,
                            ir::PartialResultBuffer &out) {
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  std::vector<std::string_view> names(node->enumerators().begin(),
                                      node->enumerators().end());
  absl::flat_hash_map<uint64_t, ir::RegOr<type::Enum::underlying_type>>
      specified_values;

  uint64_t i = 0;
  for (uint64_t i = 0; i < names.size(); ++i) {
    if (auto iter = node->specified_values().find(names[i]);
        iter != node->specified_values().end()) {
      specified_values.emplace(
          i, EmitAs<type::Enum::underlying_type>(iter->second.get()));
    }
  }

  // TODO: allocate the type upfront so it can be used in incomplete contexts.
  switch (node->kind()) {
    case ast::EnumLiteral::Kind::Enum: {
      out.append(current_block()->Append(type::EnumInstruction{
          .type              = type::Allocate<type::Enum>(&context().module()),
          .names_            = std::move(names),
          .specified_values_ = std::move(specified_values),
          .result            = builder().CurrentGroup()->Reserve()}));
    } break;
    case ast::EnumLiteral::Kind::Flags: {
      out.append(current_block()->Append(type::FlagsInstruction{
          .type              = type::Allocate<type::Flags>(&context().module()),
          .names_            = std::move(names),
          .specified_values_ = std::move(specified_values),
          .result            = builder().CurrentGroup()->Reserve()}));
    } break;
    default: UNREACHABLE();
  }
}

}  // namespace compiler
