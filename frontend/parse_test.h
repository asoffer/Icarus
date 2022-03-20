#ifndef ICARUS_FRONTEND_PARSE_TEST_H
#define ICARUS_FRONTEND_PARSE_TEST_H

#include <optional>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "frontend/parse.h"

namespace frontend {

bool ParseAssignment(absl::Span<Lexeme const> &lexemes,
                     std::unique_ptr<ast::Node> &n);
bool ParseCallArgument(absl::Span<Lexeme const> &lexemes,
                       ast::Call::Argument &out);
bool ParseDeclaration(absl::Span<Lexeme const> &lexemes, ast::Declaration &out);
bool ParseDeclarationId(absl::Span<Lexeme const> &lexemes,
                        ast::Declaration::Id &out);
bool ParseLabel(absl::Span<Lexeme const> &lexemes,
                std::optional<ast::Label> &out);
bool ParseReturnStatement(absl::Span<Lexeme const> &lexemes,
                          std::unique_ptr<ast::Node> &n);
bool ParseStringLiteral(absl::Span<Lexeme const> &lexemes,
                        std::unique_ptr<ast::Expression> &out);
bool ParseYieldStatement(absl::Span<Lexeme const> &lexemes,
                         std::unique_ptr<ast::Node> &n);
}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_TEST_H
