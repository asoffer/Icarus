#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>
#include <vector>

#include "frontend/source/source.h"
#include "ast/ast_fwd.h"
#include "diagnostic/consumer/consumer.h"

namespace frontend {

std::vector<std::unique_ptr<ast::Node>> Parse(
    Source* src, diagnostic::DiagnosticConsumer& diag);
}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H
