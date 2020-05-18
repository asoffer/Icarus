#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>
#include <vector>

#include "ast/node.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"

namespace frontend {

std::vector<std::unique_ptr<ast::Node>> Parse(
    Source* src, diagnostic::DiagnosticConsumer& diag,
    LineNum initial_line_num = LineNum(1));
}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H
