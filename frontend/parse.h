#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>
#include <vector>

#include "ast/node.h"
#include "diagnostic/consumer/consumer.h"

namespace frontend {

std::vector<std::unique_ptr<ast::Node>> Parse(
    std::string_view content, diagnostic::DiagnosticConsumer& diag);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H
