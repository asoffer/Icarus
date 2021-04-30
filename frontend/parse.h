#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>
#include <vector>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "ast/node.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"

ABSL_DECLARE_FLAG(bool, debug_parser);

namespace frontend {

std::vector<std::unique_ptr<ast::Node>> Parse(
    Source& src, diagnostic::DiagnosticConsumer& diag, size_t chunk = 0);
}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H
