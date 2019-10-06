#ifndef ICARUS_FRONTEND_PARSE_H
#define ICARUS_FRONTEND_PARSE_H

#include <memory>
#include <vector>

#include "ast/ast_fwd.h"

namespace frontend {
struct Source;
std::vector<std::unique_ptr<ast::Node>> Parse(Source *src);
}  // namespace frontend

#endif  // ICARUS_FRONTEND_PARSE_H
