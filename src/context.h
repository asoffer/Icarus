#ifndef ICARUS_CONTEXT_H
#define ICARUS_CONTEXT_H

#include <map>

#include "ast/bound_constants.h"
#include "error/log.h"

struct Context {
  size_t num_errors() { return error_log_.size(); }
  void DumpErrors() { error_log_.Dump(); }

  error::Log error_log_;
  AST::BoundConstants bound_constants_;
};

#endif // ICARUS_CONTEXT_H
