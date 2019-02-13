#ifndef ICARUS_MISC_INFERENCE_STATE_H
#define ICARUS_MISC_INFERENCE_STATE_H

#include <queue>
#include <unordered_map>
#include "base/debug.h"

struct Context;

namespace ast {
struct Expression;
struct Declaration;
}

namespace type {
struct Type;
}

struct InferenceState {
  InferenceState(Context *ctx) : ctx_(ASSERT_NOT_NULL(ctx)) {}
  std::queue<std::pair<ast::Expression const *, type::Type const *>> match_queue_;
  std::unordered_map<ast::Declaration const *, type::Type const *> matches_;
  Context *ctx_;
};

#endif  // ICARUS_MISC_INFERENCE_STATE_H
