#ifndef ICARUS_BACKEND_BACKEND_H
#define ICARUS_BACKEND_BACKEND_H

#include <deque>

#include "base/any_invocable.h"
#include "base/meta.h"
#include "ir/subroutine.h"
#include "ir/value/fn.h"

namespace backend {
namespace internal_backend {

template <typename T>
struct CompilationPass {
  explicit CompilationPass(base::any_invocable<T(ir::Subroutine const &)> emit)
      : emit_(std::move(emit)) {}

 private:
  base::any_invocable<T(ir::Subroutine const &)> emit_;
};

}  // namespace internal_backend

template <typename... Ts>
struct CompilationPassSet {
  explicit CompilationSet(
      base::any_invocable<T(ir::Subroutine const &)>... emitters)
      : internal_backend::CompilationPass<Ts>(std::move(emitters))... {}

  template <base::one_of<Ts...> T>
  std::pair<T const &, type::Function const *> function(
      ir::LocalFnId id) const {
    ASSERT(id.value() < functions_.size());
    return std::pair<T const &, type::Function const *>(
        std::get<T>(functions_[id.value()]), function_types_[id.value()]);
  }

  ir::LocalFnId MakePlaceholder(type::Function const *f) {
    size_t n = functions_.size();
    functions_.push_back({.type_ = f});
    return ir::LocalFnId(n);
  }

  Fn InsertFunction(type::Function const *t,
                    absl::FunctionRef<void(ir::Subroutine &)> initializer) {
    auto n = ir::LocalFnId(functions_.size());

    ir::Subroutine subroutine(t);
    initializer(subroutine);

    auto &info = functions_.emplace_back(NativeFunctionInformation{
        .byte_code = emit_byte_code_(subroutine),
        .type_     = t,
    });

    return Fn(module_id_, n);
  }

 private:
  using data_type = std::tuple<Ts...>;
  std::deque<data_type> functions_;
  std::deque<type::Function const *> function_types_;
};

}  // namespace backend

#endif  // ICARUS_BACKEND_BACKEND_H
