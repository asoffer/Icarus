#ifndef ICARUS_IR_VALUE_NATIVE_FN_H
#define ICARUS_IR_VALUE_NATIVE_FN_H

#include <cstring>
#include <string_view>
#include <memory>
#include <vector>

#include "base/extend.h"
#include "ir/compiled_fn.h"
#include "type/function.h"

namespace ir {
struct NativeFnSet;

// `NativeFn` represents a function callable in the language that is defined
// internally. These are "just normal functions" in the language. The are
// distinguished from foreign functions.
struct NativeFn : base::Extend<NativeFn, 1>::With<base::AbslFormatExtension,
                                                  base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "NativeFn(fn = %p)";

  explicit NativeFn(CompiledFn *fn);

  explicit NativeFn(NativeFnSet *set, type::Function const *fn_type,
                    core::Params<type::Typed<ast::Declaration const *>> p);

  CompiledFn *get() const;
  type::Function const *type() const;

  CompiledFn *operator->() { return get(); }

 private:
  friend base::EnableExtensions;
  friend struct Fn;

  NativeFn() = default;

  CompiledFn *fn_;
};

struct NativeFnSet {
  std::vector<std::unique_ptr<CompiledFn>> fns;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_NATIVE_FN_H
