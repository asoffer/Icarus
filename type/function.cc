#include "type/function.h"

#include "base/global.h"

namespace type {

static base::Global<absl::flat_hash_map<
    core::Params<QualType>, absl::node_hash_map<std::vector<Type>, Function>>>
    funcs_;
Function const *Func(core::Params<QualType> in, std::vector<Type> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);

  // output_span is backed by a vector that doesn't move even when the
  // containing function does so this is safe to reference even after `f` is
  // moved.
  auto handle           = funcs_.lock();
  auto &ret_map         = (*handle)[std::move(in)];
  auto [iter, inserted] = ret_map.emplace(out, std::move(f));
  auto const &[ret, fn] = *iter;
  return &fn;
}

void Function::WriteTo(std::string *result) const {
  result->append("(");
  std::string_view sep = "";
  for (auto const &param : params()) {
    result->append(sep);
    if (not param.name.empty()) {
      absl::StrAppend(result, param.name,
                      param.value.constant() ? " :: " : ": ");
    }
    param.value.type().get()->WriteTo(result);
    sep = ", ";
  }
  result->append(") -> (");

  sep = "";
  for (Type out : output()) {
    result->append(sep);
    out.get()->WriteTo(result);
    sep = ", ";
  }
  result->append(")");
}

core::Bytes Function::bytes(core::Arch const &a) const {
  return a.function().bytes();
}

core::Alignment Function::alignment(core::Arch const &a) const {
  return a.function().alignment();
}

}  // namespace type
