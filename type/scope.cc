#include "type/scope.h"

#include "absl/container/node_hash_map.h"
#include "base/global.h"

namespace type {

static base::Global<absl::node_hash_map<core::Parameters<QualType>, Scope>>
    scopes_;
Scope const *Scp(core::Parameters<QualType> in) {
  auto f      = Scope(in);
  auto handle = scopes_.lock();
  auto const &[iter, inserted] =
      handle->try_emplace(std::move(in), std::move(f));
  return &iter->second;
}

void Scope::WriteTo(std::string *result) const {
  result->append("scope (");
  std::string_view sep = "";
  for (auto const &param : parameters()) {
    result->append(sep);
    if (not param.name.empty()) {
      absl::StrAppend(result, param.name,
                      param.value.constant() ? " :: " : ": ");
    }
    param.value.type().get()->WriteTo(result);
    sep = ", ";
  }
  result->append(")");
}

core::Bytes Scope::bytes(core::Arch const &a) const {
  return a.pointer().bytes();
}

core::Alignment Scope::alignment(core::Arch const &a) const {
  return a.pointer().alignment();
}

void Scope::ShowValue(std::ostream &os,
                      ir::CompleteResultRef const &value) const {
  // TODO: Invert the dependency on //ir/value:fn so this can be implemented
  // correctly.
  os << "<<scope>>";
}

}  // namespace type
