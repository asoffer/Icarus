#include "type/function.h"

#include "absl/container/node_hash_set.h"
#include "base/global.h"
#include "type/system.h"

namespace type {

static base::Global<absl::node_hash_set<Function>> funcs_;
Function const *Func(core::Params<QualType> in, std::vector<Type> out) {
  auto handle = funcs_.lock();
  auto [iter, inserted] =
      handle->insert(Function(std::move(in), std::move(out), false));
  auto const *fn = &*iter;
  GlobalTypeSystem.insert(Type(fn));
  return fn;
}

Function const *EagerFunc(core::Params<QualType> in, std::vector<Type> out) {
  auto handle = funcs_.lock();
  auto [iter, inserted] =
      handle->insert(Function(std::move(in), std::move(out), true));
  auto const *fn = &*iter;
  GlobalTypeSystem.insert(Type(fn));
  return fn;
}

void Function::WriteTo(std::string *result) const {
  result->append(eager() ? "!(" : "(");
  std::string_view sep = "";
  for (auto const &param : params()) {
    result->append(sep);
    if (not param.name.empty()) {
      absl::StrAppend(result, param.name,
                      param.value.constant() ? " :: " : ": ");
    }
    absl::StrAppend(result, "{", (int)param.flags, "}");
    param.value.type().get()->WriteTo(result);
    sep = ", ";
  }
  result->append(") -> (");

  sep = "";
  for (Type out : return_types()) {
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

void Function::ShowValue(std::ostream &os,
                         ir::CompleteResultRef const &value) const {
  // TODO: Invert the dependency on //ir/value:fn so this can be implemented
  // correctly.
  os << "<<function>>";
}

bool operator==(Function const &lhs, Function const &rhs) {
  return lhs.eager() == rhs.eager() and lhs.params() == rhs.params() and
         lhs.return_types() == rhs.return_types();
}

}  // namespace type
