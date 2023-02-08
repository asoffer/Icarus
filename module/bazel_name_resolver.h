#ifndef ICARUS_MODULE_BAZEL_NAME_RESOLVER_H
#define ICARUS_MODULE_BAZEL_NAME_RESOLVER_H

#include <string>

#include "absl/functional/any_invocable.h"
#include "base/file.h"
#include "module/module_name.h"
#include "serialization/module_map.h"

namespace module {

absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
BazelNameResolver(std::string const& file_name);

}  // namespace module

#endif  // ICARUS_MODULE_BAZEL_NAME_RESOLVER_H
