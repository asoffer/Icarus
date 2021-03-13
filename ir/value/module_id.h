#ifndef ICARUS_IR_VALUE_MODULE_ID_H
#define ICARUS_IR_VALUE_MODULE_ID_H

#include <cstddef>
#include <memory>

#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/flyweight_map.h"
#include "base/global.h"
#include "frontend/source/file_name.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ir {
namespace internal_module {

// Because these are never deleted, there's no need to store the module ids in a
// std::unique_ptr.
struct ModuleData {
  base::flyweight_map<frontend::CanonicalFileName> ids;
  std::vector<module::BasicModule *> modules;
};

inline base::Global<ModuleData> all_modules;

}  // namespace internal_module

// A value-type representing a module (unit of compilation).
struct ModuleId : base::Extend<ModuleId, 1>::With<base::AbslHashExtension,
                                                  base::AbslFormatExtension> {
  static constexpr std::string_view kAbslFormatString = "ModuleId(%u)";

  constexpr ModuleId() : id_(std::numeric_limits<size_t>::max()) {}
  constexpr explicit ModuleId(size_t n) : id_(n) {}

  static constexpr ModuleId Invalid() {
    return ModuleId(std::numeric_limits<size_t>::max());
  }

  template <typename ModuleType>
  static std::tuple<ModuleId, ModuleType*, bool> FromFile(
      frontend::CanonicalFileName const& filename) {
    auto handle   = internal_module::all_modules.lock();
    size_t id     = handle->ids.get(filename);
    bool inserted = false;
    ModuleType* p;
    if (id == handle->modules.size()) {
      inserted = true;
      p        = new ModuleType;
      handle->modules.push_back(p);
    } else {
      // TODO: Not sure this is allowed, but it's a workaround for the time
      // being because BasicModule isn't complete yet.
      p = reinterpret_cast<ModuleType*>(handle->modules.back());
    }
    ASSERT(id < handle->modules.size());
    return std::tuple(ModuleId(id), p, inserted);
  }

  frontend::CanonicalFileName const& filename() const {
    return internal_module::all_modules.lock()->ids.get(id_);
  }

  module::BasicModule const* get() const {
    auto handle = internal_module::all_modules.lock();
    return handle->modules[id_];
  }

 private:
  friend base::EnableExtensions;

  size_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_MODULE_ID_H
