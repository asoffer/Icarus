#ifndef ICARUS_MODULE_TRIVIAL_IMPORTER_H
#define ICARUS_MODULE_TRIVIAL_IMPORTER_H

#include <string_view>

#include "gmock/gmock.h"
#include "ir/value/module_id.h"
#include "module/importer.h"

namespace module {

struct TrivialImporter : Importer {
  ir::ModuleId Import(Module const* requestor,
                      std::string_view module_locator) override {
    return ir::ModuleId::Invalid();
  }
  Module& get(ir::ModuleId id) override { UNREACHABLE(); }
};

}  // namespace module

#endif  // ICARUS_MODULE_TRIVIAL_IMPORTER_H
