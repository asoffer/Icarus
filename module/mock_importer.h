#ifndef ICARUS_MODULE_MOCK_IMPORTER_H
#define ICARUS_MODULE_MOCK_IMPORTER_H

#include <string_view>

#include "gmock/gmock.h"
#include "ir/value/module_id.h"
#include "module/importer.h"

namespace module {

struct MockImporter : Importer {
  MOCK_METHOD(ir::ModuleId, Import, (std::string_view module_locator),
              (override));
  MOCK_METHOD(BasicModule const&, get, (ir::ModuleId id), (override));
  MOCK_METHOD(void, CompleteWork, (), (override));
};

}  // namespace module

#endif  // ICARUS_MODULE_MOCK_IMPORTER_H
