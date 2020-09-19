#ifndef ICARUS_MODULE_MOCK_IMPORTER_H
#define ICARUS_MODULE_MOCK_IMPORTER_H

#include "frontend/source/file_name.h"
#include "gmock/gmock.h"
#include "ir/value/module_id.h"
#include "module/importer.h"

namespace module {

struct MockImporter : Importer {
  MOCK_METHOD(ir::ModuleId, Import,
              (frontend::CanonicalFileName const& filename), (override));
};

}  // namespace

#endif // ICARUS_MODULE_MOCK_IMPORTER_H
