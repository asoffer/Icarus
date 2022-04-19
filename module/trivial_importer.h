#ifndef ICARUS_MODULE_TRIVIAL_IMPORTER_H
#define ICARUS_MODULE_TRIVIAL_IMPORTER_H

#include <string_view>

#include "gmock/gmock.h"
#include "ir/value/module_id.h"
#include "module/importer.h"
#include "module/shared_context.h"

namespace module {

// Importer that can load modules in a shared context but not add any modules to
// the context.
struct TrivialImporter : Importer {
  explicit TrivialImporter(module::SharedContext* shared_context)
      : shared_context_(*ASSERT_NOT_NULL(shared_context)) {}

  ir::ModuleId Import(Module const* requestor,
                      std::string_view module_locator) override {
    return ir::ModuleId::Invalid();
  }
  Module& get(ir::ModuleId id) override {
    return *ASSERT_NOT_NULL(shared_context_.module_table().module(id));
  }

 private:
  SharedContext& shared_context_;
};

}  // namespace module

#endif  // ICARUS_MODULE_TRIVIAL_IMPORTER_H
