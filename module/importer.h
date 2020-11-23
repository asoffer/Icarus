#ifndef ICARUS_MODULE_IMPORTER_H
#define ICARUS_MODULE_IMPORTER_H

#include <thread>

#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "ir/value/module_id.h"
#include "module/module.h"

namespace module {

// `Importer` is responsible for scheduling any imports requested from an
// `import` expression.
struct Importer {
  virtual ir::ModuleId Import(std::string_view module_locator) = 0;
};

template <typename ModuleType>
struct FileImporter : Importer {
  ir::ModuleId Import(std::string_view module_locator) override {
    auto file_name = frontend::CanonicalFileName::Make(
        frontend::FileName(std::string(module_locator)));
    auto [id, mod, inserted] = ir::ModuleId::FromFile<ModuleType>(file_name);
    if (not inserted) { return id; }

    if (auto maybe_file_src =
            frontend::FileSource::Make(id.template filename<ModuleType>())) {
      std::thread t(
          [mod = mod, file_src = std::move(*maybe_file_src)]() mutable {
            diagnostic::StreamingConsumer diag(stderr, &file_src);
            mod->AppendNodes(frontend::Parse(file_src, diag), diag);
          });
      t.detach();
      return id;
    } else {
      diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
      diag.Consume(frontend::MissingModule{
          .source    = id.template filename<ModuleType>(),
          .requestor = "",
          .reason    = stringify(maybe_file_src),
      });
      return ir::ModuleId::Invalid();
    }
  }
};

}  // namespace module

#endif  // ICARUS_MODULE_IMPORTER_H
