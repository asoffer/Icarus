#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include <thread>

#include "compiler/compiler.h"
#include "compiler/extract_jumps.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "ir/value/module_id.h"

namespace compiler {
struct LibraryModule : CompiledModule {
  explicit LibraryModule() {}
  ~LibraryModule() override {}

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag) override {
    ExportsComplete();

    Compiler c({
        .builder             = ir::GetBuilder(),
        .data                = data(),
        .diagnostic_consumer = diag,
    });

    for (ast::Node const *node : nodes) {
      ExtractJumps(&c.data().extraction_map_, node);
    }
    c.VerifyAll(nodes);
    if (c.diag().num_consumed() > 0) { return; }

    for (ast::Node const *node : nodes) { c.EmitValue(node); }
    c.CompleteDeferredBodies();

    CompilationComplete();
  }
};

// Returns a pointer to a module of type given by the template parameter, by
// loading the module from the filesystem denoted by the file named `file_name`.
// When the module is returned it may not be ready for consumption yet as the
// processing is started in a separate thread. Each module implementation has
// its own criteria for which parts are available when and how to access them.
// BasicModule provides no such guarantees.
inline ir::ModuleId ImportLibraryModule(
    frontend::CanonicalFileName const &file_name) {
  auto [id, mod, inserted] = ir::ModuleId::FromFile<LibraryModule>(file_name);

  // TODO Need to add dependencies even if the node was already scheduled
  // (hence the "already scheduled" check is done after this).
  //
  // TODO detect dependency cycles.

  if (not inserted) { return id; }

  if (auto maybe_file_src =
          frontend::FileSource::Make(id.filename<LibraryModule>())) {
    std::thread t([mod = mod, file_src = std::move(*maybe_file_src)]() mutable {
      diagnostic::StreamingConsumer diag(stderr, &file_src);
      mod->ProcessFromSource(&file_src, diag);
      // TODO annoying we have to do these together. ProcessFromSource needs
      // to be split.
    });
    t.detach();
    return id;
  } else {
    diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
    diag.Consume(diagnostic::MissingModule{
        .source    = id.filename<LibraryModule>(),
        .requestor = "",
    });
    return ir::ModuleId::Invalid();
  }
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_LIBRARY_MODULE_H
