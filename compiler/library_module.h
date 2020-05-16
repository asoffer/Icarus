#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include <thread>

#include "compiler/compiler.h"
#include "compiler/extract_jumps.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"

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
    for (ast::Node const *node : nodes) { c.VerifyType(node); }
    if (c.diag().num_consumed() > 0) { return; }

    for (ast::Node const *node : nodes) { c.EmitValue(node); }
    c.CompleteDeferredBodies();

    CompilationComplete();
  }
};

namespace internal_compiler {

inline base::NoDestructor<base::guarded<
    absl::flat_hash_map<ir::ModuleId, std::unique_ptr<LibraryModule>>>>
    all_library_modules;

}  // namespace internal_compiler

// Returns a pointer to a module of type given by the template parameter, by
// loading the module from the filesystem denoted by the file named `file_name`.
// When the module is returned it may not be ready for consumption yet as the
// processing is started in a separate thread. Each module implementation has
// its own criteria for which parts are available when and how to access them.
// BasicModule provides no such guarantees.
LibraryModule *ImportLibraryModule(
    frontend::CanonicalFileName const &file_name) {
  auto id = ir::ModuleId::FromFile(file_name);

  auto handle = internal_compiler::all_library_modules->lock();

  // TODO Need to add dependencies even if the node was already scheduled
  // (hence the "already scheduled" check is done after this).
  //
  // TODO detect dependency cycles.

  auto [iter, inserted] = handle->try_emplace(id);
  auto &mod             = iter->second;

  if (not inserted) { return mod.get(); }

  if (auto maybe_file_src = frontend::FileSource::Make(id.filename())) {
    mod = std::make_unique<LibraryModule>();

    std::thread t(
        [mod = mod.get(), file_src = std::move(*maybe_file_src)]() mutable {
          auto *src =
              frontend::Source::Make<frontend::FileSource>(std::move(file_src));
          diagnostic::StreamingConsumer diag(stderr, src);
          mod->ProcessFromSource(src, diag);
          // TODO annoying we have to do these together. ProcessFromSource needs to be split.
        });
    t.detach();
    return mod.get();
  } else {
    diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
    diag.Consume(diagnostic::MissingModule{
        .source    = id.filename(),
        .requestor = "",
    });
    return nullptr;
  }
}

}  // namespace compiler

#endif  // ICARUS_COMPILER_LIBRARY_MODULE_H
