#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include "compiler/context.h"
#include "module/module.h"
#include "module/writer.h"

namespace compiler {

struct CompiledModule : module::Module {
  explicit CompiledModule(std::string identifier)
      : Module(std::move(identifier)), context_(&ir_module_), module_("") {
    context_.set_qt_callback(
        [&](ast::Declaration::Id const *id, type::QualType qt) {
          if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
            auto &entries = exported_[id->name()];
            indices_.emplace(id, entries.size());
            entries.push_back(
                Module::SymbolInformation{.qualified_type = qt, .id = id});
          }
        });

    context_.set_value_callback(
        [&](ast::Declaration::Id const *id, ir::CompleteResultBuffer buffer) {
          if (id->declaration().hashtags.contains(ir::Hashtag::Export)) {
            auto iter = indices_.find(id);
            if (iter == indices_.end()) { return; }
            exported_[id->name()][iter->second].value = std::move(buffer);
          }
        });
  }

  friend void BaseSerialize(module::ModuleWriter &w, CompiledModule const &m) {
    base::Serialize(w, m.identifier(), type::GlobalTypeSystem, m.exported_);
  }

  Context const &context() const { return context_; }
  Context &context() { return context_; }

  template <std::input_iterator Iter>
  base::PtrSpan<ast::Node const> insert(Iter b, Iter e) {
    return module_.insert(b, e);
  }

  base::PtrSpan<ast::Node const> set_module(ast::Module m) {
    module_ = std::move(m);
    return module_.stmts();
  }

  absl::Span<Module::SymbolInformation const> Exported(std::string_view name) {
    auto iter = exported_.find(name);
    if (iter == exported_.end()) { return {}; }

    // TODO: handle exported embedded modules here too.
    return iter->second;
  }

  ast::Module const &module() const { return module_; }

  ast::Scope const &scope() const { return module_.body_scope(); }
  ast::Scope &scope() { return module_.body_scope(); }

 private:
  ir::Module ir_module_;
  Context context_;

  // It is important for caching that symbols be exported in a consistent
  // manner. We use an ordered container to guarantee repeated invocations
  // produce the same output.
  absl::btree_map<std::string_view, std::vector<SymbolInformation>>
      exported_;
  absl::flat_hash_map<ast::Declaration::Id const *, size_t> indices_;

  ast::Module module_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
