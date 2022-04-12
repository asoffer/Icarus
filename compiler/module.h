#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include "ast/module.h"
#include "compiler/context.h"
#include "compiler/instructions.h"
#include "module/module.h"
#include "module/writer.h"

namespace compiler {

struct CompiledModule : module::Module {
  explicit CompiledModule(std::string identifier, ir::ModuleId id)
      : Module(std::move(identifier)),
        ir_module_(id, EmitByteCode),
        context_(&ir_module_),
        module_(this),
        id_(id) {
    context_.set_qt_callback([&](ast::Declaration::Id const *id,
                                 type::QualType qt) {
      auto &entries = exported_[id->name()];
      indices_.emplace(id, entries.size());
      entries.push_back(Module::SymbolInformation{
          .qualified_type = qt,
          .id             = id,
          .visibility = id->declaration().hashtags.contains(ir::Hashtag::Export)
                            ? Visibility::Exported
                            : Visibility::Private,
      });
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

  constexpr ir::ModuleId id() const { return id_; }

  friend void BaseSerialize(module::ModuleWriter &w, CompiledModule const &m) {
    base::Serialize(w, m.identifier(), type::GlobalTypeSystem, m.exported_);
  }

  Context const &context() const { return context_; }
  Context &context() { return context_; }

  template <std::input_iterator Iter>
  base::PtrSpan<ast::Node const> insert(Iter b, Iter e) {
    return module_.insert(b, e);
  }

  absl::Span<Module::SymbolInformation const> Symbols(
      std::string_view name) const override {
    auto iter = exported_.find(name);
    if (iter == exported_.end()) { return {}; }

    // TODO: handle exported embedded modules here too.
    return iter->second;
  }

  FunctionInformation Function(ir::LocalFnId id) const override {
    auto const &info = ir_module_.function(id);
    return FunctionInformation{.type      = info.type(),
                               .byte_code = &info.byte_code};
  }

  bool has_error_in_dependent_module() const {
    return depends_on_module_with_errors_;
  }
  void set_dependent_module_with_errors() {
    depends_on_module_with_errors_ = true;
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
  // This flag should be set to true if this module is ever found to depend on
  // another which has errors, even if those errors do not effect
  // code-generation in this module.
  //
  // TODO: As we move towards separate compilation in separate processes, this
  // will become irrelevant.
  bool depends_on_module_with_errors_ = false;

  ast::Module module_;
  ir::ModuleId id_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
