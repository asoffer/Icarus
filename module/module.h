#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "base/cast.h"
#include "base/guarded.h"
#include "base/iterator.h"
#include "base/macros.h"
#include "ir/byte_code/byte_code.h"
#include "ir/byte_code/byte_code_view.h"
#include "ir/subroutine.h"
#include "ir/value/fn.h"
#include "type/qual_type.h"

// TODO: Remove this forward declaration when, when declaration ids are no
// longer needed on symbol information.
namespace ast {
struct Declaration_Id;
}  // namespace ast

namespace module {

enum class Linkage { Internal, External };

// Module:
//
// Represents a unit of compilation, beyond which all intercommunication must be
// explicit.
struct Module : base::Cast<Module> {
  explicit Module(std::string identifier)
      : identifier_(std::move(identifier)) {}
  virtual ~Module() {}

  enum class Visibility { Private, Exported };
  struct SymbolInformation {
    type::QualType qualified_type;
    ir::CompleteResultBuffer value;
    // TODO: Remove this. It's only here as a temporary mechanism to work nicely
    // with generics until we have a decent cross-module solution for them. It
    // will only be populated for generics.
    ast::Declaration_Id const *id;
    Visibility visibility;

    friend std::ostream &operator<<(std::ostream &os,
                                    SymbolInformation const &symbol) {
      std::string_view visibility =
          symbol.visibility == Visibility::Private ? "Private" : "Exported";
      return os << "[" << visibility << " symbol of type "
                << symbol.qualified_type << "]";
    }
  };

  // Returns an identifier for this module unique across all modules being
  // linked together.
  std::string_view identifier() const { return identifier_; }

  // Given a symbol `name`, must return a range of `SymbolInformation`
  // describing all symbols of that name in the module, regardless of
  // visibility. The range of symbol information has no ordering guarantees.
  virtual absl::Span<SymbolInformation const> Symbols(
      std::string_view name) const = 0;

  // Given a symbol `name`, returns a range of `SymbolInformation` describing
  // any symbols of that name in the module which are exported.
  auto Exported(std::string_view name) const {
    return base::iterator_range(
        filter_iterator<Visibility::Exported>(Symbols(name)), filter_end{});
  }

  // Given a symbol `name`, returns a range of `SymbolInformation` describing
  // any symbols of that name in the module which are private.
  auto Private(std::string_view name) const {
    return base::iterator_range(
        filter_iterator<Visibility::Private>(Symbols(name)), filter_end{});
  }

  struct FunctionInformation {
    type::Function const *type;
    ir::Subroutine const *subroutine;
  };
  // Must return a `FunctionInformation` object capturing the type and byte code
  // for the function with the given `id` in this module.
  virtual FunctionInformation Function(ir::LocalFnId id) const = 0;

 protected:
  void set_identifier(std::string id) { identifier_ = std::move(id); }

 private:
  struct filter_end {};
  template <Visibility V>
  struct filter_iterator {
    using iterator_category = std::input_iterator_tag;
    using value_type        = SymbolInformation;
    using difference_type   = size_t;
    using pointer           = value_type const *;
    using reference         = value_type const &;

    constexpr filter_iterator &operator++() {
      ++ptr_;
      for (; ptr_ != end_ and ptr_->visibility != V; ++ptr_) {}
      return *this;
    }
    constexpr filter_iterator operator++(int) { return ++*this; }

    constexpr reference operator*() const { return *ptr_; }
    constexpr pointer operator->() const { return ptr_; }

    friend bool operator==(filter_iterator f, filter_end) {
      return f.ptr_ == f.end_;
    }
    friend bool operator==(filter_end, filter_iterator f) {
      return f.ptr_ == f.end_;
    }
    friend bool operator!=(filter_iterator f, filter_end) {
      return f.ptr_ != f.end_;
    }
    friend bool operator!=(filter_end, filter_iterator f) {
      return f.ptr_ != f.end_;
    }

   private:
    friend struct Module;
    filter_iterator(absl::Span<SymbolInformation const> span)
        : ptr_(span.begin()), end_(span.end()) {
      for (; ptr_ != end_ and ptr_->visibility != V; ++ptr_) {}
    }
    pointer ptr_;
    pointer end_;
  };

  // TODO: Move ModuleId to live here.
  std::string identifier_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
