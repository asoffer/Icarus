#ifndef ICARUS_TYPE_PTR_H
#define ICARUS_TYPE_PTR_H

#include <vector>

struct Type;

namespace llvm {
class Type;
} // namespace llvm

struct TypePtr {
  TypePtr(Type *t = nullptr);
  TypePtr(const TypePtr &t) : get(t.get) {}

  TypePtr &operator=(const TypePtr &t);

  std::string to_string() const;

  bool is_primitive() const;
  bool is_array() const;
  bool is_tuple() const;
  bool is_pointer() const;
  bool is_function() const;
  bool is_struct() const;
  bool is_enum() const;
  bool is_fwd_decl() const;
  bool is_dependent_type() const;
  bool is_type_variable() const;

  bool is_big() const;
  bool stores_data() const;

  operator llvm::Type *() const;

  operator bool () const;
  Type *get;
};

std::ostream &operator<<(std::ostream &os, const TypePtr &t);


// Necessary because you can have std::set<TypePtr>
inline bool operator <(TypePtr lhs, TypePtr rhs) { return lhs.get < rhs.get; }

inline bool operator==(TypePtr lhs, TypePtr rhs) { return lhs.get == rhs.get; }
inline bool operator!=(TypePtr lhs, TypePtr rhs) { return !(lhs == rhs); }
#endif
