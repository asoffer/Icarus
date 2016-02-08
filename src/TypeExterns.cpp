#include "Type.h"

Primitive* Error;
Primitive* Unknown;
Primitive* Bool;
Primitive* Char;
Primitive* Int;
Primitive* Real;
Primitive* Type_;
Primitive* Uint;
Primitive* Void;
Pointer* RawPtr;
Structure* String;

namespace TypeSystem {
  std::map<std::string, Type*> Literals;
  std::map<Language::Operator, std::vector<Function*>> operator_table;

  Type* get(const std::string& name) {
    auto enum_ptr = Enum(name);
    if (enum_ptr) return enum_ptr;

    auto struct_ptr = Struct(name);
    assert(struct_ptr && "No struct found matching this name");
    return struct_ptr;
  }


  void initialize() {
    // TODO do we need to pair of strings and their types?
    Literals["bool"] = Bool  = new Primitive(Primitive::TypeEnum::Bool);
    Literals["char"] = Char  = new Primitive(Primitive::TypeEnum::Char);
    Literals["int"]  = Int   = new Primitive(Primitive::TypeEnum::Int);
    Literals["real"] = Real  = new Primitive(Primitive::TypeEnum::Real);
    Literals["type"] = Type_ = new Primitive(Primitive::TypeEnum::Type);
    Literals["uint"] = Uint  = new Primitive(Primitive::TypeEnum::Uint);
    Literals["void"] = Void  = new Primitive(Primitive::TypeEnum::Void);

    Error   = new Primitive(Primitive::TypeEnum::Error);
    Unknown = new Primitive(Primitive::TypeEnum::Unknown);
    RawPtr = Ptr(Char);

    operator_table[Language::Operator::Arrow] = { Func({ Type_, Type_ }, Type_) };

    operator_table[Language::Operator::OrEq]  = { Func({ Bool, Bool }, Void) };
    operator_table[Language::Operator::XorEq] = { Func({ Bool, Bool }, Void) };
    operator_table[Language::Operator::AndEq] = { Func({ Bool, Bool }, Void) };

    operator_table[Language::Operator::AddEq] = {
      Func({ Int,  Int }, Void),
      Func({ Uint, Uint }, Void),
      Func({ Real, Real }, Void)
    };

    operator_table[Language::Operator::SubEq] = {
      Func({ Int,  Int }, Void),
      Func({ Uint, Uint }, Void),
      Func({ Real, Real }, Void)
    };

    operator_table[Language::Operator::MulEq] = {
      Func({ Int,  Int }, Void),
      Func({ Uint, Uint }, Void),
      Func({ Real, Real }, Void)
    };

    operator_table[Language::Operator::DivEq] = {
      Func({ Int,  Int }, Void),
      Func({ Uint, Uint }, Void),
      Func({ Real, Real }, Void)
    };

    operator_table[Language::Operator::ModEq] = {
      Func({ Int,  Int }, Void),
      Func({ Uint, Uint }, Void),
      Func({ Real, Real }, Void)
    };

    operator_table[Language::Operator::Or]  = { Func({ Bool, Bool }, Bool) };
    operator_table[Language::Operator::Xor] = { Func({ Bool, Bool }, Bool) };
    operator_table[Language::Operator::And] = { Func({ Bool, Bool }, Bool) };

    operator_table[Language::Operator::LessThan] = {
      Func({ Int,  Int }, Bool),
      Func({ Uint, Uint }, Bool),
      Func({ Real, Real }, Bool)
    };

    operator_table[Language::Operator::LessEq] = {
      Func({ Int,  Int }, Bool),
      Func({ Uint, Uint }, Bool),
      Func({ Real, Real }, Bool)
    };

    operator_table[Language::Operator::Equal] = {
      Func({ Bool,  Bool }, Bool),
      Func({ Char,  Char }, Bool),
      Func({ Int,  Int }, Bool),
      Func({ Uint, Uint }, Bool),
      Func({ Real, Real }, Bool),
      Func({ Type_, Type_ }, Bool)
    };

    operator_table[Language::Operator::NotEqual] = {
      Func({ Bool,  Bool }, Bool),
      Func({ Char,  Char }, Bool),
      Func({ Int,  Int }, Bool),
      Func({ Uint, Uint }, Bool),
      Func({ Real, Real }, Bool),
      Func({ Type_, Type_ }, Bool)
    };

    operator_table[Language::Operator::GreaterEq] = {
      Func({ Int,  Int }, Bool),
      Func({ Uint, Uint }, Bool),
      Func({ Real, Real }, Bool)
    };

    operator_table[Language::Operator::GreaterThan] = {
      Func({ Int,  Int }, Bool),
      Func({ Uint, Uint }, Bool),
      Func({ Real, Real }, Bool)
    };

    operator_table[Language::Operator::Add] = {
      Func({ Int,  Int }, Int),
      Func({ Uint, Uint }, Uint),
      Func({ Real, Real }, Real)
    };

    operator_table[Language::Operator::Sub] = {
      Func({ Int,  Int }, Int),
      Func({ Uint, Uint }, Uint),
      Func({ Real, Real }, Real)
    };

    operator_table[Language::Operator::Mul] = {
      Func({ Int,  Int }, Int),
      Func({ Uint, Uint }, Uint),
      Func({ Real, Real }, Real)
    };

    operator_table[Language::Operator::Div] = {
      Func({ Int,  Int }, Int),
      Func({ Uint, Uint }, Uint),
      Func({ Real, Real }, Real)
    };

    operator_table[Language::Operator::Mod] = {
      Func({ Int,  Int }, Int),
      Func({ Uint, Uint }, Uint)
    };

    operator_table[Language::Operator::Not] = { Func(Bool, Bool) };
  }

  // TODO make this lookup better
  Type* get_operator(Language::Operator op, Type* signature) {
    auto operator_set = operator_table[op];
    for (const auto& fn : operator_set) {
      if (signature == fn->argument_type()) {
        return fn->return_type();
      }
    }
    return nullptr;
  }

}  // namespace TypeSystem

Array* Arr(Type* t) {
  static std::vector<Array*> array_types_;
  for (auto arr : array_types_)
    if (arr->type_ == t) return arr;

  auto arr_type = new Array(t);
  array_types_.push_back(arr_type);
  return arr_type;
}

Tuple* Tup(const std::vector<Type*>& types) {
  static std::vector<Tuple*> tuple_types_;
  for (auto tuple_type : tuple_types_) {
    if (tuple_type->entry_types() == types) return tuple_type;
  }

  auto tuple_type = new Tuple(types);
  tuple_types_.push_back(tuple_type);

  return tuple_type;
}

Pointer* Ptr(Type* t) {
  static std::vector<Pointer*> pointer_types_;
  for (const auto& ptr : pointer_types_) {
    if (ptr->pointee_type() == t) return ptr;
  }

  auto ptr_type = new Pointer(t);
  pointer_types_.push_back(ptr_type);
  return ptr_type;
}

Function* Func(Type* in, Type* out) {
  static std::vector<Function*> fn_types_;
  for (const auto& fn_type : fn_types_) {
    if (fn_type->argument_type() != in) continue;
    if (fn_type->return_type() != out)  continue;
    return fn_type;
  }

  auto fn_type = new Function(in, out);
  fn_types_.push_back(fn_type);
  return fn_type;
}

Function* Func(std::vector<Type*> in, Type* out) {
  switch (in.size()) {
    case 0:  return Func(Void, out);
    case 1:  return Func(in.front(), out);
    default: return Func(Tup(in), out);
  }
}

Function* Func(Type* in, std::vector<Type*> out) {
  switch (out.size()) {
    case 0:  return Func(in, Void);
    case 1:  return Func(in, out.front());
    default: return Func(in, Tup(out));
  }
}

Function* Func(std::vector<Type*> in, std::vector<Type*> out) {
  switch (in.size()) {
    case 0:  return Func(Void, out);
    case 1:  return Func(in.front(), out);
    default: return Func(Tup(in), out);
  }
}

Enumeration* Enum(const std::string& name, const AST::EnumLiteral* e) {
  static std::map<std::string, Enumeration*> enum_types_;
  auto iter = enum_types_.find(name);
  if (iter != enum_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (e == nullptr) return nullptr;

  auto enum_type = new Enumeration(name, e);
  return enum_types_[name] = enum_type;
}

Structure* Struct(const std::string& name, AST::TypeLiteral* t) {
  static std::map<std::string, Structure*> struct_types_;
  auto iter = struct_types_.find(name);
  if (iter != struct_types_.end()) return iter->second;

  // If you don't provide something to create it with,
  // it's just meant to be a check for existance
  // TODO merge this with Contexts
  if (t == nullptr) return nullptr;

  auto struct_type = new Structure(name, t);

  if (struct_type->to_string() == "string") String = struct_type;

  return struct_types_[name] = struct_type;
}
