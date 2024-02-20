#include "type/type_system.h"

#include "common/identifier.h"
#include "nth/debug/debug.h"

namespace ic::type {

PointerType TypeSystem::pointer_type(Type t) {
  return PointerType(pointee_types.index(pointee_types.insert(t).first));
}

BufferPointerType TypeSystem::buffer_pointer_type(Type t) {
  return BufferPointerType(
      buffer_pointee_types.index(buffer_pointee_types.insert(t).first));
}

SliceType TypeSystem::slice_type(Type t) {
  return SliceType(
      slice_element_types.index(slice_element_types.insert(t).first));
}

ParametersType TypeSystem::parameter_type(
    std::vector<ParametersType::Parameter>&& p) {
  return ParametersType(
      parameters.index(parameters.insert(std::move(p)).first));
}

ParametersType TypeSystem::parameter_type(
    std::vector<ParametersType::Parameter> const& p) {
  return ParametersType(parameters.index(parameters.insert(p).first));
}

FunctionType TypeSystem::function(ParametersType p, std::vector<Type> r,
                                  Evaluation e) {
  uint64_t rt = returns.index(returns.insert(std::move(r)).first);
  return FunctionType(functions.index(functions.insert({p, rt, e}).first));
}

TypeSystem::ReindexTable TypeSystem::merge_from(TypeSystem const& ts) {
  enum class Action {
    Reindex,
    Pointer,
    BufferPointer,
    Slice,
    Function,
    ReindexReturn,
    ReindexParameters,
    UpdateParameter,
    ResolveParameters,
    UpdateReturn,
    ResolveReturns,
    Drop,
    Swap,
  } action;

  nth::stack<Action> work;
  union Data {
    uint64_t return_index;
    Type type;
    ParametersType::Parameter* parameter;
    std::vector<ParametersType::Parameter>* parameters;
    Type* return_type;
    std::vector<Type>* returns;
  };
  nth::stack<Data> types;

  size_t i = 0;
  for (Type t : ts.pointee_types) {
    types.push({.type = PointerType(i++)});
    types.push({.type = t});
    work.push(Action::Drop);
    work.push(Action::Pointer);
    work.push(Action::Reindex);
  }

  i = 0;
  for (Type t : ts.buffer_pointee_types) {
    types.push({.type = BufferPointerType(i++)});
    types.push({.type = t});
    work.push(Action::Drop);
    work.push(Action::BufferPointer);
    work.push(Action::Reindex);
  }

  i = 0;
  for (Type t : ts.slice_element_types) {
    types.push({.type = SliceType(i++)});
    types.push({.type = t});
    work.push(Action::Drop);
    work.push(Action::Slice);
    work.push(Action::Reindex);
  }

  i = 0;
  for (auto const& [p, ret_index, ev] : ts.functions) {
    types.push({.type = FunctionType(i++)});
    types.push({.type = p});
    types.push({.return_index = ret_index});
    work.push(Action::Drop);
    work.push(Action::Function);
    work.push(Action::Reindex);
    work.push(Action::Swap);
    work.push(Action::ReindexReturn);
  }

  ReindexTable table;
  table.returns_.resize(ts.returns.size(), -1);
  std::vector<uint64_t>& reindexed_returns = table.returns_;
  absl::flat_hash_map<Type, Type>& reindexed = table.mapping_;

  while (not work.empty()) {
    auto action = work.top();
    work.pop();
    switch (action) {
      case Action::Reindex:
        if (auto iter = reindexed.find(types.top().type);
            iter != reindexed.end()) {
          types.top().type = iter->second;
          continue;
        }
        switch (types.top().type.kind()) {
          case Type::Kind::Opaque: // TODO: Merging here is actually tricky.
          case Type::Kind::Primitive: break;
          case Type::Kind::Pointer:
            work.push(Action::Pointer);
            work.push(Action::Reindex);
            types.push({.type = ts.pointee_types.from_index(
                            types.top().type.index())});
            break;
          case Type::Kind::BufferPointer:
            work.push(Action::BufferPointer);
            work.push(Action::Reindex);
            types.push({.type = ts.buffer_pointee_types.from_index(
                            types.top().type.index())});
            break;
          case Type::Kind::Slice:
            work.push(Action::Slice);
            work.push(Action::Reindex);
            types.push({.type = ts.slice_element_types.from_index(
                            types.top().type.index())});
            break;
          case Type::Kind::Parameters:
            types.push(
                {.parameters = new std::vector<ParametersType::Parameter>(
                     ts.parameters.from_index(types.top().type.index()))});
            work.push(Action::ReindexParameters);
            break;
          default: NTH_UNIMPLEMENTED();
        }
        break;
      case Action::ReindexParameters: {
        auto& parameters = *types.top().parameters;
        work.push(Action::ResolveParameters);
        for (ParametersType::Parameter& p : parameters) {
          work.push(Action::UpdateParameter);
          work.push(Action::Reindex);
          types.push({.parameter = &p});
          types.push({.type = p.type});
        }
      } break;
      case Action::ReindexReturn: {
        if (reindexed_returns[types.top().return_index] != -1) {
          types.top().return_index =
              reindexed_returns[types.top().return_index];
        } else {
          auto* returns = new std::vector<Type>(
              ts.returns.from_index(types.top().return_index));
          types.push({.returns = returns});
          work.push(Action::ResolveReturns);
          for (Type& t : *returns) {
            work.push(Action::UpdateReturn);
            work.push(Action::Reindex);
            types.push({.return_type = &t});
            types.push({.type = t});
          }
        }
      } break;
      case Action::Drop: types.pop(); break;
      case Action::Swap: {
        auto x = types.top();
        types.pop();
        auto y = std::exchange(types.top(), x);
        types.push(y);
      } break;
      case Action::Pointer: {
        Type new_pointee = types.top().type;
        types.pop();
        Type old_pointer = types.top().type;
        types.top()      = {.type = pointer_type(new_pointee)};
        reindexed.emplace(old_pointer, types.top().type);
      } break;
      case Action::BufferPointer: {
        Type new_pointee = types.top().type;
        types.pop();
        Type old_pointer = types.top().type;
        types.top()      = {.type = buffer_pointer_type(new_pointee)};
        reindexed.emplace(old_pointer, types.top().type);
      } break;
      case Action::Slice: {
        Type new_element_type = types.top().type;
        types.pop();
        Type old_slice = types.top().type;
        types.top()    = {.type = slice_type(new_element_type)};
        reindexed.emplace(old_slice, types.top().type);
      } break;
      case Action::UpdateParameter: {
        Type new_parameter_type = types.top().type;
        types.pop();
        ParametersType::Parameter& param_ref = *types.top().parameter;
        types.pop();
        // TODO: Serialization of the name.
        param_ref.name = Identifier("").value();
        param_ref.type = new_parameter_type;
      } break;
      case Action::UpdateReturn: {
        Type new_ret_type = types.top().type;
        types.pop();
        Type& old_ret_type_ptr = *types.top().return_type;
        types.pop();
        old_ret_type_ptr = new_ret_type;
      } break;
      case Action::ResolveParameters: {
        auto* parameters    = types.top().parameters;
        Type new_parameters = parameter_type(std::move(*parameters));
        delete parameters;
        types.pop();
        Type old_parameters = types.top().type;
        types.top()         = {.type = new_parameters};
        reindexed.emplace(old_parameters, types.top().type);
      } break;
      case Action::ResolveReturns: {
        auto* rets = types.top().returns;
        uint64_t new_returns =
            returns.index(returns.insert(std::move(*rets)).first);
        delete rets;
        types.pop();
        uint64_t old_returns           = types.top().return_index;
        types.top()                    = {.return_index = new_returns};
        reindexed_returns[old_returns] = new_returns;
      } break;
      case Action::Function: {
        ParametersType p = types.top().type.AsParameters();
        types.pop();
        uint64_t return_index = types.top().return_index;
        types.pop();

        auto old_function_type = types.top().type;
        auto e =
            std::get<2>(ts.functions.from_index(old_function_type.index()));
        types.top()            = {.type = FunctionType(functions.index(
                                      functions.insert({p, return_index, e}).first))};
        reindexed.emplace(old_function_type, types.top().type);
      } break;
    }
  }
  return table;
}

Type TypeSystem::ReindexTable::operator()(Type t) const {
  if (t.kind() == Type::Kind::Primitive) { return t; }
  auto iter = mapping_.find(t);
  NTH_REQUIRE((v.debug), iter != mapping_.end());
  return iter->second;
}

}  // namespace ic::type
