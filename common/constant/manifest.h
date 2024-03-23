#ifndef ICARUS_COMMON_CONSTANT_MANIFEST_H
#define ICARUS_COMMON_CONSTANT_MANIFEST_H

#include "absl/container/flat_hash_map.h"
#include "common/constant/category.h"
#include "common/constant/component.h"
#include "common/internal/identifiers.h"
#include "common/internal/integers.h"
#include "common/internal/parameters.h"
#include "common/internal/strings.h"
#include "common/result.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/serialize/serialize.h"
#include "nth/utility/bytes.h"

namespace ic {

struct ConstantManifest {
  static ConstantManifest const& Global();

  ConstantComponent const& operator[](size_t n) const;

  size_t size() const { return components_.size(); }

  Constant NextSlot() const;

  std::vector<ConstantComponent> const& table() const { return components_; }

  friend Result NthSerialize(auto& s, ConstantManifest const& manifest) {
    auto const& ids = internal_common::Identifiers();
    nth::io::write_integer(s, ids.size());
    for (auto const& id : ids) { nth::io::serialize(s, id); }

    auto const& integers = internal_common::Integers();
    nth::io::write_integer(s, integers.size());
    for (auto const& [n, unused] : integers) { nth::io::serialize(s, n); }

    auto const& strings = internal_common::Strings();
    nth::io::write_integer(s, strings.size());
    for (auto const& [str, unused] : strings) { nth::io::serialize(s, str); }

    return nth::io::serialize(s, nth::io::as_sequence(manifest.components_));
  }

  friend Result NthDeserialize(auto& d, ConstantManifest& manifest) {
    co_return nth::io::deserialize(d,
                                   nth::io::as_sequence(manifest.components_));
  }

 private:
  static ConstantManifest& MutableGlobal();
  friend Result MergeConstantManifest(auto&);
  friend void InsertIntoGlobalConstantManifest(ConstantCategory, uint32_t);

  std::vector<ConstantComponent> components_;
};

void InsertIntoGlobalConstantManifest(ConstantCategory category, uint32_t n);

Result MergeIntegers(auto& d,
                     absl::flat_hash_map<size_t, Constant>& translation) {
  size_t count;
  auto& integers = internal_common::Integers();
  co_await nth::io::read_integer(d, count);
  for (size_t i = 0; i < count; ++i) {
    nth::integer n;
    co_await nth::io::deserialize(d, n);
    auto [iter, inserted] = integers.try_emplace(
        std::move(n), ConstantManifest::Global().NextSlot());
    translation.emplace(i, iter->second);
  }
  co_return Result::success();
}

Result MergeIdentifiers(auto& d,
                        absl::flat_hash_map<size_t, size_t>& translation) {
  size_t count;
  auto& ids = internal_common::Identifiers();
  co_await nth::io::read_integer(d, count);
  for (size_t i = 0; i < count; ++i) {
    std::string id;
    size_t length;
    co_await nth::io::read_integer(d, length);
    id.resize(length, '\0');
    co_await d.read(ToBytes(id));
    auto [iter, inserted] = ids.insert(std::move(id));
    translation.emplace(i, ids.index(iter));
  }
  co_return Result::success();
}

Result MergeStrings(auto& d,
                    absl::flat_hash_map<size_t, Constant>& translation) {
  size_t count;
  auto& strings = internal_common::Strings();
  co_await nth::io::read_integer(d, count);
  for (size_t i = 0; i < count; ++i) {
    std::string s;
    size_t length;
    co_await nth::io::read_integer(d, length);
    s.resize(length, '\0');
    co_await d.read(ToBytes(s));
    auto [iter, inserted] = strings.try_emplace(
        std::move(s), ConstantManifest::Global().NextSlot());
    translation.emplace(i, iter->second);
  }
  co_return Result::success();
}

Result MergeConstantManifest(auto& d) {
  absl::flat_hash_map<size_t, size_t> id_translation;
  co_await MergeIdentifiers(d, id_translation);

  absl::flat_hash_map<size_t, Constant> integer_translation;
  co_await MergeIntegers(d, integer_translation);

  absl::flat_hash_map<size_t, Constant> string_translation;
  co_await MergeStrings(d, string_translation);

  co_await nth::io::deserialize(d, ConstantManifest::MutableGlobal());

  absl::flat_hash_map<size_t, size_t> type_translation;

  size_t i        = 0;
  std::span table = ConstantManifest::Global().table();
  while (i < table.size()) {
    switch (table[i].category()) {
      case ConstantCategory::String: ++i; break;
      case ConstantCategory::Integer: ++i; break;
      case ConstantCategory::Followup: NTH_UNREACHABLE();
      case ConstantCategory::ParametersType: {
        size_t count = table[i].value();
        std::vector<type::Parameter> parameters;
        for (size_t j = 0; j < count; ++j) {
          auto iter = id_translation.find(table[i + 2 * j + 1].value());
          if (iter == id_translation.end()) { co_return Result(false); }
          parameters.push_back({
              .name = internal_common::Identifiers().from_index(iter->second),
              .type = table[i + 2 * j + 2].value(),
          });
          NTH_LOG("`{}`: @{}") <<=
              {internal_common::Identifiers().from_index(iter->second),
               table[i + 2 * j + 2].value()};
        }
        i += 2 * count + 1;
        auto [iter, inserted] =
            internal_common::Parameters().insert(type::ParameterInsertionType{
                .parameters = parameters,
                .index      = table.size(),
            });
        NTH_LOG("{} {} {}") <<= {*iter, inserted, internal_common::Parameters()};
      } break;
      case ConstantCategory::FunctionType: {
        size_t count = table[i].value();
        std::vector<uint32_t> return_indices;
        for(size_t j=0; j < count; ++j){
          return_indices.push_back(table[i + 2 + j].value());
        }
        NTH_LOG("@{} -> {}") <<= {table[i + 1].value(), return_indices};
        i += count + 3;
      } break;
      case ConstantCategory::SliceType: {
        NTH_LOG("\\@{}") <<= {table[i].value()};
        ++i;
      } break;
      case ConstantCategory::PointerType: {
        NTH_LOG("*@{}") <<= {table[i].value()};
        ++i;
      } break;
      case ConstantCategory::BufferPointerType: {
        NTH_LOG("[*]@{}") <<= {table[i].value()};
        ++i;
      } break;
      default: NTH_UNIMPLEMENTED();
    }
  }

  co_return Result::success();
}

}  // namespace ic

#endif  // ICARUS_COMMON_CONSTANT_MANIFEST_H
