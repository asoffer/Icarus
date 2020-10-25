#include "compiler/dispatch/parameters_and_arguments.h"

#include "ast/ast.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"

namespace compiler {
namespace {

struct ExprData {
  // TODO you only really have this for testing purposes (same with the
  // non-const params() method.
  explicit ExprData() = default;

  explicit ExprData(type::Type t, core::Params<type::QualType> q,
                    std::vector<type::Type> return_types = {})
      : type_(t),
        params_(std::move(q)),
        return_types_(std::move(return_types)) {}

  type::Type type() const { return type_; }

  std::vector<type::Type> const &return_types() { return return_types_; }

  core::Params<type::QualType> const &params() const & { return params_; }

 private:
  type::Type type_ = nullptr;
  core::Params<type::QualType> params_;
  std::vector<type::Type> return_types_;
};

using param_type = core::Param<type::QualType>;

using ::testing::ElementsAre;

decltype(auto) GetParams(int, ExprData const &data) { return data.params(); };

TEST(ParamsCoverArgs, EmptyArguments) {
  auto args = core::FnArgs<type::QualType>(/* pos = */ {}, /* named = */ {});

  {  // Empty overloads
    absl::flat_hash_map<int, ExprData> table;
    // We need to have an entry in the table... but the default will be empty
    // parameters which should have the proper coverage.
    table[0];
    EXPECT_TRUE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // One overload
    core::Params params{
        param_type("param0", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, ExprData> table{{0, ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }

  {  // Multiple overloads
    core::Params params{
        param_type("param0", type::QualType::NonConstant(type::Bool)),
        param_type("param1", type::QualType::NonConstant(type::Bool))};
    absl::flat_hash_map<int, ExprData> table{{0, ExprData(nullptr, params)}};
    EXPECT_FALSE(ParamsCoverArgs(args, table, GetParams));
  }
}

}  // namespace
}  // namespace compiler
