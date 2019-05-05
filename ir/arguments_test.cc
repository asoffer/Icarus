#include "ir/arguments.h"

#include "test/catch.h"
#include "test/ordered_elements_are.h"
#include "type/array.h"
#include "type/function.h"

namespace ir {
namespace {
using test::OrderedElementsAre;

TEST_CASE("construction") {
  SECTION("void function") {
    Arguments args(type::Func({}, {}));
    CHECK(args.input_types().size() == 0u);
  }

  SECTION("function with one arg") {
    Arguments args(type::Func({type::Bool}, {}));
    CHECK_THAT(args.input_types(), OrderedElementsAre(type::Bool));
  }

  SECTION("function with multiple args") {
    Arguments args(type::Func({type::Bool, type::Int64}, {}));
    CHECK_THAT(args.input_types(), OrderedElementsAre(type::Bool, type::Int64));
  }
}

TEST_CASE("prepare call buffer") {
  SECTION("empty") {
    Arguments args(type::Func({type::Bool, type::Int64}, {}));
    base::untyped_buffer regs;
    auto result_buf = args.PrepareCallBuffer({}, regs);
    CHECK(result_buf.size() == 0);
  }

  SECTION("multiple immediate args") {
    Arguments args(type::Func({type::Bool, type::Int64}, {}),
                   Results{true, uint64_t{17}});
    base::untyped_buffer regs;
    auto result_buf =
        args.PrepareCallBuffer({{Reg::Arg(0), 0}, {Reg::Arg(1), 8}}, regs);
    CHECK(result_buf.size() == 16);
    CHECK(result_buf.get<bool>(0) == true);
    CHECK(result_buf.get<uint64_t>(8) == 17);
  }

  SECTION("mixed register/immediate args") {
    Arguments args(type::Func({type::Bool, type::Int64}, {}),
                   Results{true, Reg{7}});
    base::untyped_buffer regs;
    regs.append(uint64_t{17});
    auto result_buf = args.PrepareCallBuffer({{Reg{7}, 0}}, regs);
    CHECK(result_buf.size() == 16);
    CHECK(result_buf.get<bool>(0) == true);
    CHECK(result_buf.get<uint64_t>(8) == 17);
  }

  SECTION("multiple register args") {
    Arguments args(type::Func({type::Bool, type::Int64}, {}),
                   Results{Reg{13}, Reg{7}});
    base::untyped_buffer regs;
    regs.append(true);
    regs.append(uint64_t{17});
    auto result_buf =
        args.PrepareCallBuffer({{Reg{13}, 0}, {Reg{7}, 8}}, regs);
    CHECK(result_buf.size() == 16);
    CHECK(result_buf.get<bool>(0) == true);
    CHECK(result_buf.get<uint64_t>(8) == 17);
  }

  SECTION("big args") {
    Arguments args(type::Func({type::Arr(3, type::Int64)}, {}),
                   Results{Reg{13}});
    base::untyped_buffer regs;
    regs.append(Addr::ReadOnly(123));
    auto result_buf = args.PrepareCallBuffer({{Reg{13}, 0}}, regs);
    CHECK(result_buf.size() == sizeof(Addr));
    CHECK(result_buf.get<Addr>(0) == Addr::ReadOnly(123));
  }
}

}  // namespace
}  // namespace ir
