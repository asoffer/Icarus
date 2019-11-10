#include "base/untyped_buffer.h"

#include "test/catch.h"

namespace base {
namespace {

TEST_CASE("Default constructor invariants") {
  untyped_buffer buf;
  CHECK(buf.size() == 0);
  CHECK(buf.empty());
}

TEST_CASE("Append") {
  untyped_buffer buf;
  buf.append(3);
  buf.append(4);
  CHECK(buf.size() == 2 * sizeof(int));
  CHECK_FALSE(buf.empty());
}

TEST_CASE("Assignment") {
  untyped_buffer buf1, buf2;
  buf1.append(3);

  CHECK_FALSE(buf1.empty());
  buf1 = std::move(buf2);
  CHECK(buf1.empty());
}

TEST_CASE("Access") {
  untyped_buffer buf;

  auto access_1 = buf.append(1);
  CHECK(buf.get<int>(access_1) == 1);

  auto access_2 = buf.append(2);
  auto access_3 = buf.append(3);
  auto access_4 = buf.append(4);
  CHECK(buf.get<int>(access_1) == 1);
  CHECK(buf.get<int>(access_2) == 2);
  CHECK(buf.get<int>(access_3) == 3);
  CHECK(buf.get<int>(access_4) == 4);

  buf.set(access_2, -2);
  CHECK(buf.get<int>(access_1) == 1);
  CHECK(buf.get<int>(access_2) == -2);
  CHECK(buf.get<int>(access_3) == 3);
  CHECK(buf.get<int>(access_4) == 4);
}

TEST_CASE("to_string") {
  CHECK(untyped_buffer{}.to_string() == "");
  CHECK(untyped_buffer{10}.to_string() == "");

  {
    untyped_buffer buf;
    buf.append(char{0});
    buf.append(char{1});
    CHECK(buf.to_string() == "00 01");
  }

  {
    untyped_buffer buf;
    for (char i = 0; i < 13; ++i) { buf.append(i); }
    CHECK(buf.to_string(3, 4) ==
          "    00 01 02\n"
          "    03 04 05\n"
          "    06 07 08\n"
          "    09 0a 0b\n"
          "    0c");
  }
}

TEST_CASE("iterator") {
  untyped_buffer buf;
  buf.append(123);
  buf.append(true);
  buf.append(456);
  buf.append(false);
  auto iter = buf.begin();
  CHECK(iter.read<int>() == 123);
  CHECK(iter.read<bool>());
  CHECK(iter.read<int>() == 456);
  CHECK_FALSE(iter.read<bool>());
}

TEST_CASE("const_iterator") {
  untyped_buffer buf;
  buf.append(123);
  buf.append(true);
  buf.append(456);
  buf.append(false);
  auto iter = static_cast<untyped_buffer const&>(buf).begin();
  CHECK(iter.read<int>() == 123);
  CHECK(iter.read<bool>());
  CHECK(iter.read<int>() == 456);
  CHECK_FALSE(iter.read<bool>());
}

TEST_CASE("unaligned_iterator") {
  // Layout for this buffer will be
  // * 1 byte for bool
  // * 4 bytes for int32_t
  // * 1 byte leftover
  //
  // This means that buf.unaligned_end() should not be equal to the iterator
  // after the int32_t is read.
  auto buf = untyped_buffer::MakeFull(sizeof(int32_t) + 2);

  auto iter = buf.unaligned_begin();
  iter.write(true);
  iter.write(int32_t{0x12345678});

  iter = buf.unaligned_begin();
  iter.skip(1);
  CHECK(int32_t{0x12345678} == iter.read<int>());
  CHECK(iter != buf.unaligned_end());
}
}  // namespace
}  // namespace base
