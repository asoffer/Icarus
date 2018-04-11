#include "test.h"

#include "base/types.h"
#include "frontend/numbers.h"

TEST(Base2Integer) {
  EXPECT(RepresentationAsIntInBase<2>({0}) == 0);
  EXPECT(RepresentationAsIntInBase<2>({0, 0}) == 0);
  EXPECT(RepresentationAsIntInBase<2>({0, 1}) == 1);
  EXPECT(RepresentationAsIntInBase<2>({1, 0}) == 2);
  EXPECT(RepresentationAsIntInBase<2>({1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                       1, 1, 1, 1, 1, 1, 1, 1, 1}) ==
         std::numeric_limits<i32>::max());
  EXPECT(RepresentationAsIntInBase<2>({1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0}) == -1);
  EXPECT(RepresentationAsIntInBase<2>({1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0, 1}) == -1);
}

TEST(Base8Integer) {
  EXPECT(RepresentationAsIntInBase<8>({0}) == 0);
  EXPECT(RepresentationAsIntInBase<8>({0, 7}) == 7);
  EXPECT(RepresentationAsIntInBase<8>({0, 1}) == 1);
  EXPECT(RepresentationAsIntInBase<8>({1, 1}) == 9);
  EXPECT(RepresentationAsIntInBase<8>({1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}) ==
         std::numeric_limits<i32>::max());
  EXPECT(RepresentationAsIntInBase<8>({2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}) == -1);
  EXPECT(RepresentationAsIntInBase<8>(
             {7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}) == -1);
}

TEST(Base10Integer) {
  EXPECT(RepresentationAsIntInBase<10>({0}) == 0);
  EXPECT(RepresentationAsIntInBase<10>({0, 7}) == 7);
  EXPECT(RepresentationAsIntInBase<10>({0, 1}) == 1);
  EXPECT(RepresentationAsIntInBase<10>({1, 1}) == 11);
  EXPECT(RepresentationAsIntInBase<10>({2, 1, 4, 7, 4, 8, 3, 6, 4, 7}) ==
         std::numeric_limits<i32>::max());
  EXPECT(RepresentationAsIntInBase<10>({2, 1, 4, 7, 4, 8, 3, 6, 4, 8}) == -1);
  EXPECT(RepresentationAsIntInBase<10>({9, 9, 9, 9, 9, 9, 9, 9, 9, 9}) == -1);
}

TEST(Base16Integer) {
  EXPECT(RepresentationAsIntInBase<16>({0}) == 0);
  EXPECT(RepresentationAsIntInBase<16>({0, 7}) == 7);
  EXPECT(RepresentationAsIntInBase<16>({0, 1}) == 1);
  EXPECT(RepresentationAsIntInBase<16>({1, 1}) == 17);
  EXPECT(RepresentationAsIntInBase<16>({7, 15, 15, 15, 15, 15, 15, 15}) ==
         std::numeric_limits<i32>::max());
  EXPECT(RepresentationAsIntInBase<16>({8, 0, 0, 0, 0, 0, 0, 0}) == -1);
  EXPECT(RepresentationAsIntInBase<16>({15, 15, 15, 15, 15, 15, 15, 15}) == -1);
}
