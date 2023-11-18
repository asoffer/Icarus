#include <climits>
#include <cstdio>
#include <type_traits>

template <typename T>
void PrintIntegerDeclaration(char const *name) {
  std::printf("let %-*s ::= %c%lu\n", 18, name,
              std::is_signed<T>::value ? 'i' : 'u', sizeof(T) * CHAR_BIT);
}

template <typename T>
void PrintFloatingPointDeclaration(char const *name) {
  std::printf("let %-*s ::= f%lu\n", 18, name, sizeof(T) * CHAR_BIT);
}

int main() {
  PrintIntegerDeclaration<signed char>("signed_char");
  PrintIntegerDeclaration<unsigned char>("unsigned_char");
  PrintIntegerDeclaration<signed short>("short");
  PrintIntegerDeclaration<unsigned short>("unsigned_short");
  PrintIntegerDeclaration<signed int>("int");
  PrintIntegerDeclaration<unsigned int>("unsigned_int");
  PrintIntegerDeclaration<signed long>("long");
  PrintIntegerDeclaration<unsigned long>("unsigned_long");
  PrintIntegerDeclaration<signed long long>("long_long");
  PrintIntegerDeclaration<unsigned long long>("unsigned_long_long");
  PrintFloatingPointDeclaration<float>("float");
  PrintFloatingPointDeclaration<double>("double");

  return 0;
}
