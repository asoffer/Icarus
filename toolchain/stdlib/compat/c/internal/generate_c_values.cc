#include <cstdio>
#include <cstdlib>
#include <cstring>

char const *FormatSpecifier(char const *type) {
  if (std::strcmp(type, "int") == 0) { return "d"; }
  if (std::strcmp(type, "long") == 0) { return "ld"; }
  if (std::strcmp(type, "long long") == 0) { return "lld"; }
  if (std::strcmp(type, "unsigned int") == 0) { return "u"; }
  if (std::strcmp(type, "unsigned long") == 0) { return "lu"; }
  if (std::strcmp(type, "unsigned long long") == 0) { return "llu"; }
  if (std::strcmp(type, "char") == 0) { return "c"; }
  if (std::strcmp(type, "double") == 0) { return "f"; }
  if (std::strcmp(type, "float") == 0) { return "f"; }
  return "WTF";
}

int main(int argc, char const *argv[]) {
  std::puts("#include <cstdio>");
  std::puts(argv[1]);
  std::puts("int main() {");
  for (int i = 2; i < argc; i += 2) {
    std::printf("  std::printf(\"%s\\t%%%s\\n\", static_cast<%s>(%s));\n",
                argv[i], FormatSpecifier(argv[i + 1]), argv[i + 1], argv[i]);
  }
  std::puts("  return 0;\n}");

  return 0;
}

