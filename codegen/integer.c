#include<stdio.h>

void *utype(char *name, size_t sz) {
  printf("\npublic export\n");
  printf("0 %s : Type\n", name);
  printf("%s = Bits%d\n", name, 8*sz);
}

void *stype(char *name, size_t sz) {
  printf("\npublic export\n");
  printf("0 %s : Type\n", name);
  printf("%s = Int%d\n", name, 8*sz);
}

void *main() {
  stype("Short", sizeof(short));
  stype("CInt", sizeof(int));
  stype("Long", sizeof(long));
  stype("LongLong", sizeof(long long));

  utype("UShort", sizeof(unsigned short));
  utype("UInt", sizeof(unsigned int));
  utype("ULong", sizeof(unsigned long));
  utype("ULongLong", sizeof(unsigned long long));

  stype("SsizeT", sizeof(ssize_t));
  utype("SizeT", sizeof(size_t));
}
