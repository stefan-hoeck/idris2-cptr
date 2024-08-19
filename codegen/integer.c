#include<stdio.h>

void *utype(char *name, size_t sz) {
  printf("\npublic export\n");
  printf("0 %s : Type\n", name);
  printf("%s = Bits%d\n", name, 4*sz);
}

void *stype(char *name, size_t sz) {
  printf("\npublic export\n");
  printf("0 %s : Type\n", name);
  printf("%s = Int%d\n", name, 4*sz);
}

void *main() {
  stype("CInt", sizeof(int));

  stype("Short", sizeof(short));

  stype("Long", sizeof(long));

  stype("LongLong", sizeof(long long));
}
