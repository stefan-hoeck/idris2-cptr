// Copyright 2024 Stefan Höck

#include <stdio.h>
#include <sys/types.h>
#include <time.h>

void *utype(char *name, size_t sz) {
  printf("\npublic export\n");
  printf("0 %s : Type\n", name);
  printf("%s = Bits%zd\n", name, 8 * sz);
}

void *stype(char *name, size_t sz) {
  printf("\npublic export\n");
  printf("0 %s : Type\n", name);
  printf("%s = Int%zd\n", name, 8 * sz);
}

void *tsize(char *name, size_t sz) {
  printf("\npublic export %%inline\n");
  printf("%sSize : Bits32\n", name);
  printf("%sSize = %zd\n", name, sz);
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

  stype("PidT", sizeof(pid_t));
  stype("UidT", sizeof(uid_t));
  stype("GidT", sizeof(gid_t));
  stype("IdT", sizeof(id_t));
  utype("KeyT", sizeof(key_t));

  stype("SsizeT", sizeof(ssize_t));
  utype("SizeT", sizeof(size_t));

  stype("ModeT", sizeof(mode_t));
  stype("OffT", sizeof(off_t));
  stype("TimeT", sizeof(time_t));
  stype("ClockT", sizeof(clock_t));
  stype("SusecondsT", sizeof(suseconds_t));
  stype("NsecT", sizeof(struct timespec) - sizeof(time_t));
  tsize("Timespec", sizeof(struct timespec));
  tsize("AnyPtr", sizeof(void *));

  stype("FsBlkCntT", sizeof(fsblkcnt_t));
  stype("FsFilCntT", sizeof(fsfilcnt_t));
  stype("DevT", sizeof(dev_t));
  stype("InoT", sizeof(ino_t));
  stype("NlinkT", sizeof(nlink_t));
  stype("BlkSizeT", sizeof(blksize_t));
  stype("BlkCntT", sizeof(blkcnt_t));

  return 0;
}
