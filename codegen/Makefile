LDFLAGS =
CPPFLAGS =

CC_VERSION = $(shell $(CC) --version)

ifeq ($(findstring clang,$(CC_VERSION)),clang)
	DYLIB_WORKAROUND = cp "${INSTALLDIR}/${TARGET}" "${INSTALLDIR}/${TARGET}.dylib"
else
	DYLIB_WORKAROUND = cp "${INSTALLDIR}/${TARGET}" "${INSTALLDIR}/${TARGET}.so"
	LDFLAGS += -fuse-ld=gold
endif

.PHONY: all
all: integer_gen

.PHONY: integer_gen
integer_gen:
	$(CC) $(LDFLAGS) -o integer_gen integer.c
