#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void *cptr_malloc(size_t bytes) {
  if (bytes == 0) {
    return NULL;
  } else {
    return malloc(bytes);
  }
}

void *cptr_calloc(size_t n, size_t size) {
  if (n == 0) {
    return NULL;
  } else {
    return calloc(n, size);
  }
}

void *cptr_free(void *ptr) { free(ptr); }

char *cptr_inc_ptr(char *arr, size_t pos) { return arr + pos; }

uint8_t cptr_deref_bits8(void *ptr) { return *(uint8_t *)ptr; }

uint16_t cptr_deref_bits16(void *ptr) { return *(uint16_t *)ptr; }

uint32_t cptr_deref_bits32(void *ptr) { return *(uint32_t *)ptr; }

uint64_t cptr_deref_bits64(void *ptr) { return *(uint64_t *)ptr; }

int8_t cptr_deref_int8(void *ptr) { return *(int8_t *)ptr; }

int16_t cptr_deref_int16(void *ptr) { return *(int16_t *)ptr; }

int32_t cptr_deref_int32(void *ptr) { return *(int32_t *)ptr; }

int64_t cptr_deref_int64(void *ptr) { return *(int64_t *)ptr; }

void *cptr_set_bits8(void *ptr, uint8_t v) { ((uint8_t *)ptr)[0] = v; }

void *cptr_set_bits16(void *ptr, uint16_t v) { ((uint16_t *)ptr)[0] = v; }

void *cptr_set_bits32(void *ptr, uint32_t v) { ((uint32_t *)ptr)[0] = v; }

void *cptr_set_bits64(void *ptr, uint64_t v) { ((uint64_t *)ptr)[0] = v; }

void *cptr_set_int8(void *ptr, int8_t v) { ((int8_t *)ptr)[0] = v; }

void *cptr_set_int16(void *ptr, int16_t v) { ((int16_t *)ptr)[0] = v; }

void *cptr_set_int32(void *ptr, int32_t v) { ((int32_t *)ptr)[0] = v; }

void *cptr_set_int64(void *ptr, int64_t v) { ((int64_t *)ptr)[0] = v; }

struct timespec *cptr_allocTimespec() {
  return cptr_calloc(1, sizeof(struct timespec));
}
