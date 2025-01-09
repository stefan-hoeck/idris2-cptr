// Copyright 2024 Stefan Höck

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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

void *cptr_copy(void *src, void *dest, size_t len) { memcpy(dest, src, len); }

void *cptr_free(void *ptr) { free(ptr); }

char *cptr_inc_ptr(char *arr, size_t n, size_t pos) { return arr + n * pos; }

uint8_t get_bits8(uint8_t *ptr, uint32_t ix) { return ptr[ix]; }

void set_bits8(uint8_t *ptr, uint32_t ix, uint8_t v) { ptr[ix] = v; }

uint8_t cptr_deref_bits8(void *ptr) { return *(uint8_t *)ptr; }

uint16_t cptr_deref_bits16(void *ptr) { return *(uint16_t *)ptr; }

uint32_t cptr_deref_bits32(void *ptr) { return *(uint32_t *)ptr; }

uint64_t cptr_deref_bits64(void *ptr) { return *(uint64_t *)ptr; }

int8_t cptr_deref_int8(void *ptr) { return *(int8_t *)ptr; }

int16_t cptr_deref_int16(void *ptr) { return *(int16_t *)ptr; }

int32_t cptr_deref_int32(void *ptr) { return *(int32_t *)ptr; }

int64_t cptr_deref_int64(void *ptr) { return *(int64_t *)ptr; }

char *cptr_deref_str(void **ptr) { return ptr[0]; }

uint8_t cptr_is_null(void **ptr) { return ptr[0] == NULL; }

void *cptr_set_bits8(void *ptr, uint8_t v) { ((uint8_t *)ptr)[0] = v; }

void *cptr_set_bits16(void *ptr, uint16_t v) { ((uint16_t *)ptr)[0] = v; }

void *cptr_set_bits32(void *ptr, uint32_t v) { ((uint32_t *)ptr)[0] = v; }

void *cptr_set_bits64(void *ptr, uint64_t v) { ((uint64_t *)ptr)[0] = v; }

void *cptr_set_int8(void *ptr, int8_t v) { ((int8_t *)ptr)[0] = v; }

void *cptr_set_int16(void *ptr, int16_t v) { ((int16_t *)ptr)[0] = v; }

void *cptr_set_int32(void *ptr, int32_t v) { ((int32_t *)ptr)[0] = v; }

void *cptr_set_int64(void *ptr, int64_t v) { ((int64_t *)ptr)[0] = v; }

void *cptr_set_str(void **ptr, char *v) { ptr[0] = v; }

void *cptr_set_null(void **ptr) { cptr_set_str(ptr, NULL); }

struct timespec *cptr_allocTimespec() {
  return cptr_calloc(1, sizeof(struct timespec));
}
