#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <mc2core.h>
#include <mc2string.h>


mc2_block_t mc2_string_alloc(const char *cstr) {
  mc2_size_t cstrlen;
  mc2_size_t wosize;
  mc2_block_t b;

  assert(cstr != NULL);

  cstrlen = strlen(cstr);
  wosize = mc2_wosize_of_bosize(cstrlen + sizeof(mc2_block_t));
  b = mc2_core_alloc((wosize << 8) | MC2_TAG_STRING);
  memcpy(b->block_data, cstr, cstrlen + 1);

  return b;
}


mc2_block_t mc2_string_of_int(mc2_value_t i) {
  char buffer[128];

  assert(mc2_value_is_long(i));
  assert(sizeof(mc2_long_t) == sizeof(long));

  snprintf(buffer, sizeof(buffer), "%ld", (long) mc2_long_of_value(i));

  return mc2_string_alloc(buffer);
}


