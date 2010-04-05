#ifndef __MC2_STRING_H__
#define __MC2_STRING_H__

#include <mc2types.h>

static inline char *mc2_string_data_of_block(mc2_block_t b) {
  return (char *) &b->block_data[0];
}
static inline char *mc2_string_data_of_value(mc2_value_t v) {
  return mc2_string_data_of_block(mc2_block_of_value(v));
}

mc2_block_t mc2_string_alloc(const char *cstr) MC2_GNUC_MALLOC MC2_GNUC_WARN_UNUSED_RESULT;

#endif /* !__MC2_STRING_H__ */
