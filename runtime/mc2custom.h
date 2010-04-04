#ifndef __MC2_CUSTOM_H__
#define __MC2_CUSTOM_H__

#include <mc2types.h>

typedef void *mc2_custom_data_t;
typedef const struct mc2_custom_operations_t *mc2_custom_operations_t;

struct mc2_custom_operations_t {
  void       (*custom_finalize)(mc2_custom_data_t d);
  int        (*custom_compare)(mc2_custom_data_t d1, mc2_custom_data_t d2);
  mc2_long_t (*custom_hash)(mc2_custom_data_t d);
};

static inline mc2_custom_data_t mc2_custom_data_of_block(mc2_block_t b) {
  return (mc2_custom_data_t) &b->block_data[1];
}
static inline mc2_custom_data_t mc2_custom_data_of_value(mc2_value_t v) {
  return mc2_custom_data_of_block(mc2_block_of_value(v));
}
static inline mc2_custom_operations_t mc2_custom_operations_of_block(mc2_block_t b) {
  return (mc2_custom_operations_t) b->block_data[0];
}
static inline mc2_custom_operations_t mc2_custom_operations_of_value(mc2_value_t v) {
  return mc2_custom_operations_of_block(mc2_block_of_value(v));
}

mc2_block_t mc2_custom_alloc(mc2_size_t size, mc2_custom_operations_t ops) MC2_GNUC_MALLOC MC2_GNUC_WARN_UNUSED_RESULT;

#endif /* !__MC2_CUSTOM_H__ */
