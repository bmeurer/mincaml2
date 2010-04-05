#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

#include <mc2core.h>
#include <mc2custom.h>
#include <mc2eh.h>


mc2_block_t mc2_custom_alloc(mc2_size_t size, mc2_custom_operations_t ops) {
  mc2_size_t wosize;
  mc2_block_t b;

  assert(size > 0);
  assert(ops != NULL);

  wosize = 1 + mc2_wosize_of_bosize(size + sizeof(mc2_block_t) - 1);
  b = mc2_core_alloc((wosize << 8) | MC2_TAG_CUSTOM);
  mc2_block_setfield(b, 0, mc2_value_of_block((mc2_block_t) ops));

  return b;
}


mc2_value_t mc2_custom_compare(mc2_value_t v1, mc2_value_t v2) {
  mc2_custom_operations_t ops;
  ptrdiff_t n;

  assert(mc2_value_is_custom(v1));
  assert(mc2_value_is_custom(v2));

  ops = mc2_custom_operations_of_value(v1);
  n = ops - mc2_custom_operations_of_value(v2);
  if (n == 0) {
    if (ops == NULL)
      mc2_eh_raise_invalid_argument("compare: abstract value");
    n = (*ops->custom_compare)(mc2_custom_data_of_value(v1), mc2_custom_data_of_value(v2));
  }

  return mc2_value_of_long((n < 0) ? -1 : ((n > 0) ? 1 : 0));
}
