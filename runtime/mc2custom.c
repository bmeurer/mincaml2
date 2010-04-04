#include <assert.h>
#include <stdlib.h>

#include <mc2core.h>
#include <mc2custom.h>


mc2_block_t mc2_custom_alloc(mc2_size_t size, mc2_custom_operations_t ops) {
  mc2_size_t wosize;
  mc2_block_t b;

  assert(size > 0);
  assert(ops != NULL);

  wosize = 2 + mc2_wosize_of_bosize(size + sizeof(mc2_block_t) - 1);
  b = mc2_core_alloc((wosize << 8) | MC2_TAG_CUSTOM);
  b->block_data[0] = (mc2_block_t) ops;

  return b;
}
