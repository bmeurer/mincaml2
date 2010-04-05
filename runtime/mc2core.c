#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <gc/gc.h>

#include <mc2core.h>
#include <mc2custom.h>
#include <mc2eh.h>
#include <mc2string.h>


mc2_block_t mc2_core_alloc(mc2_header_t hd) {
  mc2_block_t block;
  mc2_size_t  bosize;
  mc2_tag_t   tag = mc2_tag_of_header(hd);
  mc2_size_t  wosize = mc2_wosize_of_header(hd);

  assert(tag < 256);
  assert(wosize >= 0);

  bosize = mc2_bosize_of_wosize(wosize + 1);
  block = (mc2_block_t) ((tag < MC2_TAG_ATOMIC)
                         ? GC_MALLOC(bosize)
                         : GC_MALLOC_ATOMIC(bosize));
  if (block == NULL)
    mc2_eh_raise_out_of_memory();
  block->block_header = hd;

  return block;
}


mc2_value_t mc2_core_compare(mc2_value_t v1, mc2_value_t v2) {
  if (mc2_value_is_long(v1) && mc2_value_is_long(v2)) {
    return mc2_value_of_long(mc2_long_of_value(v1) - mc2_long_of_value(v2));
  }
  else if (mc2_value_is_long(v1)) {
    return mc2_value_of_long(-1);
  }
  else if (mc2_value_is_long(v2)) {
    return mc2_value_of_long(1);
  }
  else {
    mc2_long_t wosize;
    mc2_long_t tag;
    mc2_long_t n;

    /* check tags */
    tag = mc2_tag_of_value(v1);
    n = tag - mc2_tag_of_value(v2);
    if (n != 0)
      return mc2_value_of_long(n);

    /* check wosizes */
    wosize = mc2_wosize_of_value(v1);
    n = wosize - mc2_wosize_of_value(v2);
    if (n != 0)
      return mc2_value_of_long(n);

    switch (tag) {
    case MC2_TAG_CLOSURE:
      /* we cannot compare closures */
      mc2_eh_raise_invalid_argument("compare: functional value");

    case MC2_TAG_FLOAT:
      do {
        double d1 = mc2_float_of_value(v1);
        double d2 = mc2_float_of_value(v2);
        if (d1 < d2)
          return mc2_value_of_long(-1);
        else if (d1 > d2)
          return mc2_value_of_long(1);
        else
          return mc2_value_of_long(0);
      } while (0);

    case MC2_TAG_STRING:
      return mc2_value_of_long(strcmp(mc2_string_data_of_value(v1), mc2_string_data_of_value(v2)));

    case MC2_TAG_CUSTOM:
      return mc2_custom_compare(v1, v2);

    default:
      for (n = 0; n < wosize; ++n) {
        mc2_value_t res = mc2_core_compare(mc2_block_field(mc2_block_of_value(v1), n),
                                           mc2_block_field(mc2_block_of_value(v2), n));
        if (mc2_long_of_value(res) != 0)
          return res;
      }
      return mc2_value_of_long(0);
    }
  }
}


int main(int argc, char **argv) {
  extern void mc2_entry();

  GC_all_interior_pointers = 1; /* for closures */
  GC_INIT();

  mc2_entry();

  return EXIT_SUCCESS;
}
