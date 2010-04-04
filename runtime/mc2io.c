#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mc2io.h>


struct mc2_io_t {
  FILE *fp;
};

static const struct mc2_custom_operations_t mc2_io_operations = {
  NULL, /* TODO */
};


mc2_block_t mc2_io_fdopen(mc2_value_t fd, mc2_value_t in) {
  mc2_block_t b;
  mc2_io_t io;

  assert(mc2_value_is_long(fd));
  assert(mc2_value_is_long(in));

  b = mc2_custom_alloc(sizeof(*io), &mc2_io_operations);
  io = mc2_custom_data_of_block(b);
  io->fp = fdopen(mc2_long_of_value(fd), (mc2_long_of_value(in) != 0) ? "r" : "w");
  if (io->fp == NULL) {
    /* TODO */
  }
  return b;
}
