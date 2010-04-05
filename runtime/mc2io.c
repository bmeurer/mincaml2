#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mc2custom.h>
#include <mc2eh.h>
#include <mc2io.h>
#include <mc2string.h>


static void mc2_io_raise_sys_error(int errnum) MC2_GNUC_NORETURN;


struct mc2_io_t {
  FILE *fp;
};

static const struct mc2_custom_operations_t mc2_io_operations = {
  NULL, /* TODO */
};


static void mc2_io_raise_sys_error(int errnum) {
  mc2_eh_raise_sys_error(strerror(errnum));
}


mc2_block_t mc2_io_fdopen(mc2_value_t fd, mc2_value_t in) {
  mc2_block_t b;
  mc2_io_t io;

  assert(mc2_value_is_long(fd));
  assert(mc2_value_is_long(in));

  b = mc2_custom_alloc(sizeof(*io), &mc2_io_operations);
  io = mc2_custom_data_of_block(b);
  io->fp = fdopen(mc2_long_of_value(fd), (mc2_long_of_value(in) != 0) ? "r" : "w");
  if (io->fp == NULL)
    mc2_io_raise_sys_error(errno);

  return b;
}


mc2_value_t mc2_io_fclose(mc2_value_t channel) {
  mc2_io_t io;

  assert(!mc2_value_is_null(channel));
  assert(mc2_value_is_custom(channel));
  assert(mc2_custom_operations_of_value(channel) == &mc2_io_operations);

  io = mc2_custom_data_of_value(channel);
  if (io->fp != NULL && fclose(io->fp) == EOF)
    mc2_io_raise_sys_error(errno);
  io->fp = NULL;

  return mc2_value_of_long(0);
}


mc2_value_t mc2_io_fflush(mc2_value_t channel) {
  mc2_io_t io;

  assert(!mc2_value_is_null(channel));
  assert(mc2_value_is_custom(channel));
  assert(mc2_custom_operations_of_value(channel) == &mc2_io_operations);

  io = mc2_custom_data_of_value(channel);
  if (io->fp != NULL && fflush(io->fp) == EOF)
    mc2_io_raise_sys_error(errno);

  return mc2_value_of_long(0);
}


mc2_value_t mc2_io_fputc(mc2_value_t channel, mc2_value_t c) {
  mc2_io_t io;

  assert(mc2_value_is_long(c));
  assert(!mc2_value_is_null(channel));
  assert(mc2_value_is_custom(channel));
  assert(mc2_custom_operations_of_value(channel) == &mc2_io_operations);

  io = mc2_custom_data_of_value(channel);
  if (io->fp == NULL)
    mc2_io_raise_sys_error(EBADF);
  if (fputc((int) mc2_long_of_value(c), io->fp) == EOF)
    mc2_io_raise_sys_error(errno);

  return mc2_value_of_long(0);
}


mc2_value_t mc2_io_fputs(mc2_value_t channel, mc2_value_t s) {
  mc2_io_t io;

  assert(!mc2_value_is_null(s));
  assert(mc2_value_is_string(s));
  assert(!mc2_value_is_null(channel));
  assert(mc2_value_is_custom(channel));
  assert(mc2_custom_operations_of_value(channel) == &mc2_io_operations);

  io = mc2_custom_data_of_value(channel);
  if (io->fp == NULL)
    mc2_io_raise_sys_error(EBADF);
  if (fputs(mc2_string_data_of_value(s), io->fp) == EOF)
    mc2_io_raise_sys_error(errno);

  return mc2_value_of_long(0);
}
