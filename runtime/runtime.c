#include <gc/gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "runtime.h"

/* the current exception frame */
volatile mc2_eh_frame_t *mc2_eh_frame_current = NULL;

/* predefined immutable strings */
const mc2_Match_failure_t mc2_Match_failure = MC2_STATIC_STRING_INIT("Match_failure");
const mc2_Out_of_memory_t mc2_Out_of_memory = MC2_STATIC_STRING_INIT("Out_of_memory");
const mc2_Stack_overflow_t mc2_Stack_overflow = MC2_STATIC_STRING_INIT("Stack_overflow");
const mc2_Invalid_argument_t mc2_Invalid_argument = MC2_STATIC_STRING_INIT("Invalid_argument");
const mc2_Failure_t mc2_Failure = MC2_STATIC_STRING_INIT("Failure");
const mc2_Not_found_t mc2_Not_found = MC2_STATIC_STRING_INIT("Not_found");
const mc2_Division_by_zero_t mc2_Division_by_zero = MC2_STATIC_STRING_INIT("Division_by_zero");

/* preallocated exceptions */
const mc2_value_t mc2_exn_Out_of_memory[2] = { (2 << 8), (mc2_value_t) &mc2_Out_of_memory };
const mc2_value_t mc2_exn_Stack_overflow[2] = { (2 << 8), (mc2_value_t) &mc2_Stack_overflow };
const mc2_value_t mc2_exn_Division_by_zero[2] = { (2 << 8), (mc2_value_t) &mc2_Division_by_zero };


mc2_value_t mc2_alloc(mc2_header_t hd) {
  mc2_tag_t tag;
  mc2_size_t size;
  mc2_value_t val;

  tag = mc2_tag_hd(hd);
  size = mc2_wosize_hd(hd) * sizeof(mc2_value_t);
  val = (mc2_value_t)((tag < MC2_ATOMIC_TAG)
                      ? GC_MALLOC(size)
                      : GC_MALLOC_ATOMIC(size));
  if (!val) {
    /* not enough memory, raise Out_of_memory */
    mc2_eh_frame_t *frame = (mc2_eh_frame_t *) mc2_eh_frame_current;
    mc2_eh_frame_current = frame->eh_frame_prev;
    frame->eh_frame_prev = (volatile void *) mc2_exn_Out_of_memory;
    __builtin_longjmp(frame->eh_frame_data, 1);
  }
  mc2_hd_val(val) = hd;
  return val;
}


mc2_value_t mc2_alloc_custom(mc2_size_t bosize, const mc2_custom_operations_t *ops) {
  mc2_value_t val;
  mc2_value_t wosize;
  
  wosize = 2 + ((bosize + sizeof(mc2_value_t) - 1) / sizeof(mc2_value_t));
  val = mc2_alloc(wosize << 8 | MC2_CUSTOM_TAG);
  mc2_custom_operations_val(val) = ops;
  return val;
}


static const mc2_custom_operations_t mc2_io_operations = { /* TODO */
  NULL,
  NULL
};


mc2_value_t mc2_open_descriptor_in(mc2_value_t fd) {
  mc2_value_t val;

  val = mc2_alloc_custom(sizeof(FILE*), &mc2_io_operations);
  ((FILE **) mc2_custom_data_val(val))[0] = fdopen(mc2_int_val(fd), "r");
  return val;
}

mc2_value_t mc2_open_descriptor_out(mc2_value_t fd) {
  mc2_value_t val;

  val = mc2_alloc_custom(sizeof(FILE*), &mc2_io_operations);
  ((FILE **) mc2_custom_data_val(val))[0] = fdopen(mc2_int_val(fd), "w");
  return val;
}

mc2_value_t mc2_flush(mc2_value_t channel) {
  fflush(((FILE **) mc2_custom_data_val(channel))[0]);
  return mc2_val_unit;
}

mc2_value_t mc2_output_string(mc2_value_t channel, mc2_value_t string) {
  fprintf(((FILE **) mc2_custom_data_val(channel))[0], "%s", mc2_string_val(string));
  return mc2_val_unit;
}


int main(int argc, char **argv) {
  mc2_eh_frame_t eh_frame0;

  GC_INIT();

  eh_frame0.eh_frame_prev = NULL;
  mc2_eh_frame_current = &eh_frame0;
  if (__builtin_setjmp(eh_frame0.eh_frame_data)) {
    fprintf(stderr, "Uncaught exception\n");
    return EXIT_FAILURE;
  }
  else {
    mc2_entry();
  }

  return EXIT_SUCCESS;
}
