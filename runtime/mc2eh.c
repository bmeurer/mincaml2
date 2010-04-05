#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mc2core.h>
#include <mc2eh.h>
#include <mc2string.h>


static volatile mc2_eh_frame_t *mc2_eh_current_frame = NULL;
static const struct mc2_block_t mc2_eh_out_of_memory = { 2 << 8, { MC2_EXN_OUT_OF_MEMORY } };


mc2_value_t mc2_eh_enter(mc2_eh_frame_t *frame) {
  assert(frame != NULL);

  /* activate the new frame */
  frame->eh_frame_head = mc2_eh_current_frame;
  mc2_eh_current_frame = frame;
  if (__builtin_setjmp(frame->eh_frame_data)) {
    /* An exception was raised. This will have
     * actually reset the frame to the previous
     * one and saved the exception in this frame.
     */
    mc2_value_t exn = mc2_value_of_block((mc2_block_t) frame->eh_frame_head);

    /* an exception must be a block in either case */
    assert(mc2_value_is_block(exn));
    assert(!mc2_value_is_null(exn));

    return exn;
  }

  /* frame was saved, return unit */
  return mc2_value_of_long(0);
}


void mc2_eh_leave() {
  mc2_eh_frame_t *frame = (mc2_eh_frame_t *) mc2_eh_current_frame;

  assert(frame != NULL);

  mc2_eh_current_frame = frame->eh_frame_head;
#ifndef NDEBUG
  frame->eh_frame_head = NULL;
#endif
}


void mc2_eh_raise(mc2_value_t exn) {
  mc2_eh_frame_t *frame = (mc2_eh_frame_t *) mc2_eh_current_frame;

  assert(mc2_value_is_block(exn));
  assert(!mc2_value_is_null(exn));

  /* check if we have an active exception frame */
  if (frame == NULL) {
    mc2_eh_uncaught_exception(exn);
  }
  else {
    /* remove this frame from the list */
    mc2_eh_current_frame = frame->eh_frame_head;

    /* save the exception in this frame */
    frame->eh_frame_head = mc2_block_of_value(exn);

    /* jump back to the check point */
    __builtin_longjmp(frame->eh_frame_data, 1);
  }
}


void mc2_eh_raise_out_of_memory() {
  mc2_eh_raise(mc2_value_of_block((mc2_block_t) &mc2_eh_out_of_memory));
}


void mc2_eh_raise_sys_error(const char *msg) {
  mc2_block_t exn;

  assert(msg != NULL);

  exn = mc2_core_alloc((3 << 8) | 0);
  mc2_block_setfield(exn, 0, mc2_value_of_block(MC2_EXN_SYS_ERROR));
  mc2_block_setfield(exn, 1, mc2_value_of_block(mc2_string_alloc(msg)));
  mc2_eh_raise(mc2_value_of_block(exn));
}


void mc2_eh_raise_invalid_argument(const char *msg) {
  mc2_block_t exn;

  assert(msg != NULL);

  exn = mc2_core_alloc((3 << 8) | 0);
  mc2_block_setfield(exn, 0, mc2_value_of_block(MC2_EXN_INVALID_ARGUMENT));
  mc2_block_setfield(exn, 1, mc2_value_of_block(mc2_string_alloc(msg)));
  mc2_eh_raise(mc2_value_of_block(exn));
}


void mc2_eh_uncaught_exception(mc2_value_t exn) {
  // TODO - print the exception
  fprintf(stderr, "Fatal error: exception\n");

  // TODO - atexit handling?
  exit(2);
}
