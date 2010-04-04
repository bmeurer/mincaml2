#include <assert.h>
#include <gc/gc.h>

#include <mc2core.h>


static volatile mc2_frame_t *mc2_current_frame = NULL;


mc2_value_t mc2_core_enter(mc2_frame_t *frame) {
  assert(frame != NULL);

  /* activate the new frame */
  frame->frame_head = mc2_current_frame;
  mc2_current_frame = frame;
  if (__builtin_setjmp(frame->frame_data)) {
    /* An exception was raised. This will have
     * actually reset the frame to the previous
     * one and saved the exception in this frame.
     */
    mc2_value_t exn = mc2_value_of_block((mc2_block_t) frame->frame_head);

    /* an exception must be a block in either case */
    assert(mc2_value_is_block(exn));
    assert(!mc2_value_is_null(exn));

    return exn;
  }

  /* frame was saved, return unit */
  return mc2_value_of_long(0);
}


void mc2_core_leave() {
  mc2_frame_t *frame = (mc2_frame_t *) mc2_current_frame;

  assert(frame != NULL);

  mc2_current_frame = frame->frame_head;
#ifndef NDEBUG
  frame->frame_head = NULL;
#endif
}


void mc2_core_raise(mc2_value_t exn) {
  mc2_frame_t *frame = (mc2_frame_t *) mc2_current_frame;

  assert(frame != NULL);
  assert(mc2_value_is_block(exn));
  assert(!mc2_value_is_null(exn));

  /* remove this frame from the list */
  mc2_current_frame = frame->frame_head;

  /* save the exception in this frame */
  frame->frame_head = mc2_block_of_value(exn);

  /* jump back to the check point */
  __builtin_longjmp(frame->frame_data, 1);
}


void mc2_core_raise_oom() {
  static const struct mc2_block_t oom = { 2 << 8, { MC2_EXN_OUT_OF_MEMORY } };

  mc2_core_raise(mc2_value_of_block((mc2_block_t) &oom));
}

  
mc2_block_t mc2_core_alloc(mc2_header_t hd) {
  mc2_block_t block;
  mc2_size_t  bosize;
  mc2_tag_t   tag = mc2_tag_of_header(hd);
  mc2_size_t  wosize = mc2_wosize_of_header(hd);

  assert(tag < 256);
  assert(wosize > 0);

  bosize = mc2_bosize_of_wosize(wosize);
  block = (mc2_block_t) ((tag < MC2_TAG_ATOMIC)
                         ? GC_MALLOC(bosize)
                         : GC_MALLOC_ATOMIC(bosize));
  if (block == NULL)
    mc2_core_raise_oom();
  block->block_header = hd;

  return block;
}
