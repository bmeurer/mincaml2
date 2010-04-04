#ifndef __MC2_CORE_H__
#define __MC2_CORE_H__

#include <mc2types.h>

/* exception handling */
struct mc2_frame_t {
  volatile void *frame_head;
  void          *frame_data[5];
};

typedef struct mc2_frame_t mc2_frame_t;

mc2_value_t mc2_core_enter(mc2_frame_t *frame) MC2_GNUC_WARN_UNUSED_RESULT;
void        mc2_core_leave();
void        mc2_core_raise(mc2_value_t exn) MC2_GNUC_NORETURN;
void        mc2_core_raise_oom() MC2_GNUC_NORETURN;

/* memory allocation */
mc2_block_t mc2_core_alloc(mc2_header_t hd) MC2_GNUC_MALLOC MC2_GNUC_WARN_UNUSED_RESULT;

#endif /* !__MC2_CORE_H__ */
