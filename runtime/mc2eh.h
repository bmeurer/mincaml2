#ifndef __MC2_EH_H__
#define __MC2_EH_H__

#include <mc2types.h>

struct mc2_eh_frame_t {
  volatile void *eh_frame_head;
  void          *eh_frame_data[5];
};

typedef struct mc2_eh_frame_t mc2_eh_frame_t;

mc2_value_t mc2_eh_enter(mc2_eh_frame_t *frame) MC2_GNUC_WARN_UNUSED_RESULT;
void        mc2_eh_leave();
void        mc2_eh_raise(mc2_value_t exn) MC2_GNUC_NORETURN;
void        mc2_eh_raise_out_of_memory() MC2_GNUC_NORETURN;
void        mc2_eh_raise_sys_error(const char *msg) MC2_GNUC_NORETURN;
void        mc2_eh_raise_invalid_argument(const char *msg) MC2_GNUC_NORETURN;
void        mc2_eh_uncaught_exception(mc2_value_t exn) MC2_GNUC_NORETURN;

#endif /* !__MC2_EH_H__ */
