#ifndef __MC2_RUNTIME_H__
#define __MC2_RUNTIME_H__

#include "values.h"

struct mc2_eh_frame_t {
  volatile void *eh_frame_prev;
  void          *eh_frame_data[5];
};

typedef struct mc2_eh_frame_t mc2_eh_frame_t;

extern volatile mc2_eh_frame_t *mc2_eh_frame_current;

#define MC2_STATIC_STRING_WOSIZE(s) ((sizeof(s) + sizeof(mc2_value_t) - 1) / sizeof(mc2_value_t))
#define MC2_STATIC_STRING_BSIZE(s) (MC2_STATIC_STRING_WOSIZE(s) * sizeof(mc2_value_t))
#define MC2_STATIC_STRING_TYPE(s) struct { mc2_header_t hd; char sz[MC2_STATIC_STRING_BSIZE(s)]; }
#define MC2_STATIC_STRING_HD(s) ((mc2_header_t) (((MC2_STATIC_STRING_WOSIZE(s) + 1) << 8) | MC2_STRING_TAG))
#define MC2_STATIC_STRING_INIT(s) { MC2_STATIC_STRING_HD(s), s }

typedef MC2_STATIC_STRING_TYPE("Match_failure") mc2_Match_failure_t;
typedef MC2_STATIC_STRING_TYPE("Out_of_memory") mc2_Out_of_memory_t;
typedef MC2_STATIC_STRING_TYPE("Stack_overflow") mc2_Stack_overflow_t;
typedef MC2_STATIC_STRING_TYPE("Invalid_argument") mc2_Invalid_argument_t;
typedef MC2_STATIC_STRING_TYPE("Failure") mc2_Failure_t;
typedef MC2_STATIC_STRING_TYPE("Not_found") mc2_Not_found_t;
typedef MC2_STATIC_STRING_TYPE("Division_by_zero") mc2_Division_by_zero_t;

extern const mc2_Match_failure_t mc2_Match_failure;
extern const mc2_Out_of_memory_t mc2_Out_of_memory;
extern const mc2_Stack_overflow_t mc2_Stack_overflow;
extern const mc2_Invalid_argument_t mc2_Invalid_argument;
extern const mc2_Failure_t mc2_Failure;
extern const mc2_Not_found_t mc2_Not_found;
extern const mc2_Division_by_zero_t mc2_Division_by_zero;

extern const mc2_value_t mc2_exn_Out_of_memory[2];
extern const mc2_value_t mc2_exn_Stack_overflow[2];
extern const mc2_value_t mc2_exn_Division_by_zero[2];

extern mc2_value_t mc2_entry();

#endif /* !__MC2_RUNTIME_H__ */
