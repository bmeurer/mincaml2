#ifndef __MC2_TYPES_H__
#define __MC2_TYPES_H__

#include <stdint.h>

#if defined(__GNUC__) && defined(__GNUC_MINOR__)
# define MC2_GNUC_PREREQ(major, minor) ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((major) << 16) + (minor))
#else
# define MC2_GNUC_PREREQ(major, minor) 0
#endif

#if MC2_GNUC_PREREQ(2,5)
# define MC2_GNUC_NORETURN __attribute__((__noreturn__))
#else
# define MC2_GNUC_NORETURN
#endif

#if MC2_GNUC_PREREQ(2,96)
# define MC2_GNUC_MALLOC __attribute__((__malloc__))
#else
# define MC2_GNUC_MALLOC
#endif

#if MC2_GNUC_PREREQ(3,4)
# define MC2_GNUC_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
# define MC2_GNUC_WARN_UNUSED_RESULT
#endif

typedef uintptr_t mc2_tag_t;
typedef uintptr_t mc2_header_t;
typedef struct mc2_block_t *mc2_block_t;
typedef intptr_t mc2_long_t;
typedef uintptr_t mc2_size_t;
typedef union mc2_value_t mc2_value_t;

struct mc2_block_t {
  mc2_header_t block_header;
  mc2_block_t  block_data[1];
};

union mc2_value_t {
  mc2_block_t value_block;
  mc2_long_t value_long;
};

static inline int mc2_value_is_null(mc2_value_t v) { return v.value_long == 0; }
static inline int mc2_value_is_block(mc2_value_t v) { return (v.value_long & 1) == 0; }
static inline int mc2_value_is_long(mc2_value_t v) { return (v.value_long & 1) != 0; }

static inline mc2_value_t mc2_value_of_block(mc2_block_t b) {
  mc2_value_t v; v.value_block = b; return v;
}
static inline mc2_value_t mc2_value_of_long(mc2_long_t l) {
  mc2_value_t v; v.value_long = (l << 1) | 1; return v;
}
static inline mc2_block_t mc2_block_of_value(mc2_value_t v) { return v.value_block; }
static inline mc2_long_t mc2_long_of_value(mc2_value_t v) { return v.value_long >> 1; }

static inline mc2_value_t mc2_block_field(mc2_block_t b, int n) {
  return mc2_value_of_block(b->block_data[n]);
}
static inline void mc2_block_setfield(mc2_block_t b, int n, mc2_value_t v) {
  b->block_data[n] = mc2_block_of_value(v);
}

static inline double mc2_float_of_block(mc2_block_t b) {
  return ((double *) &b->block_data[0])[0];
}
static inline double mc2_float_of_value(mc2_value_t v) {
  return mc2_float_of_block(mc2_block_of_value(v));
}

static inline mc2_tag_t mc2_tag_of_header(mc2_header_t hd) { return hd & 0xff; }
static inline mc2_size_t mc2_wosize_of_header(mc2_header_t hd) { return hd >> 8; }

static inline mc2_tag_t mc2_tag_of_block(mc2_block_t b) { return mc2_tag_of_header(b->block_header); }
static inline mc2_size_t mc2_wosize_of_block(mc2_block_t b) { return mc2_wosize_of_header(b->block_header); }
static inline mc2_tag_t mc2_tag_of_value(mc2_value_t v) { return mc2_tag_of_block(mc2_block_of_value(v)); }
static inline mc2_size_t mc2_wosize_of_value(mc2_value_t v) { return mc2_wosize_of_block(mc2_block_of_value(v)); }

static inline mc2_size_t mc2_bosize_of_wosize(mc2_size_t wosize) { return wosize * sizeof(mc2_block_t); }
static inline mc2_size_t mc2_wosize_of_bosize(mc2_size_t bosize) { return bosize / sizeof(mc2_block_t); }

#define MC2_TAG_CLOSURE (252)
#define MC2_TAG_FLOAT   (MC2_TAG_CLOSURE + 1)
#define MC2_TAG_STRING  (MC2_TAG_FLOAT + 1)
#define MC2_TAG_CUSTOM  (MC2_TAG_STRING + 1)
#define MC2_TAG_ATOMIC  (MC2_TAG_FLOAT)

static inline int mc2_block_is_custom(mc2_block_t b) {
  return mc2_tag_of_block(b) == MC2_TAG_CUSTOM;
}
static inline int mc2_value_is_custom(mc2_value_t v) {
  return mc2_value_is_block(v) && mc2_block_is_custom(mc2_block_of_value(v));
}
static inline int mc2_block_is_string(mc2_block_t b) {
  return mc2_tag_of_block(b) == MC2_TAG_STRING;
}
static inline int mc2_value_is_string(mc2_value_t v) {
  return mc2_value_is_block(v) && mc2_block_is_string(mc2_block_of_value(v));
}

/* predefined exception blocks (actually longs) */
#define MC2_EXN_OF_TAG(tag)      ((mc2_block_t) (((tag) << 1) | 1))
#define MC2_EXN_MATCH_FAILURE    (MC2_EXN_OF_TAG(0))
#define MC2_EXN_OUT_OF_MEMORY    (MC2_EXN_OF_TAG(1))
#define MC2_EXN_INVALID_ARGUMENT (MC2_EXN_OF_TAG(2))
#define MC2_EXN_FAILURE          (MC2_EXN_OF_TAG(3))
#define MC2_EXN_NOT_FOUND        (MC2_EXN_OF_TAG(4))
#define MC2_EXN_SYS_ERROR        (MC2_EXN_OF_TAG(5))
#define MC2_EXN_END_OF_FILE      (MC2_EXN_OF_TAG(6))
#define MC2_EXN_DIVISION_BY_ZERO (MC2_EXN_OF_TAG(7))
#define MC2_EXN_STACK_OVERFLOW   (MC2_EXN_OF_TAG(8))
#define MC2_EXN_SYS_BLOCKED_IO   (MC2_EXN_OF_TAG(9))
#define MC2_EXN_ASSERT_FAILURE   (MC2_EXN_OF_TAG(10))

#endif /* !__MC2_TYPES_H__ */
