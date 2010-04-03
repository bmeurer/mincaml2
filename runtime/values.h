#ifndef __MC2_VALUES_H__
#define __MC2_VALUES_H__

#include <sys/types.h>

typedef ssize_t mc2_int_t;
typedef size_t  mc2_value_t;
typedef size_t  mc2_tag_t;
typedef size_t  mc2_header_t;
typedef size_t  mc2_size_t;

#define mc2_is_int(x)   (((x) & 1) != 0)
#define mc2_is_block(x) (((x) & 1) == 0)

#define mc2_val_int(x) ((((mc2_value_t)(x)) << 1) | 1)
#define mc2_int_val(x) ((mc2_int_t)(((mc2_value_t)(x)) >> 1))

#define mc2_tag_hd(hd)    ((mc2_tag_t)(((mc2_header_t)(hd)) & 0xff))
#define mc2_wosize_hd(hd) ((mc2_size_t)(((mc2_header_t)(hd)) >> 8))

#define mc2_hd_val(v) (((mc2_header_t*)((mc2_value_t)(v)))[0])

#define mc2_string_val(v) ((char *) (((mc2_value_t *)(v)) + 1))

#define mc2_val_unit (mc2_val_int(0))

struct mc2_custom_operations_t {
  void (*mc2_custom_finalize)(mc2_value_t);
  int (*mc2_custom_compare)(mc2_value_t, mc2_value_t);
};

typedef struct mc2_custom_operations_t mc2_custom_operations_t;

#define mc2_custom_operations_val(v) (((const mc2_custom_operations_t **)(v))[1])
#define mc2_custom_data_val(v) ((void *) (((mc2_value_t *)(v)) + 2))

#define MC2_CLOSURE_TAG (252)
#define MC2_FLOAT_TAG (253)
#define MC2_STRING_TAG (254)
#define MC2_CUSTOM_TAG (255)

/* The lowest tag for blocks containing no value */
#define MC2_ATOMIC_TAG (253)

#endif /* !__MC2_VALUES_H__ */
