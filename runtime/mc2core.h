#ifndef __MC2_CORE_H__
#define __MC2_CORE_H__

#include <mc2types.h>

mc2_block_t mc2_core_alloc(mc2_header_t hd) MC2_GNUC_MALLOC MC2_GNUC_WARN_UNUSED_RESULT;

mc2_value_t mc2_core_compare(mc2_value_t v1, mc2_value_t v2) MC2_GNUC_WARN_UNUSED_RESULT;

#endif /* !__MC2_CORE_H__ */
