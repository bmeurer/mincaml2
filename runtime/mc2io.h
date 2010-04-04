#ifndef __MC2_IO_H__
#define __MC2_IO_H__

#include <mc2custom.h>

typedef struct mc2_io_t *mc2_io_t;

mc2_block_t mc2_io_fdopen(mc2_value_t fd, mc2_value_t in) MC2_GNUC_MALLOC MC2_GNUC_WARN_UNUSED_RESULT;

#endif /* !__MC2_IO_H__ */
