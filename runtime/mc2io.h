#ifndef __MC2_IO_H__
#define __MC2_IO_H__

#include <mc2types.h>

typedef struct mc2_io_t *mc2_io_t;

mc2_block_t mc2_io_fdopen(mc2_value_t fd, mc2_value_t in) MC2_GNUC_MALLOC MC2_GNUC_WARN_UNUSED_RESULT;
mc2_value_t mc2_io_fclose(mc2_value_t channel);
mc2_value_t mc2_io_fflush(mc2_value_t channel);
mc2_value_t mc2_io_fputc(mc2_value_t channel, mc2_value_t c);
mc2_value_t mc2_io_fputs(mc2_value_t channel, mc2_value_t s);

#endif /* !__MC2_IO_H__ */
