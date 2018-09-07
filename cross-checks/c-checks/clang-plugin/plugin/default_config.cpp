
#include "config.h"
#include "crosschecks.h"

namespace crosschecks {

// Hard-coded default configuration settings for system headers
const char CrossCheckInserter::default_config[] = R"EOF(
---
# `_G_fpos_t` includes `__mbstate_t`, which is a union
# Old location of `_G_fpos_t`
- file: "/usr/include/**/bits/_G_config.h"
  priority: -1000000
  items:
        - item: struct
          name: "_G_fpos_t"
          disable_xchecks: true

        - item: struct
          name: "_G_fpos64_t"
          disable_xchecks: true

# New location of `_G_fpos_t` (glibc 2.28+)
- file: "/usr/include/**/bits/types/__fpos_t.h"
  priority: -1000000
  items:
        - item: struct
          name: "_G_fpos_t"
          disable_xchecks: true

- file: "/usr/include/**/bits/types/__fpos64_t.h"
  priority: -1000000
  items:
        - item: struct
          name: "_G_fpos64_t"
          disable_xchecks: true

# Contains a union, needs custom cross-check
- file: "/usr/include/**/bits/types/__mbstate_t.h"
  priority: -1000000
  items:
        - item: struct
          name: "__mbstate_t"
          disable_xchecks: true

# Contains a union
- file: "/usr/include/**/bits/thread-shared-types.h"
  priority: -1000000
  items:
        - item: struct
          name: "__pthread_cond_s"
          disable_xchecks: true

# Requires a hash function for `__locale_data`
- file: "/usr/include/**/bits/types/__locale_t.h"
  priority: -1000000
  items:
        - item: struct
          name: "__locale_struct"
          disable_xchecks: true

# `sockaddr_storage` contains an array field named `__ss_padding`,
# which we can't cross-check on the Rust size because of its unusual size
- file: "/usr/include/**/bits/socket.h"
  priority: -1000000
  items:
        - item: struct
          name: "sockaddr_storage"
          fields:
              __ss_padding: disabled

# Inline functions in the header
- file: "/usr/include/**/bits/byteswap.h"
  priority: -1000000
  items:
        - item: function
          name: "__bswap_16"
          disable_xchecks: true

        - item: function
          name: "__bswap_32"
          disable_xchecks: true

        - item: function
          name: "__bswap_64"
          disable_xchecks: true

# On some systems, `stat` is an inline function in `sys/stat.h`
- file: "/usr/include/**/sys/stat.h"
  priority: -1000000
  items:
        - item: function
          name: "stat"
          disable_xchecks: true

        - item: function
          name: "lstat"
          disable_xchecks: true

        - item: function
          name: "fstat"
          disable_xchecks: true

        - item: function
          name: "fstatat"
          disable_xchecks: true

        - item: function
          name: "mknod"
          disable_xchecks: true

        - item: function
          name: "mknodat"
          disable_xchecks: true

        - item: function
          name: "stat64"
          disable_xchecks: true

        - item: function
          name: "lstat64"
          disable_xchecks: true

        - item: function
          name: "fstat64"
          disable_xchecks: true

        - item: function
          name: "fstatat64"
          disable_xchecks: true

# `FILE*` has too many fields with problems, so we implement the hash function
# manually in our runtime
- file: "/usr/include/**/bits/types/struct_FILE.h"
  priority: -1000000
  items:
        - item: struct
          name: "_IO_FILE"
          disable_xchecks: true

        - item: struct
          name: "_IO_FILE_complete"
          disable_xchecks: true

# Older glibc versions define these in `/usr/include/bits/libio.h`
- file: "/usr/include/**/bits/libio.h"
  priority: -1000000
  items:
        - item: struct
          name: "_IO_FILE"
          disable_xchecks: true

        - item: struct
          name: "_IO_FILE_complete"
          disable_xchecks: true

# Header contains 2 inline functions
- file: "/usr/include/**/pthread.h"
  priority: -1000000
  items:
        - item: function
          name: "__pthread_cleanup_routine"
          disable_xchecks: true

        - item: function
          name: "pthread_equal"
          disable_xchecks: true

...
)EOF";

} // namespace crosschecks
