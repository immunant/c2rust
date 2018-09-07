
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

...
)EOF";

} // namespace crosschecks
