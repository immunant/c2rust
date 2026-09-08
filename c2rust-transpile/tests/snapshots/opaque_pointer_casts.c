struct Opaque;
union OpaqueUnion;
typedef struct Opaque Opaque;

Opaque *sentinel(void) { return (Opaque *)8; }
const Opaque *const_sentinel(void) { return (const Opaque *)8; }
union OpaqueUnion *union_sentinel(void) { return (union OpaqueUnion *)8; }
Opaque *from_address(unsigned long address) { return (Opaque *)address; }

static const Opaque *static_sentinel = (const Opaque *)8;
const Opaque *get_static_sentinel(void) { return static_sentinel; }
