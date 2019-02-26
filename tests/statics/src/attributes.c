// These three are initialized normally
int __attribute__((used, section("foo"))) used_static = 1;
int __attribute__((__used__, __section__("bar"))) used_static2 = 2;
static int __attribute__((used, section("barz"))) used_static4 = 1;

// This static's initializer is sectioned off
const int __attribute__((used, section("baz"))) used_static3 = 1 + 1U;

// Attribute-less static
int no_attrs = 1;

/// private static variables get pruned out when not used:
void ensure_use(void) {
    used_static4++;
}
