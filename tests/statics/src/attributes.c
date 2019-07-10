#ifndef __APPLE__

// These three are initialized normally
int __attribute__((used, section("foo"))) used_static = 1;
int __attribute__((__used__, __section__("bar"))) used_static2 = 2;
static int __attribute__((used, section("barz"))) used_static4 = 1;

// This static's initializer is sectioned off
const int __attribute__((used, section("baz"))) used_static3 = 1 + 1U;

// Attribute-less static
int no_attrs = 1;

// In the following case the extern var creates the canonical var decl
// and the variable definition is non canonical. This previously
// meant that attributes were not persisted to the definition.
extern int initialized_extern;

int __attribute__((section("fb"))) initialized_extern = 1;

extern int __attribute__((alias("no_attrs"), used)) aliased_static;

#endif // __APPLE__
