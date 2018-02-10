In many cases, we can add identical cross-checks to the original C and the transpiled Rust code, e.g., when the C code is naively translated to the perfectly equivalent Rust code, and everything just works. However, this might not always be the case, and we need to handle mismatches such as:
  * Type mismatches between C and Rust, e.g., a C `const char*` (with or without an attached length parameter) being translated to a `str`. Additionally, if a string+length value pair (with the types `const char*` and `size_t`) gets translated to a single `str`, we may want to omit the cross-check on the length parameter.
  * Whole functions added or removed by the transpiler or refactoring tool, e.g., helpers.

Note that this list is not exhaustive, so there may be many more cases of mismatches.

To handle all these cases, we need a language that lets us add new cross-checks, or modify or delete existing ones.

# The cross-check language
The cross-check metadata is stored as a YAML encoding of an array of configuration entries. Each configuration entry describes the configuration for that specific check.

An example configuration file for a function `foo` with 3 arguments `a`, `alen` and `b` looks something like:
```yaml
main.c:
  - item: defaults
    disable_xchecks: true

  - item: function
    name: foo
    disable_xchecks: false
    args:
      a: default
      alen: none
      b: default
    return: no

main.rs:
  - item: function
    name: foo
    args:
      a: default
      b: default
    return: no
```

## Inline vs external configuration
We can store the cross-check configuration entries in a few places:
 * Externally in separate configuration files.
 * Inline in the source code, attached to the checked functions and structures.
 
Each approach has advantages and drawbacks.
Inline configuration entries are simpler to maintain, but do not scale as well to larger codebases or more complex cross-check configuration entries.
Conversely, external configuration entries are more flexible and can potentially express complex configurations in a cleaner and more elegant way, but can easily get out of sync with their corresponding source code.
We currently support both approaches, with external configuration settings taking priority over inline attributes where both are present.

In the current implementation of the Rust cross-checker, inline configuration settings are passed to the enclosing scope's `#[cross_check]` attribute, e.g.:
```rust
#[cross_check(yes, entry(djb2="foo"))]
fn bar() { }

#[cross_check(yes, entry(fixed=0x1234))]
fn baz() { }
```

## Configuration file format
At the top level, each configuration file is a YAML associative array mapping file names to their configuration entries.
Each array element maps a file name (represented as a string) to a list of individual items, each item representing a Rust/C scope entity, i.e., function or structure.
Each item is encoded in YAML as an associative array.
All items have a few common array members:
 * `item` specifies the type of the current item, e.g., `function`, `struct` or others.
 * `name` specifies the name of the item, i.e., the name of the function or structure.

## Function cross-check configuration
Function cross-checks are configured using entries with `item: function`.
Function entries support the following fields:

 Field  |  Role
------- | ------
`disable_xchecks` | Disables all cross-checks for this function and everything in it if set to `true`.
`entry` | Configures the function entry cross-check (see [below](#xcheck_types) for information on accepted values).
`exit` | Configures the function exit cross-check.
`all_args` | Specifies a cross-check override for all of this function's arguments. For example, setting `all_args: none` disables cross-checks for all arguments.
`args` | An associative array that maps argument names to their corresponding cross-checks. This can be used to customize the cross-checks for some of the function arguments individually. This setting overrides both the global default and the one specified in `all_args` for the current function.
`return` | Configures the function return value cross-check.
`ahasher` and `shasher` | Override the default values for the aggregate and simple hasher for this function (see **TODO** for the meaning of these fields).
`nested` | Recursively configures the items nested inside the current items. Since Rust allows arbitrarily deep function and structure nesting, we use this to recursively configure nested functions.
`entry_extra` | Specifies a list of additional custom cross-checks to perform after the argument. Each cross-check accepts an optional `tag` parameter that overrides the default `UNKNOWN` tag.
`exit_extra` | Specifies a list of additional custom cross-checks to perform on function return.

## Structure cross-check configuration
Structure entries configure cross-checks for Rust structure, tuple and enumeration types, and are tagged with `item: struct`.
For a general overview of cross-checking for structures (aggregate types), see **TODO**.
Structure entries support the following fields:

 Field  |  Role
------- | ------
`disable_xchecks` | Disable automatic cross-check emission for this structure (this is generally best left out, unless the default is `true` and needs to be reset to `false`).
`field_hasher` | Configures the replacement hasher for this structure. The hasher is a Rust object that implements the `cross_check_runtime::hash::CrossCheckHasher` trait.
`custom_hash` | Specifies a function to call to hash objects of this type, instead of the default implementation. This function should have the signature `fn foo<XCHA, XCHS>(arg: &T, depth: usize) -> u64` where `T` is the name of the current type. `XCHA` and `XCHS` are template parameters passed by the caller that specify the aggregate and simple hasher to use for this computation (and can be overridden using `ahasher` and `shasher` below).
`fields` | An associative array that specifies custom hash computations for some or all of the structure's fields. Accepts values in the format of [cross-check types](#xcheck_types).
`ahasher` and `shasher` | Override the aggregate and simple hasher for the default hash implementation for the current type (mainly useful if `field_hasher` is left out). These are recursively passed to the hash function call for each structure field.

The `field_hasher` and `custom_hash` provide two alternative methods of customizing the hashing algorithm for a given structure: users may either provide a custom implementation of `CrossCheckHasher` and pass that to `field_hasher`, or implement a hashing function and pass it to `custom_hash`. The two alternatives are mostly equivalent, and users may use whichever is more convenient. Additionally, users can choose to completely disable the automatic derivation of `CrossCheckHash`, and manually implement `CrossCheckHasher` for some of the types instead.

## <a name="xcheck_types"></a>Cross-check types
There are several types of cross-check implemented in the compiler:

  Check   | Value Type  |  Behavior
--------- | ------------ | ----------
`default` | | Lets the compiler perform the default cross-check.
`none` or `disabled` | | Disables cross-checking or hashing for the current value.
`fixed` | `u64` | Sets the cross-checked value to the given 64-bit integer.
`djb2` | `String` | Sets the cross-checked value to the [djb2](http://www.cse.yorku.ca/~oz/hash.html) hash of the given string. This is mainly useful for overriding function entry cross-checks, in case the function names don't match between languages.
`custom` | `String` | Parses the given string as a C or Rust expression and uses it to compute the cross-checked value. In most cases, the string is inserted verbatim into the cross-check code, e.g., for function argument cross-checks.
 
 Each cross-check is encoded in YAML as either a single word with the type, e.g., `default`, or a single-element associative array mapping the type to its argument, e.g., `{ fixed: 0x1234 }`.
 
More cross-check types may be added as needed.

### Custom hash functions for structures
 If `custom_hash: { custom: "hash_foo" }` is a configuration entry for structure `Foo`, then the compiler will insert a call to `hash_foo` to perform the cross checks. This function should have the following signature:
```rust
fn hash_foo<XCHA, XCHS>(foo: &Foo, depth: usize) -> u64 { ... }
```
The hash function receives a reference to a `Foo` object and a maximum depth, and should return the 64-bit hash value for the given object.

### Custom hash functions for structure fields
 If `bar: { custom: "hash_bar" }` is a configuration entry for field `bar`, then the compiler will insert a call to `hash_bar` to compute the hash for `bar`. This function should have the following signature:
```rust
fn hash_bar<XCHA, XCHS, S, F>(h: &mut XCHA, foo: &S, bar: &F, depth: usize)
       where XCHA: cross_check_runtime::hash::CrossCheckHasher { ... }
```
The function receives the following arguments:
 * The current aggregate hasher for this structure. The function can call the hasher's `write_u64` function as many times as needed.
 * The structure containing this field. This argument has generic type `S`, so the same function can be reused for different structures.
 * The field itself, with generic type `F`. The function may require additional type bounds for `F` to make it compatible with its callers.
 * The maximum hashing depth (explained in **TODO**).
 * The type parameters `XCHA` and `XCHS` bound to the current aggregate and simple value hasher for the current invocation.
 
This function should not return the hash value of the field. Instead, the function should call the hasher's `write_u64` method directly.

## Per-file default settings
The special `defaults` item type specifies the default cross-check settings for all items in a file.
We currently support the following entries:

Field  |  Role
------- | ------
`disable_xchecks` | Disables all cross-checks for this file. Can be individually overridden per function or structure.
`entry` | Configures the default entry cross-check for all functions in this file.
`exit` | Similarly configures the function exit cross-check.
`all_args` | Specifies a cross-check override for all arguments to all functions in this file. For example, setting `all_args: default` enables cross-checks for all arguments.
`return` | Configures the function return value cross-check.

## More examples
### Function example
Example configuration for a function `baz1(a, b)`:
```yaml
main.rs:
  - item: function
    name: baz1
    entry: { djb2: "baz" }    // Cross-check the function as "baz"
    args:
      a: { custom: "foo(a)" } // Cross-check a as foo(a)
      b: none                 // Do not cross-check b
    entry_extra:              // Cross-check foo(b) with a FUNCTION_ARG tag
      - { custom: "foo(b)", tag: FUNCTION_ARG }
      - { custom: "a" }       // Cross-check the value "a" with UNKNOWN_TAG
```

### Structure example
Example configuration for a structure `Foo` (illustrated on an object `foo` of type `Foo`):
```yaml
main.rs:
  - item: struct
    name: Foo
    field_hasher: "FooHasher"  // Use FooHasher as the aggregate hasher
    fields:
      a: { fixed: 0x12345678 } // Use 0x12345678 as the hash of foo.a
      b: { custom: "hash_b" }  // Hash foo.b using hash_b(foo.b)
      c: none                  // Ignore foo.c when hashing foo
```

# Inline cross-check configuration
In addition to the external configuration format, a subset of cross-checks can also be configured inline in the program source code. The compiler plugin provides a custom `#[cross_check]` attribute used to annotate functions, structures and fields with custom cross-check metadata.

## Inline function configuration
The `#[cross_check]` function attribute currently supports the following arguments:

  Argument  |  Type  |  Role
 ---------- | ------ | ------
 `none` or `disabled` | | Disable cross-checks for this function and all its sub-items (this attribute is inherited). Each sub-item can individually override this with `yes` or `enabled`.
 `yes` or `enabled` | | Enable cross-checks for this function and its sub-items. Each nested item can also override this setting with `none` or `disabled`.
 `entry` | `XCheckType` | Cross-check to use on function entry, same as for external configuration.
 `exit` | `XCheckType` | Cross-check to use on function entry, same as for external configuration.
 `all_args` | `XCheckType` | Enable cross-checks for this function's arguments (disabled by default). Takes the cross-check type as its argument.
 `args(...)` | | Per-argument cross-check overrides (same as for external configuration).
 `return` | `XCheckType` | Cross-check to perform on the function return value, same as for external configuration.
 `ahasher` and `shasher` | `String` | Same as for external configuration.
 `entry_extra` and `exit_extra` | Same as for external configuration.
 
### Function example
```rust
#[cross_check(yes, entry(djb2="foo"))] // Cross-check this function as "foo"
fn foo1() {
  #[cross_check(none)]
  fn bar() { ... }
  bar();
  
  #[cross_check(yes, all_args(default), args(a(fixed=0x123)))]
  fn baz(a: u8, b: u16, c: u32) { ... }
  baz(1, 2, 3);
}
```

## Inline structure configuration
The compiler plugin also supports a subset of the full external configuration settings as `#[cross_check]` arguments:

  Argument  |  Type  |  Role
 ---------- | ------ | ------
 `field_hasher` | `String` | Same as for external configuration.
 `custom_hash` | `String` | Same as for external configuration.
 `ahasher` and `shasher` | `String` | Same as for external configuration.

The `#[cross_check]` attribute can also be attached to structure fields to configure hashing:

  Argument  |  Type  |  Role
 ---------- | ------ | ------
 `none` or `disabled` | | This field is skipped during hashing.
 `fixed`    | `u64` | Fixed 64-bit integer to use as the hash value for this field. Identical to the `fixed` external cross-check type.
 `custom_hash` | `String` | Same as for external configuration.

### Structure example
```rust
#[cross_check(field_hasher="MyHasher")]
struct Foo {
  #[cross_check(none)]
  foo: u64,
  
  #[cross_check(fixed=0x1234)]
  bar: String,
  
  #[cross_check(custom_hash="hash_baz")]
  baz: String,
}
```

# Caveats
## Duplicate items
At any level or scope, there may be duplicate items, i.e., multiple items with the same names.
It is not clear at this point how to best handle this case, since we have several conflicting requirements.
On the one hand, we may wish to allow the configuration for one source file to be spread across multiple configuration files, and entries from later configuration files to be appended or replace entries from earlier files.
On the other hand, we may have identically-named structures or functions in nested scopes that we want to configure separately. For an example, consider the following code:
```Rust
fn foo(x: u32) -> u32 {
    if x > 22 {
        fn bar(x: u32) -> u32 {
            x - 22
        };
        bar(x)
    } else {
        fn bar(x: u32) -> u32 {
            x + 34
        }
        bar(x)
    }
}
```
In this example, there are two distinct `foo::bar` functions, and we wish to configure them separately.
However, at the top level of a file, there may only be one `foo` function, so we can merge all entries for `foo` together. Alternatively, we could check for multiple top-level items with the same name and exit with an error if we encounter any duplicates.

## Configuration priority
Currently, if a certain cross-check is configured using both an external entry and an inline `#[cross_check(...)]` attribute, the external entry takes priority. Alternatively, we may reverse this priority, or exit with an error if both are present.

## Scope configuration inheritance
The configuration settings described above apply to the scope of an item. While most settings apply exclusively to the scope itself (for example, `args` and `all_args` settings only apply to the current function, e.g., `foo` above and not any of the `bar` functions) and not any of its nested sub-items, there are a few that apply to everything inside the scope. These attributes are internally "inherited" from each scope by its child scopes. Currently, the only inherited attributes are `disable_xchecks` (so that disabling cross-checks for a module or function disables them for everything inside that function), `ahasher` and `shasher`.

## Custom cross-check parameters
Custom cross-check definitions have a different format for each language. The rustc plugin accepts any Rust expression that is valid on function entry as a custom cross-check.

The clang plugin, on the other hand, only accepts a limited subset of C expressions: each cross-check specification contains the name of the function to call, optionally followed by a list of parameters to pass to the function, e.g., `function` or `function(arg1, arg2, ...)`. Each parameter is the name of a global variable or function argument, and is optionally preceded by `&` (to pass the parameter by address instead of value) or by `*` (to dereference the value if it is a pointer).
