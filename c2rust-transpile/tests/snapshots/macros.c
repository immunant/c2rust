#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct S {
  int i;
};

#define LITERAL_INT 0xFFFF
#define LITERAL_BOOL true
#define LITERAL_FLOAT 3.14
#define LITERAL_CHAR 'x'
#define LITERAL_STR "hello"
#define LITERAL_ARRAY {1, 2, 3}
#define LITERAL_STRUCT ((struct S){.i = 5})

#define NESTED_INT LITERAL_INT
#define NESTED_BOOL LITERAL_BOOL
#define NESTED_FLOAT LITERAL_FLOAT
#define NESTED_CHAR LITERAL_CHAR
#define NESTED_STR LITERAL_STR
#define NESTED_ARRAY LITERAL_ARRAY
#define NESTED_STRUCT LITERAL_STRUCT

#define INT_ARITHMETIC NESTED_INT + LITERAL_INT + 1
#define MIXED_ARITHMETIC LITERAL_INT + NESTED_FLOAT *LITERAL_CHAR - true
#define PARENS (NESTED_INT * (LITERAL_CHAR + true))
#define PTR_ARITHMETIC (LITERAL_STR + 5) - 3
#define WIDENING_CAST ((unsigned long long)LITERAL_INT)
#define NARROWING_CAST (char)LITERAL_INT
#define CONVERSION_CAST ((double)LITERAL_INT)
#define INDEXING NESTED_STR[(int)LITERAL_FLOAT]
#define STR_CONCATENATION NESTED_STR " " LITERAL_STR " world"
#define BUILTIN __builtin_clz(LITERAL_INT)
#define REF_MACRO &INDEXING
#define REF_LITERAL &LITERAL_STRUCT
#define TERNARY LITERAL_BOOL ? 1 : 2
#define MEMBER LITERAL_STRUCT.i

#define STMT_EXPR                                                              \
  ({                                                                           \
    int builtin = BUILTIN;                                                     \
    char indexing = INDEXING;                                                  \
    float mixed = MIXED_ARITHMETIC;                                            \
    for (int i = 0; i < builtin; i++) {                                        \
      mixed += (float)indexing;                                                \
    }                                                                          \
    mixed;                                                                     \
  })

void local_muts() {
  int literal_int = LITERAL_INT;
  bool literal_bool = LITERAL_BOOL;
  float literal_float = LITERAL_FLOAT;
  char literal_char = LITERAL_CHAR;
  const char *literal_str_ptr = LITERAL_STR;
  char literal_str[] = LITERAL_STR;
  int literal_array[] = LITERAL_ARRAY;
  struct S literal_struct = LITERAL_STRUCT;

  int nested_int = NESTED_INT;
  bool nested_bool = NESTED_BOOL;
  float nested_float = NESTED_FLOAT;
  char nested_char = NESTED_CHAR;
  const char *nested_str_ptr = NESTED_STR;
  char nested_str[] = NESTED_STR;
  int nested_array[] = NESTED_ARRAY;
  struct S nested_struct = NESTED_STRUCT;

  int int_arithmetic = INT_ARITHMETIC;
  float mixed_arithmetic = MIXED_ARITHMETIC;
  int parens = PARENS;
  const char *ptr_arithmetic = PTR_ARITHMETIC;
  unsigned long long widening_cast = WIDENING_CAST;
  char narrowing_cast = NARROWING_CAST;
  double conversion_cast = CONVERSION_CAST;
  char indexing = INDEXING;
  const char *str_concatenation_ptr = STR_CONCATENATION;
  char str_concatenation[] = STR_CONCATENATION;
  int builtin = BUILTIN;
  const char *ref_indexing = REF_MACRO;
  const struct S *ref_struct = REF_LITERAL;
  int ternary = TERNARY;
  int member = MEMBER;
  float stmt_expr = STMT_EXPR;
}

void local_consts() {
  const int literal_int = LITERAL_INT;
  const bool literal_bool = LITERAL_BOOL;
  const float literal_float = LITERAL_FLOAT;
  const char literal_char = LITERAL_CHAR;
  const char *const literal_str_ptr = LITERAL_STR;
  const char literal_str[] = LITERAL_STR;
  const int literal_array[] = LITERAL_ARRAY;
  const struct S literal_struct = LITERAL_STRUCT;

  const int nested_int = NESTED_INT;
  const bool nested_bool = NESTED_BOOL;
  const float nested_float = NESTED_FLOAT;
  const char nested_char = NESTED_CHAR;
  const char *const nested_str_ptr = NESTED_STR;
  const char nested_str[] = NESTED_STR;
  const int nested_array[] = NESTED_ARRAY;
  const struct S nested_struct = NESTED_STRUCT;

  const int int_arithmetic = INT_ARITHMETIC;
  const float mixed_arithmetic = MIXED_ARITHMETIC;
  const int parens = PARENS;
  const char *const ptr_arithmetic = PTR_ARITHMETIC;
  const unsigned long long widening_cast = WIDENING_CAST;
  const char narrowing_cast = NARROWING_CAST;
  const double conversion_cast = CONVERSION_CAST;
  const char indexing = INDEXING;
  const char *const str_concatenation_ptr = STR_CONCATENATION;
  const char str_concatenation[] = STR_CONCATENATION;
  const int builtin = BUILTIN;
  const char *const ref_indexing = REF_MACRO;
  const struct S *const ref_struct = REF_LITERAL;
  const int ternary = TERNARY;
  const int member = MEMBER;
  const float stmt_expr = STMT_EXPR;
}

// TODO These are declared in the global scope and thus clash,
// which is an error for statics.
#if 0
void local_static_consts() {
  static const int literal_int = LITERAL_INT;
  static const bool literal_bool = LITERAL_BOOL;
  static const float literal_float = LITERAL_FLOAT;
  static const char literal_char = LITERAL_CHAR;
  static const char *const literal_str_ptr = LITERAL_STR;
  static const char literal_str[] = LITERAL_STR;
  static const int literal_array[] = LITERAL_ARRAY;
  static const struct S literal_struct = LITERAL_STRUCT;

  static const int nested_int = NESTED_INT;
  static const bool nested_bool = NESTED_BOOL;
  static const float nested_float = NESTED_FLOAT;
  static const char nested_char = NESTED_CHAR;
  static const char *const nested_str_ptr = NESTED_STR;
  static const char nested_str[] = NESTED_STR;
  static const int nested_array[] = NESTED_ARRAY;
  static const struct S nested_struct = NESTED_STRUCT;

  static const int int_arithmetic = INT_ARITHMETIC;
  static const float mixed_arithmetic = MIXED_ARITHMETIC;
  static const int parens = PARENS;
  static const char *const ptr_arithmetic = PTR_ARITHMETIC;
  static const unsigned long long widening_cast = WIDENING_CAST;
  static const char narrowing_cast = NARROWING_CAST;
  static const double conversion_cast = CONVERSION_CAST;
  static const char indexing = INDEXING;
  static const char *const str_concatenation_ptr = STR_CONCATENATION;
  static const char str_concatenation[] = STR_CONCATENATION;
  static const int builtin = BUILTIN;
  static const char *const ref_indexing = REF_MACRO;
  static const struct S *const ref_struct = REF_LITERAL;
  static const int ternary = TERNARY;
  static const int member = MEMBER;
  static const float stmt_expr = STMT_EXPR;
}
#endif

// global static consts

static const int global_static_const_literal_int = LITERAL_INT;
static const bool global_static_const_literal_bool = LITERAL_BOOL;
static const float global_static_const_literal_float = LITERAL_FLOAT;
static const char global_static_const_literal_char = LITERAL_CHAR;
static const char *const global_static_const_literal_str_ptr = LITERAL_STR;
static const char global_static_const_literal_str[] = LITERAL_STR;
static const int global_static_const_literal_array[] = LITERAL_ARRAY;
static const struct S global_static_const_literal_struct = LITERAL_STRUCT;

static const int global_static_const_nested_int = NESTED_INT;
static const bool global_static_const_nested_bool = NESTED_BOOL;
static const float global_static_const_nested_float = NESTED_FLOAT;
static const char global_static_const_nested_char = NESTED_CHAR;
static const char *const global_static_const_nested_str_ptr = NESTED_STR;
static const char global_static_const_nested_str[] = NESTED_STR;
static const int global_static_const_nested_array[] = NESTED_ARRAY;
static const struct S global_static_const_nested_struct = NESTED_STRUCT;

static const int global_static_const_int_arithmetic = INT_ARITHMETIC;
static const float global_static_const_mixed_arithmetic = MIXED_ARITHMETIC;
static const int global_static_const_parens = PARENS;
static const char *const global_static_const_ptr_arithmetic = PTR_ARITHMETIC;
static const unsigned long long global_static_const_widening_cast =
    WIDENING_CAST;
static const char global_static_const_narrowing_cast = NARROWING_CAST;
static const double global_static_const_conversion_cast = CONVERSION_CAST;
static const char global_static_const_indexing = INDEXING;
static const char *const global_static_const_str_concatenation_ptr =
    STR_CONCATENATION;
static const char global_static_const_str_concatenation[] = STR_CONCATENATION;
static const int global_static_const_builtin = BUILTIN;
static const char *const global_static_const_ref_indexing = REF_MACRO;
static const struct S *const global_static_const_ref_struct = REF_LITERAL;
static const int global_static_const_ternary = TERNARY;
static const int global_static_const_member = MEMBER;
// static const float global_static_const_stmt_expr = STMT_EXPR; // Statement expression not allowed at file scope.

void global_static_consts() {
  // Need to use `static`s or else they'll be removed when translated.
  (void)global_static_const_literal_int;
  (void)global_static_const_literal_bool;
  (void)global_static_const_literal_float;
  (void)global_static_const_literal_char;
  (void)global_static_const_literal_str_ptr;
  (void)global_static_const_literal_str;
  (void)global_static_const_literal_array;
  (void)global_static_const_literal_struct;

  (void)global_static_const_nested_int;
  (void)global_static_const_nested_bool;
  (void)global_static_const_nested_float;
  (void)global_static_const_nested_char;
  (void)global_static_const_nested_str_ptr;
  (void)global_static_const_nested_str;
  (void)global_static_const_nested_array;
  (void)global_static_const_nested_struct;

  (void)global_static_const_int_arithmetic;
  (void)global_static_const_mixed_arithmetic;
  (void)global_static_const_parens;
  (void)global_static_const_ptr_arithmetic;
  (void)global_static_const_widening_cast;
  (void)global_static_const_narrowing_cast;
  (void)global_static_const_conversion_cast;
  (void)global_static_const_indexing;
  (void)global_static_const_str_concatenation_ptr;
  (void)global_static_const_str_concatenation;
  (void)global_static_const_builtin;
  (void)global_static_const_ref_indexing;
  (void)global_static_const_ref_struct;
  (void)global_static_const_ternary;
  (void)global_static_const_member;
  // (void)global_static_const_stmt_expr;
}

// global consts

const int global_const_literal_int = LITERAL_INT;
const bool global_const_literal_bool = LITERAL_BOOL;
const float global_const_literal_float = LITERAL_FLOAT;
const char global_const_literal_char = LITERAL_CHAR;
const char *const global_const_literal_str_ptr = LITERAL_STR;
const char global_const_literal_str[] = LITERAL_STR;
const int global_const_literal_array[] = LITERAL_ARRAY;
const struct S global_const_literal_struct = LITERAL_STRUCT;

const int global_const_nested_int = NESTED_INT;
const bool global_const_nested_bool = NESTED_BOOL;
const float global_const_nested_float = NESTED_FLOAT;
const char global_const_nested_char = NESTED_CHAR;
const char *const global_const_nested_str_ptr = NESTED_STR;
const char global_const_nested_str[] = NESTED_STR;
const int global_const_nested_array[] = NESTED_ARRAY;
const struct S global_const_nested_struct = NESTED_STRUCT;

const int global_const_int_arithmetic = INT_ARITHMETIC;
const float global_const_mixed_arithmetic = MIXED_ARITHMETIC;
const int global_const_parens = PARENS;
const char *const global_const_ptr_arithmetic = PTR_ARITHMETIC;
const unsigned long long global_const_widening_cast = WIDENING_CAST;
const char global_const_narrowing_cast = NARROWING_CAST;
const double global_const_conversion_cast = CONVERSION_CAST;
const char global_const_indexing = INDEXING;
const char *const global_const_str_concatenation_ptr = STR_CONCATENATION;
const char global_const_str_concatenation[] = STR_CONCATENATION;
const int global_const_builtin = BUILTIN;
const char *const global_const_ref_indexing = REF_MACRO;
const struct S *const global_const_ref_struct = REF_LITERAL;
const int global_const_ternary = TERNARY;
const int global_const_member = MEMBER;
// const float global_const_stmt_expr = STMT_EXPR; // Statement expression not allowed at file scope.

typedef unsigned long long U64;

#define TEST_FN_MACRO(x) ((x) * (x))

int test_fn_macro(int x) { return TEST_FN_MACRO(x); }

#define TEST_CONST1 1
#define TEST_NESTED 2
#define TEST_CONST2 TEST_NESTED
#define TEST_PARENS (TEST_CONST2 + 1) * 3

int reference_define() {
  int x = TEST_CONST1;
  x += TEST_CONST2;
  if (3 < TEST_PARENS)
    x += TEST_PARENS;
  return x;
}

// Exercise an edge case where a struct initializer needs to be in an unsafe
// block
struct fn_ptrs {
  void *v;
  int (*fn1)(void);
  int (*fn2)(int);
};

typedef int (*fn_ptr_ty)(char);

// TODO Skip for now since it uses `libc`, which we don't test in snapshots.
// const struct fn_ptrs fns = {NULL, NULL, NULL};
const struct fn_ptrs fns = {};

// Make sure we can't refer to globals in a const macro
#define GLOBAL_REF &fns
const struct fn_ptrs *p = GLOBAL_REF;

// `size_t` not used since it translates differently on macOS,
// and we don't care about testing that here.
// `long` should suffice for testing as well.
// typedef size_t zstd_platform_dependent_type;
typedef long zstd_platform_dependent_type;

#define ZSTD_STATIC_ASSERT(c) (void)sizeof(char[(c) ? 1 : -1])
#define ZSTD_WINDOWLOG_MAX_32 30
#define ZSTD_WINDOWLOG_MAX_64 31
#define ZSTD_WINDOWLOG_MAX                                                     \
  ((int)(sizeof(zstd_platform_dependent_type) == 4 ? ZSTD_WINDOWLOG_MAX_32     \
                                                   : ZSTD_WINDOWLOG_MAX_64))
U64 test_zstd() {
  // This static assert was causing us trouble by somehow giving a valid
  // expression for ZSTD_WINDOWLOG_MAX which shouldn't be possible to translate
  // to a const.
  ZSTD_STATIC_ASSERT(ZSTD_WINDOWLOG_MAX <= 31);
  return ZSTD_WINDOWLOG_MAX;
}

#define inc(ptr)                                                               \
  ({                                                                           \
    (*ptr)++;                                                                  \
    *ptr;                                                                      \
  })

// Ensure the macro generated stmt expr block is codegen'd
int stmt_expr_inc(void) {
  int a = 0;
  int *b = &a;

  // unused
  inc(b);

  // used
  return inc(b);
}

int test_switch(int x) {
  switch (x) {
  case TEST_CONST1:
    return 10;
  case TEST_NESTED:
    return 20;
  }

  return 0;
}

// From #853.
#define silk_int16_MIN ((short)0x8000)

int test_silk_int16_MIN() {
  // _Static_assert((int)silk_int16_MIN == -0x8000, "mistranslated");
  char _null = ""[((int)silk_int16_MIN + 0x8000)];
  return silk_int16_MIN; // Should be -0x8000
}

// From #803.
int extern_fn(void);

#define EXTERN_VALUE (extern_fn())

int use_extern_value(void) { return EXTERN_VALUE; }

int local_fn(void) { return 1234; }

#define LOCAL_VALUE (local_fn())

int use_local_value(void) { return LOCAL_VALUE; }

bool use_portable_type(uintptr_t len) { return len <= UINTPTR_MAX / 2; }

// From `curl`'s `curl_ntlm_core.c`.

struct ntlmdata {
  unsigned int target_info_len;
};

// Should not translate since it references an out-of-scope `ntlm` variable.
#define NTLMv2_BLOB_LEN (44 - 16 + ntlm->target_info_len + 4)

unsigned int ntlm_v2_blob_len(struct ntlmdata *ntlm) { return NTLMv2_BLOB_LEN; }

// The variable `i` lacks an initializer, but is still declared within the macro,
// so it should be translated like `STMT_EXPR` is.
#define LATE_INIT_VAR ({ int i; i = 1; i; })

int late_init_var() {
  return LATE_INIT_VAR;
}
