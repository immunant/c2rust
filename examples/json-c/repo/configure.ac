AC_PREREQ(2.64)

# Process this file with autoconf to produce a configure script.
AC_INIT([json-c], 0.13.1, [json-c@googlegroups.com])

AM_INIT_AUTOMAKE

AC_CONFIG_MACRO_DIRS([autoconf-archive/m4])

AC_PROG_MAKE_SET

AC_CANONICAL_HOST

AC_ARG_ENABLE(threading,
 AS_HELP_STRING([--enable-threading],
   [Enable code to support partly multi-threaded use]),
[if test x$enableval = xyes; then
  enable_threading=yes
  AC_DEFINE(ENABLE_THREADING, 1, [Enable partial threading support])
fi])

if test "x$enable_threading" = "xyes"; then
  AC_MSG_RESULT([Partial multi-threaded support enabled.])
else
  AC_MSG_RESULT([Multi-threaded support disabled. Use --enable-threading to enable.])
fi

AC_ARG_ENABLE(rdrand,
 AS_HELP_STRING([--enable-rdrand],
   [Enable RDRAND Hardware RNG Hash Seed generation on supported x86/x64 platforms.]),
[if test x$enableval = xyes; then
  enable_rdrand=yes
  AC_DEFINE(ENABLE_RDRAND, 1, [Enable RDRAND Hardware RNG Hash Seed])
fi])

if test "x$enable_rdrand" = "xyes"; then
  AC_MSG_RESULT([RDRAND Hardware RNG Hash Seed enabled on supported x86/x64 platforms])
else
  AC_MSG_RESULT([RDRAND Hardware RNG Hash Seed disabled. Use --enable-rdrand to enable])
fi

# enable silent build by default
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Checks for programs.

# Checks for libraries.

# Checks for header files.
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_CONFIG_HEADER(config.h)
AC_CONFIG_HEADER(json_config.h)
AC_HEADER_STDC
AC_CHECK_HEADERS(fcntl.h limits.h strings.h syslog.h unistd.h [sys/cdefs.h] [sys/param.h] stdarg.h locale.h xlocale.h endian.h)
AC_CHECK_HEADER(inttypes.h,[AC_DEFINE([JSON_C_HAVE_INTTYPES_H],[1],[Public define for json_inttypes.h])])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

AC_CACHE_CHECK([for __thread support], ac_cv___thread, [dnl
AC_LINK_IFELSE([dnl
AC_LANG_PROGRAM([[#undef __thread
static __thread int a; int foo (int b) { return a + b; }]],
		[[exit (foo (0));]])],
	       ac_cv___thread=yes, ac_cv___thread=no)
])
AS_IF([test "x$ac_cv___thread" != xno],
[AC_DEFINE(HAVE___THREAD, 1, [Have __thread])
 AC_DEFINE(SPEC___THREAD, [__thread], [Specifier for __thread])]
)

# Checks for library functions.
AC_FUNC_VPRINTF
AC_FUNC_MEMCMP
AC_CHECK_FUNCS([realloc])
AC_CHECK_FUNCS(strcasecmp strdup strerror snprintf vsnprintf vasprintf open vsyslog strncasecmp setlocale)
AC_CHECK_DECLS([INFINITY], [], [], [[#include <math.h>]])
AC_CHECK_DECLS([nan], [], [], [[#include <math.h>]])
AC_CHECK_DECLS([isnan], [], [], [[#include <math.h>]])
AC_CHECK_DECLS([isinf], [], [], [[#include <math.h>]])
AC_CHECK_DECLS([_isnan], [], [], [[#include <float.h>]])
AC_CHECK_DECLS([_finite], [], [], [[#include <float.h>]])
AC_MSG_CHECKING(for GCC atomic builtins)
AC_LINK_IFELSE(
[
 AC_LANG_SOURCE([[
   int main() {
      volatile unsigned int val = 1;
      /* Note: __sync_val_compare_and_swap isn't checked here
       * because it's protected by __GCC_HAVE_SYNC_COMPARE_AND_SWAP_<n>,
       * which is automatically defined by gcc.
       */
      __sync_add_and_fetch(&val, 1);
      __sync_sub_and_fetch(&val, 1);
      return 0;
   }
 ]])
],
[
 AC_MSG_RESULT([yes])
 AC_DEFINE([HAVE_ATOMIC_BUILTINS],[1],[Has atomic builtins])
],
[
 AC_MSG_RESULT([no])
 AC_MSG_WARN([json-c will be built without atomic refcounts because atomic builtins are missing])
])

case "${host_os}" in
	linux*)
		AC_CHECK_FUNCS([uselocale])
		;;
	*)	# Nothing
		;;
esac

if test "$ac_cv_have_decl_isnan" = "yes" ; then
   AC_TRY_LINK([#include <math.h>], [float f = 0.0; return isnan(f)], [], [LIBS="$LIBS -lm"])
fi

#check if .section.gnu.warning accepts long strings (for __warn_references)
AC_LANG_PUSH([C])

AC_MSG_CHECKING([if .gnu.warning accepts long strings])
AC_LINK_IFELSE([AC_LANG_SOURCE([[
extern void json_object_get();
__asm__(".section .gnu.json_object_get,\n\t.ascii \"Please link against libjson-c instead of libjson\"\n\t.text");

int main(int c,char* v) {return 0;}
]])], [
    AC_DEFINE(HAS_GNU_WARNING_LONG, 1, [Define if .gnu.warning accepts long strings.])
    AC_MSG_RESULT(yes)
], [
   AC_MSG_RESULT(no)
])

AC_LANG_POP([C])

LT_INIT

# Check for the -Bsymbolic-functions linker flag
AC_ARG_ENABLE([Bsymbolic],
              [AS_HELP_STRING([--disable-Bsymbolic], [Avoid linking with -Bsymbolic-function])],
              [],
              [enable_Bsymbolic=check])

AS_IF([test "x$enable_Bsymbolic" = "xcheck"],
      [
        saved_LDFLAGS="${LDFLAGS}"
        AC_MSG_CHECKING([for -Bsymbolic-functions linker flag])
        LDFLAGS=-Wl,-Bsymbolic-functions
        AC_TRY_LINK([], [int main (void) { return 0; }],
                    [
                      AC_MSG_RESULT([yes])
                      enable_Bsymbolic=yes
                    ],
                    [
                      AC_MSG_RESULT([no])
                      enable_Bsymbolic=no
                    ])
        LDFLAGS="${saved_LDFLAGS}"
      ])

AS_IF([test "x$enable_Bsymbolic" = "xyes"], [JSON_BSYMBOLIC_LDFLAGS=-Wl[,]-Bsymbolic-functions])
AC_SUBST(JSON_BSYMBOLIC_LDFLAGS)

AX_APPEND_COMPILE_FLAGS([-Wall -Werror -Wcast-qual -Wno-error=deprecated-declarations])
AX_APPEND_COMPILE_FLAGS([-Wextra -Wwrite-string -Wno-unused-parameter])
AX_APPEND_COMPILE_FLAGS([-D_GNU_SOURCE])

AC_LANG_PUSH([C])
AC_MSG_CHECKING([for compatibility with _REENTRANT and toolchain headers])
AC_LINK_IFELSE(
[
 AC_LANG_SOURCE([[
/* uClibc toolchains without threading barf when _REENTRANT is defined */
#define _REENTRANT 1
#include <sys/types.h>
int main ()
{
  return 0;
}
]])], [
    AC_MSG_RESULT(yes)
    AX_APPEND_COMPILE_FLAGS([-D_REENTRANT])
], [
    AC_MSG_RESULT(no)
])
AC_LANG_POP([C])


AX_COMPILE_CHECK_SIZEOF(int)
AX_COMPILE_CHECK_SIZEOF(long)
AX_COMPILE_CHECK_SIZEOF(long long)
AX_COMPILE_CHECK_SIZEOF(size_t, [#include <stdint.h>])
AX_COMPILE_CHECK_SIZEOF(int64_t, [#include <stdint.h>])

AC_CONFIG_FILES([
Makefile
json-c.pc
tests/Makefile
json-c-uninstalled.pc
])

AC_OUTPUT

