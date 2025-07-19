#include <stdarg.h>

typedef int char_to_int(char);
typedef int (*char_to_int_fp)(char);
typedef int (*va_char_to_int_fp)(char, ...);
typedef int (*char_int_to_int_fp)(char, int);

typedef int knr();

struct pointer_st {
  int (*fn)();
};

int intval(const char c) { return c; }
int negintval(const char c) { return -c; }

int varargs_intval(const char c, ...) { return c; }

// See #1281. Varargs don't yet work on aarch64.
#ifndef __aarch64__

int varargs_fp(const int c, ...) {
  va_list arg;
  va_start(arg, c);
  char_to_int *fp = va_arg(arg, char_to_int *);
  return fp((char)c);
}

#endif

void entry3(const unsigned sz, int buffer[const]) {
  int i = 0;

  char_to_int *p0;
  char_to_int *p1 = 0;
  char_to_int *p2 = intval;
  char_to_int *p3 = &intval;
  p3 = intval;
  p3 = &intval;

  if (!p1 && p3 && !!p3 && p3 != 0) {
    buffer[i] = p3('a');
  }
  i++;

  char_to_int_fp p4;
  char_to_int_fp p5 = 0;
  char_to_int_fp p6 = intval;
  char_to_int_fp p7 = &intval;
  p7 = intval;
  p7 = &intval;

  if (!p5 && p7 && !!p7 && p7 != 0) {
    buffer[i] = (*p7)('a');
  }
  i++;

  char_to_int_fp funs[3] = {intval, negintval};

  for (int j = 0; funs[j]; j++) {
    buffer[i++] = funs[j] ? funs[j]('a' + j) : 55;
  }

  // validate function pointer comparison to null
  buffer[i++] = p1 == 0;
  buffer[i++] = p1 != 0;
  buffer[i++] = p2 == 0;
  buffer[i++] = p2 != 0;

  // These should now use is_some, is_none:
  int j = p4 == 0;
  j = 0 == p4;
  j = p4 != 0;
  j = 0 != p4;

#ifndef __aarch64__
  va_char_to_int_fp p8 = varargs_intval;
  buffer[i++] = p8('A');
  buffer[i++] = (*p8)('B', 'C');
#endif

  // Test valid casts between function pointers
  // with additional parameters
  char_int_to_int_fp p9 = (char_int_to_int_fp)&intval,
                     p10 = (char_int_to_int_fp)p7;
  buffer[i++] = p9('D', 42);
  buffer[i++] = p10('E', 1337);

  // Test K&R style function pointers
  knr *p11 = (knr *)1;
  knr *p12 = (knr *)intval;
  knr *p13 = (knr *)&intval;
  struct pointer_st s;
  s.fn = (int (*)())intval;
  buffer[i++] = p12('a');
  buffer[i++] = p13('a');
  buffer[i++] = (*(s).fn)(('a'));

  buffer[i++] = p2 == intval;
#ifndef __aarch64__
  buffer[i++] = varargs_fp('a', intval);
  buffer[i++] = varargs_fp('b', p2);
#endif
}
