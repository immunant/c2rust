#include <stdarg.h>
#include <stdio.h>

// Can we correctly call an extern varargs function
void call_printf(void) {
    printf("%d, %f\n", 10, 1.5);
}

void call_vprintf(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vprintf(format, ap);
  va_end(ap);
}

// Simplified version of printf
void my_printf(const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  while (*fmt) {
    switch (*fmt) {
    case '%':
      fmt++;
      if (!*fmt)
        break;

      switch (*fmt) {
      case 'i':
      case 'd':
        printf("%d", va_arg(ap, int));
        break;
      case 'f':
        printf("%f", va_arg(ap, double));
        break;
      case 's':
        printf("%s", va_arg(ap, char*));
        break;
      }

      break;

    default:
      putchar(*fmt);
      break;
    }

    fmt++;
  }

  va_end(ap);
}

/* void simple_vacopy(const char *fmt, ...) { */
/*   va_list ap, aq; */

/*   va_start(ap, fmt); */
/*   va_copy(aq, ap); */
/*   vprintf(fmt, ap); */
/*   vprintf(fmt, aq); */
/*   va_end(aq); */
/*   va_end(ap); */
/* } */
