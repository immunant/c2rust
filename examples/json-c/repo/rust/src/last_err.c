#include <stdarg.h>
#include <stdio.h>

static char _last_err[256] = "";

const char *json_util_get_last_err()
{
	if (_last_err[0] == '\0')
		return NULL;
	return _last_err;
}

void _json_c_set_last_err(const char *err_fmt, ...)
{
	va_list ap;
	va_start(ap, err_fmt);
	// Ignore (attempted) overruns from snprintf
	(void)vsnprintf(_last_err, sizeof(_last_err), err_fmt, ap);
	va_end(ap);
}

