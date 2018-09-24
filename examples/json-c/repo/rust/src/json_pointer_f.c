#define _GNU_SOURCE
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "../../json_pointer.h"

int json_pointer_getf(struct json_object *obj, struct json_object **res, const char *path_fmt, ...)
{
	char *path_copy = NULL;
	int rc = 0;
	va_list args;

	if (!obj || !path_fmt) {
		errno = EINVAL;
		return -1;
	}

	va_start(args, path_fmt);
	rc = vasprintf(&path_copy, path_fmt, args);
	va_end(args);

	if (rc < 0)
		return rc;

    rc = json_pointer_get(obj, path_copy, res);
out:
	free(path_copy);

	return rc;
}

int json_pointer_setf(struct json_object **obj, struct json_object *value, const char *path_fmt, ...)
{

	char *path_copy = NULL;
	int rc = 0;
	va_list args;

	if (!obj || !path_fmt) {
		errno = EINVAL;
		return -1;
	}

	va_start(args, path_fmt);
	rc = vasprintf(&path_copy, path_fmt, args);
	va_end(args);

	if (rc < 0)
		return rc;

    rc = json_pointer_set(obj, path_copy, value);
out:
	free(path_copy);

	return rc;
}
