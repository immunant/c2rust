#ACLOCAL_AMFLAGS = -I autoconf-archive/m4

EXTRA_DIST = README.md README.html
EXTRA_DIST += config.h.win32
EXTRA_DIST += Doxyfile
EXTRA_DIST += issues_closed_for_0.13.md

dist-hook:
	test -d "$(distdir)/doc" || mkdir "$(distdir)/doc"
	chmod -R u+w "$(distdir)/doc"
	cd "$(distdir)" && doxygen

SUBDIRS = . tests

lib_LTLIBRARIES = libjson-c.la

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = json-c.pc

libjson_cincludedir = $(includedir)/json-c
libjson_cinclude_HEADERS = \
	arraylist.h \
	bits.h \
	debug.h \
	json.h \
	json_c_version.h \
	json_config.h \
	json_inttypes.h \
	json_object.h \
	json_object_iterator.h \
	json_pointer.h \
	json_tokener.h \
	json_util.h \
	json_visit.h \
	linkhash.h \
	printbuf.h

noinst_HEADERS=\
	json_object_private.h \
	math_compat.h \
	strdup_compat.h \
	snprintf_compat.h \
	vasprintf_compat.h \
	random_seed.h \
	strerror_override.h

libjson_c_la_LDFLAGS = -version-info 4:0:0 -no-undefined @JSON_BSYMBOLIC_LDFLAGS@

libjson_c_la_SOURCES = \
	arraylist.c \
	debug.c \
	json_c_version.c \
	json_object.c \
	json_object_iterator.c \
	json_pointer.c \
	json_tokener.c \
	json_util.c \
	json_visit.c \
	linkhash.c \
	printbuf.c \
	random_seed.c \
	strerror_override.c \
	strerror_override_private.h


DISTCLEANFILES=
DISTCLEANFILES+= \
	config.h \
	json-c-uninstalled.pc \
	json-c.pc \
	json_config.h

distclean-local:
	-rm -rf $(testsubdir)

JSON_CLEANFILES=
JSON_CLEANFILES+= \
	Makefile.in \
	aclocal.m4 \
	autom4te.cache/ \
	compile \
	config.guess \
	config.h.in \
	config.sub \
	configure \
	depcomp \
	install-sh \
	ltmain.sh \
	missing \
	test-driver \
	tests/Makefile.in
JSON_CLEANFILES+= \
	libtool \
	stamp-h1 \
	stamp-h2

# There's no built-in way to remove these after all the other
# maintainer-clean steps happen, so do it explicitly here.
really-clean:
	$(MAKE) maintainer-clean
	rm -rf ${JSON_CLEANFILES}

uninstall-local:
	rm -rf "$(DESTDIR)@includedir@/json-c"
	rm -f "$(DESTDIR)@includedir@/json"

ANDROID_CFLAGS = -I$(top_srcdir) -DHAVE_CONFIG_H

Android.mk: Makefile.am
	androgenizer -:PROJECT json-c \
		-:SHARED libjson-c \
		-:TAGS eng debug \
		-:REL_TOP $(top_srcdir) -:ABS_TOP $(abs_top_srcdir) \
		-:SOURCES $(libjson_c_la_SOURCES) $(nodist_libjson_c_la_SOURCES) \
		-:CFLAGS $(DEFS) $(ANDROID_CFLAGS) $(libjson_c_la_CFLAGS) \
		-:LDFLAGS $(libjson_c_la_LDFLAGS) $(libjson_c_la_LIBADD) \
		-:HEADER_TARGET json-c \
		-:HEADERS $(libjson_cinclude_HEADERS) \
		-:PASSTHROUGH LOCAL_ARM_MODE:=arm \
	> $@
