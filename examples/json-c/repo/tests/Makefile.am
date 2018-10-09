
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/tests

LDADD= $(LIBJSON_LA)

LIBJSON_LA=$(top_builddir)/libjson-c.la

TESTS=
TESTS+= test_util_file.test
TESTS+= test_float.test
TESTS+= test1.test
TESTS+= test2.test
TESTS+= test4.test
TESTS+= testReplaceExisting.test
TESTS+= test_parse_int64.test
TESTS+= test_deep_copy.test
TESTS+= test_null.test
TESTS+= test_cast.test
TESTS+= test_double_serializer.test
TESTS+= test_parse.test
TESTS+= test_locale.test
TESTS+= test_charcase.test
TESTS+= test_printbuf.test
TESTS+= test_set_serializer.test
TESTS+= test_compare.test
TESTS+= test_set_value.test
TESTS+= test_visit.test
TESTS+= test_json_pointer.test
TESTS+= test_int_add.test

check_PROGRAMS=
check_PROGRAMS += $(TESTS:.test=)

EXTRA_DIST=
EXTRA_DIST += $(TESTS)
EXTRA_DIST += $(TESTS:.test=.expected)
EXTRA_DIST += test-defs.sh
EXTRA_DIST += valid.json


# Note: handled by test1.test
check_PROGRAMS += test1Formatted
test1Formatted_SOURCES = test1.c parse_flags.c parse_flags.h
test1Formatted_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_FORMATTED
EXTRA_DIST+= test1Formatted_plain.expected
EXTRA_DIST+= test1Formatted_pretty.expected
EXTRA_DIST+= test1Formatted_spaced.expected

# Note: handled by test2.test
check_PROGRAMS += test2Formatted
test2Formatted_SOURCES = test2.c parse_flags.c parse_flags.h
test2Formatted_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_FORMATTED
EXTRA_DIST+= test2Formatted_plain.expected
EXTRA_DIST+= test2Formatted_pretty.expected
EXTRA_DIST+= test2Formatted_spaced.expected

test_util_file_SOURCES = test_util_file.c

testsubdir=testSubDir
TESTS_ENVIRONMENT       = top_builddir=$(top_builddir)

distclean-local:
	-rm -rf $(testsubdir)
	-rm -f *.vg.out
