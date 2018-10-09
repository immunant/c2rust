#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "json.h"
#include "json_tokener.h"
#include "json_visit.h"

static void test_basic_parse(void);
static void test_verbose_parse(void);
static void test_incremental_parse(void);

int main(void)
{
	MC_SET_DEBUG(1);

	static const char separator[] = "==================================";
	test_basic_parse();
	puts(separator);
	test_verbose_parse();
	puts(separator);
	test_incremental_parse();
	puts(separator);
}

static json_c_visit_userfunc clear_serializer;
static void do_clear_serializer(json_object *jso);

static void single_basic_parse(const char *test_string, int clear_serializer)
{
	json_object *new_obj;

	new_obj = json_tokener_parse(test_string);
	if (clear_serializer)
		do_clear_serializer(new_obj);
	printf("new_obj.to_string(%s)=%s\n", test_string, json_object_to_json_string(new_obj));
	json_object_put(new_obj);
}
static void test_basic_parse()
{
	single_basic_parse("\"\003\"", 0);
	single_basic_parse("/* hello */\"foo\"", 0);
	single_basic_parse("// hello\n\"foo\"", 0);
	single_basic_parse("\"foo\"blue", 0);
	single_basic_parse("\"\\u0041\\u0042\\u0043\"", 0);
	// Test with a "short" high surrogate
	single_basic_parse("[9,'\\uDAD", 0);
	single_basic_parse("null", 0);
	single_basic_parse("NaN", 0);
	single_basic_parse("-NaN", 0); /* non-sensical, returns null */

	single_basic_parse("Inf", 0); /* must use full string, returns null */
	single_basic_parse("inf", 0); /* must use full string, returns null */
	single_basic_parse("Infinity", 0);
	single_basic_parse("infinity", 0);
	single_basic_parse("-Infinity", 0);
	single_basic_parse("-infinity", 0);
	single_basic_parse("{ \"min\": Infinity, \"max\": -Infinity}", 0);

	single_basic_parse("Infinity!", 0);
	single_basic_parse("Infinitynull", 0);
	single_basic_parse("InfinityXXXX", 0);
	single_basic_parse("-Infinitynull", 0);
	single_basic_parse("-InfinityXXXX", 0);
	single_basic_parse("Infinoodle", 0);
	single_basic_parse("InfinAAA", 0);
	single_basic_parse("-Infinoodle", 0);
	single_basic_parse("-InfinAAA", 0);

	single_basic_parse("True", 0);
	single_basic_parse("False", 0);

	single_basic_parse("12", 0);
	single_basic_parse("12.3", 0);
	single_basic_parse("12.3.4", 0); /* non-sensical, returns null */
	/* was returning (int)2015 before patch, should return null */
	single_basic_parse("2015-01-15", 0);

	/* ...but this works.  It's rather inconsistent, and a future major release
	 * should change the behavior so it either always returns null when extra
	 * bytes are present (preferred), or always return object created from as much
	 * as was able to be parsed.
	 */
	single_basic_parse("12.3xxx", 0);

	single_basic_parse("{\"FoO\"  :   -12.3E512}", 0);
	single_basic_parse("{\"FoO\"  :   -12.3E51.2}", 0); /* non-sensical, returns null */
	single_basic_parse("[\"\\n\"]", 0);
	single_basic_parse("[\"\\nabc\\n\"]", 0);
	single_basic_parse("[null]", 0);
	single_basic_parse("[]", 0);
	single_basic_parse("[false]", 0);
	single_basic_parse("[\"abc\",null,\"def\",12]", 0);
	single_basic_parse("{}", 0);
	single_basic_parse("{ \"foo\": \"bar\" }", 0);
	single_basic_parse("{ \"foo\": \"bar\", \"baz\": null, \"bool0\": true }", 0);
	single_basic_parse("{ \"foo\": [null, \"foo\"] }", 0);
	single_basic_parse("{ \"abc\": 12, \"foo\": \"bar\", \"bool0\": false, \"bool1\": true, \"arr\": [ 1, 2, 3, null, 5 ] }", 0);
	single_basic_parse("{ \"abc\": \"blue\nred\\ngreen\" }", 0);

	// Clear serializer for these tests so we see the actual parsed value.
	single_basic_parse("[0e]", 1);
	single_basic_parse("[0e+]", 1);
	single_basic_parse("[0e+-1]", 1);
	single_basic_parse("[18446744073709551616]", 1);
}

// Clear the re-serialization information that the tokener
// saves to ensure that the output reflects the actual
// values we parsed, rather than just the original input.
static void do_clear_serializer(json_object *jso)
{
	json_c_visit(jso, 0, clear_serializer, NULL);
}

static int clear_serializer(json_object *jso, int flags,
                     json_object *parent_jso,
                     const char *jso_key,
                     size_t *jso_index, void *userarg)
{
	if (jso)
		json_object_set_serializer(jso, NULL, NULL, NULL);
	return JSON_C_VISIT_RETURN_CONTINUE;
}

static void test_verbose_parse()
{
	json_object *new_obj;
	enum json_tokener_error error = json_tokener_success;

	new_obj = json_tokener_parse_verbose("{ foo }", &error);
	assert (error == json_tokener_error_parse_object_key_name);
	assert (new_obj == NULL);

	new_obj = json_tokener_parse("{ foo }");
	assert (new_obj == NULL);

	new_obj = json_tokener_parse("foo");
	assert (new_obj == NULL);
	new_obj = json_tokener_parse_verbose("foo", &error);
	assert (new_obj == NULL);

	/* b/c the string starts with 'f' parsing return a boolean error */
	assert (error == json_tokener_error_parse_boolean);

	puts("json_tokener_parse_versbose() OK");
}

struct incremental_step {
	const char *string_to_parse;
	int length;
	int char_offset;
	enum json_tokener_error expected_error;
	int reset_tokener;
} incremental_steps[] = {

	/* Check that full json messages can be parsed, both w/ and w/o a reset */
	{ "{ \"foo\": 123 }", -1, -1, json_tokener_success,  0 },
	{ "{ \"foo\": 456 }", -1, -1, json_tokener_success,  1 },
	{ "{ \"foo\": 789 }", -1, -1, json_tokener_success,  1 },

	/*  Check a basic incremental parse */
	{ "{ \"foo",          -1, -1, json_tokener_continue, 0 },
	{ "\": {\"bar",       -1, -1, json_tokener_continue, 0 },
	{ "\":13}}",          -1, -1, json_tokener_success,  1 },

	/* Check that json_tokener_reset actually resets */
	{ "{ \"foo",          -1, -1, json_tokener_continue, 1 },
	{ ": \"bar\"}",       -1, 0, json_tokener_error_parse_unexpected, 1 },

	/* Check incremental parsing with trailing characters */
	{ "{ \"foo",          -1, -1, json_tokener_continue, 0 },
	{ "\": {\"bar",       -1, -1, json_tokener_continue, 0 },
	{ "\":13}}XXXX",      10, 6, json_tokener_success,  0 },
	{ "XXXX",              4, 0, json_tokener_error_parse_unexpected, 1 },

	/* Check that trailing characters can change w/o a reset */
	{ "{\"x\": 123 }\"X\"", -1, 11, json_tokener_success, 0 },
	{ "\"Y\"",            -1, -1, json_tokener_success, 1 },

	/* To stop parsing a number we need to reach a non-digit, e.g. a \0 */
	{ "1",                 1, 1, json_tokener_continue, 0 },
	/* This should parse as the number 12, since it continues the "1" */
	{ "2",                 2, 1, json_tokener_success, 0 },
	{ "12{",               3, 2, json_tokener_success, 1 },

	/* Similar tests for other kinds of objects: */
	/* These could all return success immediately, since regardless of
	   what follows the false/true/null token we *will* return a json object,
       but it currently doesn't work that way.  hmm... */
	{ "false",             5, 5, json_tokener_continue, 1 },
	{ "false",             6, 5, json_tokener_success, 1 },
	{ "true",              4, 4, json_tokener_continue, 1 },
	{ "true",              5, 4, json_tokener_success, 1 },
	{ "null",              4, 4, json_tokener_continue, 1 },
	{ "null",              5, 4, json_tokener_success, 1 },

	{ "Infinity",          9, 8, json_tokener_success, 1 },
	{ "infinity",          9, 8, json_tokener_success, 1 },
	{ "-infinity",        10, 9, json_tokener_success, 1 },
	{ "infinity",          9, 0, json_tokener_error_parse_unexpected, 3 },
	{ "-infinity",        10, 1, json_tokener_error_parse_unexpected, 3 },

	{ "inf",               3, 3, json_tokener_continue, 0 },
	{ "inity",             6, 5, json_tokener_success, 1 },
	{ "-inf",              4, 4, json_tokener_continue, 0 },
	{ "inity",             6, 5, json_tokener_success, 1 },

	{ "i",                 1, 1, json_tokener_continue, 0 },
	{ "n",                 1, 1, json_tokener_continue, 0 },
	{ "f",                 1, 1, json_tokener_continue, 0 },
	{ "i",                 1, 1, json_tokener_continue, 0 },
	{ "n",                 1, 1, json_tokener_continue, 0 },
	{ "i",                 1, 1, json_tokener_continue, 0 },
	{ "t",                 1, 1, json_tokener_continue, 0 },
	{ "y",                 1, 1, json_tokener_continue, 0 },
	{ "",                  1, 0, json_tokener_success, 1 },

	{ "-",                 1, 1, json_tokener_continue, 0 },
	{ "inf",               3, 3, json_tokener_continue, 0 },
	{ "ini",               3, 3, json_tokener_continue, 0 },
	{ "ty",                3, 2, json_tokener_success, 1 },

	{ "-",                 1, 1, json_tokener_continue, 0 },
	{ "i",                 1, 1, json_tokener_continue, 0 },
	{ "nfini",             5, 5, json_tokener_continue, 0 },
	{ "ty",                3, 2, json_tokener_success, 1 },

	{ "-i",                2, 2, json_tokener_continue, 0 },
	{ "nfinity",           8, 7, json_tokener_success, 1 },

	{ "InfinityX",        10, 8, json_tokener_success, 0 },
	{ "X",                 1, 0, json_tokener_error_parse_unexpected, 1 },

	{ "Infinity1234",     13, 8, json_tokener_success, 0 },
	{ "1234",              5, 4, json_tokener_success, 1 },

	{ "Infinity9999",      8, 8, json_tokener_continue, 0 },
	/* returns the Infinity loaded up by the previous call: */
	{ "1234",              5, 0, json_tokener_success, 0 },
	{ "1234",              5, 4, json_tokener_success, 1 },

	/* offset=1 because "n" is the start of "null".  hmm... */
	{ "noodle",            7, 1, json_tokener_error_parse_null, 1 },
	/* offset=2 because "na" is the start of "nan".  hmm... */
	{ "naodle",            7, 2, json_tokener_error_parse_null, 1 },
	/* offset=2 because "tr" is the start of "true".  hmm... */
	{ "track",             6, 2, json_tokener_error_parse_boolean, 1 },

	/* Although they may initially look like they should fail,
	   the next few tests check that parsing multiple sequential
       json objects in the input works as expected */
	{ "null123",           9, 4, json_tokener_success, 0 },
	{ "null123" + 4,       4, 3, json_tokener_success, 1 },
	{ "nullx",             5, 4, json_tokener_success, 0 },
	{ "nullx" + 4,         2, 0, json_tokener_error_parse_unexpected, 1 },
	{ "{\"a\":1}{\"b\":2}",15, 7, json_tokener_success, 0 },
	{ "{\"a\":1}{\"b\":2}" + 7,
	                       8, 7, json_tokener_success, 1 },

	/* Some bad formatting. Check we get the correct error status */
	{ "2015-01-15",       10, 4, json_tokener_error_parse_number, 1 },

	/* Strings have a well defined end point, so we can stop at the quote */
	{ "\"blue\"",         -1, -1, json_tokener_success, 0 },

	/* Check each of the escape sequences defined by the spec */
	{ "\"\\\"\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\\\\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\b\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\f\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\n\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\r\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\t\"",         -1, -1, json_tokener_success, 0 },
	{ "\"\\/\"",         -1, -1, json_tokener_success, 0 },
	// Escaping a forward slash is optional
	{ "\"/\"",           -1, -1, json_tokener_success, 0 },

	{ "[1,2,3]",          -1, -1, json_tokener_success, 0 },

	/* This behaviour doesn't entirely follow the json spec, but until we have
	   a way to specify how strict to be we follow Postel's Law and be liberal
	   in what we accept (up to a point). */
	{ "[1,2,3,]",         -1, -1, json_tokener_success, 0 },
	{ "[1,2,,3,]",        -1, 5, json_tokener_error_parse_unexpected, 0 },

	{ "[1,2,3,]",         -1, 7, json_tokener_error_parse_unexpected, 3 },
	{ "{\"a\":1,}",         -1, 7, json_tokener_error_parse_unexpected, 3 },

	{ NULL, -1, -1, json_tokener_success, 0 },
};

static void test_incremental_parse()
{
	json_object *new_obj;
	enum json_tokener_error jerr;
	struct json_tokener *tok;
	const char *string_to_parse;
	int ii;
	int num_ok, num_error;

	num_ok = 0;
	num_error = 0;

	printf("Starting incremental tests.\n");
	printf("Note: quotes and backslashes seen in the output here are literal values passed\n");
	printf("     to the parse functions.  e.g. this is 4 characters: \"\\f\"\n");

	string_to_parse = "{ \"foo"; /* } */
	printf("json_tokener_parse(%s) ... ", string_to_parse);
	new_obj = json_tokener_parse(string_to_parse);
	if (new_obj == NULL) puts("got error as expected");

	/* test incremental parsing in various forms */
	tok = json_tokener_new();
	for (ii = 0; incremental_steps[ii].string_to_parse != NULL; ii++)
	{
		int this_step_ok = 0;
		struct incremental_step *step = &incremental_steps[ii];
		int length = step->length;
		int expected_char_offset = step->char_offset;

		if (step->reset_tokener & 2)
			json_tokener_set_flags(tok, JSON_TOKENER_STRICT);
		else
			json_tokener_set_flags(tok, 0);

		if (length == -1)
			length = strlen(step->string_to_parse);
		if (expected_char_offset == -1)
			expected_char_offset = length;

		printf("json_tokener_parse_ex(tok, %-12s, %3d) ... ",
			step->string_to_parse, length);
		new_obj = json_tokener_parse_ex(tok, step->string_to_parse, length);

		jerr = json_tokener_get_error(tok);
		if (step->expected_error != json_tokener_success)
		{
			if (new_obj != NULL)
				printf("ERROR: invalid object returned: %s\n",
				       json_object_to_json_string(new_obj));
			else if (jerr != step->expected_error)
				printf("ERROR: got wrong error: %s\n",
				       json_tokener_error_desc(jerr));
			else if (tok->char_offset != expected_char_offset)
				printf("ERROR: wrong char_offset %d != expected %d\n",
				       tok->char_offset,
				       expected_char_offset);
			else
			{
				printf("OK: got correct error: %s\n",
				       json_tokener_error_desc(jerr));
				this_step_ok = 1;
			}
		}
		else
		{
			if (new_obj == NULL &&
			    !(step->length >= 4 &&
			      strncmp(step->string_to_parse, "null", 4) == 0))
				printf("ERROR: expected valid object, instead: %s\n",
				       json_tokener_error_desc(jerr));
			else if (tok->char_offset != expected_char_offset)
				printf("ERROR: wrong char_offset %d != expected %d\n",
				       tok->char_offset,
				       expected_char_offset);
			else
			{
				printf("OK: got object of type [%s]: %s\n",
					json_type_to_name(json_object_get_type(new_obj)),
					json_object_to_json_string(new_obj));
				this_step_ok = 1;
			}
		}

		if (new_obj)
			json_object_put(new_obj);

		if (step->reset_tokener & 1)
			json_tokener_reset(tok);

		if (this_step_ok)
			num_ok++;
		else
			num_error++;
	}

	json_tokener_free(tok);

	printf("End Incremental Tests OK=%d ERROR=%d\n", num_ok, num_error);
}
