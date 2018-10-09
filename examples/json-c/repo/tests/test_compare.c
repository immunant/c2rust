/*
* Tests if json_object_equal behaves correct.
*/

#include <stdio.h>
#include <string.h>
#include "config.h"

#include "json_inttypes.h"
#include "json_object.h"
#include "json_tokener.h"

int main()
{
	/* integer tests */
	struct json_object *int1 = json_object_new_int(0);
	struct json_object *int2 = json_object_new_int(1);
	struct json_object *int3 = json_object_new_int(1);

	if (!json_object_equal(int1, int2))
		printf("JSON integer comparision is correct\n");
	else
		printf("JSON integer comparision failed\n");

	if (json_object_equal(int1, int1))
		printf("JSON same object comparision is correct\n");
	else
		printf("JSON same object comparision failed\n");

	if (json_object_equal(int2, int3))
		printf("JSON same integer comparision is correct\n");
	else
		printf("JSON same integer comparision failed\n");

	json_object_put(int1);
	json_object_put(int2);
	json_object_put(int3);

	/* string tests */
	struct json_object *str1 = json_object_new_string("TESTSTRING");
	struct json_object *str2 = json_object_new_string("TESTSTRING");
	struct json_object *str3 = json_object_new_string("DIFFERENT");

	if (json_object_equal(str1, str2))
		printf("Comparing equal strings is correct\n");
	else
		printf("Comparing equal strings failed\n");

	if (!json_object_equal(str1, str3))
		printf("Comparing different strings is correct\n");
	else
		printf("Comparing different strings failed\n");

	json_object_put(str1);
	json_object_put(str2);
	json_object_put(str3);

	/* double tests */
	struct json_object *dbl1 = json_object_new_double(3.14159);
	struct json_object *dbl2 = json_object_new_double(3.14159);
	struct json_object *dbl3 = json_object_new_double(3.0);

	if (json_object_equal(dbl1, dbl2))
		printf("Comparing equal doubles is correct\n");
	else
		printf("Comparing equal doubles failed\n");

	if (!json_object_equal(dbl1, dbl3))
		printf("Comparing different doubles is correct\n");
	else
		printf("Comparing different doubles failed\n");

	json_object_put(dbl1);
	json_object_put(dbl2);
	json_object_put(dbl3);

	/* array tests */
	struct json_object *ar1 = json_object_new_array();
	struct json_object *ar2 = json_object_new_array();
	struct json_object *ar3 = json_object_new_array();
	struct json_object *ar4 = json_object_new_array();

	json_object_array_add(ar1, json_object_new_int(1));
	json_object_array_add(ar1, json_object_new_int(2));

	json_object_array_add(ar2, json_object_new_int(1));
	json_object_array_add(ar2, json_object_new_int(2));

	json_object_array_add(ar3, json_object_new_int(1));
	json_object_array_add(ar3, json_object_new_int(1));

	if (json_object_equal(ar1, ar2))
		printf("Comparing equal arrays is correct\n");
	else
		printf("Comparing equal arrays failed\n");

	json_object_array_add(ar2, json_object_new_int(1));
	if (!json_object_equal(ar1, ar2))
		printf("Comparing arrays of different len is correct\n");
	else
		printf("Comparing arrays of different len failed\n");

	if (!json_object_equal(ar1, ar3))
		printf("Comparing different arrays is correct\n");
	else
		printf("Comparing different arrays failed\n");

	if (!json_object_equal(ar1, ar4))
		printf("Comparing different arrays (one empty) is correct\n");
	else
		printf("Comparing different arrays (one empty) failed\n");

	json_object_put(ar1);
	json_object_put(ar2);
	json_object_put(ar3);
	json_object_put(ar4);

	/* object tests */
	struct json_object *obj1 = json_object_new_object();
	struct json_object *obj2 = json_object_new_object();

	json_object_object_add(obj1, "test1", json_object_new_int(123));
	json_object_object_add(obj1, "test2", json_object_new_int(321));
	json_object_object_add(obj1, "test3", json_object_new_int(320));
	json_object_object_add(obj1, "test4", json_object_new_int(319));
	json_object_object_add(obj1, "test5", json_object_new_int(318));

	json_object_object_add(obj2, "test5", json_object_new_int(318));
	json_object_object_add(obj2, "test4", json_object_new_int(319));
	json_object_object_add(obj2, "test3", json_object_new_int(320));
	json_object_object_add(obj2, "test2", json_object_new_int(321));
	json_object_object_add(obj2, "test1", json_object_new_int(123));

	/* key-order is different between obj1 and obj2, should still be equal */
	if (json_object_equal(obj1, obj2))
		printf("Comparing JSON object with different key order is correct\n");
	else
		printf("Comparing JSON object with different key order is incorrect\n");

	/* make obj2 look different to obj1 */
	json_object_object_add(obj2, "test3", json_object_new_int(234));
	if (!json_object_equal(obj1, obj2))
		printf("Comparing different objects is correct\n");
	else
		printf("Comparing different objects is incorrect\n");

	json_object_put(obj1);
	json_object_put(obj2);

	return 0;
}
