#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "json.h"

int main(int argc, char **argv)
{
	json_object *tmp=json_object_new_int(123);
	assert (json_object_get_int(tmp)==123);
	json_object_set_int(tmp,321);
	assert (json_object_get_int(tmp)==321); 
	printf("INT PASSED\n");
	json_object_set_int64(tmp,(int64_t)321321321);
	assert (json_object_get_int64(tmp)==321321321); 
	json_object_put(tmp);
	printf("INT64 PASSED\n");
	tmp=json_object_new_boolean(TRUE);
	assert (json_object_get_boolean(tmp)==TRUE); 
	json_object_set_boolean(tmp,FALSE);
	assert (json_object_get_boolean(tmp)==FALSE); 
	json_object_set_boolean(tmp,TRUE);
	assert (json_object_get_boolean(tmp)==TRUE); 
	json_object_put(tmp);
	printf("BOOL PASSED\n");
	tmp=json_object_new_double(12.34);
	assert (json_object_get_double(tmp)==12.34); 
	json_object_set_double(tmp,34.56);
	assert (json_object_get_double(tmp)==34.56); 
	json_object_set_double(tmp,6435.34);
	assert (json_object_get_double(tmp)==6435.34); 
	json_object_put(tmp);
	printf("DOUBLE PASSED\n");
	#define SHORT "SHORT"
	#define MID   "A MID STRING"
	//             12345678901234567890123456789012....
	#define HUGE  "A string longer than 32 chars as to check non local buf codepath"
	tmp=json_object_new_string(SHORT);
	assert (strcmp(json_object_get_string(tmp),SHORT)==0); 
	json_object_set_string(tmp,MID);
	assert (strcmp(json_object_get_string(tmp),MID)==0); 
	json_object_set_string(tmp,HUGE);
	assert (strcmp(json_object_get_string(tmp),HUGE)==0); 
	json_object_set_string(tmp,SHORT);
	assert (strcmp(json_object_get_string(tmp),SHORT)==0); 
	json_object_put(tmp);
	printf("STRING PASSED\n");
	
	
	printf("PASSED\n");
	return 0;
}
