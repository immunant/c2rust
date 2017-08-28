/*
 * random_seed.c
 *
 * Copyright (c) 2013 Metaparadigm Pte. Ltd.
 * Michael Clark <michael@metaparadigm.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the MIT license. See COPYING for details.
 *
 */

#include <time.h>

static int get_time_seed()
{
    //DEBUG_SEED("get_time_seed");
    
    return (int)time(NULL) * 433494437;
}


/* json_c_get_random_seed */

int json_c_get_random_seed()
{
    return get_time_seed();
}
