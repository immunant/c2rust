#include "hash/sha1.h"

int hash_value(int x) {
    return sha1_mix(x) + 1;
}
