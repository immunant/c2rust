#include <stdint.h>
#include <stdlib.h>

uint32_t cur_rand_seed = 0;

void set_rand_seed(uint32_t s) {
	cur_rand_seed = s;
}

uint32_t get_rand_seed() {
	const uint32_t INCREMENT = 1;
	const uint32_t MULTIPLIER = 0x015A4E35;
	cur_rand_seed = MULTIPLIER * cur_rand_seed + INCREMENT;
	uint32_t ret = abs((int32_t)cur_rand_seed);
	return ret;
}
