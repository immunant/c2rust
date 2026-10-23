int* unop_addressof(int x) {
	return &x;
}

int unop_deref_int(int* x) {
	return *x;
}

// XXX: https://github.com/immunant/c2rust/pull/1135
/*
int unop_deref_volatile(volatile int x) {
    int* x2 = x++;
    return *x2;
}
*/

// int unop_deref_volatile(int* x) {
// 	volatile const int x1 = &x;


//     volatile const int x2 = x1;
//     volatile int x3 = unop_deref_int(x2);
// 	int* x4 = x3;

//     return x4;
// }

// int unop_deref_addressof(int x) {
// 	int* x1 = &x;
// 	int x2 = *x1;
// 	return x2;
// }

typedef int int_to_int(int);
int unop_deref_function(int x) {
	int_to_int* fn = &unop_deref_int;

	return (*fn)(x);
}

// char unop_deref_array(char *x) {
// 	return *x;
// }

int unop_plus(int x) {
	return +x;
}

int unop_postincrement(int x) {
	return x++;
}

int unop_preincrement(int x) {
	return ++x;
}

signed int unop_negate_signed_int(signed int x) {
	return -x;
}

unsigned int unop_negate_unsigned_int(unsigned int x) {
	return -x;
}

int unop_postdecrement(int x) {
	return x--;
}

int unop_predecrement(int x) {
	return --x;
}

int unop_complement(int x) {
	return ~x;
}

int unop_not(int x) {
	return !x;
}

/*
int unop_real(int x) {
	return __real x;
}
*/

/*
int unop_imag(int x) {
	return __imag x;
}
*/

int unop_extension(int x) {
	return __extension__ x;
}

/*
#ifdef co_await
int unop_coawait(int x) {
	return co_await x;
}
#endif
*/

void unused_unary(void) {
	-2;
	+2;
}

void something(void) {
	int *x = (void*)0;
	int *y = &*x;
	int *z = *&y;

	const int *w = *&z;
}
