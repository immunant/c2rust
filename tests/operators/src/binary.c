int binop_multiply_signed(int x, int y) {
	return x * y;
}

unsigned int binop_multiply_unsigned(unsigned int x, unsigned int y) {
	return x * y;
}

int binop_divide_signed(int x, int y) {
	return x / y;
}

unsigned int binop_divide_unsigned(unsigned int x, unsigned int y) {
	return x / y;
}

int binop_modulus_signed(int x, int y) {
	return x % y;
}

unsigned int binop_modulus_unsigned(unsigned int x, unsigned int y) {
	return x % y;
}

int binop_add_signed(int x, int y) {
	return x + y;
}

unsigned int binop_add_unsigned(unsigned int x, unsigned int y) {
	return x + y;
}

int binop_subtract_signed(int x, int y) {
	return x - y;
}

unsigned int binop_subtract_unsigned(unsigned int x, unsigned int y) {
	return x - y;
}

int binop_shiftleft_signed(int x, int y) {
	return x << y;
}

unsigned int binop_shiftleft_unsigned(unsigned int x, unsigned int y) {
	return x << y;
}

int binop_shiftright_signed(int x, int y) {
	return x >> y;
}

unsigned int binop_shiftright_unsigned(unsigned int x, unsigned int y) {
	return x >> y;
}

int binop_less_signed(int x, int y) {
	return x < y;
}

unsigned int binop_less_unsigned(unsigned int x, unsigned int y) {
	return x < y;
}

int binop_greater_signed(int x, int y) {
	return x > y;
}

unsigned int binop_greater_unsigned(unsigned int x, unsigned int y) {
	return x > y;
}

int binop_lessequal_signed(int x, int y) {
	return x <= y;
}

unsigned int binop_lessequal_unsigned(unsigned int x, unsigned int y) {
	return x <= y;
}

int binop_greaterequal_signed(int x, int y) {
	return x >= y;
}

unsigned int binop_greaterequal_unsigned(unsigned int x, unsigned int y) {
	return x >= y;
}

int binop_equalequal_signed(int x, int y) {
	return x == y;
}

unsigned int binop_equalequal_unsigned(unsigned int x, unsigned int y) {
	return x == y;
}

int binop_notequal_signed(int x, int y) {
	return x != y;
}

unsigned int binop_notequal_unsigned(unsigned int x, unsigned int y) {
	return x != y;
}

int binop_bitand_signed(int x, int y) {
	return x & y;
}

unsigned int binop_bitand_unsigned(unsigned int x, unsigned int y) {
	return x & y;
}

int binop_bitxor_signed(int x, int y) {
	return x ^ y;
}

unsigned int binop_bitxor_unsigned(unsigned int x, unsigned int y) {
	return x ^ y;
}

int binop_bitor_signed(int x, int y) {
	return x | y;
}

unsigned int binop_bitor_unsigned(unsigned int x, unsigned int y) {
	return x | y;
}

int binop_and_signed(int x, int y) {
	return x && y;
}

unsigned int binop_and_unsigned(unsigned int x, unsigned int y) {
	return x && y;
}

int binop_or_signed(int x, int y) {
	return x || y;
}

unsigned int binop_or_unsigned(unsigned int x, unsigned int y) {
	return x || y;
}


int binop_assignadd_signed(int x, int y) {
	return x += y;
}

unsigned int binop_assignadd_unsigned(unsigned int x, unsigned int y) {
	return x += y;
}

int binop_assignsubtract_signed(int x, int y) {
	return x -= y;
}

unsigned int binop_assignsubtract_unsigned(unsigned int x, unsigned int y) {
	return x -= y;
}

int binop_assignmultiply_signed(int x, int y) {
	return x *= y;
}

unsigned int binop_assignmultiply_unsigned(unsigned int x, unsigned int y) {
	return x *= y;
}

int binop_assigndivide_signed(int x, int y) {
	return x /= y;
}

unsigned int binop_assigndivide_unsigned(unsigned int x, unsigned int y) {
	return x /= y;
}

int binop_assignmodulus_signed(int x, int y) {
	return x %= y;
}

unsigned int binop_assignmodulus_unsigned(unsigned int x, unsigned int y) {
	return x %= y;
}

int binop_assignbitxor_signed(int x, int y) {
	return x ^= y;
}

unsigned int binop_assignbitxor_unsigned(unsigned int x, unsigned int y) {
	return x ^= y;
}

int binop_assignshiftleft_signed(int x, int y) {
	return x <<= y;
}

unsigned int binop_assignshiftleft_unsigned(unsigned int x, unsigned int y) {
	return x <<= y;
}

int binop_assignshiftright_signed(int x, int y) {
	return x >>= y;
}

unsigned int binop_assignshiftright_unsigned(unsigned int x, unsigned int y) {
	return x >>= y;
}

int binop_assignbitor_signed(int x, int y) {
	return x |= y;
}

unsigned int binop_assignbitor_unsigned(unsigned int x, unsigned int y) {
	return x |= y;
}

int binop_assignbitand_signed(int x, int y) {
	return x &= y;
}

unsigned int binop_assignbitand_unsigned(unsigned int x, unsigned int y) {
	return x &= y;
}


int binop_assign_signed(int x, int y) {
	return x = y;
}

unsigned int binop_assign_unsigned(unsigned int x, unsigned int y) {
	return x = y;
}

int binop_comma_signed(int x, int y) {
	return x , y;
}

unsigned int binop_comma_unsigned(unsigned int x, unsigned int y) {
	return x , y;
}


void unused_binary(void) {
	1 + 1;
	1 - 1;
	1 && 1;
	1 || 1;
}

long int _var_array(unsigned long int size) {
	long int arr[size];
	return arr;
}

void assignment(void) {
	volatile int x = 0;
	x = 1;
	x += 2;
	// volatile int x1 = x + 1000;
	// volatile int x2 = x - 1000;
	// volatile int x3 = x * x;
	// volatile int x4 = x / x;
	// volatile int x5 = x % x;
	// volatile int x6 = x4 == x5;
	// volatile int x7 = x4 != x5;

	unsigned int y = 0;
	y += 3;
	y -= 2;
	// unsigned int y1 = y + 1000;
	// unsigned int y2 = y - 1000;
	// unsigned int y3 = y * y;
	// unsigned int y4 = y / y;
	// unsigned int y5 = y % y;
	// unsigned int y6 = y4 == y5;
	// unsigned int y7 = y4 != y5;

	float z = 0;
	z *= x;
	z /= y;

	// _Bool q = 1;
	// q += 1;
}

void pre_increment(void) {
	// _Float16 h = 0.f16;
	// ++h;
	// _Float16 h2 = ++h;

	float f = 0.f;
	++f;
	float f2 = ++f;

	double d = 0.;
	++d;
	double d2 = ++d;

	long double ld = 0.l;
	++ld;
	long double ld2 = ++ld;
}

void post_increment(void) {
	int i = 0;
	i++;
	int i2 = i++;
	int i3 = i--;
	i--;

	volatile int vi = 0;
	vi++;
	// XXX: https://github.com/immunant/c2rust/pull/1135
	// volatile int vi2 = vi++;
	// volatile int vi3 = vi--;
	vi--;

	unsigned int ui = 0u;
	ui++;
	unsigned int ui2 = ui++;
	unsigned int ui3 = ui--;
	ui--;

	// _Float16 h = 0.f16;
	// h++;
	// _Float16 h2 = h++;
	// _Float16 h3 = h--;
	// h--;

	float f = 0.f;
	f++;
	float f2 = f++;
	float f3 = f--;
	f--;

	double d = 0.;
	d++;
	double d2 = d++;
	double d3 = d--;
	d--;

	long double ld = 0.l;
	ld++;
	long double ld2 = ld++;
	long double ld3 = ld--;
	ld--;

	int *pi = i;
	*pi++;
	int *pi2 = *pi++;
	int *pi3 = *pi--;
	*pi--;

	int *v = _var_array(37);
	*v++;
	int v2 = *v++;
	int v3 = *v--;
	*v--;
}
