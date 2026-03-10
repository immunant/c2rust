#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    double angle = 3.14159 / 2.0;
    double _result = sin(angle);

    // Multiple calls in one expression
    double _z = sin(0.0) + sin(1.0);

    // Nested call
    double _w = sin(cos(0.5));

    // Test sinf (f32 variant)
    float angle_f32 = 1.5f;
    float _result_f32 = sinf(angle_f32);

    // Mixed calls
    double _mixed = sin(1.0) + (double)sinf(1.0f);

    // Additional math functions
    double _c = cos(0.25);
    double _t = tan(0.25);
    double _asn = asin(0.5);
    double _acs = acos(0.5);
    double _atn = atan(1.0);
    double _snh = sinh(1.0);
    double _csh = cosh(1.0);
    double _tnh = tanh(1.0);
    double _s = sqrt(4.0);
    double _p = pow(2.0, 3.0);
    double _l = log(2.0);
    double _e = exp(1.0);
    double _a = fabs(-1.0);

    // Integer absolute value functions
    int _i = abs(-42);
    long _j = labs(-123456789L);
    long long _k = llabs(-987654321LL);

    // Rounding functions
    double _fl = floor(3.7);
    float _flf = floorf(3.7f);
    double _ce = ceil(3.2);
    float _cef = ceilf(3.2f);
    double _rnd = round(3.5);
    double _tr = trunc(3.9);
    float _trf = truncf(3.9f);

    // Binary functions
    double _at2 = atan2(1.0, 1.0);
    double _hyp = hypot(3.0, 4.0);
    double _mx = fmax(1.0, 2.0);

    return 0;
}
