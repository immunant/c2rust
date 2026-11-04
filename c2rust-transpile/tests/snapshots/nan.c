float static_nansf = __builtin_nansf("");
double static_nans = __builtin_nans("");

const char *next_payload(void);

void nan_payloads(void) {
    float qf = __builtin_nanf("");
    double qd = __builtin_nan("");
    long double ql = __builtin_nanl("");
    float sf = __builtin_nansf("");
    double sd = __builtin_nans("");
    long double sl = __builtin_nansl("");

    // Payloads are integers in any radix clang accepts.
    double q1 = __builtin_nan("1");
    double s1 = __builtin_nans("0x1f");
    float s2 = __builtin_nansf("010");
    // Clearing the quiet bit leaves an empty payload, so the default is used.
    double s3 = __builtin_nans("0x8000000000000");
    // How macOS defines `NAN`.
    float q2 = __builtin_nanf("0x7fc00000");

    // Clang doesn't fold these; libm returns the default NaN for a malformed payload.
    double q3 = __builtin_nan("xyz");
    double q4 = __builtin_nan(next_payload());
}
