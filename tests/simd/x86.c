#include <xmmintrin.h>
#include <emmintrin.h>
#include <immintrin.h>

void unpack_128_2x128(__m128i data, __m128i* data_lo, __m128i* data_hi)
{
    *data_lo = _mm_unpacklo_epi8 (data, _mm_setzero_si128());
    *data_hi = _mm_unpackhi_epi8 (data, _mm_setzero_si128());
}

void zero_init_all() {
    __m128 a;
    __m256 b;
    __m128d c;
    __m128i d;
    __m256d e;
    __m256i f;
    __m64 g;
}
