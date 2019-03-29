#include <xmmintrin.h>
#include <emmintrin.h>
#include <immintrin.h>

// Our travis-ci machines don't support AVX2 so we conditionally compile those bits out

typedef struct {
    __m64 a;
    __m128 b;
    __m128d c;
    __m256 d;
    __m256d e;
    __m128i f, g, h, o;
#ifdef __AVX2__
    __m256i i, j, k;
#endif
    __m64 l;
    __m128i m;
#ifdef __AVX2__
    __m256i n;
#endif
} ShuffleVectors;

typedef struct {
    __m128 a;
    __m256 b;
    __m128d c;
    __m256d d;
    __m128i e;
    __m256i f;
} VectorInitLists;

void unpack_128_2x128(__m128i data, __m128i* data_lo, __m128i* data_hi)
{
    *data_lo = _mm_unpacklo_epi8 (data, _mm_setzero_si128());
    *data_hi = _mm_unpackhi_epi8 (data, _mm_setzero_si128());
}

void zero_init_all(void) {
    __m128 a;
    __m256 b;
    __m128d c;
    __m128i d;
    __m256d e;
    __m256i f;
    __m64 g;
}

ShuffleVectors call_all(void) {
    __m128 a = _mm_setr_ps(7.8, 5.6, 3.4, 1.2);
    __m128d b = _mm_set1_pd(4.13);
    __m64 c = _mm_set_pi32(1, 2);
    __m256 d = _mm256_set1_ps(45.2);
    __m256d e = _mm256_set_pd(1.1, 2.2, 3.3, 4.4);
    __m128i f = _mm_set1_epi8(123);
    __m256i g = _mm256_set_epi32(14, 18, 22, 33, -11, -3, 8, 300);

    ShuffleVectors sv = {
        // Actual Builtin:
        _mm_shuffle_pi16(c, _MM_SHUFFLE(0, 1, 2, 3)),

        // Super builtins(in clang 6, but actual in 7):
        _mm_shuffle_ps(a, a, _MM_SHUFFLE(3, 2, 1, 0)),
        _mm_shuffle_pd(b, b, (1 << 1 | 1 << 0)),
        _mm256_shuffle_ps(d, d, _MM_SHUFFLE(1, 2, 2, 1)),
        _mm256_shuffle_pd(e, e, (1 << 3 | 1 << 2 | 0 << 1 | 0 << 0)),
        _mm_shuffle_epi32(f, _MM_SHUFFLE(1, 0, 0, 1)),
        _mm_shufflehi_epi16(f, _MM_SHUFFLE(0, 1, 2, 3)),
        _mm_shufflelo_epi16(f, _MM_SHUFFLE(3, 2, 3, 1)),
        _mm_slli_si128(f, 2),
#ifdef __AVX2__
        _mm256_shuffle_epi32(g, _MM_SHUFFLE(0, 3, 2, 0)),
        _mm256_shufflehi_epi16(g, _MM_SHUFFLE(1, 2, 3, 3)),
        _mm256_shufflelo_epi16(g, _MM_SHUFFLE(2, 3, 2, 3)),
#endif
        // Functions:
        _mm_shuffle_pi8(c, c),
        _mm_shuffle_epi8(f, f),
#ifdef __AVX2__
        _mm256_shuffle_epi8(g, g),
#endif
    };

    return sv;
}

ShuffleVectors call_all_used(void) {
    __m128 aa = _mm_setr_ps(1.2, 3.4, 5.6, 7.8);
    __m128d bb = _mm_set1_pd(3.14);
    __m64 cc = _mm_set_pi32(1, 2);
    __m256 dd = _mm256_set1_ps(3.34);
    __m256d ee = _mm256_set_pd(4.4, 3.3, 2.2, 1.1);
    __m128i ff = _mm_set1_epi8(13);
    __m256i gg = _mm256_set_epi32(-12, 33, 44, 100, -44, 42, -33, -100);

    __m64 a;
    __m128 b;
    __m128d c;
    __m256 d;
    __m256d e;
    __m128i f, g, h, o;
    __m256i i, j, k;
    __m64 l;
    __m128i m;
    __m256i n;

    // Actual Builtin:
    a = _mm_shuffle_pi16(cc, _MM_SHUFFLE(0, 1, 2, 3));

    // Super builtins(in clang 6, but actual in 7):
    b = _mm_shuffle_ps(aa, aa, _MM_SHUFFLE(3, 2, 1, 0));
    c = _mm_shuffle_pd(bb, bb, (1 << 1 | 1 << 0));
    d = _mm256_shuffle_ps(dd, dd, _MM_SHUFFLE(1, 2, 2, 1));
    e = _mm256_shuffle_pd(ee, ee, (1 << 3 | 1 << 2 | 0 << 1 | 0 << 0));
    f = _mm_shuffle_epi32(ff, _MM_SHUFFLE(1, 0, 0, 1));
    g = _mm_shufflehi_epi16(f, _MM_SHUFFLE(0, 1, 2, 3));
    h = _mm_shufflelo_epi16(g, _MM_SHUFFLE(3, 2, 3, 1));
#ifdef __AVX2__
    i = _mm256_shuffle_epi32(gg, _MM_SHUFFLE(0, 3, 2, 0));
    j = _mm256_shufflehi_epi16(gg, _MM_SHUFFLE(1, 2, 3, 3));
    k = _mm256_shufflelo_epi16(gg, _MM_SHUFFLE(2, 3, 2, 3));
#endif
    o = _mm_slli_si128(g, 2);

    // Functions:
    l = _mm_shuffle_pi8(cc, cc);
    m = _mm_shuffle_epi8(ff, ff);
#ifdef __AVX2__
    n = _mm256_shuffle_epi8(gg, gg);
#endif

    ShuffleVectors sv = {
        a, b, c, d, e, f, g, h, o,

#ifdef __AVX2__
        i, j, k,
#endif
        l, m,

#ifdef __AVX2__
        n,
#endif
    };

    return sv;
}

VectorInitLists vector_init_lists(void) {
    VectorInitLists il = {
        {1.3f, 2.3f, 3.4f, 4.4f},
        {2.2f, 4.4f, 5.6f, 4.3f, 6.7f, 6.6f, 5.5f, 8.8f},
        {2.2, 4.4},
        {2.2, 3.3, 4.4, 5.5},
        {45LL, 32LL},
        {12LL, 34LL, 56LL, 78LL},
    };

    return il;
}

VectorInitLists vector_init_lists_used(void) {
    __m128 a = {1.3f, 2.3f, 3.4f, 4.4f};
    __m256 b = {2.2f, 4.4f, 5.6f, 4.3f, 6.7f, 6.6f, 5.5f, 8.8f};
    __m128d c = {2.2, 4.4};
    __m256d d = {2.2, 3.3, 4.4, 5.5};
    __m128i e = {45LL, 32LL};
    __m256i f = {12LL, 34LL, 56LL, 78LL};

    VectorInitLists il = {
        a, b, c, d, e, f,
    };

    return il;
}

__m128 static_m128 = {1.3f, 2.3f, 3.4f, 4.4f};
__m256 static_m256 = {2.2f, 4.4f, 5.6f, 4.3f, 6.7f, 6.6f, 5.5f, 8.8f};
__m128d static_m128d = {2.2, 4.4};
__m256d static_m256d = {2.2, 3.3, 4.4, 5.5};
__m128i static_m128i = {45LL, 32LL};
__m256i static_m256i = {12LL, 34LL, 56LL, 78LL};

__m128 static_uninit_m128;
__m256 static_uninit_m256;
__m128d static_uninit_m128d;
__m256d static_uninit_m256d;
__m128i static_uninit_m128i;
__m256i static_uninit_m256i;
