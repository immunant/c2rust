/*
   BLAKE reference C implementation

   Copyright (c) 2012 Jean-Philippe Aumasson <jeanphilippe.aumasson@gmail.com>

   To the extent possible under law, the author(s) have dedicated all copyright
   and related and neighboring rights to this software to the public domain
   worldwide. This software is distributed without any warranty.

   You should have received a copy of the CC0 Public Domain Dedication along
   with this software. If not, see
   <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#include <stdint.h>

typedef struct
{
  unsigned int h[8];
  unsigned int  s[4];
  unsigned int  t[2];
  int buflen;
  int  nullt;
  unsigned char buf[64];
} blakestate256;

typedef unsigned long long crypto_uint64;
typedef unsigned int crypto_uint32;
typedef unsigned char crypto_uint8;

typedef crypto_uint64 u64;
typedef crypto_uint32 u32;
typedef crypto_uint8 u8; 

#define U8TO32(p)					      \
  (((uint32_t)((p)[0]) << 24) | ((uint32_t)((p)[1]) << 16) |  \
   ((uint32_t)((p)[2]) <<  8) | ((uint32_t)((p)[3])      ))
#define U32TO8(p, v)         \
  (p)[0] = (uint8_t)((v) >> 24); \
  (p)[1] = (uint8_t)((v) >> 16); \
  (p)[2] = (uint8_t)((v) >> 8);  \
  (p)[3] = (uint8_t)((v));

static const u32 cst[16] = {
  0x243F6A88,0x85A308D3,0x13198A2E,0x03707344,
  0xA4093822,0x299F31D0,0x082EFA98,0xEC4E6C89,
  0x452821E6,0x38D01377,0xBE5466CF,0x34E90C6C,
  0xC0AC29B7,0xC97C50DD,0x3F84D5B5,0xB5470917};

static const u8 padding[] =
  {0x80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

#define BLAKE256_ROT(x,n) (((x)<<(32-n))|( (x)>>(n)))

void blake256_compress( blakestate256 *S, const unsigned char *block )
{
  u32 m0;
  u32 m1;
  u32 m2;
  u32 m3;
  u32 m4;
  u32 m5;
  u32 m6;
  u32 m7;
  u32 m8;
  u32 m9;
  u32 m10;
  u32 m11;
  u32 m12;
  u32 m13;
  u32 m14;
  u32 m15;
  u32 v0;
  u32 v1;
  u32 v2;
  u32 v3;
  u32 v4;
  u32 v5;
  u32 v6;
  u32 v7;
  u32 v8;
  u32 v9;
  u32 v10;
  u32 v11;
  u32 v12;
  u32 v13;
  u32 v14;
  u32 v15;

  m0 = U8TO32(block + 0);
  m1 = U8TO32(block + 4);
  m2 = U8TO32(block + 8);
  m3 = U8TO32(block + 12);
  m4 = U8TO32(block + 16);
  m5 = U8TO32(block + 20);
  m6 = U8TO32(block + 24);
  m7 = U8TO32(block + 28);
  m8 = U8TO32(block + 32);
  m9 = U8TO32(block + 36);
  m10 = U8TO32(block + 40);
  m11 = U8TO32(block + 44);
  m12 = U8TO32(block + 48);
  m13 = U8TO32(block + 52);
  m14 = U8TO32(block + 56);
  m15 = U8TO32(block + 60);
  v0 = S->h[0];
  v1 = S->h[1];
  v2 = S->h[2];
  v3 = S->h[3];
  v4 = S->h[4];
  v5 = S->h[5];
  v6 = S->h[6];
  v7 = S->h[7];
  v8 = S->s[0] ^ 0x243F6A88;
  v9 = S->s[1] ^ 0x85A308D3;
  v10 = S->s[2] ^ 0x13198A2E;
  v11 = S->s[3] ^ 0x03707344;
  v12 = 0xA4093822;
  v13 = 0x299F31D0;
  v14 = 0x082EFA98;
  v15 = 0xEC4E6C89;
  if (S->nullt == 0) { 
    v12 ^= S->t[0];
    v13 ^= S->t[0];
    v14 ^= S->t[1];
    v15 ^= S->t[1];
  }

#define ROUND(m0,c0,m1,c1,m2,c2,m3,c3,m4,c4,m5,c5,m6,c6,m7,c7,m8,c8,m9,c9,m10,c10,m11,c11,m12,c12,m13,c13,m14,c14,m15,c15) \
    v0 += m0 ^ c0; \
    v0 += v4; \
    v12 ^= v0; \
    v12 = BLAKE256_ROT( v12,16); \
    v8 += v12; \
    v4 ^= v8; \
    v4 = BLAKE256_ROT( v4,12); \
      v1 += m2 ^ c2; \
      v1 += v5; \
      v13 ^= v1; \
      v13 = BLAKE256_ROT( v13,16); \
      v9 += v13; \
      v5 ^= v9; \
      v5 = BLAKE256_ROT( v5,12); \
        v2 += m4 ^ c4; \
        v2 += v6; \
        v14 ^= v2; \
        v14 = BLAKE256_ROT( v14,16); \
        v10 += v14; \
        v6 ^= v10; \
        v6 = BLAKE256_ROT( v6,12); \
          v3 += m6 ^ c6; \
          v3 += v7; \
          v15 ^= v3; \
          v15 = BLAKE256_ROT( v15,16); \
          v11 += v15; \
          v7 ^= v11; \
          v7 = BLAKE256_ROT( v7,12); \
        v2 += m5 ^ c5; \
        v2 += v6; \
        v14 ^= v2; \
        v14 = BLAKE256_ROT( v14, 8); \
        v10 += v14; \
        v6 ^= v10; \
        v6 = BLAKE256_ROT( v6, 7); \
          v3 += m7 ^ c7; \
          v3 += v7; \
          v15 ^= v3; \
          v15 = BLAKE256_ROT( v15, 8); \
          v11 += v15; \
          v7 ^= v11; \
          v7 = BLAKE256_ROT( v7, 7); \
      v1 += m3 ^ c3; \
      v1 += v5; \
      v13 ^= v1; \
      v13 = BLAKE256_ROT( v13, 8); \
      v9 += v13; \
      v5 ^= v9; \
      v5 = BLAKE256_ROT( v5, 7); \
    v0 += m1 ^ c1; \
    v0 += v4; \
    v12 ^= v0; \
    v12 = BLAKE256_ROT( v12, 8); \
    v8 += v12; \
    v4 ^= v8; \
    v4 = BLAKE256_ROT( v4, 7); \
            v0 += m8 ^ c8; \
            v0 += v5; \
            v15 ^= v0; \
            v15 = BLAKE256_ROT( v15,16); \
            v10 += v15; \
            v5 ^= v10; \
            v5 = BLAKE256_ROT( v5,12); \
              v1 += m10 ^ c10; \
              v1 += v6; \
              v12 ^= v1; \
              v12 = BLAKE256_ROT( v12,16); \
              v11 += v12; \
              v6 ^= v11; \
              v6 = BLAKE256_ROT( v6,12); \
                v2 += m12 ^ c12; \
                v2 += v7; \
                v13 ^= v2; \
                v13 = BLAKE256_ROT( v13,16); \
                v8 += v13; \
                v7 ^= v8; \
                v7 = BLAKE256_ROT( v7,12); \
                  v3 += m14 ^ c14; \
                  v3 += v4; \
                  v14 ^= v3; \
                  v14 = BLAKE256_ROT( v14,16); \
                  v9 += v14; \
                  v4 ^= v9; \
                  v4 = BLAKE256_ROT( v4,12); \
                v2 += m13 ^ c13; \
                v2 += v7; \
                v13 ^= v2; \
                v13 = BLAKE256_ROT( v13, 8); \
                v8 += v13; \
                v7 ^= v8; \
                v7 = BLAKE256_ROT( v7, 7); \
                  v3 += m15 ^ c15; \
                  v3 += v4; \
                  v14 ^= v3; \
                  v14 = BLAKE256_ROT( v14, 8); \
                  v9 += v14; \
                  v4 ^= v9; \
                  v4 = BLAKE256_ROT( v4, 7); \
              v1 += m11 ^ c11; \
              v1 += v6; \
              v12 ^= v1; \
              v12 = BLAKE256_ROT( v12, 8); \
              v11 += v12; \
              v6 ^= v11; \
              v6 = BLAKE256_ROT( v6, 7); \
            v0 += m9 ^ c9; \
            v0 += v5; \
            v15 ^= v0; \
            v15 = BLAKE256_ROT( v15, 8); \
            v10 += v15; \
            v5 ^= v10; \
            v5 = BLAKE256_ROT( v5, 7); \

  ROUND(m0,cst[1],m1,cst[0],m2,cst[3],m3,cst[2],m4,cst[5],m5,cst[4],m6,cst[7],m7,cst[6],m8,cst[9],m9,cst[8],m10,cst[11],m11,cst[10],m12,cst[13],m13,cst[12],m14,cst[15],m15,cst[14])
  ROUND(m14,cst[10],m10,cst[14],m4,cst[8],m8,cst[4],m9,cst[15],m15,cst[9],m13,cst[6],m6,cst[13],m1,cst[12],m12,cst[1],m0,cst[2],m2,cst[0],m11,cst[7],m7,cst[11],m5,cst[3],m3,cst[5])
  ROUND(m11,cst[8],m8,cst[11],m12,cst[0],m0,cst[12],m5,cst[2],m2,cst[5],m15,cst[13],m13,cst[15],m10,cst[14],m14,cst[10],m3,cst[6],m6,cst[3],m7,cst[1],m1,cst[7],m9,cst[4],m4,cst[9])
  ROUND(m7,cst[9],m9,cst[7],m3,cst[1],m1,cst[3],m13,cst[12],m12,cst[13],m11,cst[14],m14,cst[11],m2,cst[6],m6,cst[2],m5,cst[10],m10,cst[5],m4,cst[0],m0,cst[4],m15,cst[8],m8,cst[15])
  ROUND(m9,cst[0],m0,cst[9],m5,cst[7],m7,cst[5],m2,cst[4],m4,cst[2],m10,cst[15],m15,cst[10],m14,cst[1],m1,cst[14],m11,cst[12],m12,cst[11],m6,cst[8],m8,cst[6],m3,cst[13],m13,cst[3])
  ROUND(m2,cst[12],m12,cst[2],m6,cst[10],m10,cst[6],m0,cst[11],m11,cst[0],m8,cst[3],m3,cst[8],m4,cst[13],m13,cst[4],m7,cst[5],m5,cst[7],m15,cst[14],m14,cst[15],m1,cst[9],m9,cst[1])
  ROUND(m12,cst[5],m5,cst[12],m1,cst[15],m15,cst[1],m14,cst[13],m13,cst[14],m4,cst[10],m10,cst[4],m0,cst[7],m7,cst[0],m6,cst[3],m3,cst[6],m9,cst[2],m2,cst[9],m8,cst[11],m11,cst[8])
  ROUND(m13,cst[11],m11,cst[13],m7,cst[14],m14,cst[7],m12,cst[1],m1,cst[12],m3,cst[9],m9,cst[3],m5,cst[0],m0,cst[5],m15,cst[4],m4,cst[15],m8,cst[6],m6,cst[8],m2,cst[10],m10,cst[2])
  ROUND(m6,cst[15],m15,cst[6],m14,cst[9],m9,cst[14],m11,cst[3],m3,cst[11],m0,cst[8],m8,cst[0],m12,cst[2],m2,cst[12],m13,cst[7],m7,cst[13],m1,cst[4],m4,cst[1],m10,cst[5],m5,cst[10])
  ROUND(m10,cst[2],m2,cst[10],m8,cst[4],m4,cst[8],m7,cst[6],m6,cst[7],m1,cst[5],m5,cst[1],m15,cst[11],m11,cst[15],m9,cst[14],m14,cst[9],m3,cst[12],m12,cst[3],m13,cst[0],m0,cst[13])
  ROUND(m0,cst[1],m1,cst[0],m2,cst[3],m3,cst[2],m4,cst[5],m5,cst[4],m6,cst[7],m7,cst[6],m8,cst[9],m9,cst[8],m10,cst[11],m11,cst[10],m12,cst[13],m13,cst[12],m14,cst[15],m15,cst[14])
  ROUND(m14,cst[10],m10,cst[14],m4,cst[8],m8,cst[4],m9,cst[15],m15,cst[9],m13,cst[6],m6,cst[13],m1,cst[12],m12,cst[1],m0,cst[2],m2,cst[0],m11,cst[7],m7,cst[11],m5,cst[3],m3,cst[5])
  ROUND(m11,cst[8],m8,cst[11],m12,cst[0],m0,cst[12],m5,cst[2],m2,cst[5],m15,cst[13],m13,cst[15],m10,cst[14],m14,cst[10],m3,cst[6],m6,cst[3],m7,cst[1],m1,cst[7],m9,cst[4],m4,cst[9])
  ROUND(m7,cst[9],m9,cst[7],m3,cst[1],m1,cst[3],m13,cst[12],m12,cst[13],m11,cst[14],m14,cst[11],m2,cst[6],m6,cst[2],m5,cst[10],m10,cst[5],m4,cst[0],m0,cst[4],m15,cst[8],m8,cst[15])

  v0 ^= v8;
  v1 ^= v9;
  v2 ^= v10;
  v3 ^= v11;
  v4 ^= v12;
  v5 ^= v13;
  v6 ^= v14;
  v7 ^= v15;

  v0 ^= S->s[0];
  v1 ^= S->s[1];
  v2 ^= S->s[2];
  v3 ^= S->s[3];
  v4 ^= S->s[0];
  v5 ^= S->s[1];
  v6 ^= S->s[2];
  v7 ^= S->s[3];

  S->h[0] ^= v0;
  S->h[1] ^= v1;
  S->h[2] ^= v2;
  S->h[3] ^= v3;
  S->h[4] ^= v4;
  S->h[5] ^= v5;
  S->h[6] ^= v6;
  S->h[7] ^= v7;
}
