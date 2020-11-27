#ifndef SSE2NEON_H
#define SSE2NEON_H

#if defined(__GNUC__) || defined(__clang__)
  #pragma push_macro("FORCE_INLINE")
  #pragma push_macro("ALIGN_STRUCT")

  #define FORCE_INLINE static inline __attribute__((always_inline))
  #define ALIGN_STRUCT(x) __attribute__((aligned(x)))
#else
  #ifdef FORCE_INLINE
    #undef FORCE_INLINE
  #endif

  #define FORCE_INLINE static inline

  #ifndef ALIGN_STRUCT
    #define ALIGN_STRUCT(x) __declspec(align(x))
  #endif
#endif

#if defined(__GNUC__)
  #if defined(__arm__) && __ARM_ARCH == 7
    #if !defined(__ARM_NEON) || !defined(__ARM_NEON__)
      #error "You must enable NEON instruction (e.g. -mfpu=neon) to use ROWEX"
    #endif

    #pragma GCC push_options
    #pragma GCC target("fpu=neon")
  #elif defined(__aarch64__)
    #pragma GCC push_options
    #pragma GCC target("+simd")
  #else
    #error "Unsupported target. Must be either ARMv7-A+NEON or ARMv8-A."
  #endif
#endif

#include <arm_neon.h>

typedef int64x2_t __m128i ;

#define vreinterpretq_m128i_s32(x) vreinterpretq_s64_s32(x)
#define vreinterpretq_s32_m128i(x) vreinterpretq_s32_s64(x)

#define vreinterpretq_s8_m128i(x) vreinterpretq_s8_s64(x)
#define vreinterpretq_u8_m128i(x) vreinterpretq_u8_s64(x)
#define vreinterpretq_m128i_s8(x) vreinterpretq_s64_s8(x)
#define vreinterpretq_m128i_u8(x) vreinterpretq_s64_u8(x)

FORCE_INLINE __m128i _mm_loadu_si128(const __m128i *p)
{
  return vreinterpretq_m128i_s32(vld1q_s32((const int32_t *) p)) ;
}

FORCE_INLINE __m128i _mm_storeu_si128(__m128i *p, __m128i a)
{
  vst1q_s32((int32_t *) p, vreinterpretq_s32_m128i(a));
}

FORCE_INLINE __m128i _mm_set_epi32(int i3, int i2, int i1, int i0)
{
  int32_t ALIGN_STRUCT(16) data[4] = {i0, i1, i2, i3};
  return vreinterpretq_m128i_s32(vld1q_s32(data));
}

FORCE_INLINE __m128i _mm_shuffle_epi8(__m128i a, __m128i b)
{
  int8x16_t tbl = vreinterpretq_s8_m128i(a);
  uint8x16_t idx = vreinterpretq_u8_m128i(b);
  uint8x16_t idx_masked =
    vandq_u8(idx, vdupq_n_u8(0x8F));
#if defined(__aarch64__)
  return vreinterpretq_m128i_s8(vqtbl1q_s8(tbl, idx_masked)) ;
#elif defined(__GNUC__)
  int8x16_t ret;

  __asm__ __volatile__(
    "vtbl.8 %e[ret], {%e[tbl], %f[tbl]}, %e[idx]\n"
    "vtbl.8 %f[ret], {%e[tbl], %f[tbl]}, %f[idx]\n"
    : [ret] "=&w"(ret)
    : [tbl] "w"(tbl), [idx] "w"(idx_masked));

  return vreinterpretq_m128i_s8(ret) ;
#else
  int8x8x2_t a_split {vget_low_s8(tbl), vget_high_s8(tbl)};
  return vreinterpretq_m128i_s8(
		  vcombine_s8(vtbl2_s8(a_split, vget_loag_u8(idx_masked)),
			      vtbl2_s8(a_split, vget_high_u8(idx_masked))));
#endif
}

FORCE_INLINE __m128i _mm_cmpeq_epi8(__m128i a, __m128i b)
{
  return vreinterpretq_m128i_u8(
    vceqq_s8(vreinterpretq_s8_m128i(a), vreinterpretq_s8_m128i(b)));
}

FORCE_INLINE __m128i _mm_set1_epi8(signed char w)
{
  return vreinterpretq_m128i_s8(vdupq_n_s8(w));
}

FORCE_INLINE void _mm_pause(void)
{
  __asm__ __volatile__ ("yield");
}

FORCE_INLINE int _mm_movemask_epi8(__m128i a)
{
#if defined(__aarch64__)
    uint8x16_t input = vreinterpretq_u8_m128i(a);
    const int8_t ALIGN_STRUCT(16)
        xr[16] = {-7, -6, -5, -4, -3, -2, -1, 0, -7, -6, -5, -4, -3, -2, -1, 0};
    const uint8x16_t mask_and = vdupq_n_u8(0x80);
    const int8x16_t mask_shift = vld1q_s8(xr);
    const uint8x16_t mask_result =
        vshlq_u8(vandq_u8(input, mask_and), mask_shift);
    uint8x8_t lo = vget_low_u8(mask_result);
    uint8x8_t hi = vget_high_u8(mask_result);

    return vaddv_u8(lo) + (vaddv_u8(hi) << 8);
#else
    // Use increasingly wide shifts+adds to collect the sign bits
    // together.
    // Since the widening shifts would be rather confusing to follow in little
    // endian, everything will be illustrated in big endian order instead. This
    // has a different result - the bits would actually be reversed on a big
    // endian machine.

    // Starting input (only half the elements are shown):
    // 89 ff 1d c0 00 10 99 33
    uint8x16_t input = vreinterpretq_u8_m128i(a);

    // Shift out everything but the sign bits with an unsigned shift right.
    //
    // Bytes of the vector::
    // 89 ff 1d c0 00 10 99 33
    // \  \  \  \  \  \  \  \    high_bits = (uint16x4_t)(input >> 7)
    //  |  |  |  |  |  |  |  |
    // 01 01 00 01 00 00 01 00
    //
    // Bits of first important lane(s):
    // 10001001 (89)
    // \______
    //        |
    // 00000001 (01)
    uint16x8_t high_bits = vreinterpretq_u16_u8(vshrq_n_u8(input, 7));

    // Merge the even lanes together with a 16-bit unsigned shift right + add.
    // 'xx' represents garbage data which will be ignored in the final result.
    // In the important bytes, the add functions like a binary OR.
    //
    // 01 01 00 01 00 00 01 00
    //  \_ |  \_ |  \_ |  \_ |   paired16 = (uint32x4_t)(input + (input >> 7))
    //    \|    \|    \|    \|
    // xx 03 xx 01 xx 00 xx 02
    //
    // 00000001 00000001 (01 01)
    //        \_______ |
    //                \|
    // xxxxxxxx xxxxxx11 (xx 03)
    uint32x4_t paired16 =
        vreinterpretq_u32_u16(vsraq_n_u16(high_bits, high_bits, 7));

    // Repeat with a wider 32-bit shift + add.
    // xx 03 xx 01 xx 00 xx 02
    //     \____ |     \____ |  paired32 = (uint64x1_t)(paired16 + (paired16 >>
    //     14))
    //          \|          \|
    // xx xx xx 0d xx xx xx 02
    //
    // 00000011 00000001 (03 01)
    //        \\_____ ||
    //         '----.\||
    // xxxxxxxx xxxx1101 (xx 0d)
    uint64x2_t paired32 =
        vreinterpretq_u64_u32(vsraq_n_u32(paired16, paired16, 14));

    // Last, an even wider 64-bit shift + add to get our result in the low 8 bit
    // lanes. xx xx xx 0d xx xx xx 02
    //            \_________ |   paired64 = (uint8x8_t)(paired32 + (paired32 >>
    //            28))
    //                      \|
    // xx xx xx xx xx xx xx d2
    //
    // 00001101 00000010 (0d 02)
    //     \   \___ |  |
    //      '---.  \|  |
    // xxxxxxxx 11010010 (xx d2)
    uint8x16_t paired64 =
        vreinterpretq_u8_u64(vsraq_n_u64(paired32, paired32, 28));

    // Extract the low 8 bits from each 64-bit lane with 2 8-bit extracts.
    // xx xx xx xx xx xx xx d2
    //                      ||  return paired64[0]
    //                      d2
    // Note: Little endian would return the correct value 4b (01001011) instead.
    return vgetq_lane_u8(paired64, 0) | ((int) vgetq_lane_u8(paired64, 8) << 8);
#endif
}

#if defined(__GNUC__) || defined(__clang__)
  #pragma pop_macro("ALIGN_STRUCT")
  #pragma pop_macro("FORCE_INLINE")
#endif

#if defined(__GNUC__)
  #pragma GCC pop_options
#endif

#endif
