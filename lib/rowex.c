#include <caml/mlvalues.h>

#if defined(__clang__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#define ml_ctz __builtin_ctz
#else
int
ml_ctz(intnat x)
{
  int n;
  intnat y;
#ifdef ARCH_SIXTYFOUR
  n = 63 ;
  y = x << 32 ;
  if (y != 0) { n = n - 32 ; x = y ; }
#else
  n = 31 ;
#endif

  y = x << 16 ; if (y != 0) { n = n - 16 ; x = y ; }
  y = x <<  8 ; if (y != 0) { n = n -  8 ; x = y ; }
  y = x <<  4 ; if (y != 0) { n = n -  4 ; x = y ; }
  y = x <<  2 ; if (y != 0) { n = n -  2 ; x = y ; }
  y = x <<  1 ; if (y != 0) { n = n -  1 ; }

  return (n) ;
}
#endif

CAMLprim value
caml_ctz(value n)
{
  return (Val_long (ml_ctz (Long_val (n))));
}

uint64_t
caml_uint64_of_uint(value v)
{
  return (Unsigned_long_val (v)) ;
}

uint32_t
caml_uint32_of_uint(value v)
{
  return (Unsigned_long_val (v)) ;
}

// TODO(dinosaure): SSE
// We should be able to replace it by a portable C code. At this layer,
// the atomicity is **not required**, it was on the charge of the OCaml
// side. I just don't have the time to re-translate this code.

#if defined(ART_SSE2)
#include <emmintrin.h>
#elif defined(ART_NEON)
#include "sse2neon.h"
#else
#if defined(__arm__) || defined(__aarch64__)
#error "Unsupported target. You must support NEON."
#else
#error "Unsupported target. You must support SSE2."
#endif
#endif

int
ml_n16_get_child(const uint16_t compact_count, const uint8_t k, const uint8_t keys[16])
{
  __m128i cmp = _mm_cmpeq_epi8(_mm_set1_epi8(k ^ 128), _mm_loadu_si128((const __m128i *) (keys))) ;
  unsigned bitfield = _mm_movemask_epi8(cmp) & ((1 << compact_count) - 1) ;
  return (bitfield) ;
}

CAMLprim value
caml_n16_get_child(value compact_count, value k, value keys)
{
  return (Val_long (ml_n16_get_child (Long_val (compact_count),
                                      Long_val (k),
                                      (const uint8_t *) String_val (keys)))) ;
}
