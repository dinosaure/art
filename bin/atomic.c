#include <caml/mlvalues.h>
#include <caml/bigarray.h>

/* XXX(dinosaure): does not exists into [ocaml-freestanding]. */
#include "byteswap.h"

#define PAGE_SIZE 4096

#if defined(HAS_STDATOMIC_H)
#include <stdatomic.h>
#elif defined(__GNUC__)
typedef enum memory_order {
  memory_order_relaxed = __ATOMIC_RELAXED,
  memory_order_seq_cst = __ATOMIC_SEQ_CST,
  memory_order_release = __ATOMIC_RELEASE
} memory_order;

#define atomic_load_explicit(x, m) \
  __atomic_load_n((x), (m))
#define atomic_fetch_add(x, n, m) \
  __atomic_fetch_add((x), (n), (m))
#define atomic_store_explicit(x, v, m) \
  __atomic_store_n((x), (v), (m))
#else
#error "C11 atomics are unavailable on this platform."
#endif

static const memory_order t[] = {
  memory_order_relaxed,
  memory_order_seq_cst,
  memory_order_release
};

/* XXX(dinosaure): assume non-aligned access to the memory! */

#define memory_uint8_off(src, off) \
  ((uint8_t *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

#define memory_uintnat_off(src, off) \
  ((uintnat *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

#define memory_uint16_off(src, off) \
  ((uint16_t *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

#define memory_uint32_off(src, off) \
  ((uint32_t *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

#define memory_uint64_off(src, off) \
  ((uint64_t *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

CAMLprim value
caml_atomic_get_uint8(value memory, value addr, value memory_order)
{
  uint8_t v = atomic_load_explicit(memory_uint8_off (memory, addr), t[Long_val (memory_order)]) ;
  return Val_long (v) ;
}

CAMLprim value
caml_atomic_set_uint8(value memory, value addr, value memory_order, value v)
{
  uint8_t x = Unsigned_long_val (v) ;
  atomic_store_explicit(memory_uint8_off (memory, addr), x, t[Long_val (memory_order)]) ;
  return Val_unit ;
}

/* XXX(dinosaure): About LE/BE, the usual layout is little-endian. It does not
   exist a portable and static way to see if we compile on a
   big-endian/little-endian computer. To check that, we run a little program
   (see [conf/endian.exe]) which tells to us the endianess of the computer.

   This code is usually compiled with no flags but can be compiled with
   [-DART_BIG_ENDIAN] to switch the code to a big-endian architecture.

   [ARCH_SIXTYFOUR] is given by caml to know if [intnat] is a:
   - [uint64_t] if [ARCH_SIXTYFOUR] is defined
   - [uint32_t] otherwise
 */

CAMLprim value
caml_atomic_get_leuintnat(value memory, value addr, value memory_order)
{
  uintnat v = atomic_load_explicit(memory_uintnat_off (memory, addr), t[Long_val (memory_order)]) ;
#if defined(ART_BIG_ENDIAN) && defined(__ARCH_SIXTYFOUR)
  v = __bswap_64 (v) ;
#elif defined(ART_BIG_ENDIAN)
  v = __bswap_32 (v) ;
#endif
  return Val_long (v) ;
}

#include <stdio.h>

CAMLprim value
caml_atomic_set_leuintnat(value memory, value addr, value memory_order, value v)
{
  uintnat x = Unsigned_long_val (v) ;
#if defined(ART_BIG_ENDIAN) && defined(ARCH_SIXTYFOUR)
  x = __bswap_64 (x);
#elif defined(ART_BIG_ENDIAN)
  x = __bswap_32 (x) ;
#endif
  atomic_store_explicit(memory_uintnat_off (memory, addr), x, t[Long_val (memory_order)]) ;
  return Val_unit ;
}

CAMLprim value
caml_atomic_get_leuint16(value memory, value addr, value memory_order)
{
  uint16_t v = atomic_load_explicit(memory_uint16_off (memory, addr), t[Long_val (memory_order)]) ;
#if defined(ART_BIG_ENDIAN)
  v = __bswap_16 (v) ;
#endif
  return Val_long (v) ;
}

CAMLprim value
caml_atomic_set_leuint16(value memory, value addr, value memory_order, value v)
{
  uint16_t x = Unsigned_long_val (v) ;
#if defined(ART_BIG_ENDIAN)
  x = __bswap_16 (x) ;
#endif
  atomic_store_explicit(memory_uint16_off (memory, addr), x, t[Long_val (memory_order)]) ;
  return Val_unit ;
}

CAMLprim value
caml_atomic_get_leuint31(value memory, value addr, value memory_order)
{
  uint32_t v = atomic_load_explicit(memory_uint32_off (memory, addr), t[Long_val (memory_order)]) ;
#if defined(ART_BIG_ENDIAN)
  v = __bswap_32 (v) ;
#endif
  return Val_long (v & 0x7fffffff) ;
}

CAMLprim value
caml_atomic_set_leuint31(value memory, value addr, value memory_order, value v)
{
  uint32_t x = Unsigned_long_val (v) ;
#if defined(ART_BIG_ENDIAN)
  x = __bswap_32 (x) ;
#endif
  atomic_store_explicit(memory_uint32_off (memory, addr), (x & 0x7fffffff), t[Long_val (memory_order)]) ;
  return Val_unit ;
}

uint64_t
caml_atomic_get_leuint64(value memory, value addr, value memory_order)
{
  uint64_t v = atomic_load_explicit(memory_uint64_off (memory, addr), t[Long_val (memory_order)]) ;
#if defined(__ORDER_BIG_ENDIAN)
  v = __bswap_64 (v) ;
#endif
  return (v) ;
}

CAMLprim value
caml_atomic_set_leuint64(value memory, value addr, value memory_order, uint64_t x)
{
#if defined(__ORDER_BIG_ENDIAN)
  x = __bswap_64 (x) ;
#endif
  atomic_store_explicit(memory_uint64_off (memory, addr), x, t[Long_val (memory_order)]) ;
  return Val_unit ;
}

#include <pmmintrin.h>

#define memory_uint128_off(src, off) \
  ((__m128i *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

CAMLprim value
caml_atomic_get_leuint128(value memory, value addr, value memory_order, value res)
{
  /* XXX(dinosaure): non-aligned operation. */
  __m128i v = _mm_loadu_si128((const __m128i *) memory_uint128_off (memory, addr)) ;

#if defined(__ORDER_BIG_ENDIAN)
  _mm_shuffle_epi8(v, _mm_set_epi32(0x0C0D0E0F, 0x08090A0B, 0x04050607, 0x00010203)) ;
#endif
  _mm_storeu_si128((__m128i *) Bytes_val (res), v) ;

  return Val_unit ;
}

CAMLprim value
caml_atomic_fetch_add_leuint16(value memory, value addr, value memory_order, value v)
{
#if defined(__ORDER_BIG_ENDIAN)
#error "atomic_fetch_add on big-endian is not supported."
#else
  atomic_fetch_add(memory_uint16_off (memory, addr), Unsigned_long_val (v), t[Long_val (memory_order)]) ;
#endif
  return Val_unit;
}

CAMLprim value
caml_atomic_fetch_add_leuintnat(value memory, value addr, value memory_order, value v)
{
#if defined(__ORDER_BIG_ENDIAN)
#error "atomic_fetch_add on big-endian is not supported."
#elif defined(ARCH_SIXTYFOUR)
  atomic_fetch_add(memory_uint64_off (memory, addr), Unsigned_long_val (v), t[Long_val (memory_order)]) ;
#else
  atomic_fetch_add(memory_uint32_off (memory, addr), Unsigned_long_val (v), t[Long_val (memory_order)]) ;
#endif
  return Val_unit ;
}

#ifndef __unused
#define __unused(x) x __attribute__((unused))
#endif

#define __unit() value __unused(unit)

CAMLprim value
caml_pause_intrinsic(__unit ())
{
  _mm_pause() ; /* XXX(dinosaure): platform-dependent, see plasma_spin.h for a portable pause. */
  return Val_unit ;
}

CAMLprim value
caml_atomic_compare_exchange_strong_leuintnat(value memory, value addr, value a, value b, value memory_order)
{
  uintnat *v = memory_uintnat_off (memory, addr) ;
  uintnat va = Unsigned_long_val (a) ;
  int m_order = t[Long_val (memory_order)] ;

  if (__atomic_compare_exchange_n(v, &va, Unsigned_long_val (b), 0, m_order, m_order))
    return Val_true ;
  else
    return Val_false ;
}

CAMLprim value
caml_atomic_compare_exchange_weak_leuintnat(value memory, value addr, value a, value b, value memory_order)
{
  uintnat *v = memory_uintnat_off (memory, addr) ;
  uintnat va = Unsigned_long_val (a) ;
  int m_order = t[Long_val (memory_order)] ;

  if (__atomic_compare_exchange_n(v, &va, Unsigned_long_val (b), 1, m_order, m_order))
    return Val_true ;
  else
    return Val_false ;
}

CAMLprim value
caml_get_beint31(value memory, value addr)
{
  return Val_long(memory_uint32_off (memory, addr)[0] & 0x7fffffff) ;
}

CAMLprim value
caml_get_beintnat(value memory, value addr)
{
#if defined(ARCH_SIXTYFOUR)
  return Val_long(memory_uint64_off (memory, addr)[0]) ;
#else
  return Val_long(memory_uint32_off (memory, addr)[0]) ;
#endif
}

#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value
caml_get_c_string(value memory, value addr)
{
  CAMLparam2(memory, addr) ;
  CAMLlocal1(res) ;

  uint8_t *v = memory_uint8_off (memory, addr) ;
  res = caml_copy_string(v) ;

  CAMLreturn(res) ;
}

#include <unistd.h>

CAMLprim value
caml_get_page_size(__unit ())
{
  intnat v = PAGE_SIZE ;
  return Val_long (v) ;
}
