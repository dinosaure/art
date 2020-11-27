#include <caml/mlvalues.h>

#include <stdint.h>

#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

#ifndef __unused
#define __unused(x) x __attribute__((unused))
#endif

#define __unit() value __unused(unit)

CAMLprim value
caml_is_big_endian(__unit ())
{
  return Val_bool (IS_BIG_ENDIAN) ;
}
