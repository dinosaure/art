#include <caml/mlvalues.h>

#include <unistd.h>

#ifndef __unused
#define __unused(x) x __attribute__((unused))
#endif

#define __unit() value __unused(unit)

CAMLprim value
caml_getpagesize(__unit ())
{
  intnat v = getpagesize();
  return Val_long (v) ;
}
