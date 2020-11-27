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
