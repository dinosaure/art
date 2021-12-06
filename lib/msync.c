#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <sys/mman.h>
#include <assert.h>

CAMLprim value
part_msync(value ba) {
  struct caml_ba_array *b = Caml_ba_array_val (ba) ;

  if (b->flags & CAML_BA_MAPPED_FILE)
    assert(msync(b->data, b->dim[0], MS_SYNC | MS_INVALIDATE) == 0);

  return Val_unit;
}
