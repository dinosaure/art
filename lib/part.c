#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>

#include <unistd.h> // pwrite
#include <errno.h> // errno
#include <string.h> // memove
#include <caml/threads.h> // caml_release_runtime_system
#include <caml/unixsupport.h> // uerror

#define memory_uint8_off(src, off) \
  ((uint8_t *) ((uint8_t *) Caml_ba_data_val (src) + Unsigned_long_val (off)))

#define IO_BUFFER_SIZE 65536

CAMLprim value
caml_pwrite(value v_fd, value v_buf, value v_ofs, value v_len, value v_cur)
{
  int ret, n, written;
  off_t ofs;
  size_t len;
  char local[IO_BUFFER_SIZE];

  Begin_root (v_buf);
  ofs = Long_val(v_ofs);
  len = Long_val(v_len);
  written = 0;

  while (len > 0) {
    n = (len > IO_BUFFER_SIZE) ? IO_BUFFER_SIZE : len ;
    memmove(local, &Byte(v_buf, ofs), n);
    caml_release_runtime_system();
    ret = pwrite(Int_val (v_fd), local, len, Long_val (v_cur) + written) ;
    caml_acquire_runtime_system();
    if (ret == -1) {
      if ((errno == EAGAIN || errno == EWOULDBLOCK) && written > 0) break ;
      uerror("pwrite", Nothing);
    }
    written += ret;
    ofs += ret;
    len -= ret;
  }
  End_roots ();
  return Val_unit;
}

#include <sys/mman.h>

static const int msync_flags[] = {
  MS_ASYNC,
  MS_SYNC,
  MS_INVALIDATE
};

CAMLprim value
caml_msync(value v_memory, value v_addr, value v_len, value v_flags)
{
  int ret;
  void *addr = memory_uint8_off (v_memory, v_addr) ;

  caml_release_runtime_system();
  ret = msync(addr, Long_val (v_len), msync_flags[Long_val (v_flags)]);
  if (ret == -1) uerror("msync", Nothing);
  caml_acquire_runtime_system();
  return Val_unit;
}
