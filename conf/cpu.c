#include <string.h>

#define EAX_IDX 0
#define EBX_IDX 1
#define ECX_IDX 2
#define EDX_IDX 4

#if defined(__x86_64__) || defined(__amd64__)
#include <cpuid.h>

static inline void
cpuid(unsigned func, unsigned subfunc, unsigned cpuinfo[4])
{
  __cpuid_count(func, subfunc, cpuinfo[EAX_IDX],
		               cpuinfo[EBX_IDX],
			       cpuinfo[ECX_IDX],
			       cpuinfo[EDX_IDX]);
}
#elif (_M_X64) || defined(_M_AMD64)
#include <intrin.h>

static inline void
cpuid(unsigned func, unsigned subfunc, unsigned cpuinfo[4])
{
  __cpuidex(cpuinfo, func, subfunc)
}
#else
// XXX(dinosaure): [aarch64] branch
static inline void
cpuid(unsigned func, unsigned subfunc, unsigned cpuinfo[4])
{ }
#endif

#ifndef bit_CLFLUSH
#define bit_CLFLUSH (1 << 19)
#endif

#ifndef bit_CLFLUSHOPT
#define bit_CLFLUSHOPT (1 << 23)
#endif

#ifndef bit_CLWB
#define bit_CLWB (1 << 24)
#endif

static int
is_cpu_feature_present(unsigned func, unsigned reg, unsigned bit)
{
  unsigned cpuinfo[4] = { 0 };

  cpuid(0x0, 0x0, cpuinfo);

  if (cpuinfo[EAX_IDX] < func)
    return 0;

  cpuid(func, 0x0, cpuinfo);

  return (cpuinfo[reg] & bit) != 0;
}

#include <caml/mlvalues.h>

#ifndef __unused
#define __unused(x) x __attribute__((unused))
#endif

#define __unit() value __unused(unit)

CAMLprim value
is_cpu_clflush_present(__unit ())
{
  int ret = is_cpu_feature_present(0x1, EDX_IDX, bit_CLFLUSH);
  return Val_bool (ret);
}

CAMLprim value
is_cpu_clflushopt_present(__unit ())
{
  int ret = is_cpu_feature_present(0x7, EBX_IDX, bit_CLFLUSHOPT);
  return Val_bool (ret);
}

CAMLprim value
is_cpu_clwb_present(__unit ())
{
  int ret = is_cpu_feature_present(0x7, EBX_IDX, bit_CLWB);
  return Val_bool (ret);
}
