// http://www-unix.mcs.anl.gov/~kazutomo/rdtsc.html

#ifndef IRK_RDTSC_H
#define IRK_RDTSC_H

#if defined(__llvm__)
uint64_t rdtsc(void) __asm__ ("llvm.readcyclecounter");

#elif defined(__i386__)

static __inline__ unsigned long long rdtsc(void)
{
  unsigned long long int x;
     __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
     return x;
}
#elif defined(__x86_64__)

static __inline__ unsigned long long rdtsc(void)
{
  unsigned hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 );
}

#elif defined(__ppc64__)

static __inline__ unsigned long long rdtsc(void)
{
  unsigned long long int result=0;
  unsigned long int upper, lower,tmp;
  __asm__ volatile(
                "0:                \n"
                "\tmftbu   %0      \n"
                "\tmftb    %1      \n"
                "\tmftbu   %2      \n"
                "\tcmpw    %2,%0   \n"
                "\tbne     0b      \n"
                : "=r"(upper),"=r"(lower),"=r"(tmp)
                );
  result = upper;
  result = result<<32;
  result = result|lower;

  return(result);
}

#elif defined(__ppc__)

static __inline__ unsigned long long rdtsc(void)
{
  unsigned long long int result=0;
  unsigned long int upper, lower,tmp;
  __asm__ volatile(
                "0:                \n"
                "\tmftbu   %0      \n"
                "\tmftb    %1      \n"
                "\tmftbu   %2      \n"
                "\tcmpw    %2,%0   \n"
                "\tbne     0b      \n"
                : "=r"(upper),"=r"(lower),"=r"(tmp)
                );
  result = upper;
  result = result<<32;
  result = result|lower;

  return(result);
}

#elif defined(__riscv) && (__riscv_xlen == 64)
static __inline__ unsigned long rdtsc(void)
{
  unsigned long result;
  __asm__ volatile ("rdcycle %0" : "=r" (result));
  return result;
}


#else
error ("no rdtsc() on this platform?");
#endif

#endif // IRK_RDTSC_H
