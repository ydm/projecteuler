#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef likely
#define likely(x) __builtin_expect (!!(x), 1)
#endif

#ifndef unlikely
#define unlikely(x) __builtin_expect (!!(x), 0)
#endif

// +--------+
// | Primes |
// +--------+

static inline bool
is_prime (uint64_t *sieve, unsigned limit, unsigned x)
{
  if (unlikely (x <= 1 || x % 2 == 0))
    {
      return x == 2;
    }
  if (x <= limit)
    {
      uint64_t i = (x - 3) / 2;
      return (sieve[i / 64] & 1ULL << i % 64) != 0;
    }
  unsigned root = (unsigned)sqrt (x);
  if (unlikely (limit < root))
    {
      fprintf (stderr, "error: is_prime: too large, x=%u root=%u", x, root);
      exit (EXIT_FAILURE);
    }
  for (unsigned p = 3; p <= root; p += 2)
    {
      uint64_t i = (p - 3) / 2;
      if ((sieve[i / 64] & 1ULL << i % 64) != 0 && x % p == 0)
        {
          return false;
        }
    }
  return true;
}

static inline void
set_prime (uint64_t *sieve, unsigned limit, unsigned x)
{
  if (unlikely (x >= limit))
    {
      fprintf (stderr, "error: set_prime: %u >= LIMIT", x);
      exit (EXIT_FAILURE);
    }
  if (likely (x > 2))
    {
      uint64_t i = (x - 3) / 2;
      sieve[i / 64] |= 1ULL << i % 64;
    }
}

static unsigned
next_prime (uint64_t *sieve, unsigned limit, unsigned prime)
{
  if (unlikely (prime == 2))
    {
      return 3;
    }
  for (uint64_t x = prime + 2; x < limit; x += 2)
    {
      if (is_prime (sieve, limit, x))
        {
          return x;
        }
    }
  return 0;
}

/**
 * Apply the sieve up to the limit, inclusive.
 */
static void
eratosthenes (uint64_t *sieve, unsigned limit)
{
  sieve[0] = 0x40b6894d325a65b7ULL;
  for (unsigned candidate = 131; candidate <= limit; candidate += 2)
    {
      bool divisible = false;
      unsigned root = (unsigned)sqrt (candidate);
      for (unsigned divisor = 3; divisor < root; divisor++)
        {
          if (is_prime (sieve, limit, divisor) && candidate % divisor == 0)
            {
              divisible = true;
              break;
            }
        }
      if (!divisible)
        {
          set_prime (sieve, limit, candidate);
        }
    }
}

// +------+
// | Main |
// +------+

unsigned
concat (unsigned a, unsigned b)
{
  static uint8_t digits[20];
  memset (digits, 0, 20);
  int i;
  for (i = 0; b; i++)
    {
      digits[i] = b % 10;
      b /= 10;
    }
  unsigned ans = a;
  for (int j = i - 1; j >= 0; j--)
    {
      ans *= 10;
      ans += digits[j];
    }
  return ans;
}

static const unsigned LIMIT = 0x8000; // sqrt(1000010000)
static const unsigned STACK = 5;

static bool
check (unsigned target, uint64_t *sieve, unsigned stack[], unsigned length)
{
  for (unsigned i = 0; i < length; i++)
    {
      if (!is_prime (sieve, LIMIT, concat (target, stack[i]))
          || !is_prime (sieve, LIMIT, concat (stack[i], target)))
        {
          return false;
        }
    }
  return true;
}

static int kopele = 0;

static void
loop (uint64_t *sieve, unsigned upto, unsigned stack[STACK], unsigned index)
{
  kopele++;
  if (index >= STACK)
    {
      if (check (stack[index - 1], sieve, stack, STACK - 1))
        {
          printf ("loop:");
          for (unsigned s = 0; s < STACK; s++)
            {
              printf (" %u", stack[s]);
            }
          printf (", kur=%d\n", kopele);
          exit (EXIT_SUCCESS);
        }
      return;
    }

  unsigned p = 2u;
  if (index > 0)
    {
      p = next_prime (sieve, LIMIT, stack[index - 1]);
    }
  while (p && p < upto)
    {
      if (check (p, sieve, stack, index))
        {
          stack[index] = p;
          loop (sieve, upto, stack, index + 1);
        }
      p = next_prime(sieve, upto, p);
    }

  // for (unsigned a = 2; a && a < upto; a = next_prime (sieve, LIMIT, a))
  //   {
  //     if (!is_prime (sieve, LIMIT, a))
  //       {
  //         continue;
  //       }
  //     stack[0] = a;
  //     for (unsigned b = (a == 2) ? 3 : a + 2; b < upto; b += 2)
  //       {
  //         if (!is_prime (sieve, LIMIT, b))
  //           {
  //             continue;
  //           }
  //         if (!check (b, sieve, stack, 1))
  //           {
  //             continue;
  //           }
  //         stack[1] = b;
  //         for (unsigned c = b + 2; c < upto; c += 2)
  //           {
  //             if (!is_prime (sieve, LIMIT, c))
  //               {
  //                 continue;
  //               }
  //             if (!check (c, sieve, stack, 2))
  //               {
  //                 continue;
  //               }
  //             stack[2] = c;
  //             for (unsigned d = c + 2; d < upto; d += 2)
  //               {
  //                 if (!is_prime (sieve, LIMIT, d))
  //                   {
  //                     continue;
  //                   }
  //                 if (!check (d, sieve, stack, 3))
  //                   {
  //                     continue;
  //                   }
  //                 stack[3] = d;
  //                 for (unsigned e = d + 2; e < upto; e += 2)
  //                   {
  //                     if (!is_prime (sieve, LIMIT, e))
  //                       {
  //                         continue;
  //                       }
  //                     if (!check (e, sieve, stack, 4))
  //                       {
  //                         continue;
  //                       }
  //                     stack[4] = e;
  //                     printf ("DA! %u %u %u %u %u\n", a, b, c, d, e);
  //                     exit (EXIT_SUCCESS);
  //                   }
  //               }
  //           }
  //       }
  //   }
}

int
main ()
{
  uint64_t *sieve = calloc (LIMIT / 0x80 + 1, sizeof (uint64_t));
  eratosthenes (sieve, LIMIT);

  unsigned stack[STACK];
  memset (stack, 0, STACK * sizeof (unsigned));

  printf ("Looping ...\n");
  loop (sieve, 10000, stack, 0);

  free (sieve);
  return 0;
}
