#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// +--------+
// | Primes |
// +--------+

static inline bool is_prime(uint64_t *sieve, unsigned limit, unsigned x) {
  if (x <= 1 || x % 2 == 0) {
    return x == 2;
  }
  if (x <= limit) {
    return (sieve[x / 64] & 1ULL << x % 64) != 0;
  }
  unsigned root = (unsigned)sqrt(x);
  if (limit < root) {
    fprintf(stderr, "error: is_prime: too large, x=%u root=%u", x, root);
    exit(EXIT_FAILURE);
  }
  for (unsigned p = 3; p <= root; p += 2) {
    if ((sieve[p / 64] & 1ULL << p % 64) != 0 && x % p == 0) {
      return false;
    }
  }
  return true;
}

static inline void set_prime(uint64_t *sieve, unsigned limit, unsigned x) {
  if (x > limit) {
    fprintf(stderr, "error: set_prime: %u >= LIMIT", x);
    exit(EXIT_FAILURE);
  }
  if (x > 2) {
    sieve[x / 64] |= 1ULL << x % 64;
  }
}

static unsigned next_prime(uint64_t *sieve, unsigned limit, unsigned prime) {
  if (prime == 2) {
    return 3;
  }
  for (uint64_t x = prime + 2; x < limit; x += 2) {
    if (is_prime(sieve, limit, x)) {
      return x;
    }
  }
  return 0;
}

/**
 * Apply the sieve up to the limit, inclusive.
 */
static void eratosthenes(uint64_t *sieve, unsigned limit) {
  sieve[0] = 0x28208a20a08a28acULL;
  for (unsigned candidate = 67; candidate <= limit; candidate += 2) {
    bool divisible = false;
    unsigned root = (unsigned)sqrt(candidate);
    for (unsigned divisor = 3; divisor <= root; divisor += 2) {
      if (is_prime(sieve, limit, divisor) && candidate % divisor == 0) {
        divisible = true;
        break;
      }
    }
    if (!divisible) {
      set_prime(sieve, limit, candidate);
    }
  }
}

// +------+
// | Main |
// +------+

const uint64_t SPACE = 1ULL << 50;
const uint64_t LIMIT = 1ULL << 25;

uint64_t squared_product(uint64_t xs[], unsigned length) {
  uint64_t ans = 1;
  for (unsigned i = 0; i < length; i++) {
    ans *= xs[i] * xs[i];
  }
  return ans;
}

// void comb(uint64_t stack[], uint64_t length, /* primes stack */
//           uint64_t sieve[],                  /* sieve */
//           uint64_t index                     /* iteration index */
// ) {
//   if (index >= length) {
//     uint64_t x = squared_product(stack, 3);
//     uint64_t y = squared_product(stack + 3, 2);
//     printf("%lu %lu %lu %lu %lu i=%lu j=%lu\n", stack[0], stack[1], stack[2],
//            stack[3], stack[4], x, y);
//     for (uint64_t i = 1; i <= 24; i++) {
//       for (uint64_t j = 1; j <= 24; j++) {
//         if (x * i == y * j) {
//           printf("YES\n");
//           exit(EXIT_SUCCESS);
//         }
//       }
//     }
//     return;
//   }
// 
//   unsigned p = 2u;
//   if (index > 0) {
//     p = next_prime(sieve, LIMIT, stack[index - 1]);
//   }
//   while (p && p < SPACE) {
//     stack[index] = p;
//     comb(stack, length, sieve, limit, index + 1);
// 
//     p = next_prime(sieve, LIMIT, p);
//   }
// }
// 
// uint64_t accumulate(uint64_t stack[], uint64_t length, /* primes stack */
//                     uint64_t sieve[], uint64_t limit,  /* sieve and limit */
//                     uint64_t n,                        /* */
//                     uint64_t index, uint64_t memo      /* loop state */
// ) {
//   if (index >= length) {
//     // printf("[0]=%lu [1]=%lu product=%lu, n/p=%lu\n", stack[0], stack[1],
//     // squared_product(stack, length), n/squared_product(stack, length));
//     uint64_t ans = memo + n / squared_product(stack, length);
//     // printf("\t\tANS=%lu\n", ans);
//     return ans;
//   }
// 
//   uint64_t init = 2u;
//   if (index > 0) {
//     init = next_prime(sieve, limit, stack[index - 1]);
//   }
// 
//   uint64_t sum = 0;
//   uint64_t boundary = (uint64_t)sqrt((double)n / squared_product(stack, index));
//   // printf("START: [%lu] stack=[%lu %lu %lu %lu %lu] product=%lu boundary=%lu
//   // init=%lu next=%lu\n",
//   //        index, stack[0], stack[1], 0L, 0L, 0L, squared_product(stack,
//   //        index), boundary, init, next_prime(sieve, limit, init));
//   uint64_t k = 0;
//   for (uint64_t p = init; p && p <= boundary; p = next_prime(sieve, limit, p)) {
//     k++;
//     if (false && index == 0 && k % 10000 == 0) {
//       printf("[%lu -> %lu] stack=[%lu %lu %lu %lu %lu] product=%lu "
//              "boundary=%lu p=%lu\n",
//              index, k++, stack[0], stack[1], 0L, 0L, 0L,
//              squared_product(stack, index), boundary, p);
//     }
// 
//     stack[index] = p;
//     sum += accumulate(stack, length, sieve, limit, n, index + 1, 0);
//   }
//   // stack[index] = 0;
//   return sum;
// }

// bool f(uint64_t *sieve, uint64_t x, uint64_t limit) {
//   if (x % 4 == 0) {
//     return false;
//   }
//   for (uint64_t p = 3; p < limit; p += 2) {
//     if (is_prime(sieve, limit, p)) {
//       if (x % (p * p) == 0) {
//         return false;
//       }
//     }
//   }
//   return true;
// }

int main() {
  uint64_t *sieve = calloc(LIMIT / 64 + 1, sizeof(uint64_t));
  eratosthenes(sieve, LIMIT);
  printf("done sieving\n");

  uint64_t *stack = calloc(16, sizeof(uint64_t));
  // comb(stack, 5, sieve, 4096, 0);

  uint64_t accum = 0;
  for (uint64_t i = 0; i < 16; i++) {
    accum += accumulate(stack, i, sieve, LIMIT, N, 0, 0);
  }
  printf("accum=%" PRIu64 "\n", accum);

  uint64_t ans = N - N / 4; // squares of 2
  for (uint64_t p = 3; p < LIMIT; p += 2) {
    if (is_prime(sieve, LIMIT, p)) {
      ans -= N / p / p;
    }
  }
  printf("ans=%" PRIu64 "\n", ans);
  printf("%" PRIu64, ans + accum);

  free(stack);
  free(sieve);
  return 0;
}
