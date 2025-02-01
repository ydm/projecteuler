#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// +--------+
// | Primes |
// +--------+

typedef uint32_t prime_t;
#define xPRIpr PRIu32

static inline uint64_t *new_sieve(prime_t limit) {
  return calloc(limit / 64 + 1, sizeof(uint64_t));
}

static inline void free_sieve(uint64_t *sieve) { free(sieve); }

static inline uint64_t *segment(uint64_t *sieve, uint64_t index) {
  // #pragma clang diagnostic push
  // #pragma clang diagnostic ignored "-Wunsafe-buffer-usage"
  return &sieve[index / 64];
  // #pragma clang diagnostic pop
}

static inline bool is_prime(uint64_t *sieve, prime_t limit, prime_t x) {
  if (x <= 1 || x % 2 == 0) {
    return x == 2;
  }
  if (x <= limit) {
    uint64_t *seg = segment(sieve, x);
    return (*seg & 1ULL << x % 64) != 0;
  }
  double droot = sqrt((double)x);
  prime_t root = (uint64_t)droot;
  if (limit < root) {
    fprintf(stderr, "error: is_prime: too large, x=%" xPRIpr " root=%" xPRIpr,
            x, root);
    exit(EXIT_FAILURE);
  }
  for (prime_t p = 3; p <= root; p += 2) {
    uint64_t *seg = segment(sieve, p);
    if ((*seg & 1ULL << p % 64) != 0 && x % p == 0) {
      return false;
    }
  }
  return true;
}

static inline void set_prime(uint64_t *sieve, prime_t limit, prime_t x) {
  if (x > limit) {
    fprintf(stderr, "error: set_prime: %" xPRIpr " >= LIMIT", x);
    exit(EXIT_FAILURE);
  }
  if (x > 2) {
    uint64_t *seg = segment(sieve, x);
    *seg |= 1ULL << x % 64;
  }
}

static prime_t next_prime(uint64_t *sieve, prime_t limit, prime_t prime) {
  if (prime == 2) {
    return 3;
  }
  for (uint64_t x = prime + 2; x <= limit; x += 2) {
    if (is_prime(sieve, limit, x)) {
      return x;
    }
  }
  return 0;
}

/**
 * Apply the sieve up to the limit, inclusive.
 */
static void eratosthenes(uint64_t *sieve, prime_t limit) {
  sieve[0] = 0x28208a20a08a28acULL;
  for (prime_t candidate = 67; candidate <= limit; candidate += 2) {
    bool divisible = false;
    double droot = sqrt((double)candidate);
    prime_t root = (prime_t)(droot);
    for (prime_t divisor = 3; divisor <= root; divisor += 2) {
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

void factorize(uint64_t *sieve, int *factors, prime_t limit, prime_t x,
               int sign) {
  // if (x % 100000 == 0) {
  //   printf("%u\n", x);
  // }
  // printf("%u:\n", x);
  for (prime_t p = 2; x > 1 && p; p = next_prime(sieve, limit, p)) {
    if (is_prime(sieve, limit, x)) {
      // printf("  x=1 [1]\n");
      factors[x] += sign;
      return;
    }
    while (1) {
      if (x % p == 0) {
        x /= p;
        factors[p] += sign;
        //printf("  p=%u x=%u \n", p, x);
      } else {
        break;
      }
    }
  }
  // printf("  x=%u [0]\n", x);
}

uint64_t sum(uint64_t *sieve, int *factors, prime_t limit) {
  uint64_t s = 0;

  size_t i = 0;
  for (prime_t p = 2; p; p = next_prime(sieve, limit, p)) {
    if (factors[p] < 0) {
      printf("ops: i=%zu p=%u f=%d\n", i, p, factors[i]);
      exit(EXIT_FAILURE);
    }
    s += factors[p] * p;
  }

  return s;
}

#define xunused __attribute__((unused))

int main(int argc xunused, char **argv xunused) {
  const prime_t LIMIT = 20000000;
  // const size_t NUM_PRIMES = 664579; // number of primes <= 10m

  uint64_t *sieve = new_sieve(LIMIT);
  int *factors = calloc(LIMIT, sizeof(int));

  eratosthenes(sieve, LIMIT);

  // factorize(sieve, factors, LIMIT, 15061227, 1);
  // factorize(sieve, factors, LIMIT, 15000017, 1);
  // printf("sum: %lu\n", sum(sieve, factors, LIMIT));
  // if (1==1) {return 0;}

  for (unsigned x = 15000001; x <= 20000000; x++) {
    factorize(sieve, factors, LIMIT, x, 1);
  }
  for (unsigned x = 2; x <= 5000000; x++) {
    factorize(sieve, factors, LIMIT, x, -1);
  }
  printf("sum: %lu\n", sum(sieve, factors, LIMIT));

  free(factors);
  free_sieve(sieve);
  return 0;
}
