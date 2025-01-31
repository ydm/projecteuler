#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// +--------+
// | Primes |
// +--------+

typedef uint64_t prime_t;
#define xPRIpr PRIu64

static inline bool is_prime(uint64_t *sieve, prime_t limit, prime_t x) {
  if (x <= 1 || x % 2 == 0) {
    return x == 2;
  }
  if (x <= limit) {
    return (sieve[x / 64] & 1ULL << x % 64) != 0;
  }
  prime_t root = (prime_t)sqrt(x);
  if (limit < root) {
    fprintf(stderr, "error: is_prime: too large, x=%" xPRIpr " root=%" xPRIpr,
            x, root);
    exit(EXIT_FAILURE);
  }
  for (prime_t p = 3; p <= root; p += 2) {
    if ((sieve[p / 64] & 1ULL << p % 64) != 0 && x % p == 0) {
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
    sieve[x / 64] |= 1ULL << x % 64;
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
    prime_t root = (prime_t)sqrt(candidate);
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

const unsigned STACK = 1024;

const uint64_t SPACE = 1ULL << 50;
const unsigned LIMIT = 1ULL << 25; // sqrt(SPACE)
// const uint64_t SPACE = 10000000;
// const prime_t LIMIT = 10000;

static inline bool mul(uint64_t a, uint64_t b, uint64_t *result) {
  if (a > UINT64_MAX / b) {
    return false;
  }

  *result = a * b;
  return true;
}

static int64_t accumulate(uint64_t sieve[], prime_t sqprod, prime_t stack[],
                          unsigned index) {
  if (index >= STACK) {
    fprintf(stderr, "stack too deep");
    exit(EXIT_FAILURE);
  }

  int64_t ans = 0;

  // Initial `p` and `sq`.
  prime_t p = 2u;
  if (index > 0) {
    p = next_prime(sieve, LIMIT, stack[index - 1]);
  }
  prime_t sq = p * p;

  // Loop over primes.
  while (p) {
    // This might result in an overflow.
    if (!mul(sqprod, sq, &sqprod)) {
      break;
    }

    // Break if the product is larger than the search space.
    if (SPACE < sqprod) {
      break;
    }

    // Loop further.
    stack[index] = p;
    int64_t x = (int64_t)(SPACE / sqprod) * (index % 2 ? 1 : -1);
    // printf("[%u] p=%lu product=%lu x=%ld\n", index, p, product, x);
    ans += x + accumulate(sieve, sqprod, stack, index + 1);

    // Prepare `sqprod` for the next prime.
    sqprod /= sq;
    p = next_prime(sieve, LIMIT, p);
    sq = p * p;
  }

  return ans;
}

static void naive() __attribute__((unused));

static void naive() {
  uint64_t *sieve = calloc(LIMIT / 64 + 1, sizeof(uint64_t));
  eratosthenes(sieve, LIMIT);

  uint64_t ans = 0;
  for (uint64_t i = 1; i <= SPACE; i++) {
    bool squarefree = true;
    for (prime_t p = 2; p && p <= LIMIT; p = next_prime(sieve, LIMIT, p)) {
      uint64_t d = (uint64_t)(p * p);
      if (i % d == 0) {
        squarefree = false;
        break;
      }
    }
    if (squarefree) {
      // printf("%" PRIu64 " ", i);
      ans += 1;
    }
  }
  printf("naive: %" PRIu64 "\n", ans);

  free(sieve);
}

int main() {
  uint64_t *sieve = calloc(LIMIT / 64 + 1, sizeof(uint64_t));
  eratosthenes(sieve, LIMIT);

  printf("accumulating ...\n");
  prime_t *stack = calloc(STACK, sizeof(int64_t));
  printf("ans: %" PRIu64 "\n", SPACE + accumulate(sieve, 1, stack, 0));
  // naive();

  free(stack);
  free(sieve);
  return 0;
}
