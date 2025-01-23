#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static unsigned concat(unsigned a, unsigned b) {
  unsigned x = a;
  for (unsigned t = b; t; t /= 10) {
    x *= 10;
  }
  return x + b;
}

static const unsigned SPACE = 10000;
static const unsigned LIMIT = 31607; // sqrt(concat(SPACE, SPACE))
static const unsigned STACK = 5;

static bool check(unsigned target, uint64_t *sieve, unsigned stack[],
                  unsigned length) {
  for (unsigned i = 0; i < length; i++) {
    if (!is_prime(sieve, LIMIT, concat(target, stack[i])) ||
        !is_prime(sieve, LIMIT, concat(stack[i], target))) {
      return false;
    }
  }
  return true;
}

static void loop(unsigned space, uint64_t *sieve, unsigned stack[STACK],
                 unsigned index) {
  if (index >= STACK) {
    if (check(stack[index - 1], sieve, stack, STACK - 1)) {
      printf("stack:");
      unsigned ans = 0;
      for (unsigned s = 0; s < STACK; s++) {
        ans += stack[s];
        printf(" %u", stack[s]);
      }
      printf(", ans=%u\n", ans);
      exit(EXIT_SUCCESS);
    }
    return;
  }

  unsigned p = 2u;
  if (index > 0) {
    p = next_prime(sieve, LIMIT, stack[index - 1]);
  }
  while (p && p < space) {
    if (check(p, sieve, stack, index)) {
      stack[index] = p;
      loop(space, sieve, stack, index + 1);
    }
    p = next_prime(sieve, LIMIT, p);
  }
}

int main() {
  uint64_t *sieve = calloc(LIMIT / 64 + 1, sizeof(uint64_t));
  eratosthenes(sieve, LIMIT);

  unsigned stack[STACK];
  memset(stack, 0, STACK * sizeof(unsigned));

  loop(SPACE, sieve, stack, 0);

  free(sieve);
  return 0;
}
