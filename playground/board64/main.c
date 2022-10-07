// gcc -Wall -Wextra -O3 main.c && time ./a.out 

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

static uint64_t
f (uint64_t x)
{
  const uint64_t one = 1;

  uint64_t ans = 0;
  for (uint64_t i = 0; i < 64; i++)
    {
      const uint64_t mask = one << i;
      if (x & mask)
        {
          ans += i;
        }
    }
  return ans % 64;
}

static uint64_t
flip (uint64_t board, uint64_t index)
{
  const uint64_t one = 1;

  assert (index < 64);
  return board ^ (one << index);
}

static uint64_t
populate (uint64_t board)
{
  const uint64_t one = 1;

  uint64_t ans = 0;

  for (uint64_t i = 0; i < 64; i++)
    {
      const uint64_t copy = flip (board, i);
      ans |= one << f (copy);
    }

  return ans;
}

int main ()
{
  for (uint64_t board = 0; board < UINT64_MAX; board++)
    {
      const uint64_t ans = populate (board);
      if (ans != UINT64_MAX)
        {
          printf ("board=%" PRIu64 ", ans=%" PRIu64 "\n", board, ans);
        }
      assert (ans == UINT64_MAX);
    }

  return 0;
}
