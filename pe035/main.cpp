#include <algorithm>
#include <cmath>
#include <cstdio>
#include <functional>
#include <vector>

namespace
{

class Primes
{
public:
    using prime_t = int;

    Primes(const prime_t n);
    Primes(const Primes&) = delete;
    Primes(Primes&&) = delete;
    Primes& operator=(const Primes&) = delete;
    ~Primes() = default;

    const std::vector<prime_t>& get() const
    {
        return _primes;
    }
    bool contains(const prime_t x) const
    {
        return std::binary_search(_primes.begin(), _primes.end(), x);
    }

private:
    std::vector<prime_t> _primes;
};

Primes::Primes(const prime_t n)
: _primes({2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47})
{
    for (prime_t x = 53; x < n; x += 2)
    {
        const prime_t top = static_cast<prime_t>(std::sqrt(x));
        bool isPrime = true;
        for (const prime_t p : _primes)
        {
            if (p >= top)
            {
                break;
            }
            if (x % p == 0)
            {
                isPrime = false;
                break;
            }
        }
        if (isPrime)
        {
            _primes.push_back(x);
        }
    }
}

} // namespace

/** Return the number of digits of x. */
template<typename I>
static I ndigits(I x)
{
    I n = 0;
    do
    {
        n++;
        x /= 10;
    } while (x);
    return n;
}

/** Rotate the digits of x by putting the last one in the front. */
template<typename I>
static I rotate(const I x)
{
    const I n = ndigits(x) - 1;
    const I last = x % 10;
    const I mult = static_cast<I>(std::pow(10, n));
    return (last * mult) + (x / 10);
}

static bool isCircular(const Primes& s, const Primes::prime_t x)
{
    Primes::prime_t y = x;
    do
    {
        if (s.contains(y) == false)
        {
            return false;
        }
        y = rotate(y);
    } while (y != x);
    return true;
}

static long countCircularPrimes(const Primes& s)
{
    using namespace std;
    auto& primes = s.get();
    return count_if(
        primes.begin(), 
        primes.end(),
        bind(&isCircular, cref(s), placeholders::_1)
    );
}

int main()
{
    Primes s(1000000);
    std::printf("%ld\n", countCircularPrimes(s));
    return 0;
}
