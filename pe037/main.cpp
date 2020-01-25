#include <algorithm>
#include <cmath>
#include <iomanip> // TODO
#include <iostream>
#include <numeric>
#include <vector>

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

namespace
{

// class FlagsGuard
// {
// public:
//     FlagsGuard()
//     : _flags(std::cout.flags())
//     {
//     }
//     ~FlagsGuard()
//     {
//         std::cout.flags(_flags);
//     }
// private:
//     const std::ios_base::fmtflags _flags;
// };

class TPrimes
{
public:
    using prime_t = int;

    /** Find `n' truncatable primes. */
    TPrimes(const size_t n);
    TPrimes(const TPrimes&) = delete;
    TPrimes(TPrimes&&) = delete;
    TPrimes& operator=(const TPrimes&) = delete;
    ~TPrimes() = default;
    const std::vector<prime_t>& get() const { return _truncatable; }

private:
    bool contains(const prime_t x) const
    {
        return std::binary_search(_primes.begin(), _primes.end(), x);
    }
    bool isTruncatable(const prime_t x) const;

    std::vector<prime_t> _primes;
    std::vector<prime_t> _truncatable;
};

TPrimes::TPrimes(const size_t n)
: _primes({2, 3, 5, 7, 11})
, _truncatable()
{
    for (prime_t x = 13; _truncatable.size() < n; x += 2)
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
            if (isTruncatable(x))
            {
                _truncatable.push_back(x);
            }
        }
    }
}

bool TPrimes::isTruncatable(const prime_t x) const
{
    for (TPrimes::prime_t front = x / 10; front; front /= 10)
    {
        if (contains(front) == false)
            return false;
    }
    for (TPrimes::prime_t div = 10; (x % div) != x; div *= 10)
    {
        TPrimes::prime_t back = x % div;
        if (contains(back % div) == false)
            return false;
    }

    // OK, we got here:
    std::cout << "FOUND:  " << x << "\n";
    for (TPrimes::prime_t front = x / 10; front; front /= 10)
    {
        std::cout << " -\t" << front << "\n";
    }
    auto width = std::setw(ndigits(x));
    for (TPrimes::prime_t div = 10; (x % div) != x; div *= 10)
    {
        TPrimes::prime_t back = x % div;
        std::cout << " +\t" << width << back << "\n";
    }
    return true;
}

} // namespace

template<class T>
void printList(const T& xs)
{
    bool first = true;
    for (auto p : xs)
    {
        std::cout << (first ? "[" : ", ") << p;
        if (first)
        {
            first = false;
        }
    }
    std::cout << "]\n";
}

int main()
{
    TPrimes s(14);
    auto& ts = s.get();
    printList(ts);
    const TPrimes::prime_t ans = std::accumulate(ts.begin(), ts.end(), 0);
    std::cout << ans << "\n";
    return 0;
}
