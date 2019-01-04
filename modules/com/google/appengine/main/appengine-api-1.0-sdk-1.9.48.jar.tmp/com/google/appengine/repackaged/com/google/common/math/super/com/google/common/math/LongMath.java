/*
 * Copyright (C) 2011 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.google.common.math;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.math.MathPreconditions.checkNonNegative;
import static com.google.common.math.MathPreconditions.checkPositive;
import static com.google.common.math.MathPreconditions.checkRoundingUnnecessary;
import static java.lang.Math.min;
import static java.math.RoundingMode.HALF_EVEN;
import static java.math.RoundingMode.HALF_UP;

import com.google.common.annotations.Beta;
import com.google.common.annotations.GwtCompatible;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.primitives.UnsignedLongs;
import java.math.RoundingMode;

/**
 * A class for arithmetic on values of type {@code long}. Where possible, methods are defined and
 * named analogously to their {@code BigInteger} counterparts.
 *
 * <p>The implementations of many methods in this class are based on material from Henry S. Warren,
 * Jr.'s <i>Hacker's Delight</i>, (Addison Wesley, 2002).
 *
 * <p>Similar functionality for {@code int} and for {@link BigInteger} can be found in
 * {@link IntMath} and {@link BigIntegerMath} respectively. For other common operations on
 * {@code long} values, see {@link com.google.common.primitives.Longs}.
 *
 * @author Louis Wasserman
 * @since 11.0
 */
@GwtCompatible(emulated = true)
public final class LongMath {
  // NOTE: Whenever both tests are cheap and functional, it's faster to use &, | instead of &&, ||

  @VisibleForTesting static final long MAX_SIGNED_POWER_OF_TWO = 1L << (Long.SIZE - 2);

  /**
   * Returns the smallest power of two greater than or equal to {@code x}.  This is equivalent to
   * {@code checkedPow(2, log2(x, CEILING))}.
   *
   * @throws IllegalArgumentException if {@code x <= 0}
   * @throws ArithmeticException of the next-higher power of two is not representable as a
   *         {@code long}, i.e. when {@code x > 2^62}
   * @since 20.0
   */
  @Beta
  public static long ceilingPowerOfTwo(long x) {
    checkPositive("x", x);
    if (x > MAX_SIGNED_POWER_OF_TWO) {
      throw new ArithmeticException("ceilingPowerOfTwo(" + x + ") is not representable as a long");
    }
    return 1L << -Long.numberOfLeadingZeros(x - 1);
  }

  /**
   * Returns the largest power of two less than or equal to {@code x}.  This is equivalent to
   * {@code checkedPow(2, log2(x, FLOOR))}.
   *
   * @throws IllegalArgumentException if {@code x <= 0}
   * @since 20.0
   */
  @Beta
  public static long floorPowerOfTwo(long x) {
    checkPositive("x", x);

    // Long.highestOneBit was buggy on GWT.  We've fixed it, but I'm not certain when the fix will
    // be released.
    return 1L << ((Long.SIZE - 1) - Long.numberOfLeadingZeros(x));
  }

  /**
   * Returns {@code true} if {@code x} represents a power of two.
   *
   * <p>This differs from {@code Long.bitCount(x) == 1}, because
   * {@code Long.bitCount(Long.MIN_VALUE) == 1}, but {@link Long#MIN_VALUE} is not a power of two.
   */
  public static boolean isPowerOfTwo(long x) {
    return x > 0 & (x & (x - 1)) == 0;
  }

  /**
   * Returns 1 if {@code x < y} as unsigned longs, and 0 otherwise. Assumes that x - y fits into a
   * signed long. The implementation is branch-free, and benchmarks suggest it is measurably faster
   * than the straightforward ternary expression.
   */
  @VisibleForTesting
  static int lessThanBranchFree(long x, long y) {
    // Returns the sign bit of x - y.
    return (int) (~~(x - y) >>> (Long.SIZE - 1));
  }

  /**
   * Returns the base-2 logarithm of {@code x}, rounded according to the specified rounding mode.
   *
   * @throws IllegalArgumentException if {@code x <= 0}
   * @throws ArithmeticException if {@code mode} is {@link RoundingMode#UNNECESSARY} and {@code x}
   *     is not a power of two
   */
  @SuppressWarnings("fallthrough")
  // TODO(kevinb): remove after this warning is disabled globally
  public static int log2(long x, RoundingMode mode) {
    checkPositive("x", x);
    switch (mode) {
      case UNNECESSARY:
        checkRoundingUnnecessary(isPowerOfTwo(x));
        // fall through
      case DOWN:
      case FLOOR:
        return (Long.SIZE - 1) - Long.numberOfLeadingZeros(x);

      case UP:
      case CEILING:
        return Long.SIZE - Long.numberOfLeadingZeros(x - 1);

      case HALF_DOWN:
      case HALF_UP:
      case HALF_EVEN:
        // Since sqrt(2) is irrational, log2(x) - logFloor cannot be exactly 0.5
        int leadingZeros = Long.numberOfLeadingZeros(x);
        long cmp = MAX_POWER_OF_SQRT2_UNSIGNED >>> leadingZeros;
        // floor(2^(logFloor + 0.5))
        int logFloor = (Long.SIZE - 1) - leadingZeros;
        return logFloor + lessThanBranchFree(cmp, x);

      default:
        throw new AssertionError("impossible");
    }
  }

  /** The biggest half power of two that fits into an unsigned long */
  @VisibleForTesting static final long MAX_POWER_OF_SQRT2_UNSIGNED = 0xB504F333F9DE6484L;

  // maxLog10ForLeadingZeros[i] == floor(log10(2^(Long.SIZE - i)))
  @VisibleForTesting
  static final byte[] maxLog10ForLeadingZeros = {
    19, 18, 18, 18, 18, 17, 17, 17, 16, 16, 16, 15, 15, 15, 15, 14, 14, 14, 13, 13, 13, 12, 12, 12,
    12, 11, 11, 11, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 4, 4, 4, 3, 3, 3,
    3, 2, 2, 2, 1, 1, 1, 0, 0, 0
  };

  // halfPowersOf10[i] = largest long less than 10^(i + 0.5)

  /**
   * Returns the greatest common divisor of {@code a, b}. Returns {@code 0} if
   * {@code a == 0 && b == 0}.
   *
   * @throws IllegalArgumentException if {@code a < 0} or {@code b < 0}
   */
  public static long gcd(long a, long b) {
    /*
     * The reason we require both arguments to be >= 0 is because otherwise, what do you return on
     * gcd(0, Long.MIN_VALUE)? BigInteger.gcd would return positive 2^63, but positive 2^63 isn't an
     * int.
     */
    checkNonNegative("a", a);
    checkNonNegative("b", b);
    if (a == 0) {
      // 0 % b == 0, so b divides a, but the converse doesn't hold.
      // BigInteger.gcd is consistent with this decision.
      return b;
    } else if (b == 0) {
      return a; // similar logic
    }
    /*
     * Uses the binary GCD algorithm; see http://en.wikipedia.org/wiki/Binary_GCD_algorithm. This is
     * >60% faster than the Euclidean algorithm in benchmarks.
     */
    int aTwos = Long.numberOfTrailingZeros(a);
    a >>= aTwos; // divide out all 2s
    int bTwos = Long.numberOfTrailingZeros(b);
    b >>= bTwos; // divide out all 2s
    while (a != b) { // both a, b are odd
      // The key to the binary GCD algorithm is as follows:
      // Both a and b are odd. Assume a > b; then gcd(a - b, b) = gcd(a, b).
      // But in gcd(a - b, b), a - b is even and b is odd, so we can divide out powers of two.

      // We bend over backwards to avoid branching, adapting a technique from
      // http://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax

      long delta = a - b; // can't overflow, since a and b are nonnegative

      long minDeltaOrZero = delta & (delta >> (Long.SIZE - 1));
      // equivalent to Math.min(delta, 0)

      a = delta - minDeltaOrZero - minDeltaOrZero; // sets a to Math.abs(a - b)
      // a is now nonnegative and even

      b += minDeltaOrZero; // sets b to min(old a, b)
      a >>= Long.numberOfTrailingZeros(a); // divide out all 2s, since 2 doesn't divide b
    }
    return a << min(aTwos, bTwos);
  }

  /**
   * Returns the sum of {@code a} and {@code b} unless it would overflow or underflow in which case
   * {@code Long.MAX_VALUE} or {@code Long.MIN_VALUE} is returned, respectively.
   *
   * @since 20.0
   */
  @Beta
  public static long saturatedAdd(long a, long b) {
    long naiveSum = a + b;
    if ((a ^ b) < 0 | (a ^ naiveSum) >= 0) {
      // If a and b have different signs or a has the same sign as the result then there was no
      // overflow, return.
      return naiveSum;
    }
    // we did over/under flow, if the sign is negative we should return MAX otherwise MIN
    return Long.MAX_VALUE + ((naiveSum >>> (Long.SIZE - 1)) ^ 1);
  }

  /**
   * Returns the difference of {@code a} and {@code b} unless it would overflow or underflow in
   * which case {@code Long.MAX_VALUE} or {@code Long.MIN_VALUE} is returned, respectively.
   *
   * @since 20.0
   */
  @Beta
  public static long saturatedSubtract(long a, long b) {
    long naiveDifference = a - b;
    if ((a ^ b) >= 0 | (a ^ naiveDifference) >= 0) {
      // If a and b have the same signs or a has the same sign as the result then there was no
      // overflow, return.
      return naiveDifference;
    }
    // we did over/under flow
    return Long.MAX_VALUE + ((naiveDifference >>> (Long.SIZE - 1)) ^ 1);
  }

  /**
   * Returns the product of {@code a} and {@code b} unless it would overflow or underflow in which
   * case {@code Long.MAX_VALUE} or {@code Long.MIN_VALUE} is returned, respectively.
   *
   * @since 20.0
   */
  @Beta
  public static long saturatedMultiply(long a, long b) {
    // see checkedMultiply for explanation
    int leadingZeros =
        Long.numberOfLeadingZeros(a)
            + Long.numberOfLeadingZeros(~a)
            + Long.numberOfLeadingZeros(b)
            + Long.numberOfLeadingZeros(~b);
    if (leadingZeros > Long.SIZE + 1) {
      return a * b;
    }
    // the return value if we will overflow (which we calculate by overflowing a long :) )
    long limit = Long.MAX_VALUE + ((a ^ b) >>> (Long.SIZE - 1));
    if (leadingZeros < Long.SIZE | (a < 0 & b == Long.MIN_VALUE)) {
      // overflow
      return limit;
    }
    long result = a * b;
    if (a == 0 || result / a == b) {
      return result;
    }
    return limit;
  }

  /**
   * Returns the {@code b} to the {@code k}th power, unless it would overflow or underflow in which
   * case {@code Long.MAX_VALUE} or {@code Long.MIN_VALUE} is returned, respectively.
   *
   * @since 20.0
   */
  @Beta
  public static long saturatedPow(long b, int k) {
    checkNonNegative("exponent", k);
    if (b >= -2 & b <= 2) {
      switch ((int) b) {
        case 0:
          return (k == 0) ? 1 : 0;
        case 1:
          return 1;
        case (-1):
          return ((k & 1) == 0) ? 1 : -1;
        case 2:
          if (k >= Long.SIZE - 1) {
            return Long.MAX_VALUE;
          }
          return 1L << k;
        case (-2):
          if (k >= Long.SIZE) {
            return Long.MAX_VALUE + (k & 1);
          }
          return ((k & 1) == 0) ? (1L << k) : (-1L << k);
        default:
          throw new AssertionError();
      }
    }
    long accum = 1;
    // if b is negative and k is odd then the limit is MIN otherwise the limit is MAX
    long limit = Long.MAX_VALUE + ((b >>> Long.SIZE - 1) & (k & 1));
    while (true) {
      switch (k) {
        case 0:
          return accum;
        case 1:
          return saturatedMultiply(accum, b);
        default:
          if ((k & 1) != 0) {
            accum = saturatedMultiply(accum, b);
          }
          k >>= 1;
          if (k > 0) {
            if (-FLOOR_SQRT_MAX_LONG > b | b > FLOOR_SQRT_MAX_LONG) {
              return limit;
            }
            b *= b;
          }
      }
    }
  }

  @VisibleForTesting static final long FLOOR_SQRT_MAX_LONG = 3037000499L;

  static final long[] factorials = {
    1L,
    1L,
    1L * 2,
    1L * 2 * 3,
    1L * 2 * 3 * 4,
    1L * 2 * 3 * 4 * 5,
    1L * 2 * 3 * 4 * 5 * 6,
    1L * 2 * 3 * 4 * 5 * 6 * 7,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15 * 16,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15 * 16 * 17,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15 * 16 * 17 * 18,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15 * 16 * 17 * 18 * 19,
    1L * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15 * 16 * 17 * 18 * 19 * 20
  };

  /**
   * Returns {@code n} choose {@code k}, also known as the binomial coefficient of {@code n} and
   * {@code k}, or {@link Long#MAX_VALUE} if the result does not fit in a {@code long}.
   *
   * @throws IllegalArgumentException if {@code n < 0}, {@code k < 0}, or {@code k > n}
   */
  public static long binomial(int n, int k) {
    checkNonNegative("n", n);
    checkNonNegative("k", k);
    checkArgument(k <= n, "k (%s) > n (%s)", k, n);
    if (k > (n >> 1)) {
      k = n - k;
    }
    switch (k) {
      case 0:
        return 1;
      case 1:
        return n;
      default:
        if (n < factorials.length) {
          return factorials[n] / (factorials[k] * factorials[n - k]);
        } else if (k >= biggestBinomials.length || n > biggestBinomials[k]) {
          return Long.MAX_VALUE;
        } else if (k < biggestSimpleBinomials.length && n <= biggestSimpleBinomials[k]) {
          // guaranteed not to overflow
          long result = n--;
          for (int i = 2; i <= k; n--, i++) {
            result *= n;
            result /= i;
          }
          return result;
        } else {
          int nBits = LongMath.log2(n, RoundingMode.CEILING);

          long result = 1;
          long numerator = n--;
          long denominator = 1;

          int numeratorBits = nBits;
          // This is an upper bound on log2(numerator, ceiling).

          /*
           * We want to do this in long math for speed, but want to avoid overflow. We adapt the
           * technique previously used by BigIntegerMath: maintain separate numerator and
           * denominator accumulators, multiplying the fraction into result when near overflow.
           */
          for (int i = 2; i <= k; i++, n--) {
            if (numeratorBits + nBits < Long.SIZE - 1) {
              // It's definitely safe to multiply into numerator and denominator.
              numerator *= n;
              denominator *= i;
              numeratorBits += nBits;
            } else {
              // It might not be safe to multiply into numerator and denominator,
              // so multiply (numerator / denominator) into result.
              result = multiplyFraction(result, numerator, denominator);
              numerator = n;
              denominator = i;
              numeratorBits = nBits;
            }
          }
          return multiplyFraction(result, numerator, denominator);
        }
    }
  }

  /**
   * Returns (x * numerator / denominator), which is assumed to come out to an integral value.
   */
  static long multiplyFraction(long x, long numerator, long denominator) {
    if (x == 1) {
      return numerator / denominator;
    }
    long commonDivisor = gcd(x, denominator);
    x /= commonDivisor;
    denominator /= commonDivisor;
    // We know gcd(x, denominator) = 1, and x * numerator / denominator is exact,
    // so denominator must be a divisor of numerator.
    return x * (numerator / denominator);
  }

  /*
   * binomial(biggestBinomials[k], k) fits in a long, but not binomial(biggestBinomials[k] + 1, k).
   */
  static final int[] biggestBinomials = {
    Integer.MAX_VALUE,
    Integer.MAX_VALUE,
    Integer.MAX_VALUE,
    3810779,
    121977,
    16175,
    4337,
    1733,
    887,
    534,
    361,
    265,
    206,
    169,
    143,
    125,
    111,
    101,
    94,
    88,
    83,
    79,
    76,
    74,
    72,
    70,
    69,
    68,
    67,
    67,
    66,
    66,
    66,
    66
  };

  /*
   * binomial(biggestSimpleBinomials[k], k) doesn't need to use the slower GCD-based impl, but
   * binomial(biggestSimpleBinomials[k] + 1, k) does.
   */
  @VisibleForTesting
  static final int[] biggestSimpleBinomials = {
    Integer.MAX_VALUE,
    Integer.MAX_VALUE,
    Integer.MAX_VALUE,
    2642246,
    86251,
    11724,
    3218,
    1313,
    684,
    419,
    287,
    214,
    169,
    139,
    119,
    105,
    95,
    87,
    81,
    76,
    73,
    70,
    68,
    66,
    64,
    63,
    62,
    62,
    61,
    61,
    61
  };
  // These values were generated by using checkedMultiply to see when the simple multiply/divide
  // algorithm would lead to an overflow.

  static boolean fitsInInt(long x) {
    return (int) x == x;
  }

  /**
   * Returns the arithmetic mean of {@code x} and {@code y}, rounded toward negative infinity. This
   * method is resilient to overflow.
   *
   * @since 14.0
   */
  public static long mean(long x, long y) {
    // Efficient method for computing the arithmetic mean.
    // The alternative (x + y) / 2 fails for large values.
    // The alternative (x + y) >>> 1 fails for negative values.
    return (x & y) + ((x ^ y) >> 1);
  }

  /*
   * This bitmask is used as an optimization for cheaply testing for divisiblity by 2, 3, or 5.
   * Each bit is set to 1 for all remainders that indicate divisibility by 2, 3, or 5, so
   * 1, 7, 11, 13, 17, 19, 23, 29 are set to 0. 30 and up don't matter because they won't be hit.
   */
  private static final int SIEVE_30 =
      ~((1 << 1) | (1 << 7) | (1 << 11) | (1 << 13)
          | (1 << 17) | (1 << 19) | (1 << 23) | (1 << 29));

  /*
   * If n <= millerRabinBases[i][0], then testing n against bases millerRabinBases[i][1..] suffices
   * to prove its primality. Values from miller-rabin.appspot.com.
   *
   * NOTE: We could get slightly better bases that would be treated as unsigned, but benchmarks
   * showed negligible performance improvements.
   */
  private static final long[][] millerRabinBaseSets = {
    {291830, 126401071349994536L},
    {885594168, 725270293939359937L, 3569819667048198375L},
    {273919523040L, 15, 7363882082L, 992620450144556L},
    {47636622961200L, 2, 2570940, 211991001, 3749873356L},
    {
      7999252175582850L,
      2,
      4130806001517L,
      149795463772692060L,
      186635894390467037L,
      3967304179347715805L
    },
    {
      585226005592931976L,
      2,
      123635709730000L,
      9233062284813009L,
      43835965440333360L,
      761179012939631437L,
      1263739024124850375L
    },
    {Long.MAX_VALUE, 2, 325, 9375, 28178, 450775, 9780504, 1795265022}
  };

  private enum MillerRabinTester {
    /** Works for inputs ≤ FLOOR_SQRT_MAX_LONG. */
    SMALL {
      @Override
      long mulMod(long a, long b, long m) {
        /*
         * NOTE(lowasser, 2015-Feb-12): Benchmarks suggest that changing this to
         * UnsignedLongs.remainder and increasing the threshold to 2^32 doesn't pay for itself, and
         * adding another enum constant hurts performance further -- I suspect because bimorphic
         * implementation is a sweet spot for the JVM.
         */
        return (a * b) % m;
      }

      @Override
      long squareMod(long a, long m) {
        return (a * a) % m;
      }
    },
    /**
     * Works for all nonnegative signed longs.
     */
    LARGE {
      /** Returns (a + b) mod m. Precondition: {@code 0 <= a}, {@code b < m < 2^63}. */
      private long plusMod(long a, long b, long m) {
        return (a >= m - b) ? (a + b - m) : (a + b);
      }

      /**
       * Returns (a * 2^32) mod m. a may be any unsigned long.
       */
      private long times2ToThe32Mod(long a, long m) {
        int remainingPowersOf2 = 32;
        do {
          int shift = Math.min(remainingPowersOf2, Long.numberOfLeadingZeros(a));
          // shift is either the number of powers of 2 left to multiply a by, or the biggest shift
          // possible while keeping a in an unsigned long.
          a = UnsignedLongs.remainder(a << shift, m);
          remainingPowersOf2 -= shift;
        } while (remainingPowersOf2 > 0);
        return a;
      }

      @Override
      long mulMod(long a, long b, long m) {
        long aHi = a >>> 32; // < 2^31
        long bHi = b >>> 32; // < 2^31
        long aLo = a & 0xFFFFFFFFL; // < 2^32
        long bLo = b & 0xFFFFFFFFL; // < 2^32

        /*
         * a * b == aHi * bHi * 2^64 + (aHi * bLo + aLo * bHi) * 2^32 + aLo * bLo.
         *       == (aHi * bHi * 2^32 + aHi * bLo + aLo * bHi) * 2^32 + aLo * bLo
         *
         * We carry out this computation in modular arithmetic. Since times2ToThe32Mod accepts any
         * unsigned long, we don't have to do a mod on every operation, only when intermediate
         * results can exceed 2^63.
         */
        long result = times2ToThe32Mod(aHi * bHi /* < 2^62 */, m); // < m < 2^63
        result += aHi * bLo; // aHi * bLo < 2^63, result < 2^64
        if (result < 0) {
          result = UnsignedLongs.remainder(result, m);
        }
        // result < 2^63 again
        result += aLo * bHi; // aLo * bHi < 2^63, result < 2^64
        result = times2ToThe32Mod(result, m); // result < m < 2^63
        return plusMod(
            result,
            UnsignedLongs.remainder(aLo * bLo /* < 2^64 */, m),
            m);
      }

      @Override
      long squareMod(long a, long m) {
        long aHi = a >>> 32; // < 2^31
        long aLo = a & 0xFFFFFFFFL; // < 2^32

        /*
         * a^2 == aHi^2 * 2^64 + aHi * aLo * 2^33 + aLo^2
         *     == (aHi^2 * 2^32 + aHi * aLo * 2) * 2^32 + aLo^2
         * We carry out this computation in modular arithmetic.  Since times2ToThe32Mod accepts any
         * unsigned long, we don't have to do a mod on every operation, only when intermediate
         * results can exceed 2^63.
         */
        long result = times2ToThe32Mod(aHi * aHi /* < 2^62 */, m); // < m < 2^63
        long hiLo = aHi * aLo * 2;
        if (hiLo < 0) {
          hiLo = UnsignedLongs.remainder(hiLo, m);
        }
        // hiLo < 2^63
        result += hiLo; // result < 2^64
        result = times2ToThe32Mod(result, m); // result < m < 2^63
        return plusMod(
            result,
            UnsignedLongs.remainder(aLo * aLo /* < 2^64 */, m),
            m);
      }
    };

    static boolean test(long base, long n) {
      // Since base will be considered % n, it's okay if base > FLOOR_SQRT_MAX_LONG,
      // so long as n <= FLOOR_SQRT_MAX_LONG.
      return ((n <= FLOOR_SQRT_MAX_LONG) ? SMALL : LARGE).testWitness(base, n);
    }

    /**
     * Returns a * b mod m.
     */
    abstract long mulMod(long a, long b, long m);

    /**
     * Returns a^2 mod m.
     */
    abstract long squareMod(long a, long m);

    /**
     * Returns a^p mod m.
     */
    private long powMod(long a, long p, long m) {
      long res = 1;
      for (; p != 0; p >>= 1) {
        if ((p & 1) != 0) {
          res = mulMod(res, a, m);
        }
        a = squareMod(a, m);
      }
      return res;
    }

    /**
     * Returns true if n is a strong probable prime relative to the specified base.
     */
    private boolean testWitness(long base, long n) {
      int r = Long.numberOfTrailingZeros(n - 1);
      long d = (n - 1) >> r;
      base %= n;
      if (base == 0) {
        return true;
      }
      // Calculate a := base^d mod n.
      long a = powMod(base, d, n);
      // n passes this test if
      //    base^d = 1 (mod n)
      // or base^(2^j * d) = -1 (mod n) for some 0 <= j < r.
      if (a == 1) {
        return true;
      }
      int j = 0;
      while (a != n - 1) {
        if (++j == r) {
          return false;
        }
        a = squareMod(a, n);
      }
      return true;
    }
  }

  private LongMath() {}
}

