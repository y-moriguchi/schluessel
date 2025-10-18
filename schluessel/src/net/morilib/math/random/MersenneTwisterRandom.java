/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.math.random;

/**
 * Java program for Mersenne Twister pseudo-random number generator.
 * 
 * <p>Mersenne Twister was developed by Takuji Nishimura and
 * Makoto Matsumoto.<br />
 * The home page for this generator is located at <a
 * href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html">
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html</a>.</p>
 * 
 * <p>Originally source has coded by Takuji Nishimura and
 * Makoto Matsumoto.<br />
 * Java Translation by Y.Moriguchi.</p>
 *
 * <p>Here is the original copyright:</p>
 * <pre>
 * Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
 * All rights reserved.                          
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. The names of its contributors may not be used to endorse or promote 
 *      products derived from this software without specific prior written 
 *      permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * </pre>
 */
public class MersenneTwisterRandom implements RandomSource {

	//
	private static final int N = 624;
	private static final int M = 397;

	// constant vector a
	private static final long MATRIX_A = 0x9908b0dfl;

	// most significant w-r bits
	private static final long UPPER_MASK = 0x80000000l;

	// least significant r bits
	private static final long LOWER_MASK = 0x7fffffffl;

	// mag01[x] = x * MATRIX_A  for x=0,1
	private static long[] mag01 = new long[] {
		0x0l, MATRIX_A
	};

	// the array for the state vector
	private long[] mt = new long[N];

	// mti==N+1 means mt[N] is not initialized
	private int mti = N + 1;

	//
	/*package*/ MersenneTwisterRandom() { }

	/**
	 * 
	 * @param s
	 */
	public MersenneTwisterRandom(long s) {
		nextUnsignedInt();
		init(s);
	}

	/**
	 * 
	 * @return
	 */
	public static MersenneTwisterRandom getInstance() {
		MersenneTwisterRandom r = new MersenneTwisterRandom();

		r.nextUnsignedInt();
		r.init((long)(Math.random() * Integer.MAX_VALUE) + 1);
		return r;
	}

	/**
	 * 
	 * @param s
	 */
	public void init(long s) {
		// initializes mt[N] with a seed
	    mt[0]= s & 0xffffffffl;

	    for(mti = 1; mti < N; mti++) {
	    	mt[mti] =
	    		(1812433253l * (mt[mti-1] ^ (mt[mti-1] >>> 30)) + mti); 
	    	// See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
	    	// In the previous versions, MSBs of the seed affect
	    	// only MSBs of the array mt[].
	    	// 2002/01/09 modified by Makoto Matsumoto
	    	mt[mti] &= 0xffffffffl;
	    	// for >32 bit machines
	    }
	}

	/**
	 * 
	 * @param initKey
	 * @param keyLength
	 */
	public void initByArray(long initKey[]) {
		// initialize by an array with array-length
		// init_key is the array for initializing keys
		// key_length is its length
		// slight change for C++, 2004/2/26
		int i, j, k;

		init(19650218l);
		i = 1;  j = 0;
		k = (N > initKey.length ? N : initKey.length);

		for(; k != 0; k--) {
			// non linear
			mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >>> 30))
					* 1664525l)) + initKey[j] + j;
			mt[i] &= 0xffffffffl; // for WORDSIZE > 32 machines
			i++;  j++;
			if(i >= N) {
				mt[0] = mt[N-1];  i = 1;
			}
			if(j >= initKey.length) {
				j = 0;
			}
		}

		for(k = N - 1; k != 0; k--) {
			// non linear
			mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >>> 30))
					* 1566083941l)) - i;
			mt[i] &= 0xffffffffl; // for WORDSIZE > 32 machines
			i++;
			if(i >= N) {
				mt[0] = mt[N-1];  i = 1;
			}
		}

		// MSB is 1; assuring non-zero initial array
		mt[0] = 0x80000000l;
	}

	/**
	 * 
	 * @return
	 */
	public long nextUnsignedInt() {
		// generates a random number on [0,0xffffffff]-interval
		long y;

		if(mti >= N) { // generate N words at one time
			int kk;

			if(mti == N+1) {
				// if init_genrand() has not been called,
				// a default initial seed is used
				init(5489L);
			}

			for(kk = 0; kk < N - M; kk++) {
				y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
				mt[kk] = (mt[kk + M] ^
						(y >>> 1) ^ mag01[(int)(y & 0x1l)]);
			}
			for(; kk < N - 1; kk++) {
				y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
				mt[kk] = (mt[kk + (M - N)] ^
						(y >>> 1) ^ mag01[(int)(y & 0x1l)]);
			}
			y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
			mt[N-1] = mt[M-1] ^ (y >>> 1) ^ mag01[(int)(y & 0x1l)];

			mti = 0;
		}
		y = mt[mti++];

		// Tempering
		y ^= (y >>> 11);
		y ^= (y <<   7) & 0x9d2c5680L;
		y ^= (y <<  15) & 0xefc60000L;
		y ^= (y >>> 18);
		return y;
	}

	/**
	 * 
	 * @return
	 */
	public long nextInt() {
		return (int)nextUnsignedInt();
	}

	/**
	 * 
	 * @return
	 */
	public double nextDouble1() {
		return nextUnsignedInt() * (1.0 / 4294967295.0);
	}

	/**
	 * 
	 * @return
	 */
	public double nextDouble2() {
		return nextUnsignedInt() * (1.0 / 4294967296.0);
	}

	/**
	 * 
	 * @return
	 */
	public double nextDouble3() {
		return ((((double)nextUnsignedInt()) + 0.5) *
				(1.0 / 4294967296.0));
	}

	/**
	 * 
	 * @return
	 */
	public MersenneTwisterRandomState getState() {
		MersenneTwisterRandomState s;

		s = new MersenneTwisterRandomState();
		s.mti = mti;
		s.mt  = new long[N];
		System.arraycopy(mt, 0, s.mt, 0, N);
		return s;
	}

	/**
	 * 
	 * @param s
	 */
	public void setState(RandomState s) {
		if(s instanceof MersenneTwisterRandomState) {
			MersenneTwisterRandomState t;

			t = (MersenneTwisterRandomState)s;
			mti = t.mti;
			System.arraycopy(t.mt, 0, mt, 0, N);
		} else {
			throw new ClassCastException();
		}
	}

}
