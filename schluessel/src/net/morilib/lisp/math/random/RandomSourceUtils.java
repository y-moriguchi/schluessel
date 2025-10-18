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
package net.morilib.lisp.math.random;

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.random.MersenneTwisterRandom;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/24
 */
public class RandomSourceUtils {

	//
	private static final BigInteger VAL =
		BigInteger.valueOf(0x100000000l);

	/**
	 * 
	 */
	public static final ILispRandomSource DEFAULT =
		new LispMTRandomSource(MersenneTwisterRandom.getInstance());

	//
	/*package*/ static Datum nextInt(ILispRandomSource src, Datum d,
			LispMessage mesg) {
		if(d instanceof LispSmallInt) {
			int n = SubrUtils.getSmallInt(d, mesg);

			return LispInteger.valueOf(
					src.getRandomSource().nextUnsignedInt() % n);
		} else if(d instanceof LispInteger) {
			BigInteger n = d.getBigInteger();
			BigInteger v = BigInteger.ZERO;
			BigInteger r = BigInteger.ZERO;

			while(n.compareTo(v) < 0) {
				long l = src.getRandomSource().nextUnsignedInt();

				r = r.multiply(VAL).add(BigInteger.valueOf(l));
				v = v.multiply(VAL);
			}
			return LispInteger.valueOf(r.remainder(n));
		} else {
			throw mesg.getError("err.require.int", d);
		}
	}

}
