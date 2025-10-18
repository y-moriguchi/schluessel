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
package net.morilib.lisp.nio;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Arrays;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.SRFI74Endianness;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/12
 */
public class UintListToBlob extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		int s = SubrUtils.getSmallInt(c1a, mesg);
		BigInteger max = BigInteger.ONE.shiftLeft(s * 8);
		BigInteger ma2 = BigInteger.ONE.shiftLeft(s * 8 - 1);

		if(!(c2a instanceof SRFI74Endianness.Endian)) {
			throw mesg.getError("err.srfi74.require.endianness", c2a);
		} else {
			ConsIterator itr = new ConsIterator(c3a);
			int l = LispUtils.consLength(c3a);
			ByteBuffer b = ByteBuffer.allocate(l * s);

			b.order(((SRFI74Endianness.Endian)c2a).getByteOrder());
			while(itr.hasNext()) {
				Datum d = itr.next();
				byte[] a;

				if(d instanceof LispInteger) {
					BigInteger x = d.getBigInteger();
					
					if(x.compareTo(max) >= 0 || x.signum() < 0) {
						throw mesg.getError(
								"err.srfi74.value.outofrange", d);
					} else if(x.compareTo(ma2) >= 0) {
						a = x.toByteArray();
						b.put(a, 1, s);
					} else {
						byte[] z = x.toByteArray();

						a = new byte[s];
						Arrays.fill(a, (byte)0);
						System.arraycopy(z, 0, a, s - z.length,
								z.length);
						b.put(a);
					}
				} else {
					throw mesg.getError("err.require.int", d);
				}
			}
			SubrUtils.checkTerminated(itr, c3a, mesg);
			return new LispBlob(b);
		}
	}

}
