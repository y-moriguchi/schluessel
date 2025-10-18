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
package net.morilib.lisp.r6rs.fixnum;

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/29
 */
public class FxcopyBitField extends AbstractFixnumQuaternaryOp {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumQuaternaryOp#op1(int, int, int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected int op1(int x, int y, int z, int w, LispMessage mesg) {
		int m;

		if(z < 0 || z >= LispInteger.FIXNUM_WIDTH) {
			throw mesg.getError("err.r6rs.bitnumber.invalid", y);
		} else if(y < 0 || y > z) {
			throw mesg.getError("err.r6rs.bitrange.invalid", z);
		}
		m = ~(-1 << (z - y)) << y;
		return (x & ~m) | ((w << y) & m);
	}

}
