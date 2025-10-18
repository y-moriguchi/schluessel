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
public class FxMulWithCarry extends AbstractFixnumTernaryOp3 {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumTernaryOp3#op1(int, int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected int op1(int x, int y, int z, LispMessage mesg) {
		long s = (long)x * (long)y;
		long r = s + (long)z;

		if(x == LispInteger.FIXNUM_MIN_LONG &&
				y == LispInteger.FIXNUM_MIN_LONG) {
			if(z >= 0) {
				throw mesg.getError("err.r6rs.fixnum.overflow");
			}
			return (int)LispInteger.FIXNUM_MAX_LONG + z + 1;
		} else if(s >= 0 && z >= 0 && (int)r < 0) {
			// overflowed
			return (int)(r - LispInteger.FIXNUM_MIN_LONG);
		} else if(s <= 0 && z <= 0 && (int)r > 0) {
			// overflowed
			return (int)(r + LispInteger.FIXNUM_MIN_LONG);
		} else if(s <= 0 && z <= 0 &&
				(int)r == (int)LispInteger.FIXNUM_MIN_LONG) {
			return 0;
		} else {
			return (int)r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumTernaryOp3#op2(int, int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected int op2(int x, int y, int z, LispMessage mesg) {
		long s = (long)x * (long)y;
		long r = s + (long)z;
		int  t;

		if(x == LispInteger.FIXNUM_MIN_LONG &&
				y == LispInteger.FIXNUM_MIN_LONG) {
			if(z >= 0) {
				throw mesg.getError("err.r6rs.fixnum.overflow");
			}
			return (int)LispInteger.FIXNUM_MAX_LONG;
		}

		t = (int)((r >> (LispInteger.FIXNUM_WIDTH - 1)) +
				((r > 0) ? 0 : 1));
		if(s <= 0 && z <= 0 && ((int)r == 0 ||
			(int)r == (int)LispInteger.FIXNUM_MIN_LONG)) {
			return t - 1;
		} else {
			return t;
		}
//		return t;
	}

}
