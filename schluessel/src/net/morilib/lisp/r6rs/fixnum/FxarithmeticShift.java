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
public class FxarithmeticShift extends AbstractFixnumBinaryOp {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumBinaryOp#execute(int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected int execute(int x, int y, LispMessage mesg) {
		long r;

		if(y <= -LispInteger.FIXNUM_WIDTH) {
			throw mesg.getError("err.r6rs.shiftbit.invalid", y);
		} else if(y < 0) {
			return x >> -y;
		} else if(y == 0) {
			return x;
		} else if(y < LispInteger.FIXNUM_WIDTH) {
			r = (long)x << y;
			if(r < LispInteger.FIXNUM_MIN_LONG ||
					r > LispInteger.FIXNUM_MAX_LONG) {
				throw mesg.getError("err.r6rs.fixnum.overflow");
			}
			return (int)r;
		} else {
			throw mesg.getError("err.r6rs.shiftbit.invalid", y);
		}
	}

}
