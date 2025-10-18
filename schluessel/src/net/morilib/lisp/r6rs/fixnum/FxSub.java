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
public class FxSub extends AbstractFixnumBinaryOp2 {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumBinaryOp2#op1(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected int op1(int x, LispMessage mesg) {
		if(x == Integer.MIN_VALUE) {
			throw mesg.getError("err.r6rs.fixnum.overflow");
		} else {
			return -x;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumBinaryOp2#op2(int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected int op2(int x, int y, LispMessage mesg) {
		long r = (long)x - (long)y;
		long o = r >> (LispInteger.FIXNUM_WIDTH - 1);

		if((x > 0 && y < 0 && o != 0) ||
				(x < 0 && y > 0 && o != -1)) {
			throw mesg.getError("err.r6rs.fixnum.overflow");
		} else {
			return (int)r;
		}
	}

}
