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
package net.morilib.lisp.r6rs.flonum;

import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/31
 */
public class Fllog extends AbstractFlonumOp3 {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.flonum.AbstractFlonumOp3#op2(double, double, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected double op2(double r, double s, LispMessage m) {
		if(s <= 0) {
			return Double.NaN;
		} else {
			return Math.log(r) / Math.log(s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.flonum.AbstractFlonumOp3#op1(double, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected double op1(double x, LispMessage m) {
		return Math.log(x);
	}

}
