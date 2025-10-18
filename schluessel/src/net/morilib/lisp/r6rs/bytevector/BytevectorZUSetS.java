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
package net.morilib.lisp.r6rs.bytevector;

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/20
 */
public abstract class BytevectorZUSetS extends BytevectorNSetS {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.bytevector.BytevectorNSetS#checkNumber(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected LispReal checkNumber(Datum r, LispMessage mesg) {
		LispInteger i = SubrUtils.getInteger(r, mesg);

		if(i.signum() < 0) {
			throw mesg.getError("err.r6rs.require.rangeoverflow", i);
		} else if(i.getBigInteger().compareTo(
				BigInteger.ONE.shiftLeft(getSize() * 8)) >= 0) {
			throw mesg.getError("err.r6rs.require.rangeoverflow", i);
		}
		return i;
	}

}
