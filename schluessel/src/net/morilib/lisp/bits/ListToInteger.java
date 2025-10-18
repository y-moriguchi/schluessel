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
package net.morilib.lisp.bits;

import java.math.BigInteger;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.primitive.ByteArrayVector;
import net.morilib.util.primitive.ByteVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/03
 */
public class ListToInteger extends UnaryArgs {

	//
	/*package*/ static Datum toInteger(Datum lst, LispMessage mesg) {
		ByteVector v = new ByteArrayVector();
		ConsIterator itr = new ConsIterator(lst);
		byte x = 0;
		int  i = 0;

		for(; itr.hasNext(); i++) {
			if(itr.next().isTrue()) {
				x |= 1 << (i & 0x7);
			} else {
				x &= ~(1 << (i & 0x7));
			}

			if((i & 0x7) == 7) {
				v.addByte(x);
				x = 0;
			}
		}
		v.addByte(x);
		SubrUtils.checkTerminated(itr, lst, mesg);
		return LispInteger.valueOf(new BigInteger(v.toByteArray()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		return toInteger(c1a, mesg);
	}

}
