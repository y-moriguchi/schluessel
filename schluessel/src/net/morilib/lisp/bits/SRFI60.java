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
import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/02
 */
/*package*/ class SRFI60 {

	//
	/*package*/ static BigInteger[] getFromArgs(
			Datum body, LispMessage mesg) {
		List<BigInteger> bits = new ArrayList<BigInteger>();
		ConsIterator itr = new ConsIterator(body);

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(d instanceof LispInteger) {
				bits.add(d.getBigInteger());
			} else {
				throw mesg.getError("err.require.int", d);
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return bits.toArray(new BigInteger[0]);
	}

}
