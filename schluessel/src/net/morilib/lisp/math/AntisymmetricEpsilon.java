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
package net.morilib.lisp.math;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.Math2;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/08
 */
public class AntisymmetricEpsilon extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		IntegerVector v = new IntegerArrayVector();
		int[] a;

		while(itr.hasNext()) {
			v.add(SubrUtils.nextSmallInt(itr, mesg, body));
		}
		SubrUtils.checkTerminated(itr, body, mesg);

		a = v.toIntArray();
		for(int i = 0; i < a.length; i++) {
			if(a[i] < 0 || a[i] > a.length) {
				throw mesg.getError("err.range.invalid",
						Integer.toString(a[i]));
			}
			a[i]--;
		}
		return LispInteger.valueOf(Math2.permutationParity(a));
	}

}
