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
package net.morilib.lisp.math.random;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.math.random.RandomSource;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/24
 */
public class RandomSourcePseudoRandomizeS extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		long i = SubrUtils.getBigInteger(c2a, mesg).intValue();
		long j = SubrUtils.getBigInteger(c3a, mesg).intValue();

		if(c1a instanceof ILispRandomSource) {
			RandomSource src =
				((ILispRandomSource)c1a).getRandomSource();

			i = Math.abs(i);  j = Math.abs(j);
			src.init(i * j);
			return Undef.UNDEF;
		} else {
			throw mesg.getError("err.srfi27.require.randomsource",
					c1a);
		}
	}

}
