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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.NoArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.random.RandomSource;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/24
 */
public class RandomSourceMakeReals extends Subr {

	//
	private static final String PROCN =
		"random-real generated random-source-make-integers";

	//
	private class RandReal extends NoArgs {

		//
		private RandomSource src;
		private double unit;

		//
		private RandReal(RandomSource src, double unit) {
			super(PROCN);
			this.src  = src;
			this.unit = unit;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.NoArgs#execute(net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Environment env, LispMessage mesg) {
			return new LispDouble(src.nextDouble3() * unit);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum ud = Iterators.nextIf(itr);
		double u = 1.0;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(ud != null) {
			u = SubrUtils.getDouble(ud, mesg);
			if(u < 0.0 || u > 1.0) {
				throw mesg.getError("err.srfi27.unit.invalid", ud);
			}
		}

		if(c1a instanceof ILispRandomSource) {
			return new RandReal(
					((ILispRandomSource)c1a).getRandomSource(), u);
		} else {
			throw mesg.getError("err.srfi27.require.randomsource",
					c1a);
		}
	}

}
