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
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/24
 */
public class RandomSourceMakeIntegers extends UnaryArgs {

	//
	private static final String PROCN =
		"random-integer generated random-source-make-integers";

	//
	private class RandInt extends UnaryArgs {

		//
		private ILispRandomSource src;

		//
		private RandInt(ILispRandomSource src) {
			super(PROCN);
			this.src = src;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env, LispMessage mesg) {
			return RandomSourceUtils.nextInt(src, c1a, mesg);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof ILispRandomSource) {
			return new RandInt((ILispRandomSource)c1a);
		} else {
			throw mesg.getError("err.srfi27.require.randomsource",
					c1a);
		}
	}

}
