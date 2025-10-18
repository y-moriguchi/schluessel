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
package net.morilib.lisp.math.analysis;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/08
 */
public class LispDeltaFunction extends UnaryArgs
implements ILispFunction1, ILispIntegrable1 {

	//
	private LispReal k0;

	/**
	 * @param real
	 */
	public LispDeltaFunction(LispReal real) {
		this.k0 = real;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.analysis.ILispIntegrable1#integrate(net.morilib.lisp.LispReal, net.morilib.lisp.LispReal)
	 */
	public LispReal integrate(LispReal a, LispReal b) {
		return (a.compareTo(k0) <= 0 && b.compareTo(k0) >= 0) ?
				LispInteger.ONE : LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.analysis.ILispFunction1#substitute(net.morilib.lisp.LispNumber)
	 */
	public LispNumber substitute(LispNumber x) {
		return x.isEqualTo(k0) ?
				LispDouble.POSITIVE_INFINITY : LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		return substitute(SubrUtils.getReal(c1a, mesg));
	}

	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("delta(x - ").append(k0.print()).append(")");
	}

}
