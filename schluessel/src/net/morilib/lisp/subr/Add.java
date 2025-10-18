/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.subr;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.math.algebra.ILispAddable;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Add extends MathOperator1<ILispAddable<?>> {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.MathOperator1#calculate(net.morilib.lisp.math.algebra.ILispRing, net.morilib.lisp.math.algebra.ILispRing)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected ILispAddable calculate(ILispAddable o1,
			ILispAddable o2) {
		return (ILispAddable<?>)o1.add(o2);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.MathOperator1#initValue()
	 */
	@Override
	protected LispNumber initValue() {
		return LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.MathOperator1#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		StringBuilder b;
		Datum s;

		if(itr.hasNext()) {
			if((s = itr.next()) instanceof LispString) {
				b = new StringBuilder(s.getString());
				while(itr.hasNext()) {
					b.append(LispUtils.print(itr.next()));
				}
				SubrUtils.checkTerminated(itr, body, mesg);
				return new LispString(b.toString());
			}
		}
		return super.eval(body, env, mesg);
	}

}
