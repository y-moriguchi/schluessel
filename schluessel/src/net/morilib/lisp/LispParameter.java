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
package net.morilib.lisp;

import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/21
 */
public class LispParameter extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/21
	 */
	public static class MakeParameter extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return new LispParameter(c1a);
		}

	}

	//
	private ThreadLocal<Stack2<Datum>> params =
		new InheritableThreadLocal<Stack2<Datum>>() {

			/* (non-Javadoc)
			 * @see java.lang.InheritableThreadLocal#childValue(java.lang.Object)
			 */
			@Override
			protected Stack2<Datum> childValue(Stack2<Datum> v) {
				Stack2<Datum> d = new ArrayListStack<Datum>();

				if(!v.isEmpty()) {
					d.push(v.peek());
				}
				return d;
			}

	};

	/**
	 * 
	 * @param p
	 */
	public LispParameter(Datum p) {
		Stack2<Datum> t = new ArrayListStack<Datum>();

		t.push(p);
		params.set(t);
	}

	/*package*/ void parameterize(Datum p) {
		params.get().push(p);
	}

	/*package*/ void deparameterize() {
		params.get().pop();
	}

	/**
	 * 
	 * @return
	 */
	public Datum get() {
		return params.get().peek();
	}

	/**
	 * 
	 * @param p
	 * @return
	 */
	public Datum set(Datum p) {
		Datum r = params.get().pop();

		params.get().push(p);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<parameter>");
	}

}
