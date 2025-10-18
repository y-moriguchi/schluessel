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
package net.morilib.lisp.iterator;

import java.util.NoSuchElementException;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class MakeTreeWalker extends UnaryArgs {

	//
	private static void _w(Stack2<Datum> stk) {
		Datum d;
		int i;

		while(!stk.isEmpty()) {
			if((d = stk.peek()) instanceof Cons) {
				stk.pop();
				stk.push(((Cons)d).getCdr());
				stk.push(((Cons)d).getCar());
			} else if(d instanceof LispVector) {
				stk.pop();
				i = ((LispVector)d).size() - 1;
				for(; i >= 0; i--) {
					stk.push(((LispVector)d).get(i));
				}
			} else if(d.isNil()) {
				stk.pop();
			} else {
				break;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		final Stack2<Datum> stk = new ArrayListStack<Datum>();

		stk.push(c1a);
		_w(stk);
		return new LispIteratorDatum() {

			public boolean isTerminated() {
				return stk.isEmpty();
			}

			public ILispIterator next() {
				stk.pop();
				_w(stk);
				return this;
			}

			public Datum getCurrentDatum() {
				if(stk.isEmpty()) {
					throw new NoSuchElementException();
				}
				return stk.peek();
			}

			@Override
			public void toDisplayString(StringBuilder buf) {
				buf.append("#<tree-walker>");
			}

		};
	}

}
