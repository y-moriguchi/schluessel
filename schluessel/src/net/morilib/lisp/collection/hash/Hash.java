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
package net.morilib.lisp.collection.hash;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispVector;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/04
 */
public class Hash extends HashBase {

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static int hash(Datum d) {
		Stack2<Datum> s = new ArrayListStack<Datum>();
		int   r = Hashes.INIT;

		s.add(d);
		while(!s.isEmpty()) {
			Datum x = s.pop();

			if(x instanceof Cons) {
				s.push(((Cons)x).getCdr());
				s.push(((Cons)x).getCar());
			} else if(x instanceof LispVector) {
				for(int i = ((LispVector)x).size() - 1; i >= 0; i--) {
					s.push(((LispVector)x).get(i));
				}
			} else if(x instanceof LispString) {
				r = Hashes.A * r + x.getString().hashCode();
			} else {
				r = Hashes.A * r + x.hashCode();
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public int hash(Datum d, Environment env, LispMessage mesg) {
		return hash(d);
	}

}
