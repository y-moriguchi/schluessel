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

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class TernaryArgs extends Subr {

	/**
	 * 
	 */
	public TernaryArgs() {
		// do nothing
	}

	/**
	 * 
	 * @param name
	 */
	public TernaryArgs(String name) {
		symbolName = name;
	}

	/**
	 * 
	 * @param c1a
	 * @param c2a
	 * @param c3a
	 * @param env
	 * @param mesg
	 * @return
	 */
	protected abstract Datum execute(
			Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(
			Datum body,
			Environment env,
			LispMessage mesg) {
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			Datum c1a = c1.getCar();

			if(c1.getCdr() instanceof Cons) {
				Cons c2 = (Cons)c1.getCdr();
				Datum c2a = c2.getCar();

				if(c2.getCdr() instanceof Cons) {
					Cons c3 = (Cons)c2.getCdr();
					Datum c3a = c3.getCar();

					if(c3.getCdr() == Nil.NIL) {
						return execute(c1a, c2a, c3a, env, mesg);
					}
				}
			}
		}
		throw mesg.getError("err.argument", symbolName);
	}

}
