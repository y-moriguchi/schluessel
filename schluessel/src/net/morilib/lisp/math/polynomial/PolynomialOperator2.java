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
package net.morilib.lisp.math.polynomial;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;

public abstract class PolynomialOperator2 extends Subr {

	/**
	 * 
	 * @param o1
	 * @return
	 */
	protected abstract LispPolynomial calculateUnary(
			LispPolynomial o1);
	
	/**
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	protected abstract LispPolynomial calculate(
			LispPolynomial o1, LispPolynomial o2);
	
	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(
			Datum body,
			Environment env,
			LispMessage mesg) {
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			Datum c1a = c1.getCar();
			LispPolynomial res;
			
			if(!(c1a instanceof LispPolynomial)) {
				throw mesg.getError("err.require.poly", c1a);
				//throw new LispException("number required");
			}
			
			res = (LispPolynomial)c1a;
			Datum li = c1.getCdr();
			if(li instanceof Nil) {
				return calculateUnary(res);
			} else {
				while(true) {
					if(li instanceof Nil) {
						return res;
					} else if(li instanceof Cons) {
						Cons c2 = (Cons)li;
						Datum c2a = c2.getCar();
						
						if(c2a instanceof LispPolynomial) {
							res = calculate(res, (LispPolynomial)c2a);
							li = c2.getCdr();
						} else {
							throw mesg.getError("err.require.poly", c2a);
							//throw new LispException("number required");
						}
					} else {
						throw mesg.getError("err.list");
						//throw new LispException("proper list required");
					}
				}
			}
		} else if(body instanceof Nil) {
			throw mesg.getError("err.argument", symbolName);
			//throw new LispException("requires at least one argument");
		} else {
			throw mesg.getError("err.require.poly", body);
			//throw new LispException("number required");
		}
	}

}
