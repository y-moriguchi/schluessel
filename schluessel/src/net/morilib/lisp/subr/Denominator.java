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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.LispNumber;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Denominator extends UnaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(c1a instanceof LispNumber) {
			LispNumber n1 = (LispNumber)c1a;

			if(!n1.isReal()) {
				throw mesg.getError("err.require.real", n1);
				//throw new LispException("real number required");
			} else if(n1.isExact()) {
				return LispInteger.valueOf(
						n1.getReal().getDenominator());
			} else {
				try {
					return new LispDouble(
							n1.getReal().getDenominator().doubleValue());
				} catch(LispNotSupportedException e) {
					throw mesg.getError(e.getMessage(), c1a);
				}
			}
		}
		throw mesg.getError("err.require.real", c1a);
		//throw new LispException("real number required");
	}

}
