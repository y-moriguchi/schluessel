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
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;

public class MakeRectangular extends BinaryArgs {

	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispNumber)) {
			throw mesg.getError("err.require.real", c1a);
			//throw new LispException("real number required");
		}
		if(!(c2a instanceof LispNumber)) {
			throw mesg.getError("err.require.real", c2a);
			//throw new LispException("real number required");
		}
		
		LispNumber n1 = (LispNumber)c1a;
		LispNumber n2 = (LispNumber)c2a;
		if(n1.isNaN() || n2.isNaN()) {
			return LispDouble.NaN;
		} else if(n1.isReal() && n2.isReal()) {
			return LispComplex.newComplex(
					n1.getRealDouble(), n2.getRealDouble());
		} else if(!n1.isReal()) {
			throw mesg.getError("err.require.real", n1);
		} else {
			throw mesg.getError("err.require.real", n2);
		}
		//throw new LispException("real number required");
	}

}
