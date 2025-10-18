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

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;

public class IsOdd extends UnaryArgs {
	
	private static final BigInteger TWO = BigInteger.valueOf(2);
	
	@Override
	protected Datum execute(Datum c1a, Environment env, LispMessage mesg) {
		if(c1a instanceof LispNumber) {
			LispNumber n = (LispNumber)c1a;
			
			if(!n.isInteger()) {
				throw mesg.getError("err.require.int", n);
				//throw new LispException("integer required");
			} else if(n instanceof LispReal) {
				BigInteger v = n.getBigInteger();
				
				return LispBoolean.getInstance(
						v.mod(TWO).equals(BigInteger.ONE));
			} else {
				throw mesg.getError("err.require.int", n);
				//throw new LispException("integer required");
			}
		}
		throw mesg.getError("err.require.int", c1a);
		//throw new LispException("integer required");
	}

}
