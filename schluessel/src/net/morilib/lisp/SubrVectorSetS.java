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
package net.morilib.lisp;

import java.util.List;

public class SubrVectorSetS extends Subr {

	private void execute(
			Datum c1a, Datum c2a, Datum c3a, LispMessage mesg) {
		int idx;
		
		if(!(c2a instanceof LispSmallInt)) {
			//throw new LispException("small integer required");
			throw mesg.getError("err.require.smallint", c2a);
		}
		
		idx = ((LispSmallInt)c2a).getExactSmallInt();
		if(idx < 0) {
			//throw new LispException("argument out of range");
			throw mesg.getError("err.vector.outofrange", c2a);
		}
		
		if(c1a instanceof LispVector) {
			LispVector v = (LispVector)c1a;
			
			if(idx >= v.size()) {
				//throw new LispException("argument out of range");
				throw mesg.getError("err.vector.outofrange", c2a);
			}
			v.setS(idx, c3a);
		} else {
			//throw new LispException("vector required");
			throw mesg.getError("err.require.vector", c1a);
		}
	}
	
	
	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> b = LispUtils.consToList(body, mesg);
		
		if(b.size() != 3) {
			//throw new LispException("wrong number of arguments");
			throw mesg.getError("err.argument", symbolName);
		}
		execute(b.get(0), b.get(1), b.get(2), mesg);
		return Undef.UNDEF;
	}
	
}
