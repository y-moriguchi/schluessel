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

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public class Max extends Subr {

	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() < 1) {
			throw mesg.getError("err.argument", symbolName);
			//throw new LispException("more than 1 arguments required");
		} else if(!(lst.get(0) instanceof LispReal)) {
			throw mesg.getError("err.require.real", lst.get(0));
			//throw new LispException("real number required");
		}
		
		LispReal res = (LispReal)lst.get(0);
		for(int i = 1; i < lst.size(); i++) {
			Datum d = lst.get(i);
			
			if(d instanceof LispReal) {
				LispReal d2 = (LispReal)d;
				
				if(res.isNaN()) {
					res = d2;
				} else if(d2.isNaN()) {
					// do nothing
				} else if(!res.isExact()) {
					LispReal d3 = (LispReal)d2.toInexact();
					
					res = res.isMoreThan(d2) ? res : d3;
				} else if(!d2.isExact()) {
					res = (LispReal)res.toInexact();
					
					res = res.isMoreThan(d2) ? res : d2;
				} else {
					res = res.isMoreThan(d2) ? res : d2;
				}
			} else {
				throw mesg.getError("err.require.real", d);
				//throw new LispException("real number required");
			}
		}
		return res;
	}

}
