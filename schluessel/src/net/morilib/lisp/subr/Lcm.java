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
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public class Lcm extends Subr {

	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() < 1) {
			return LispInteger.ONE;
		} else if(!(lst.get(0) instanceof LispReal)) {
			throw mesg.getError("err.require.int", lst.get(0));
			//throw new LispException("real number required");
		}
		
		LispReal res = (LispReal)lst.get(0);
		
		if(!res.isInteger()) {
			throw mesg.getError("err.require.int", res);
			//throw new LispException("integer required");
		} else if(res.isEqualTo(LispInteger.ZERO)) {
			return res;
		}
		
		for(int i = 1; i < lst.size(); i++) {
			Datum d = lst.get(i);
			
			if(d instanceof LispReal) {
				LispReal d2 = (LispReal)d;
				BigInteger a, b, r;
				
				if(!d2.isInteger()) {
					throw mesg.getError("err.require.int", d2);
					//throw new LispException("integer required");
				}
				
				a = res.getBigInteger();
				b = d2.getBigInteger();
				if(b.equals(BigInteger.ZERO)) {
					if(res.isExact() && d2.isExact()) {
						return LispInteger.ZERO;
					} else {
						return new LispDouble(0.0);
					}
				} else {
					r = a.abs().multiply(b.abs()).divide(a.gcd(b));
					if(res.isExact() && d2.isExact()) {
						res = LispInteger.valueOf(r);
					} else {
						res = new LispDouble(r.doubleValue());
					}
				}
			} else {
				throw mesg.getError("err.require.int", d);
				//throw new LispException("integer required");
			}
		}
		return res;
	}

}
