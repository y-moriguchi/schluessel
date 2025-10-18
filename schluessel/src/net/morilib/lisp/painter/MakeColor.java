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
package net.morilib.lisp.painter;

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public class MakeColor extends Subr {

	//
	private void checkRange(double x, LispMessage mesg) {
		if(x < 0.0 || x > 1.0) {
			throw mesg.getError("err.range.0to1", Double.toString(x));
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		
		if(l.size() == 3) {
			LispUtils.checkReal(l.get(0), mesg);
			LispUtils.checkReal(l.get(1), mesg);
			LispUtils.checkReal(l.get(2), mesg);
		} else if(l.size() == 4) {
			LispUtils.checkReal(l.get(0), mesg);
			LispUtils.checkReal(l.get(1), mesg);
			LispUtils.checkReal(l.get(2), mesg);
			LispUtils.checkReal(l.get(3), mesg);
		} else {
			throw mesg.getError("err.argument", body);
		}
		
		//
		double r = l.get(0).getRealDouble();
		double g = l.get(1).getRealDouble();
		double b = l.get(2).getRealDouble();
		
		checkRange(r, mesg);
		checkRange(g, mesg);
		checkRange(b, mesg);
		if(l.size() == 3) {
			return new SchlushColor(r, g, b);
		} else {
			double a = l.get(3).getRealDouble();
			
			checkRange(a, mesg);
			return new SchlushColor(r, g, b, a);
		}
	}

}
