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
import net.morilib.lisp.painter.drawer.CoordinateMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public class MakeWindowFrame extends Subr {
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		
		if(l.size() != 2 && l.size() != 6) {
			throw mesg.getError("err.argument", body);
		} else if(l.size() == 2) {
			LispUtils.checkReal(l, 0, mesg);
			LispUtils.checkReal(l, 1, mesg);
		} else {
			LispUtils.checkReal(l, 0, mesg);
			LispUtils.checkReal(l, 1, mesg);
			LispUtils.checkReal(l, 2, mesg);
			LispUtils.checkReal(l, 3, mesg);
			LispUtils.checkReal(l, 4, mesg);
			LispUtils.checkReal(l, 5, mesg);
		}
		
		int x, y;
		x = l.get(0).getInt();
		y = l.get(1).getInt();
		
		if(l.size() == 2) {
			return new SchlushWindowFrame(x, y);
		} else {
			double x1 = l.get(2).getRealDouble();
			double y1 = l.get(3).getRealDouble();
			double x2 = l.get(4).getRealDouble();
			double y2 = l.get(5).getRealDouble();
			
			return new SchlushWindowFrame(
					x, y,
					new CoordinateMap(x1, y1, x2, y2));
		}
	}

}
