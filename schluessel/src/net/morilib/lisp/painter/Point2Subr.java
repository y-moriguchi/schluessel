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
import net.morilib.lisp.Undef;
import net.morilib.lisp.painter.drawer.Drawer;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public abstract class Point2Subr extends Subr {

	/**
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 */
	public abstract Drawer getDrawer(
			double x1, double y1, double x2, double y2);
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		
		if(l.size() == 5) {
			if(!(l.get(0) instanceof SchlushFrame)) {
				throw mesg.getError("err.require.frame", l.get(0));
			}
			LispUtils.checkReal(l, 1, mesg);
			LispUtils.checkReal(l, 2, mesg);
			LispUtils.checkReal(l, 3, mesg);
			LispUtils.checkReal(l, 4, mesg);
		} else {
			throw mesg.getError("err.argument", body);
		}
		
		//
		SchlushFrame f = (SchlushFrame)l.get(0);
		
		f.addDrawer(getDrawer(
				l.get(1).getRealDouble(),
				l.get(2).getRealDouble(),
				l.get(3).getRealDouble(),
				l.get(4).getRealDouble()));
		return Undef.UNDEF;
	}

}
