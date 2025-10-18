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
package net.morilib.lisp.swing.progress;

import javax.swing.JProgressBar;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.LispSwing;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/23
 */
public class MakeProgressBar extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		JProgressBar bar;
		Datum d1 = Iterators.nextIf(itr);
		Datum d2 = Iterators.nextIf(itr);
		Datum d3 = Iterators.nextIf(itr);
		int orient, min, max;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d1 instanceof Symbol) {
			orient = LispSwing.getOrientation(d1, mesg);
			if(d2 == null && d3 == null) {
				bar = new JProgressBar(orient);
			} else if(d2 != null && d3 != null) {
				max = SubrUtils.getSmallInt(d2, mesg);
				min = SubrUtils.getSmallInt(d3, mesg);
				if(min > max) {
					throw mesg.getError("err.range.invalid");
				}
				bar = new JProgressBar(orient, min, max);
			} else {
				throw mesg.getError("err.argument", body);
			}
		} else if(d3 != null) {
			throw mesg.getError("err.argument", body);
		} else if(d1 == null) {
			bar = new JProgressBar();
		} else if(d2 != null) {
			max = SubrUtils.getSmallInt(d1, mesg);
			min = SubrUtils.getSmallInt(d2, mesg);
			if(min > max) {
				throw mesg.getError("err.range.invalid");
			}
			bar = new JProgressBar(min, max);
		} else {
			throw mesg.getError("err.argument", body);
		}
		return new LispProgressBar(bar);
	}

}
