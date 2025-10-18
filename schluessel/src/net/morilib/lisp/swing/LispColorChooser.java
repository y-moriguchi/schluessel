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
package net.morilib.lisp.swing;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JColorChooser;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.painter.SchlushColor;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/07
 */
public class LispColorChooser extends Datum2 implements JavaObjective {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/04/07
	 */
	public static class ShowColorChooser extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Component cmp;
			String tt;
			Datum clr, par;
			Color r, cl2;

			tt  = SubrUtils.nextString(itr, mesg, body);
			if(!((clr = Iterators.nextIf(itr))
					instanceof SchlushColor) && clr != null) {
				throw mesg.getError("err.require.color", clr);
			} else if(!((par = Iterators.nextIf(itr))
					instanceof ILispComponent) && par != null) {
				throw mesg.getError("err.swing.require.component",
						par);
			} else {
				cl2 = (clr != null) ?
						((SchlushColor)clr).getColor() : null;
				cmp = (par != null) ?
						((ILispComponent)par).getComponent() : null;
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			r = JColorChooser.showDialog(cmp, tt, cl2);
			return new SchlushColor(r);
		}

	}

	//
	JColorChooser chooser;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	@Override
	public JColorChooser toObject() {
		return chooser;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<color-chooser>");
	}

}
