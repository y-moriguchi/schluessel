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
package net.morilib.lisp.swing.event;

import java.awt.Adjustable;
import java.awt.event.AdjustmentEvent;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.swing.Adjustable2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/21
 */
public class LispAdjustmentEvent extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class GetEventAdjustmentValue extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispAdjustmentEvent) {
				LispAdjustmentEvent el = (LispAdjustmentEvent)c1a;
				Adjustable a = el.event.getAdjustable();

				if(a instanceof Adjustable2) {
					return new LispDouble(
							((Adjustable2)a).getRealValue());
				} else {
					return LispInteger.valueOf(a.getValue());
				}
			} else {
				throw mesg.getError(
						"err.swing.require.event.adjustment", c1a);
			}
		}

	}

	//
	private AdjustmentEvent event;

	/**
	 * @param e
	 */
	public LispAdjustmentEvent(AdjustmentEvent e) {
		this.event = e;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<adjustment-event>");
	}

}
