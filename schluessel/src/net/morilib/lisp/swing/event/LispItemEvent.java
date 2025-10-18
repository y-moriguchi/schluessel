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

import java.awt.event.ItemEvent;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.swing.Selectable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/21
 */
public class LispItemEvent extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class GetEventSelectedItemValue extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispItemEvent) {
				Selectable.Item itm;

				itm = (Selectable.Item)(
						((LispItemEvent)c1a).event.getItem());
				return itm.getValue();
			} else {
				throw mesg.getError(
						"err.swing.require.event.item", c1a);
			}
		}

	}

	//
	private ItemEvent event;

	/**
	 * @param e
	 */
	public LispItemEvent(ItemEvent e) {
		this.event = e;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<item-event>");
	}

}
