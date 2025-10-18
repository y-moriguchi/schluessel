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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public interface Selectable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static final class Item {

		//
		private Datum datum, disp;

		/**
		 * 
		 * @param d
		 */
		public Item(Datum d) {
			this.datum = d;
			this.disp  = null;
		}

		/**
		 * 
		 * @param d
		 * @param disp
		 */
		public Item(Datum d, Datum disp) {
			this.datum = d;
			this.disp  = disp;
		}

		/**
		 * 
		 * @return
		 */
		public Datum getValue() {
			return datum;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			return LispUtils.print((disp == null) ? datum : disp);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/14
	 */
	public static class GetSelectedItemValue extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Selectable) {
				return ((Selectable)c1a).getCurrentItem().getValue();
			} else {
				throw mesg.getError(
						"err.swing.require.selectable", c1a);
			}
		}

	}

	/**
	 * 
	 * @return
	 */
	public Item getCurrentItem();

	/**
	 * 
	 * @return
	 */
	public Item[] getItems();

	/**
	 * 
	 * @param data
	 */
	public void setItems(Item... data);

}
