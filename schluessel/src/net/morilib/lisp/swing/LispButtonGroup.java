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

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Pair;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/19
 */
public class LispButtonGroup extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class MakeButtonGroup extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			LispButtonGroup g = new LispButtonGroup();
			ConsIterator itr = new ConsIterator(c1a);

			while(itr.hasNext()) {
				Datum d = itr.next();

				if(d instanceof Cons) {
					Cons c = (Cons)d;

					if(c.getCar() instanceof ILispButton) {
						g.put(((ILispButton)c.getCar()).getComponent(),
								c.getCdr());
					} else {
						throw mesg.getError(
								"err.swing.require.button",
								c.getCar());
					}
				} else {
					throw mesg.getError("err.require.pair", d);
				}
			}

			if(!itr.getTerminal().isNil()) {
				throw mesg.getError("err.list", c1a);
			}
			return g;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class GetSelectedButtonValue extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispButtonGroup) {
				return ((LispButtonGroup)c1a).getSelected();
			} else {
				throw mesg.getError(
						"err.swing.require.buttongroup", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class SetSelectedButtonValueS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispButtonGroup) {
				((LispButtonGroup)c1a).setSelected(c2a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.buttongroup", c1a);
			}
		}

	}

	//
	private ButtonGroup group;
	private OneToOneSet<AbstractButton, Datum> map =
		new HashOneToOneSet<AbstractButton, Datum>();

	//
	private LispButtonGroup() {
		this.group = new ButtonGroup();
	}

	//
	private void put(AbstractButton button, Datum d) {
		group.add(button);
		map.put(button, d);
	}

	/**
	 * 
	 * @return
	 */
	public Datum getSelected() {
		for(Pair<AbstractButton, Datum> d : map) {
			if(d.getA().isSelected()) {
				return d.getB();
			}
		}
		return LispBoolean.FALSE;
	}

	/**
	 * 
	 * @param d
	 */
	public void setSelected(Datum d) {
		AbstractButton b = map.getKey(d);

		if(b != null) {
			b.setSelected(true);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<button-group>");
	}

}
