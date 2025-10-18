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

import java.awt.Component;

import javax.swing.JComponent;
import javax.swing.JLayeredPane;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/26
 */
public class LispLayeredPane extends LightweightGUIElement
implements ILispComponent {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/26
	 */
	public static class MakeLayeredPane extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr;
			JLayeredPane pne = new JLayeredPane();
			int no = 1, i = 0;

			itr = new ConsIterator(body);
			while(itr.hasNext()) {
				itr.next();
				no++;
			}

			itr = new ConsIterator(body);
			while(itr.hasNext()) {
				Datum d = itr.next();

				if(d instanceof ILispComponent) {
					JComponent c = ((ILispComponent)d).getComponent();

					c.setBounds(0, 0, 512, 512);
					pne.add(c, no - i);
					i++;
				} else {
					throw mesg.getError(
							"err.swing.require.component", d);
				}
			}
			return new LispLayeredPane(pne);
		}

	}

	//
	private JLayeredPane layers;

	/**
	 * 
	 * @param layers
	 */
	public LispLayeredPane(JLayeredPane layers) {
		this.layers  = layers;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return layers;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return layers;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<layered-pane>");
	}

}
