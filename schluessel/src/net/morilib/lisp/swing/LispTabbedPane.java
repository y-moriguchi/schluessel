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
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeListener;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispJavaUtils;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.swing.listener.ChangeListenable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/26
 */
public class LispTabbedPane extends LightweightGUIElement
implements ILispComponent, ChangeListenable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/26
	 */
	public static class MakeTabbedPane extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			JTabbedPane pne = new JTabbedPane();
			List<Datum> lst = new ArrayList<Datum>();

			while(itr.hasNext()) {
				Datum d = itr.next();

				if(d instanceof Cons) {
					Cons c = (Cons)d;
					ILispComponent lc;

					if(!(c.getCdr() instanceof ILispComponent)) {
						throw mesg.getError(
								"err.swing.require.component", d);
					}
					lc = (ILispComponent)c.getCdr();
					pne.add(SubrUtils.getString(c.getCar(), mesg),
							lc.getComponent());
					lst.add((Datum)lc);
				} else {
					throw mesg.getError("err.require.pair", d);
				}
			}
			return new LispTabbedPane(pne, lst);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/26
	 */
	public static class GetSelectedTab extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispTabbedPane) {
				int i = ((LispTabbedPane)c1a).tabs.getSelectedIndex();

				return ((LispTabbedPane)c1a).tabslist.get(i);
			} else {
				throw mesg.getError(
						"err.swing.require.tabbedpane", c1a);
			}
		}

	}

	//
	private JTabbedPane tabs;
	private List<Datum> tabslist;

	/**
	 * 
	 * @param tabs
	 * @param tabslist
	 */
	public LispTabbedPane(
			JTabbedPane tabs, List<Datum> tabslist) {
		this.tabs = tabs;
		this.tabslist = tabslist;
	}

	/**
	 * 
	 * @param tabs
	 */
	public LispTabbedPane(JTabbedPane tabs) {
		this.tabs = tabs;
		this.tabslist = new ArrayList<Datum>();
		for(int i = 0; i < tabs.getTabCount(); i++) {
			this.tabslist.add(LispJavaUtils.newInstance(
					tabs.getTabComponentAt(i)));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return tabs;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return tabs;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ChangeListenable#addChangeListener(java.awt.event.ActionListener)
	 */
	public void addChangeListener(ChangeListener listener) {
		tabs.addChangeListener(listener);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<tabbed-pane>");
	}

}
