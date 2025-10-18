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
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;

import javax.swing.GroupLayout;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/07
 */
public class LayoutGroup extends BinaryArgs {

	//
	private static final Symbol VGAP = Symbol.getSymbol("vertical");
	private static final Symbol HGAP = Symbol.getSymbol("horizontal");

	//
	private GroupLayout.Group parseVGroup(Datum c2a, GroupLayout g,
			LispMessage m) {
		GroupLayout.Group gs = g.createSequentialGroup(), gp;
		ConsIterator itr, jtr;
		int cnt = -1, j;
		Datum d, g1, g2, g3, v;
		int i0, i1, i2, i3;

		itr = new ConsIterator(c2a);
		while(itr.hasNext()) {
			gp  = g.createParallelGroup();
			jtr = new ConsIterator(itr.next());
			for(j = 0; jtr.hasNext(); j++) {
				if((d = jtr.next()) instanceof ILispComponent) {
					gp.addComponent(
							((ILispComponent)d).getComponent());
				} else if(!(d instanceof LispVector)) {
					gp.addGroup(parseVGroup(d, g, m));
				} else {
					i0 = ((LispVector)d).getList().indexOf(VGAP);
					if(i0 < 0) {
						i1 = GroupLayout.PREFERRED_SIZE;
						i2 = GroupLayout.PREFERRED_SIZE;
						i3 = GroupLayout.PREFERRED_SIZE;
					} else {
						if(((LispVector)d).size() < i0 + 4) {
							throw m.getError(
									"err.swing.invalidlayout.group",
									c2a);
						} else {
							g1 = ((LispVector)d).get(i0 + 1);
							g2 = ((LispVector)d).get(i0 + 2);
							g3 = ((LispVector)d).get(i0 + 3);
							i1 = SubrUtils.getSmallInt(g1, m);
							i2 = SubrUtils.getSmallInt(g2, m);
							i3 = SubrUtils.getSmallInt(g3, m);
						}
					}

					v = ((LispVector)d).get(0);
					if(v instanceof Symbol) {
						if(i1 == GroupLayout.PREFERRED_SIZE) {
							throw m.getError(
									"err.swing.invalidlayout.group",
									c2a);
						}
						gp.addGap(i1, i2, i3);
					} else if(v instanceof ILispComponent) {
						gp.addComponent(
								((ILispComponent)v).getComponent(),
								i1, i2, i3);
					} else {
						throw m.getError(
								"err.swing.invalidlayout.group",
								c2a);
					}
				}
			}

			if(cnt < 0) {
				cnt = j;
			} else if(cnt != j) {
				throw m.getError("err.swing.invalidlayout.group",
						c2a);
			}
			gs.addGroup(gp);
			SubrUtils.checkProper(jtr, c2a, m);
		}
		SubrUtils.checkProper(itr, c2a, m);
		return gs;
	}

	//
	private GroupLayout.Group parseHGroup(Datum c2a, GroupLayout g,
			LispMessage m) {
		GroupLayout.Group gs = g.createSequentialGroup(), gp;
		ConsIterator itr, jtr;
		int cnt = -1, j;
		Datum d, g1, g2, g3, v;
		List<List<Object>> lst = new ArrayList<List<Object>>();
		int i0, i1, i2, i3;

		itr = new ConsIterator(c2a);
		while(itr.hasNext()) {
			jtr = new ConsIterator(itr.next());
			for(j = 0; jtr.hasNext(); j++) {
				if(cnt < 0)  lst.add(new ArrayList<Object>());
				if((d = jtr.next()) instanceof ILispComponent) {
					lst.get(j).add(((ILispComponent)d).getComponent());
				} else if(!(d instanceof LispVector)) {
					lst.get(j).add(parseHGroup(d, g, m));
				} else {
					lst.get(j).add(d);
				}
			}

			if(cnt < 0) {
				cnt = j;
			} else if(cnt != j) {
				throw m.getError("err.swing.invalidlayout.group",
						c2a);
			}
			SubrUtils.checkProper(jtr, c2a, m);
		}
		SubrUtils.checkProper(itr, c2a, m);

		for(List<Object> l1 : lst) {
			gp = g.createParallelGroup();
			for(Object l2 : l1) {
				if(l2 instanceof LispVector) {
					i0 = ((LispVector)l2).getList().indexOf(HGAP);
					if(i0 < 0) {
						i1 = GroupLayout.PREFERRED_SIZE;
						i2 = GroupLayout.PREFERRED_SIZE;
						i3 = GroupLayout.PREFERRED_SIZE;
					} else {
						if(((LispVector)l2).size() < i0 + 4) {
							throw m.getError(
									"err.swing.invalidlayout.group",
									c2a);
						} else {
							g1 = ((LispVector)l2).get(1);
							g2 = ((LispVector)l2).get(2);
							g3 = ((LispVector)l2).get(3);
							i1 = SubrUtils.getSmallInt(g1, m);
							i2 = SubrUtils.getSmallInt(g2, m);
							i3 = SubrUtils.getSmallInt(g3, m);
						}
					}

					v = ((LispVector)l2).get(0);
					if(v instanceof Symbol) {
						if(i1 == GroupLayout.PREFERRED_SIZE) {
							throw m.getError(
									"err.swing.invalidlayout.group",
									c2a);
						}
						gp.addGap(i1, i2, i3);
					} else if(v instanceof ILispComponent) {
						gp.addComponent(
								((ILispComponent)v).getComponent(),
								i1, i2, i3);
					} else {
						throw m.getError(
								"err.swing.invalidlayout.group",
								c2a);
					}
				} else if(l2 instanceof GroupLayout.Group) {
					gp.addGroup((GroupLayout.Group)l2);
				} else if(l2 instanceof Component) {
					gp.addComponent((Component)l2);
				} else {
					throw new RuntimeException();
				}
			}
			gs.addGroup(gp);
		}
		return gs;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		GroupLayout l;
		Container c;

		if(!(c1a instanceof ILispComposite)) {
			throw mesg.getError("err.swing.require.composite", c1a);
		} else {
			c = ((ILispComposite)c1a).getPane();
			l = new GroupLayout(c);
			c.setLayout(l);
			l.setVerticalGroup  (parseVGroup(c2a, l, mesg));
			l.setHorizontalGroup(parseHGroup(c2a, l, mesg));
			return Undef.UNDEF;
		}
	}

}
