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

import java.awt.Adjustable;
import java.awt.Component;
import java.awt.event.AdjustmentListener;
import java.util.List;

import javax.swing.JComponent;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.swing.listener.AdjustmentListenable;
import net.morilib.util.swing.JScrollBar2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/21
 */
public class LispScrollBar extends LightweightGUIElement
implements ILispComponent, AdjustmentListenable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public abstract static class MakeScrollBar extends Subr {

		/**
		 * @return
		 */
		protected abstract int getOrientation();

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			int value, extent, min, max;
			double dmin = Double.NaN, dmax = Double.NaN;

			if(l.size() == 6) {
				dmin = SubrUtils.getDouble(l.get(4), mesg);
				dmax = SubrUtils.getDouble(l.get(5), mesg);
			} else if(l.size() != 4) {
				throw mesg.getError("err.argument", body);
			}
			value  = SubrUtils.getSmallInt(l.get(0), mesg);
			extent = SubrUtils.getSmallInt(l.get(1), mesg);
			min    = SubrUtils.getSmallInt(l.get(2), mesg);
			max    = SubrUtils.getSmallInt(l.get(3), mesg);
			if(max <= min) {
				throw mesg.getError("err.range.invalid");
			}
			return new LispScrollBar(new JScrollBar2(
					getOrientation(),
					value, extent, min, max, dmin, dmax));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class MakeHorizontalScrollBar extends MakeScrollBar {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispScrollBar.MakeScrollBar#getOrientation()
		 */
		@Override
		protected int getOrientation() {
			return Adjustable.HORIZONTAL;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class MakeVerticalScrollBar extends MakeScrollBar {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispScrollBar.MakeScrollBar#getOrientation()
		 */
		@Override
		protected int getOrientation() {
			return Adjustable.VERTICAL;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class GetScrollBarValue extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispScrollBar) {
				JScrollBar2 b = ((LispScrollBar)c1a).scrollbar;

				if(b.isRealValued()) {
					return new LispDouble(b.getRealValue());
				} else {
					return LispInteger.valueOf(b.getValue());
				}
			} else {
				throw mesg.getError(
						"err.swing.require.scrollbar", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class GetScrollBarRelativeValue extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispScrollBar) {
				return new LispDouble(
						((LispScrollBar)c1a)
						.scrollbar.getRelativeValue());
			} else {
				throw mesg.getError(
						"err.swing.require.scrollbar", c1a);
			}
		}

	}

	//
	private JScrollBar2 scrollbar;

	/**
	 * 
	 * @param bar
	 */
	public LispScrollBar(JScrollBar2 bar) {
		this.scrollbar = bar;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return scrollbar;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return scrollbar;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.AdjustmentListenable#addAdjustmentListener(java.awt.event.AdjustmentListener)
	 */
	public void addAdjustmentListener(AdjustmentListener l) {
		scrollbar.addAdjustmentListener(l);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<scroll-bar>");
	}

}
