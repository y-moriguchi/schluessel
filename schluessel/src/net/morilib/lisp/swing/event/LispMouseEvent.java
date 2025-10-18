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

import java.awt.event.MouseEvent;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/20
 */
public class LispMouseEvent extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static abstract class MouseSubr extends UnaryArgs {

		/**
		 * 
		 * @param e
		 * @return
		 */
		protected abstract Datum execute(MouseEvent e);

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispMouseEvent) {
				return execute(((LispMouseEvent)c1a).event);
			} else {
				throw mesg.getError(
						"err.swing.require.event.mouse", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseButton extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			switch(e.getButton()) {
			case MouseEvent.NOBUTTON:
				return Symbol.getSymbol("no-button");
			case MouseEvent.BUTTON1:
				return Symbol.getSymbol("button1");
			case MouseEvent.BUTTON2:
				return Symbol.getSymbol("button2");
			case MouseEvent.BUTTON3:
				return Symbol.getSymbol("button3");
			default:
				return Symbol.getSymbol("unknown-button");
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseClickCount extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return LispInteger.valueOf(e.getClickCount());
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseX extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return LispInteger.valueOf(e.getX());
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseY extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return LispInteger.valueOf(e.getY());
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseXEtY extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return MultiValues.newValues(
					LispInteger.valueOf(e.getX()),
					LispInteger.valueOf(e.getY()));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseXOnScreen extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return LispInteger.valueOf(e.getXOnScreen());
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseYOnScreen extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return LispInteger.valueOf(e.getYOnScreen());
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetMouseXEtYOnScreen extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return MultiValues.newValues(
					LispInteger.valueOf(e.getXOnScreen()),
					LispInteger.valueOf(e.getYOnScreen()));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class IsPopupTrigger extends MouseSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.event.LispMouseEvent.MouseSubr#execute(java.awt.event.MouseEvent)
		 */
		@Override
		protected Datum execute(MouseEvent e) {
			return LispBoolean.getInstance(e.isPopupTrigger());
		}

	}

	//
	private MouseEvent event;

	/**
	 * @param e
	 */
	public LispMouseEvent(MouseEvent e) {
		this.event = e;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<mouse-event>");
	}

}
