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
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.MouseListener;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.painter.SchlushColor;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/18
 */
public abstract class GUIElement extends Datum2
implements JavaObjective {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class AddMouseListenerS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			if(body instanceof Cons) {
				Cons c = (Cons)body;

				if(c.getCar() instanceof GUIElement) {
					MouseListener l;

					l = LispSwing.createMouseListener(
							c.getCdr(), body, env, mesg);
					((GUIElement)c.getCar()).addMouseListener(l);
					return Undef.UNDEF;
				} else {
					throw mesg.getError(
							"err.swing.require.guielement",
							c.getCar());
				}
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetGuiElementWidth extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof GUIElement) {
				return LispInteger.valueOf(
						((GUIElement)c1a)
						.getAWTComponent().getWidth());
			} else {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetGuiElementHeight extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof GUIElement) {
				return LispInteger.valueOf(
						((GUIElement)c1a)
						.getAWTComponent().getHeight());
			} else {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class SetBackgroundColorS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof GUIElement)) {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			} else if(!(c2a instanceof SchlushColor)) {
				throw mesg.getError("err.require.color", c2a);
			}

			((GUIElement)c1a).setBackground(
					((SchlushColor)c2a).getColor());
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class SetPreferredWidthS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof GUIElement)) {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}

			((GUIElement)c1a).setPreferredWidth(
					SubrUtils.getSmallInt(c2a, mesg));
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class SetPreferredHeightS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof GUIElement)) {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}

			((GUIElement)c1a).setPreferredHeight(
					SubrUtils.getSmallInt(c2a, mesg));
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/26
	 */
	public static class GetBoundsX extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof GUIElement) {
				return LispInteger.valueOf(
						((GUIElement)c1a).getBoundsX());
			} else {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/26
	 */
	public static class GetBoundsY extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof GUIElement) {
				return LispInteger.valueOf(
						((GUIElement)c1a).getBoundsY());
			} else {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class SetBoundsXS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof GUIElement)) {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}

			((GUIElement)c1a).setBoundsX(
					SubrUtils.getSmallInt(c2a, mesg));
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/21
	 */
	public static class SetBoundsYS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof GUIElement)) {
				throw mesg.getError(
						"err.swing.require.guielement", c1a);
			}

			((GUIElement)c1a).setBoundsY(
					SubrUtils.getSmallInt(c2a, mesg));
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 * @return
	 */
	public abstract Component getAWTComponent();

	/**
	 * 
	 * @param l
	 */
	public void addMouseListener(MouseListener l) {
		getAWTComponent().addMouseListener(l);
	}

	/**
	 * 
	 * @param c
	 */
	public void setBackground(Color c) {
		getAWTComponent().setBackground(c);
	}

	/**
	 * 
	 * @param x
	 */
	public void setPreferredWidth(int x) {
		Component c = getAWTComponent();

		c.setPreferredSize(new Dimension(
				x, c.getPreferredSize().height));
	}

	/**
	 * 
	 * @param y
	 */
	public void setPreferredHeight(int y) {
		Component c = getAWTComponent();

		c.setPreferredSize(new Dimension(
				c.getPreferredSize().width, y));
	}

	/**
	 * 
	 * @return
	 */
	public int getBoundsX() {
		return getAWTComponent().getBounds().x;
	}

	/**
	 * 
	 * @return
	 */
	public int getBoundsY() {
		return getAWTComponent().getBounds().y;
	}

	/**
	 * 
	 * @param x
	 */
	public void setBoundsX(int x) {
		Component c = getAWTComponent();

		c.setBounds(new Rectangle(x, c.getBounds().y));
	}

	/**
	 * 
	 * @param y
	 */
	public void setBoundsY(int y) {
		Component c = getAWTComponent();

		c.setBounds(new Rectangle(c.getBounds().x, y));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return getAWTComponent();
	}

}
