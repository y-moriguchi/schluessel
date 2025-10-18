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
package net.morilib.lisp.painter;

import java.awt.Image;
import java.awt.geom.Rectangle2D;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.painter.drawer.CoordinateMap;
import net.morilib.lisp.painter.drawer.Drawer;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/19
 */
public interface SchlushFrame {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010/12/19
	 */
	public static class ImageInfo {

		//
		/*package*/ Image img;
		/*package*/ double x, y;
		/*package*/ double sx, sy;

		public ImageInfo(
				Image img, double x, double y, double sx, double sy) {
			this.img = img;
			this.x   = x;
			this.y   = y;
			this.sx  = sx;
			this.sy  = sy;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetFrameCoordinateX extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof SchlushFrame)) {
				throw mesg.getError("err.require.frame", c1a);
			} else if(!(c2a instanceof LispReal)) {
				throw mesg.getError("err.require.real", c2a);
			}

			return new LispDouble(
					((SchlushFrame)c1a).invertX(c2a.getInt()));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/20
	 */
	public static class GetFrameCoordinateY extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof SchlushFrame)) {
				throw mesg.getError("err.require.frame", c1a);
			} else if(!(c2a instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", c2a);
			}

			return new LispDouble(
					((SchlushFrame)c1a).invertY(c2a.getInt()));
		}

	}

	/**
	 * 
	 * @param img
	 * @throws InterruptedException 
	 */
	public void addImage(ImageInfo img) throws InterruptedException;

	/**
	 * 
	 * @param drawer
	 */
	public void addDrawer(Drawer drawer);

	/**
	 * 
	 * @param drawer
	 */
	public void addBackgroundDrawer(Drawer drawer);

	/**
	 * 
	 * @return
	 */
	public int getImageWidth(Image img);

	/**
	 * 
	 * @return
	 */
	public int getImageHeight(Image img);

	/**
	 * 
	 */
	public void clearScreen();

	/**
	 * 
	 * @return
	 */
	public CoordinateMap getCoordinate();

	/**
	 * 
	 * @return
	 */
	public int getHeight();

	/**
	 * 
	 * @return
	 */
	public int getWidth();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double transformX(double x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double transformY(double y);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double invertX(int x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double invertY(int y);

	/**
	 * 
	 * @param str
	 * @param font
	 * @return
	 */
	public Rectangle2D calculateFontBounds(
			String str, SchlushFont font);

	/**
	 * 
	 */
	public void repaint();

}
