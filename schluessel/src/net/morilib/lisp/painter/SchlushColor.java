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

import java.awt.Color;

import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public class SchlushColor extends Datum {

	/**
	 * 
	 */
	public static final SchlushColor BLACK =
		new SchlushColor(Color.BLACK);

	/**
	 * 
	 */
	public static final SchlushColor BLUE =
		new SchlushColor(Color.BLUE);

	/**
	 * 
	 */
	public static final SchlushColor CYAN =
		new SchlushColor(Color.CYAN);

	/**
	 * 
	 */
	public static final SchlushColor DARK_GRAY =
		new SchlushColor(Color.DARK_GRAY);

	/**
	 * 
	 */
	public static final SchlushColor GRAY =
		new SchlushColor(Color.GRAY);

	/**
	 * 
	 */
	public static final SchlushColor GREEN =
		new SchlushColor(Color.GREEN);

	/**
	 * 
	 */
	public static final SchlushColor LIGHT_GRAY =
		new SchlushColor(Color.LIGHT_GRAY);

	/**
	 * 
	 */
	public static final SchlushColor MAGENTA =
		new SchlushColor(Color.MAGENTA);

	/**
	 * 
	 */
	public static final SchlushColor ORANGE =
		new SchlushColor(Color.ORANGE);

	/**
	 * 
	 */
	public static final SchlushColor PINK =
		new SchlushColor(Color.PINK);

	/**
	 * 
	 */
	public static final SchlushColor RED =
		new SchlushColor(Color.RED);

	/**
	 * 
	 */
	public static final SchlushColor WHITE =
		new SchlushColor(Color.WHITE);

	/**
	 * 
	 */
	public static final SchlushColor YELLOW =
		new SchlushColor(Color.YELLOW);

	//
	/*package*/ Color color;

	/**
	 * 
	 * @param r
	 * @param g
	 * @param b
	 */
	public SchlushColor(double r, double g, double b) {
		color = new Color((float)r, (float)g, (float)b);
	}

	/**
	 * 
	 * @param r
	 * @param g
	 * @param b
	 * @param alpha
	 */
	public SchlushColor(double r, double g, double b, double alpha) {
		color = new Color((float)r, (float)g, (float)b, (float)alpha);
	}

	/**
	 * 
	 * @param c
	 */
	public SchlushColor(Color c) {
		color = c;
	}

	/**
	 * @return the color
	 */
	public Color getColor() {
		return color;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<color>");
	}

}
