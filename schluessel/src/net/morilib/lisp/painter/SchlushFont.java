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

import java.awt.Font;
import java.awt.geom.AffineTransform;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispString;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/03
 */
public class SchlushFont extends Datum {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/03
	 */
	public static class Style extends Datum {

		//
		private int style;

		//
		private Style(int style) {
			this.style = style;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<font-style>");
		}

	}

	/**
	 * 
	 */
	public static final Style PLAIN = new Style(Font.PLAIN);

	/**
	 * 
	 */
	public static final Style BOLD = new Style(Font.BOLD);

	/**
	 * 
	 */
	public static final Style ITALIC = new Style(Font.ITALIC);

	/**
	 * 
	 */
	public static final Style BOLD_ITALIC =
		new Style(Font.BOLD | Font.ITALIC);

	/**
	 * 
	 */
	public LispString SERIF = new LispString("Serif");

	/**
	 * 
	 */
	public LispString SANS_SERIF = new LispString("SansSerif");

	/**
	 * 
	 */
	public LispString MONOSPACED = new LispString("Monospaced");

	/**
	 * 
	 */
	public LispString DIALOG = new LispString("Dialog");

	/**
	 * 
	 */
	public LispString DIALOG_INPUT = new LispString("DialogInput");

	//
	/*package*/ Font font;

	/**
	 * 
	 * @param name
	 * @param style
	 * @param size
	 */
	public SchlushFont(String name, Style style, int size) {
		this.font = new Font(name, style.style, size);
	}

	/**
	 * 
	 * @param font
	 */
	public SchlushFont(Font font) {
		this.font = font;
	}

	/**
	 * 
	 * @param name
	 * @return
	 */
	public static SchlushFont getDefaultFont(String name) {
		return new SchlushFont(Font.getFont(name));
	}

	/**
	 * 
	 * @param af
	 * @return
	 */
	public SchlushFont transformFont(AffineTransform af) {
		return new SchlushFont(font.deriveFont(af));
	}

	/**
	 * 
	 * @param theta
	 * @return
	 */
	public SchlushFont rotateFont(double theta) {
		return transformFont(AffineTransform.getRotateInstance(theta));
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public SchlushFont deriveFont(float size) {
		return new SchlushFont(font.deriveFont(size));
	}

	/**
	 * 
	 * @param style
	 * @return
	 */
	public SchlushFont deriveFont(Style style) {
		return new SchlushFont(font.deriveFont(style.style));
	}

	/**
	 * 
	 * @param style
	 * @param size
	 * @return
	 */
	public SchlushFont deriveFont(Style style, float size) {
		return new SchlushFont(
				font.deriveFont(style.style, size));
	}

	/**
	 * 
	 * @return
	 */
	public boolean isBold() {
		return font.isBold();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isItalic() {
		return font.isItalic();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isPlain() {
		return font.isPlain();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return font.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof SchlushFont) {
			return font.equals(((SchlushFont)obj).font);
		}
		return false;
	}

}
