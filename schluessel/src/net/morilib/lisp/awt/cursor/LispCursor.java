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
package net.morilib.lisp.awt.cursor;

import java.awt.AWTException;
import java.awt.Cursor;
import java.awt.HeadlessException;
import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/07
 */
public class LispCursor extends Datum2 {

	//
	private static final Map<String, LispCursor> TYPES;

	//
	static {
		TYPES = new HashMap<String, LispCursor>();
		TYPES.put("crosshair-cursor",
				LispCursor.getInstance(Cursor.CROSSHAIR_CURSOR));
		TYPES.put("default-cursor",
				LispCursor.getInstance(Cursor.DEFAULT_CURSOR));
		TYPES.put("e-resize-cursor",
				LispCursor.getInstance(Cursor.E_RESIZE_CURSOR));
		TYPES.put("hand-cursor",
				LispCursor.getInstance(Cursor.HAND_CURSOR));
		TYPES.put("move-cursor",
				LispCursor.getInstance(Cursor.MOVE_CURSOR));
		TYPES.put("n-resize-cursor",
				LispCursor.getInstance(Cursor.N_RESIZE_CURSOR));
		TYPES.put("ne-resize-cursor",
				LispCursor.getInstance(Cursor.NE_RESIZE_CURSOR));
		TYPES.put("nw-resize-cursor",
				LispCursor.getInstance(Cursor.NW_RESIZE_CURSOR));
		TYPES.put("s-resize-cursor",
				LispCursor.getInstance(Cursor.S_RESIZE_CURSOR));
		TYPES.put("se-resize-cursor",
				LispCursor.getInstance(Cursor.SE_RESIZE_CURSOR));
		TYPES.put("sw-resize-cursor",
				LispCursor.getInstance(Cursor.SW_RESIZE_CURSOR));
		TYPES.put("text-cursor",
				LispCursor.getInstance(Cursor.TEXT_CURSOR));
		TYPES.put("w-resize-cursor",
				LispCursor.getInstance(Cursor.W_RESIZE_CURSOR));
		TYPES.put("wait-cursor",
				LispCursor.getInstance(Cursor.WAIT_CURSOR));
	}

	//
	Cursor cursor;

	/**
	 * 
	 * @param c
	 */
	protected LispCursor(Cursor c) {
		this.cursor = c;
	}

	/**
	 * 
	 * @param type
	 */
	public static LispCursor getInstance(int type) {
		return new LispCursor(Cursor.getPredefinedCursor(type));
	}

	/**
	 * 
	 * @param sym
	 * @return
	 * @throws AWTException 
	 * @throws HeadlessException 
	 */
	public static LispCursor getInstance(
			String s) throws HeadlessException, AWTException {
		LispCursor c;

		if((c = TYPES.get(s)) != null) {
			return c;
		} else {
			return new LispCursor(Cursor.getSystemCustomCursor(s));
		}
	}

	/**
	 * 
	 * @param type
	 */
	public static LispCursor getDefault() {
		return new LispCursor(Cursor.getDefaultCursor());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<cursor ").append(cursor).append(">");
	}

}
