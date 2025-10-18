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
package net.morilib.range.integer;

import net.morilib.range.CharSetException;
import net.morilib.range.CharSets;
import net.morilib.range.CharSets.CharSetHandler;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/09
 */
public class IntCharSets {

	/**
	 * 
	 */
	public static final IntInterval ALL_CHAR =
		new IntInterval(0, Character.MAX_VALUE);

	//
	private static IntInterval getintv(int ch, int ch2) {
		return new IntInterval(ch, ch2);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static IntRange parse(CharSequence s) {
		boolean r;
		final IntRange[] ra = new IntRange[1];

		ra[0] = IntRange.O;
		r = CharSets.parse(s, new CharSetHandler() {

			public void singleChar(int ch) {
				ra[0] = ra[0].join(getintv(ch, ch));
			}

			public void rangedChar(int cb, int ce) {
				ra[0] = ra[0].join(getintv(cb, ce));
			}

		});

		if(!r) {
			throw new CharSetException();
		}
		return ra[0];
	}

}
