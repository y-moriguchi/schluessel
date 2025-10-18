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
package net.morilib.util.io;

import java.util.ArrayList;
import java.util.List;

import net.morilib.range.CharSetException;
import net.morilib.range.CharSets;
import net.morilib.range.Range;
import net.morilib.util.bit.BitVector;
import net.morilib.util.bit.PackedBitVector;
import net.morilib.util.io.filter.WildcardSyntaxException;
import net.morilib.util.primitive.IntegerHashSet;
import net.morilib.util.primitive.IntegerSet;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/24
 */
public class Wildcard {

	//
	private Range[]   chars;
	private BitVector repeat;

	//
	private Wildcard(Range[] chars, BitVector repeat) {
		this.chars   = chars;
		this.repeat  = repeat;
	}

	/**
	 * 
	 * @param cs
	 * @return
	 */
	public boolean matches(CharSequence cs) {
		IntegerSet ss = new IntegerHashSet();
		IntegerSet ad = new IntegerHashSet();

		ss.add(0);
		for(int p = 0; p < cs.length(); p++) {
			IntegerIterator i = ss.intIterator();
			int c = cs.charAt(p);

			while(i.hasNext()) {
				int s = i.next();

				if(chars[s].contains(c)) {
					ad.add(s + 1);
				}
				if(!repeat.getBoolean(s)) {
					i.remove();
				}
			}

			ss.addAllInt(ad);
			ad.clear();
			if(ss.isEmpty()) {
				break;
			}
		}
		return (ss.contains(chars.length - 1));
	}

	/**
	 * 
	 * @param pt
	 * @return
	 * @throws WildcardSyntaxException 
	 */
	public static Wildcard compile(CharSequence pt) {
		List<Range> rl = new ArrayList<Range>();
		BitVector   rp = new PackedBitVector();
		boolean b = true;
		int c, p = 0;

		for(; p < pt.length(); p++) {
			switch(c = pt.charAt(p)) {
			case '*':
				if(b) {
					rp.add(true);
				}
				b = false;
				break;
			case '?':
				rl.add(CharSets.ALL_CHAR);
				if(b) {
					rp.add(false);
				}
				b = true;
				break;
			case '[':
				int z = p + 1;

				for(; true; z++) {
					if(z >= pt.length()) {
						throw new WildcardSyntaxException();
					} else if(pt.charAt(z) == ']') {
						break;
					}
				}

				try {
					rl.add(CharSets.parse(pt.subSequence(p + 1, z)));
				} catch(CharSetException e) {
					throw new WildcardSyntaxException(e);
				}
				if(b) {
					rp.add(false);
				}
				b = true;
				p = z;
				break;
			default:
				rl.add(CharSets.newCharInterval((char)c));
				if(b) {
					rp.add(false);
				}
				b = true;
				break;
			}
		}

		if(b) {
			rp.add(false);
		}
		rl.add(Range.O);
		return new Wildcard(rl.toArray(new Range[0]), rp);
	}

	/**
	 * 
	 * @param pt
	 * @param cs
	 * @return
	 */
	public static boolean matches(CharSequence pt, CharSequence cs) {
		return compile(pt).matches(cs);
	}

}
