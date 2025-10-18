/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.range;

import java.util.ArrayList;
import java.util.List;

/**
 * Constant objects and utilities for character sets
 * implemented by the Range library.
 * <p>Rangeライブラリにより実装された文字セットの定数オブジェクトと
 * ユーティリティである.
 * 
 * @author MORIGUCHI, Yuichiro 2008/01/01
 */
public final class CharSets {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/10/09
	 */
	public interface CharSetHandler {

		/**
		 * 
		 * @param ch
		 */
		public void singleChar(int ch);

		/**
		 * 
		 * @param cb
		 * @param ce
		 */
		public void rangedChar(int cb, int ce);

	}

	/**
	 * The character set of the numbers by ASCII characters.
	 * <p>ASCII文字による数字の文字セットである.
	 */
	public static final Range NUMBERS = newCharInterval('0', '9');

	/**
	 * The character set of the numbers by ASCII characters.
	 * <p>ASCII文字による数字の文字セットである.
	 */
	public static final Range ASCII_NUMBERS = newCharInterval('0', '9');

	/**
	 * The character set of the capital alphabets.
	 * <p>英大文字の文字セットである.
	 */
	public static final Range CAPITAL_LETTERS =
		newCharInterval('A', 'Z');

	/**
	 * The character set of the small alphabets.
	 * <p>英小文字の文字セットである.
	 */
	public static final Range SMALL_LETTERS = newCharInterval('a', 'z');

	/**
	 * The character set of the English alphabets.
	 * <p>英文字の文字セットである.
	 */
	public static final Range ENGLISH_ALPHABETS =
		Ranges.sum(CAPITAL_LETTERS, SMALL_LETTERS);

	/**
	 * The character set of the English alphabets and numbers.
	 * <p>英数字の文字セットである.
	 */
	public static final Range ENGLISH_ALPHABETS_NUMBERS =
		Ranges.sum(ENGLISH_ALPHABETS, ASCII_NUMBERS);

	/**
	 * The character set of the Japanese hiraganas.
	 * <p>ひらがなの文字セットである.
	 */
	public static final Range HIRAGANA =
		newCharInterval('\u3041', '\u3094');

	/**
	 * The character set of the Japanese katakanas.
	 * <p>カタカナの文字セットである.
	 */
	public static final Range KATAKANA =
		newCharInterval('\u30a1', '\u30fa');

	/**
	 * The character set of the white spaces by ASCII characters.
	 * <p>ASCII文字による空白の文字セットである.
	 */
	public static final Range SPACES = Ranges.sum(
			IntervalsInt.newPoint(' '), IntervalsInt.newPoint('\t'));

	/**
	 * The character set of the token separators by ASCII characters.
	 * <p>ASCII文字によるトークン区切り文字の文字セットである.
	 */
	public static final Range TOKEN = Ranges.sum(
			IntervalsInt.newPoint(' '), IntervalsInt.newPoint('\t'));

	/**
	 * 
	 */
	public static final Range ALL_CHAR =
		IntervalsInt.newRightOpenInterval(
				0, Character.MAX_VALUE + 1);

	//
	private CharSets() {}

	/**
	 * creates a new character set of the given characters.
	 * <p>与えられた文字を端点とする文字セットを生成する.
	 * 
	 * @param f  the starting character
	 * @param t  the ending character
	 * @return  a new character set
	 */
	public static final Range newCharInterval(char f, char t) {
		return IntervalsInt.newRightOpenInterval((int)f, (int)(t + 1));
	}

	/**
	 * 
	 * @param f
	 * @param t
	 * @return
	 */
	public static final Range newCharInterval(char c) {
		return IntervalsInt.newRightOpenInterval((int)c, (int)(c + 1));
	}

	//
	private static Interval getintv(int ch, int ch2) {
		return IntervalsInt.newRightOpenInterval(ch, ch2 + 1);
	}

	/**
	 * 
	 * @param s
	 * @param h
	 * @return
	 */
	public static boolean parse(
			CharSequence s, CharSetHandler h) {
		StringBuilder b = new StringBuilder();
		int st = 1;
		int c, cb = -1, pt = 0;

		while(pt < s.length()) {
			c = s.charAt(pt++);
			if(st == 1) {
				if(c == '\\') {
					st = 2;
				} else {
					cb = c;  st = 4;
				}
			} else if(st == 2) {
				switch(c) {
				case 'x':
				case 'u':   st = 3;  break;
				case '\\':  cb = c;  st = 4;  break;
				case 't':   cb = '\t';  st = 4;  break;
				case 'n':   cb = '\n';  st = 4;  break;
				case 'r':   cb = '\r';  st = 4;  break;
				default:   return false;
				}
			} else if(st == 3) {
				if(pt + 2 >= s.length()) {
					return false;
				}
				b.append((char)c);
				b.append(s.charAt(pt++));
				b.append(s.charAt(pt++));
				b.append(s.charAt(pt++));
				try {
					cb = Integer.parseInt(b.toString(), 16);
					b.delete(0, b.length());
					st = 4;
				} catch(NumberFormatException e) {
					throw new RuntimeException("Illegal code");
				}
			} else if(st == 4) {
				if(c == '\\') {
					h.singleChar(cb);
					st = 2;
				} else if(c == '-') {
					st = 5;
				} else {
					h.singleChar(cb);
					cb = c;  st = 4;
				}
			} else if(st == 5) {
				if(c == '\\') {
					st = 6;
				} else {
					if(c < cb) {
						return false;
					}
					h.rangedChar(cb, c);
					st = 1;
				}
			} else if(st == 6) {
				switch(c) {
				case 'x':
				case 'u':   st = 7;  break;
				case '\\':  break;
				case 't':   c = '\t';  break;
				case 'n':   c = '\n';  break;
				case 'r':   c = '\r';  break;
				default:   return false;
				}

				if(c < cb) {
					return false;
				}
				h.rangedChar(cb, c);
				st = 1;
			} else if(st == 7) {
				if(pt + 2 >= s.length()) {
					return false;
				}
				b.append((char)c);
				b.append(s.charAt(pt++));
				b.append(s.charAt(pt++));
				b.append(s.charAt(pt++));
				try {
					int cz = Integer.parseInt(b.toString(), 16);

					if(cz < cb) {
						return false;
					}
					h.rangedChar(cb, cz);
					b.delete(0, b.length());
					st = 1;
				} catch(NumberFormatException e) {
					throw new RuntimeException("Illegal code");
				}
			}
		}

		if(st == 4) {
			h.singleChar(cb);
		}
		return true;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static Range parse(CharSequence s) {
		boolean r;
		final RangeAdder ra = new RangeAdder();

		r = parse(s, new CharSetHandler() {

			public void singleChar(int ch) {
				ra.add(getintv(ch, ch));
			}

			public void rangedChar(int cb, int ce) {
				ra.add(getintv(cb, ce));
			}

		});

		if(!r) {
			throw new CharSetException();
		}
		return ra.toRange();
	}

	//
	private static List<Interval> parseTr(CharSequence s) {
		boolean r;
		final List<Interval> ra = new ArrayList<Interval>();

		r = parse(s, new CharSetHandler() {

			public void singleChar(int ch) {
				ra.add(getintv(ch, ch));
			}

			public void rangedChar(int cb, int ce) {
				ra.add(getintv(cb, ce));
			}

		});

		if(!r) {
			throw new CharSetException();
		}
		return ra;
	}

	//
	private static int card(Interval r1) {
		return (int)IntervalsInt.cardinality(r1);
	}

	//
	private static int card(List<Interval> r1) {
		int r = 0;

		for(int i = 0; i < r1.size(); i++) {
			r += (int)IntervalsInt.cardinality(r1.get(i));
		}
		return r;
	}

	/**
	 * 
	 * @param s
	 * @param t1
	 * @param t2
	 * @return
	 */
	public static String tr(CharSequence s,
			CharSequence t1, CharSequence t2) {
		StringBuilder b = new StringBuilder();
		List<Interval> r1 = parseTr(t1);
		List<Interval> r2 = parseTr(t2);

		if(card(r1) != card(r2)) {
			throw new IllegalArgumentException();
		}

		outer: for(int i = 0; i < s.length(); i++) {
			int ind = 0;
			int c   = s.charAt(i);

			for(int j = 0; j < r1.size(); j++) {
				Interval i1 = r1.get(j);

				if(i1.contains(c)) {
					int in2 = 0;
					int c1  = (int)IntervalsInt.indexOf(i1, c);

					for(int k = 0; k < r2.size(); k++) {
						int cd2 = card(r2.get(k));

						if(ind + c1 < in2 + cd2) {
							b.append((char)IntervalsInt.intAt(
									r2.get(k), c1));
							continue outer;
						}
						in2 += cd2;
					}
					throw new RuntimeException();
				}
				ind += card(i1);
			}
			b.append((char)c);
		}
		return b.toString();
	}

}
