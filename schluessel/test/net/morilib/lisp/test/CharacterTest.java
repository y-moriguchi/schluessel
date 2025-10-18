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
package net.morilib.lisp.test;

import net.morilib.lisp.Scheme;

public class CharacterTest extends TCSubr {
	
	public void testCharLessThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char<? #\\a #\\b #\\c #\\z)", T);
		eq(l, "(char<? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char<? #\\z #\\c #\\b #\\a)", F);
		eq(l, "(char<? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char<? #\\a #\\a #\\a #\\a)", F);
		eq(l, "(char<? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char<? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char<? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char<? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char<? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char<? #\\a 'a #\\a)");
		lperr(l, "(char<? #\\a '(3) #\\a)");
		lperr(l, "(char<? #\\a)");
	}
	
	public void testCharLessThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char<=? #\\a #\\b #\\c #\\z)", T);
		eq(l, "(char<=? #\\a #\\a #\\c #\\z)", T);
		eq(l, "(char<=? #\\z #\\c #\\b #\\a)", F);
		eq(l, "(char<=? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char<=? #\\a #\\a #\\a #\\a)", T);
		eq(l, "(char<=? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char<=? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char<=? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char<=? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char<=? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char<=? #\\a 'a #\\a)");
		lperr(l, "(char<=? #\\a '(3) #\\a)");
		lperr(l, "(char<=? #\\a)");
	}
	
	public void testCharMoreThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char>? #\\a #\\b #\\c #\\z)", F);
		eq(l, "(char>? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char>? #\\z #\\c #\\b #\\a)", T);
		eq(l, "(char>? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char>? #\\a #\\a #\\a #\\a)", F);
		eq(l, "(char>? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char>? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char>? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char>? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char>? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char>? #\\a 'a #\\a)");
		lperr(l, "(char>? #\\a '(3) #\\a)");
		lperr(l, "(char>? #\\a)");
	}
	
	public void testCharMoreThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char>=? #\\a #\\b #\\c #\\z)", F);
		eq(l, "(char>=? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char>=? #\\z #\\c #\\b #\\a)", T);
		eq(l, "(char>=? #\\z #\\b #\\b #\\a)", T);
		eq(l, "(char>=? #\\a #\\a #\\a #\\a)", T);
		eq(l, "(char>=? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char>=? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char>=? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char>=? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char>=? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char>=? #\\a 'a #\\a)");
		lperr(l, "(char>=? #\\a '(3) #\\a)");
		lperr(l, "(char>=? #\\a)");
	}
	
	public void testCharEqual() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char=? #\\a #\\b #\\c #\\z)", F);
		eq(l, "(char=? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char=? #\\z #\\c #\\b #\\a)", F);
		eq(l, "(char=? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char=? #\\a #\\a #\\a #\\a)", T);
		eq(l, "(char=? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char=? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char=? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char=? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char=? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char=? #\\a 'a #\\a)");
		lperr(l, "(char=? #\\a '(3) #\\a)");
		lperr(l, "(char=? #\\a)");
	}
	
	public void testCharCiLessThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-ci<? #\\a #\\b #\\c #\\z)", T);
		eq(l, "(char-ci<? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char-ci<? #\\z #\\c #\\b #\\a)", F);
		eq(l, "(char-ci<? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char-ci<? #\\a #\\a #\\a #\\a)", F);
		eq(l, "(char-ci<? #\\a #\\B #\\c #\\z)", T);
		eq(l, "(char-ci<? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char-ci<? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char-ci<? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char-ci<? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char-ci<? #\\a 'a #\\a)");
		lperr(l, "(char-ci<? #\\a '(3) #\\a)");
		lperr(l, "(char-ci<? #\\a)");
	}
	
	public void testCharCiLessThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-ci<=? #\\a #\\b #\\c #\\z)", T);
		eq(l, "(char-ci<=? #\\a #\\a #\\c #\\z)", T);
		eq(l, "(char-ci<=? #\\z #\\c #\\b #\\a)", F);
		eq(l, "(char-ci<=? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char-ci<=? #\\a #\\a #\\a #\\a)", T);
		eq(l, "(char-ci<=? #\\a #\\B #\\c #\\z)", T);
		eq(l, "(char-ci<=? #\\a #\\A #\\C #\\z)", T);
		eq(l, "(char-ci<=? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char-ci<=? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char-ci<=? #\\a #\\A #\\A #\\a)", T);
		lperr(l, "(char-ci<=? #\\a 'a #\\a)");
		lperr(l, "(char-ci<=? #\\a '(3) #\\a)");
		lperr(l, "(char-ci<=? #\\a)");
	}
	
	public void testCharCiMoreThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-ci>? #\\a #\\b #\\c #\\z)", F);
		eq(l, "(char-ci>? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char-ci>? #\\z #\\c #\\b #\\a)", T);
		eq(l, "(char-ci>? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char-ci>? #\\a #\\a #\\a #\\a)", F);
		eq(l, "(char-ci>? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char-ci>? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char-ci>? #\\z #\\C #\\B #\\a)", T);
		eq(l, "(char-ci>? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char-ci>? #\\a #\\A #\\A #\\a)", F);
		lperr(l, "(char-ci>? #\\a 'a #\\a)");
		lperr(l, "(char-ci>? #\\a '(3) #\\a)");
		lperr(l, "(char-ci>? #\\a)");
	}
	
	public void testCharCiMoreThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-ci>=? #\\a #\\b #\\c #\\z)", F);
		eq(l, "(char-ci>=? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char-ci>=? #\\z #\\c #\\b #\\a)", T);
		eq(l, "(char-ci>=? #\\z #\\b #\\b #\\a)", T);
		eq(l, "(char-ci>=? #\\a #\\a #\\a #\\a)", T);
		eq(l, "(char-ci>=? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char-ci>=? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char-ci>=? #\\z #\\C #\\B #\\a)", T);
		eq(l, "(char-ci>=? #\\Z #\\B #\\b #\\a)", T);
		eq(l, "(char-ci>=? #\\a #\\A #\\A #\\a)", T);
		lperr(l, "(char-ci>=? #\\a 'a #\\a)");
		lperr(l, "(char-ci>=? #\\a '(3) #\\a)");
		lperr(l, "(char-ci>=? #\\a)");
	}
	
	public void testCharCiEqual() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-ci=? #\\a #\\b #\\c #\\z)", F);
		eq(l, "(char-ci=? #\\a #\\a #\\c #\\z)", F);
		eq(l, "(char-ci=? #\\z #\\c #\\b #\\a)", F);
		eq(l, "(char-ci=? #\\z #\\b #\\b #\\a)", F);
		eq(l, "(char-ci=? #\\a #\\a #\\a #\\a)", T);
		eq(l, "(char-ci=? #\\a #\\B #\\c #\\z)", F);
		eq(l, "(char-ci=? #\\a #\\A #\\C #\\z)", F);
		eq(l, "(char-ci=? #\\z #\\C #\\B #\\a)", F);
		eq(l, "(char-ci=? #\\Z #\\B #\\b #\\a)", F);
		eq(l, "(char-ci=? #\\a #\\A #\\A #\\a)", T);
		lperr(l, "(char-ci=? #\\a 'a #\\a)");
		lperr(l, "(char-ci=? #\\a '(3) #\\a)");
		lperr(l, "(char-ci=? #\\a)");
	}
	
	public void testIsCharAlphabetic() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-alphabetic? #\\A)", T);
		eq(l, "(char-alphabetic? #\\a)", T);
		eq(l, "(char-alphabetic? #\\=)", F);
		eq(l, "(char-alphabetic? #\\0)", F);
		eq(l, "(char-alphabetic? #\\space)", F);
		eq(l, "(char-alphabetic? #\\newline)", F);
		eq(l, "(char-alphabetic? #\\\u0393)", T);  // upper gamma
		eq(l, "(char-alphabetic? #\\\u03b1)", T);  // lower alpha
		eq(l, "(char-alphabetic? #\\\u3044)", F);  // hiragana i
		eq(l, "(char-alphabetic? #\\\u30a4)", F);  // katagana i
		eq(l, "(char-alphabetic? #\\\u6f22)", F);  // kanji kan
		eq(l, "(char-alphabetic? #\\\u4e09)", F);  // kanji san(3)
		eq(l, "(char-alphabetic? #\\\uff72)", F);  // katakana i(half-w)
		eq(l, "(char-alphabetic? #\\\uff10)", F);  // full-w number "0"
		eq(l, "(char-alphabetic? #\\\uff23)", T);  // full-width B
		eq(l, "(char-alphabetic? #\\\u3000)", F);  // full-w whitespace
		lperr(l, "(char-alphabetic? 100)");
		lperr(l, "(char-alphabetic? 'a)");
		lperr(l, "(char-alphabetic?)");
	}
	
	public void testIsCharNumeric() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-numeric? #\\A)", F);
		eq(l, "(char-numeric? #\\a)", F);
		eq(l, "(char-numeric? #\\=)", F);
		eq(l, "(char-numeric? #\\0)", T);
		eq(l, "(char-numeric? #\\space)", F);
		eq(l, "(char-numeric? #\\newline)", F);
		eq(l, "(char-numeric? #\\\u0393)", F);  // upper gamma
		eq(l, "(char-numeric? #\\\u03b1)", F);  // lower alpha
		eq(l, "(char-numeric? #\\\u3044)", F);  // hiragana i
		eq(l, "(char-numeric? #\\\u30a4)", F);  // katagana i
		eq(l, "(char-numeric? #\\\u6f22)", F);  // kanji kan
		eq(l, "(char-numeric? #\\\u4e09)", F);  // kanji san(3)
		eq(l, "(char-numeric? #\\\uff72)", F);  // katakana i(half-w)
		eq(l, "(char-numeric? #\\\uff10)", T);  // full-w number "0"
		eq(l, "(char-numeric? #\\\uff23)", F);  // full-width B
		eq(l, "(char-numeric? #\\\u3000)", F);  // full-w whitespace
		lperr(l, "(char-numeric? 100)");
		lperr(l, "(char-numeric? 'a)");
		lperr(l, "(char-numeric?)");
	}
	
	public void testIsCharLowerCase() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-lower-case? #\\A)", F);
		eq(l, "(char-lower-case? #\\a)", T);
		eq(l, "(char-lower-case? #\\=)", F);
		eq(l, "(char-lower-case? #\\0)", F);
		eq(l, "(char-lower-case? #\\space)", F);
		eq(l, "(char-lower-case? #\\newline)", F);
		eq(l, "(char-lower-case? #\\\u0393)", F);  // upper gamma
		eq(l, "(char-lower-case? #\\\u03b1)", T);  // lower alpha
		eq(l, "(char-lower-case? #\\\u3044)", F);  // hiragana i
		eq(l, "(char-lower-case? #\\\u30a4)", F);  // katagana i
		eq(l, "(char-lower-case? #\\\u6f22)", F);  // kanji kan
		eq(l, "(char-lower-case? #\\\u4e09)", F);  // kanji san(3)
		eq(l, "(char-lower-case? #\\\uff72)", F);  // katakana i(half-w)
		eq(l, "(char-lower-case? #\\\uff10)", F);  // full-w number "0"
		eq(l, "(char-lower-case? #\\\uff23)", F);  // full-width B
		eq(l, "(char-lower-case? #\\\u3000)", F);  // full-w whitespace
		lperr(l, "(char-lower-case? 100)");
		lperr(l, "(char-lower-case? 'a)");
		lperr(l, "(char-lower-case?)");
	}
	
	public void testIsCharUpperCase() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-upper-case? #\\A)", T);
		eq(l, "(char-upper-case? #\\a)", F);
		eq(l, "(char-upper-case? #\\=)", F);
		eq(l, "(char-upper-case? #\\0)", F);
		eq(l, "(char-upper-case? #\\space)", F);
		eq(l, "(char-upper-case? #\\newline)", F);
		eq(l, "(char-upper-case? #\\\u0393)", T);  // upper gamma
		eq(l, "(char-upper-case? #\\\u03b1)", F);  // lower alpha
		eq(l, "(char-upper-case? #\\\u3044)", F);  // hiragana i
		eq(l, "(char-upper-case? #\\\u30a4)", F);  // katagana i
		eq(l, "(char-upper-case? #\\\u6f22)", F);  // kanji kan
		eq(l, "(char-upper-case? #\\\u4e09)", F);  // kanji san(3)
		eq(l, "(char-upper-case? #\\\uff72)", F);  // katakana i(half-w)
		eq(l, "(char-upper-case? #\\\uff10)", F);  // full-w number "0"
		eq(l, "(char-upper-case? #\\\uff23)", T);  // full-width B
		eq(l, "(char-upper-case? #\\\u3000)", F);  // full-w whitespace
		lperr(l, "(char-upper-case? 100)");
		lperr(l, "(char-upper-case? 'a)");
		lperr(l, "(char-upper-case?)");
	}
	
	public void testIsCharWhitespace() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(char-whitespace? #\\A)", F);
		eq(l, "(char-whitespace? #\\a)", F);
		eq(l, "(char-whitespace? #\\=)", F);
		eq(l, "(char-whitespace? #\\0)", F);
		eq(l, "(char-whitespace? #\\space)", T);
		eq(l, "(char-whitespace? #\\newline)", T);
		eq(l, "(char-whitespace? #\\\u0393)", F);  // upper gamma
		eq(l, "(char-whitespace? #\\\u03b1)", F);  // lower alpha
		eq(l, "(char-whitespace? #\\\u3044)", F);  // hiragana i
		eq(l, "(char-whitespace? #\\\u30a4)", F);  // katagana i
		eq(l, "(char-whitespace? #\\\u6f22)", F);  // kanji kan
		eq(l, "(char-whitespace? #\\\u4e09)", F);  // kanji san(3)
		eq(l, "(char-whitespace? #\\\uff72)", F);  // katakana i(half-w)
		eq(l, "(char-whitespace? #\\\uff10)", F);  // full-w number "0"
		eq(l, "(char-whitespace? #\\\uff23)", F);  // full-width B
		eq(l, "(char-whitespace? #\\\u3000)", T);  // full-w whitespace
		lperr(l, "(char-whitespace? 100)");
		lperr(l, "(char-whitespace? 'a)");
		lperr(l, "(char-whitespace?)");
	}
	
	public void testCharUpcase() {
		Scheme l = Scheme.newInstance();
		
		eqc(l, "(char-upcase #\\A)", 'A');
		eqc(l, "(char-upcase #\\a)", 'A');
		eqc(l, "(char-upcase #\\=)", '=');
		eqc(l, "(char-upcase #\\0)", '0');
		eqc(l, "(char-upcase #\\space)", ' ');
		eqc(l, "(char-upcase #\\newline)", '\n');
		eqc(l, "(char-upcase #\\\u0393)", '\u0393');  // upper gamma
		eqc(l, "(char-upcase #\\\u03b3)", '\u0393');  // lower gamma
		eqc(l, "(char-upcase #\\\u3044)", '\u3044');  // hiragana i
		eqc(l, "(char-upcase #\\\u30a4)", '\u30a4');  // katagana i
		eqc(l, "(char-upcase #\\\u6f22)", '\u6f22');  // kanji kan
		eqc(l, "(char-upcase #\\\u4e09)", '\u4e09');  // kanji san(3)
		lperr(l, "(char-upcase 100)");
		lperr(l, "(char-upcase 'a)");
		lperr(l, "(char-upcase)");
	}
	
	public void testCharDowncase() {
		Scheme l = Scheme.newInstance();
		
		eqc(l, "(char-downcase #\\A)", 'a');
		eqc(l, "(char-downcase #\\a)", 'a');
		eqc(l, "(char-downcase #\\=)", '=');
		eqc(l, "(char-downcase #\\0)", '0');
		eqc(l, "(char-downcase #\\space)", ' ');
		eqc(l, "(char-downcase #\\newline)", '\n');
		eqc(l, "(char-downcase #\\\u0393)", '\u03b3');  // upper gamma
		eqc(l, "(char-downcase #\\\u03b3)", '\u03b3');  // lower gamma
		eqc(l, "(char-downcase #\\\u3044)", '\u3044');  // hiragana i
		eqc(l, "(char-downcase #\\\u30a4)", '\u30a4');  // katagana i
		eqc(l, "(char-downcase #\\\u6f22)", '\u6f22');  // kanji kan
		eqc(l, "(char-downcase #\\\u4e09)", '\u4e09');  // kanji san(3)
		lperr(l, "(char-downcase 100)");
		lperr(l, "(char-downcase 'a)");
		lperr(l, "(char-downcase)");
	}
	
	public void testCharToInteger() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(char->integer #\\A)", 'A');
		eqi(l, "(char->integer #\\a)", 'a');
		eqi(l, "(char->integer #\\=)", '=');
		eqi(l, "(char->integer #\\0)", '0');
		eqi(l, "(char->integer #\\space)", ' ');
		eqi(l, "(char->integer #\\newline)", '\n');
		eqi(l, "(char->integer #\\\u0393)", 0x0393);  // upper gamma
		eqi(l, "(char->integer #\\\u03b3)", 0x03b3);  // lower gamma
		eqi(l, "(char->integer #\\\u3044)", 0x3044);  // hiragana i
		eqi(l, "(char->integer #\\\u30a4)", 0x30a4);  // katagana i
		eqi(l, "(char->integer #\\\u6f22)", 0x6f22);  // kanji kan
		eqi(l, "(char->integer #\\\u4e09)", 0x4e09);  // kanji san(3)
		lperr(l, "(char->integer 100)");
		lperr(l, "(char->integer 'a)");
		lperr(l, "(char->integer)");
	}
	
	public void testIntegerToChar() {
		Scheme l = Scheme.newInstance();
		
		eqc(l, "(integer->char 65)", 'A');
		eqc(l, "(integer->char 97)", 'a');
		eqc(l, "(integer->char 61)", '=');
		eqc(l, "(integer->char 48)", '0');
		eqc(l, "(integer->char 32)", ' ');
		eqc(l, "(integer->char 10)", '\n');
		eqc(l, "(integer->char #x0393)", '\u0393');  // upper gamma
		eqc(l, "(integer->char #x03b3)", '\u03b3');  // lower gamma
		eqc(l, "(integer->char #x3044)", '\u3044');  // hiragana i
		eqc(l, "(integer->char #x30a4)", '\u30a4');  // katagana i
		eqc(l, "(integer->char #x6f22)", '\u6f22');  // kanji kan
		eqc(l, "(integer->char #x4e09)", '\u4e09');  // kanji san(3)
		lperr(l, "(integer->char #\\a)");
		lperr(l, "(integer->char 'a)");
		lperr(l, "(integer->char)");
	}
	
}
