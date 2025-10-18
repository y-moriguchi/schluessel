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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/09
 */
public class SRFI60Test extends TCSubr {

	public void testLogand() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(logand #b1100 #b1010)", 8);
		eqi  (l,"(logand #b11100 #b11010 #b10000)", 16);
		eqi  (l,"(logand -5 -3)", -7);
		eqi  (l,"(logand -5 5)", 1);
		eqi  (l,"(logand 5)", 5);
		eqi  (l,"(logand)", -1);
		lperr(l,"(logand 5.0)");
	}

	public void testLogior() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(logior #b1100 #b1010)", 14);
		eqi  (l,"(logior #b11100 #b11010 #b10000)", 30);
		eqi  (l,"(logior -5 -3)", -1);
		eqi  (l,"(logior -5 5)", -1);
		eqi  (l,"(logior 5)", 5);
		eqi  (l,"(logior)", 0);
		lperr(l,"(logior 5.0)");
	}

	public void testLogxor() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(logxor #b1100 #b1010)", 6);
		eqi  (l,"(logxor #b11100 #b11010 #b10000)", 22);
		eqi  (l,"(logxor -5 -3)", 6);
		eqi  (l,"(logxor -5 5)", -2);
		eqi  (l,"(logxor 5)", 5);
		eqi  (l,"(logxor)", 0);
		lperr(l,"(logxor 5.0)");
	}

	public void testLognot() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(lognot 1)", -2);
		eqi  (l,"(lognot -1)", 0);
		lperr(l,"(lognot)");
		lperr(l,"(lognot 5.0)");
	}

	public void testBitwiseIf() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(bitwise-if #b01110 #b10100 #b01011)", 5);
		eqi  (l,"(bitwise-if -15 #b01011 #b10100)", 5);
		lperr(l,"(bitwise-if -15 2)");
		lperr(l,"(bitwise-if -15 7 5.0)");
	}

	public void testLogtest() {
		Scheme l = Scheme.newInstance();

		equal(l,"(logtest #b0100 #b1011)", F);
		equal(l,"(logtest #b0100 #b0111)", T);
		equal(l,"(logtest -1 -2)", T);
		equal(l,"(logtest -4 1)", F);
		lperr(l,"(logtest -15)");
		lperr(l,"(logtest -15 5.0)");
	}

	public void testLogcount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(logcount #b10101010)", 4);
		eqi  (l,"(logcount 0)", 0);
		eqi  (l,"(logcount -2)", 1);
		lperr(l,"(logcount)");
		lperr(l,"(logcount 5.0)");
	}

	public void testIntegerLength() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(integer-length #b10101010)", 8);
		eqi  (l,"(integer-length 0)", 0);
		eqi  (l,"(integer-length #b1111)", 4);
		eqi  (l,"(integer-length -4)", 2);
		lperr(l,"(integer-length)");
		lperr(l,"(integer-length 5.0)");
	}

	public void testLog2BinaryFactors() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(log2-binary-factors   0)", -1);
		eqi  (l,"(log2-binary-factors   1)", 0);
		eqi  (l,"(log2-binary-factors   2)", 1);
		eqi  (l,"(log2-binary-factors   3)", 0);
		eqi  (l,"(log2-binary-factors   4)", 2);
		eqi  (l,"(log2-binary-factors   5)", 0);
		eqi  (l,"(log2-binary-factors   6)", 1);
		eqi  (l,"(log2-binary-factors   7)", 0);
		eqi  (l,"(log2-binary-factors   8)", 3);
		eqi  (l,"(log2-binary-factors   9)", 0);
		eqi  (l,"(log2-binary-factors  10)", 1);
		eqi  (l,"(log2-binary-factors  11)", 0);
		eqi  (l,"(log2-binary-factors  12)", 2);
		eqi  (l,"(log2-binary-factors  13)", 0);
		eqi  (l,"(log2-binary-factors  14)", 1);
		eqi  (l,"(log2-binary-factors  15)", 0);
		eqi  (l,"(log2-binary-factors  16)", 4);
		eqi  (l,"(log2-binary-factors  -1)", 0);
		eqi  (l,"(log2-binary-factors  -2)", 1);
		eqi  (l,"(log2-binary-factors  -3)", 0);
		eqi  (l,"(log2-binary-factors  -4)", 2);
		eqi  (l,"(log2-binary-factors  -5)", 0);
		eqi  (l,"(log2-binary-factors  -6)", 1);
		eqi  (l,"(log2-binary-factors  -7)", 0);
		eqi  (l,"(log2-binary-factors  -8)", 3);
		eqi  (l,"(log2-binary-factors  -9)", 0);
		eqi  (l,"(log2-binary-factors -10)", 1);
		eqi  (l,"(log2-binary-factors -11)", 0);
		eqi  (l,"(log2-binary-factors -12)", 2);
		eqi  (l,"(log2-binary-factors -13)", 0);
		eqi  (l,"(log2-binary-factors -14)", 1);
		eqi  (l,"(log2-binary-factors -15)", 0);
		eqi  (l,"(log2-binary-factors -16)", 4);
		lperr(l,"(log2-binary-factors)");
		lperr(l,"(log2-binary-factors 5.0)");
	}

	public void testIsLogbit() {
		Scheme l = Scheme.newInstance();

		equal(l,"(logbit? 0 #b1101)", T);
		equal(l,"(logbit? 1 #b1101)", F);
		equal(l,"(logbit? 2 #b1101)", T);
		equal(l,"(logbit? 3 #b1101)", T);
		equal(l,"(logbit? 4 #b1101)", F);
		equal(l,"(logbit? 0 -7)", T);
		equal(l,"(logbit? 1 -7)", F);
		equal(l,"(logbit? 2 -7)", F);
		equal(l,"(logbit? 3 -7)", T);
		lperr(l,"(logbit? -15)");
		lperr(l,"(logbit? -15 5.0)");
	}

	public void testCopyBit() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(copy-bit 0 0 #t)", 1);
		eqi  (l,"(copy-bit 2 0 #t)", 4);
		eqi  (l,"(copy-bit 2 #b1111 #f)", 0xb);
		eqi  (l,"(copy-bit 2 -1 #f)", -5);
		lperr(l,"(copy-bit 2.0 0 #t)");
	}

	public void testBitField() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(bit-field #b1101101010 0 4)", 10);
		eqi  (l,"(bit-field #b1101101010 4 9)", 0x16);
		eqi  (l,"(bit-field #b1010 1 5)", 5);
		eqi  (l,"(bit-field -11 1 5)", 10);
		eqi  (l,"(bit-field #b1010 1 1)", 0);
		lperr(l,"(bit-field #b1010 -1 3)");
		lperr(l,"(bit-field #b1010 3 2)");
		lperr(l,"(bit-field 10.0 1 2)");
	}

	public void testCopyBitField() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(copy-bit-field #b1101101010 0 0 4)", 0x360);
		eqi  (l,"(copy-bit-field #b1101101010 -1 0 4)", 0x36f);
		eqi  (l,"(copy-bit-field #b110100100010000 -1 5 9)", 0x69f0);
		eqi  (l,"(copy-bit-field -11 0 0 3)", -16);
		eqi  (l,"(copy-bit-field 10 0 3 7)", 2);
		eqi  (l,"(copy-bit-field -11 0 3 7)", -123);
		lperr(l,"(copy-bit-field -11 0 -1 7)");
		lperr(l,"(copy-bit-field -11 0 3 2)");
		lperr(l,"(copy-bit-field 11.0 0 3 2)");
		lperr(l,"(copy-bit-field 11 2.0 3 2)");
	}

	public void testAsh() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(ash #b1 3)", 8);
		eqi  (l,"(ash #b1010 -1)", 5);
		eqi  (l,"(ash -2 3)", -16);
		eqi  (l,"(ash -11 -1)", -6);
		eqi  (l,"(ash -11 0)", -11);
		lperr(l,"(ash 1.0 3)");
	}

	public void testRotateBitField() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(rotate-bit-field #b0100 3 0 4)", 2);
		eqi  (l,"(rotate-bit-field #b0101 3 0 4)", 10);
		eqi  (l,"(rotate-bit-field #b0100 -1 0 4)", 2);
		eqi  (l,"(rotate-bit-field #b0100 7 0 4)", 2);
		eqi  (l,"(rotate-bit-field #b0100 -5 0 4)", 2);
		eqi  (l,"(rotate-bit-field #b110100100010000 -1 5 9)", 0x6890);
		eqi  (l,"(rotate-bit-field #b110100100010000 1 5 9)", 0x6830);
		eqi  (l,"(rotate-bit-field #b0100 3 2 6)", 0x20);
		eqi  (l,"(rotate-bit-field #b0100 -1 2 6)", 0x20);
		eqi  (l,"(rotate-bit-field -5 3 2 6)", -0x21);
		eqi  (l,"(rotate-bit-field -5 -1 2 6)", -0x21);
		eqi  (l,"(rotate-bit-field #b0100 0 2 6)", 4);
		eqi  (l,"(rotate-bit-field #b0100 3 2 2)", 4);
		lperr(l,"(rotate-bit-field 4.0 3 1 2)");
		lperr(l,"(rotate-bit-field #b0100 3 -1 2)");
		lperr(l,"(rotate-bit-field #b0100 3 3 2)");
	}

	public void testReverseBitField() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(reverse-bit-field 1 0 8)", 0x80);
		eqi  (l,"(reverse-bit-field 1 0 24)", 0x800000);
		eqi  (l,"(reverse-bit-field #xa7 0 8)", 0xe5);
		eqi  (l,"(reverse-bit-field #xa7 4 12)", 0x507);
		eqi  (l,"(reverse-bit-field #x-a8 0 8)", -0xe6);
		eqi  (l,"(reverse-bit-field #x-a8 4 12)", -0x508);
		eqi  (l,"(reverse-bit-field #xffffa7 0 8)", 0xffffe5);
		eqi  (l,"(reverse-bit-field #x-ffffa8 0 8)", -0xffffe6);
		eqi  (l,"(reverse-bit-field #xa7 2 3)", 0xa7);
		eqi  (l,"(reverse-bit-field #xa7 2 2)", 0xa7);
		lperr(l,"(reverse-bit-field 1.0 2 3)");
		lperr(l,"(reverse-bit-field 1 -1 3)");
		lperr(l,"(reverse-bit-field 1 3 2)");
	}

	public void testIntegerToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(integer->list 10 3)", list(F, T, F));
		equal(l,"(integer->list 10 6)", list(F, T, F, T, F, F));
		equal(l,"(integer->list 10)", list(F, T, F, T));
		equal(l,"(integer->list -11 3)", list(T, F, T));
		equal(l,"(integer->list -11 6)", list(T, F, T, F, T, T));
		equal(l,"(integer->list -11)", list(T, F, T, F));
		lperr(l,"(integer->list 10 -1)");
		lperr(l,"(integer->list 10.0 1)");
	}

	public void testListToInteger() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list->integer '(#f #t #f #t))", 10);
		eqi  (l,"(list->integer '())", 0);
	}

	public void testBooleansToInteger() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(booleans->integer #f #t #f #t)", 10);
		eqi  (l,"(booleans->integer)", 0);
	}

}
