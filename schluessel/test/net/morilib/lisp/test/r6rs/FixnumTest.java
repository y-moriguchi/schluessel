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
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/30
 */
public class FixnumTest extends TCSubr {

	public void testFixnumProps() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(fixnum? 1)", T);
		eq   (l,"(fixnum? 1.0)", F);
		eqi  (l,"(fixnum-width)", 32);
		eqi  (l,"(least-fixnum)", Integer.MIN_VALUE);
		eqi  (l,"(greatest-fixnum)", Integer.MAX_VALUE);
	}

	public void testFixnumProposition() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(fx=? 1 1 1)", T);
		eq   (l,"(fx=? 1 2 3)", F);
		eq   (l,"(fx=? 1 1 3)", F);
		eq   (l,"(fx=? 3 2 1)", F);
		eq   (l,"(fx=? 3 1 1)", F);
		eq   (l,"(fx=? 1 3 2)", F);

		eq   (l,"(fx<? 1 1 1)", F);
		eq   (l,"(fx<? 1 2 3)", T);
		eq   (l,"(fx<? 1 1 3)", F);
		eq   (l,"(fx<? 3 2 1)", F);
		eq   (l,"(fx<? 3 1 1)", F);
		eq   (l,"(fx<? 1 3 2)", F);

		eq   (l,"(fx<=? 1 1 1)", T);
		eq   (l,"(fx<=? 1 2 3)", T);
		eq   (l,"(fx<=? 1 1 3)", T);
		eq   (l,"(fx<=? 3 2 1)", F);
		eq   (l,"(fx<=? 3 1 1)", F);
		eq   (l,"(fx<=? 1 3 2)", F);

		eq   (l,"(fx>? 1 1 1)", F);
		eq   (l,"(fx>? 1 2 3)", F);
		eq   (l,"(fx>? 1 1 3)", F);
		eq   (l,"(fx>? 3 2 1)", T);
		eq   (l,"(fx>? 3 1 1)", F);
		eq   (l,"(fx>? 1 3 2)", F);

		eq   (l,"(fx>=? 1 1 1)", T);
		eq   (l,"(fx>=? 1 2 3)", F);
		eq   (l,"(fx>=? 1 1 3)", F);
		eq   (l,"(fx>=? 3 2 1)", T);
		eq   (l,"(fx>=? 3 1 1)", T);
		eq   (l,"(fx>=? 1 3 2)", F);

		eq   (l,"(fxzero?  0)", T);
		eq   (l,"(fxzero?  1)", F);
		eq   (l,"(fxzero? -1)", F);
		eq   (l,"(fxzero?  2)", F);
		eq   (l,"(fxzero? -2)", F);

		eq   (l,"(fxpositive?  0)", F);
		eq   (l,"(fxpositive?  1)", T);
		eq   (l,"(fxpositive? -1)", F);
		eq   (l,"(fxpositive?  2)", T);
		eq   (l,"(fxpositive? -2)", F);

		eq   (l,"(fxnegative?  0)", F);
		eq   (l,"(fxnegative?  1)", F);
		eq   (l,"(fxnegative? -1)", T);
		eq   (l,"(fxnegative?  2)", F);
		eq   (l,"(fxnegative? -2)", T);

		eq   (l,"(fxodd?  0)", F);
		eq   (l,"(fxodd?  1)", T);
		eq   (l,"(fxodd? -1)", T);
		eq   (l,"(fxodd?  2)", F);
		eq   (l,"(fxodd? -2)", F);

		eq   (l,"(fxeven?  0)", T);
		eq   (l,"(fxeven?  1)", F);
		eq   (l,"(fxeven? -1)", F);
		eq   (l,"(fxeven?  2)", T);
		eq   (l,"(fxeven? -2)", T);
	}

	public void testFxExtrema() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fxmax 1 3 2 5 4)", 5);
		eqi  (l,"(fxmax 1)", 1);

		eqi  (l,"(fxmin 1 3 2 5 4)", 1);
		eqi  (l,"(fxmin 1)", 1);
	}

	public void testFxRingOperation() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fx+  2  3)", 5);
		eqi  (l,"(fx+  1  2147483646)", 2147483647);
		eqi  (l,"(fx+ -1 -2147483647)", -2147483648);
		lperr(l,"(fx+  1  2147483647)");
		lperr(l,"(fx+ -1 -2147483648)");

		eqi  (l,"(fx-  2  3)", -1);
		eqi  (l,"(fx-  1 -2147483646)", 2147483647);
		eqi  (l,"(fx- -1  2147483647)", -2147483648);
		lperr(l,"(fx-  1 -2147483647)");
		lperr(l,"(fx- -1  2147483648)");

		eqi  (l,"(fx- 2)", -2);
		eqi  (l,"(fx- 2147483647)", -2147483647);
		lperr(l,"(fx- -2147483648)");

		eqi  (l,"(fx*  2 3)", 6);
		eqi  (l,"(fx*  2 1073741823)", 2147483646);
		eqi  (l,"(fx* -2 1073741824)", -2147483648);
		lperr(l,"(fx*  2 1073741824)");
		lperr(l,"(fx* -2 1073741825)");
	}

	public void testFxDivMod() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fxdiv  123  10)", 12);
		eqi  (l,"(fxmod  123  10)", 3);
		eqi  (l,"(fxdiv  123 -10)", -12);
		eqi  (l,"(fxmod  123 -10)", 3);
		eqi  (l,"(fxdiv -123  10)", -13);
		eqi  (l,"(fxmod -123  10)", 7);
		eqi  (l,"(fxdiv -123 -10)", 13);
		eqi  (l,"(fxmod -123 -10)", 7);
		eqi  (l,"(fxdiv-and-mod  123  10)", 12, 3);
		eqi  (l,"(fxdiv-and-mod  123 -10)", -12, 3);
		eqi  (l,"(fxdiv-and-mod -123  10)", -13, 7);
		eqi  (l,"(fxdiv-and-mod -123 -10)", 13, 7);

		eqi  (l,"(fxdiv0  123  10)", 12);
		eqi  (l,"(fxmod0  123  10)", 3);
		eqi  (l,"(fxdiv0  123 -10)", -12);
		eqi  (l,"(fxmod0  123 -10)", 3);
		eqi  (l,"(fxdiv0 -123  10)", -12);
		eqi  (l,"(fxmod0 -123  10)", -3);
		eqi  (l,"(fxdiv0 -123 -10)", 12);
		eqi  (l,"(fxmod0 -123 -10)", -3);
		eqi  (l,"(fxdiv0-and-mod0  123  10)", 12, 3);
		eqi  (l,"(fxdiv0-and-mod0  123 -10)", -12, 3);
		eqi  (l,"(fxdiv0-and-mod0 -123  10)", -12, -3);
		eqi  (l,"(fxdiv0-and-mod0 -123 -10)", 12, -3);

		lperr(l,"(fxdiv 1 0)");
		lperr(l,"(fxmod 1 0)");
		lperr(l,"(fxdiv-and-mod 1 0)");
		lperr(l,"(fxdiv0 1 0)");
		lperr(l,"(fxmod0 1 0)");
		lperr(l,"(fxdiv0-and-mod0 1 0)");
	}

	public void testFxCarryOperation() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fx+/carry  2  3 1)", 6, 0);
		eqi  (l,"(fx+/carry  1  2147483646 0)", 2147483647, 0);
		eqi  (l,"(fx+/carry -1 -2147483646 0)", -2147483647, 0);
		eqi  (l,"(fx+/carry -1 -2147483647 0)", 0, -1);
		eqi  (l,"(fx+/carry  1  2147483647 0)", 0, 1);
		eqi  (l,"(fx+/carry  1  2147483647 -1)", 2147483647, 0);
		eqi  (l,"(fx+/carry -1 -2147483648 1)", 0, -1);
		eqi  (l,"(fx+/carry -1 -2147483648 0)", -1, -1);

		eqi  (l,"(fx-/carry  2  3 1)", -2, 0);
		eqi  (l,"(fx-/carry  1 -2147483646 0)", 2147483647, 0);
		eqi  (l,"(fx-/carry -1  2147483647 0)", 0, -1);
		eqi  (l,"(fx-/carry  1 -2147483647 0)", 0, 1);
		eqi  (l,"(fx-/carry  1 -2147483647 1)", 2147483647, 0);
		eqi  (l,"(fx+/carry 2147483647 1 -1)", 2147483647, 0);
		eqi  (l,"(fx+/carry -2147483648 -1 1)", 0, -1);

		eqi  (l,"(fx*/carry  2 3 1)", 7, 0);
		eqi  (l,"(fx*/carry  1 1 2147483647)", 0, 1);
		eqi  (l,"(fx*/carry -1 -1 2147483647)", 0, 1);
		eqi  (l,"(fx*/carry -1 1 2147483647)", 2147483646, 0);
		eqi  (l,"(fx*/carry  2 1073741823 0)", 2147483646, 0);
		eqi  (l,"(fx*/carry -2 1073741824 0)", 0, -1);
		eqi  (l,"(fx*/carry  2 1073741824 0)", 0, 1);
		eqi  (l,"(fx*/carry -2 1073741825 0)", -2, -1);
		eqi  (l,"(fx*/carry  4 1073741823 0)", 2147483644, 1);
		eqi  (l,"(fx*/carry  4 1073741823 4)", 0, 2);
		eqi  (l,"(fx*/carry  4 1073741824 0)", 0, 2);
		eqi  (l,"(fx*/carry -4 1073741824 0)", 0, -2);
		eqi  (l,"(fx*/carry -4 1073741824 -1)", -1, -2);
		eqi  (l,"(fx*/carry 2147483647 2147483647 0)", 1, 2147483646);
		eqi  (l,"(fx*/carry 2147483647 2147483647 2147483647)", 0, 2147483647);
		eqi  (l,"(fx*/carry -2147483648 -2147483648 -1)", 2147483647, 2147483647);
		eqi  (l,"(fx*/carry -2147483648 -2147483648 -2147483648)", 0, 2147483647);
		eqi  (l,"(fx*/carry -2147483648 2147483647 0)", 0, -2147483647);
		eqi  (l,"(fx*/carry -2147483648 2147483647 2147483647)", -1, -2147483646);
		eqi  (l,"(fx*/carry -2147483648 2147483647 -2147483648)", 0, -2147483648);
		lperr(l,"(fx*/carry -2147483648 -2147483648 0)");
	}

	public void testFxBitLogicalOperation() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fxnot -1)", 0);
		eqi  (l,"(fxnot  0)", -1);
		eqi  (l,"(fxand #b1100 #b1010)", 8);
		eqi  (l,"(fxand #b1100 #b1010 #b1111)", 8);
		eqi  (l,"(fxior #b1100 #b1010)", 14);
		eqi  (l,"(fxior #b1110 #b1010 #b0000)", 14);
		eqi  (l,"(fxxor #b1100 #b1010)", 6);
		eqi  (l,"(fxxor #b1100 #b1010 #b1111)", 9);
		eqi  (l,"(fxif #b1100 #b0101 #b1010)", 6);
	}

	public void testFxBitOperation() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fxbit-count #b1011011)", 5);
		eqi  (l,"(fxbit-count -91)", -5);
		eqi  (l,"(fxbit-count 0)", 0);
		eqi  (l,"(fxbit-count -1)", -1);

		eqi  (l,"(fxlength #b1011011)", 7);
		eqi  (l,"(fxlength -91)", 7);
		eqi  (l,"(fxlength 0)", 0);
		eqi  (l,"(fxlength -1)", 0);

		eqi  (l,"(fxfirst-bit-set 91)", 0);
		eqi  (l,"(fxfirst-bit-set 72)", 3);
		eqi  (l,"(fxfirst-bit-set -4)", 2);
		eqi  (l,"(fxfirst-bit-set 0)", -1);

		eq   (l,"(fxbit-set? #b10 0)", F);
		eq   (l,"(fxbit-set? #b10 1)", T);
		eq   (l,"(fxbit-set? -1 31)", T);
		lperr(l,"(fxbit-set? -1 -1)");
		lperr(l,"(fxbit-set? -1 32)");

		eqi  (l,"(fxcopy-bit #b1001000 0 1)", 73);
		eqi  (l,"(fxcopy-bit #b1001000 0 0)", 72);
		eqi  (l,"(fxcopy-bit #b1001000 3 1)", 72);
		eqi  (l,"(fxcopy-bit #b1001000 3 0)", 64);
		lperr(l,"(fxcopy-bit 72 -1 1)");
		lperr(l,"(fxcopy-bit 72 32 1)");
		lperr(l,"(fxcopy-bit 72 0 2)");
		lperr(l,"(fxcopy-bit 72 0 -1)");

		eqi  (l,"(fxbit-field #b1011011 1 4)", 5);
		eqi  (l,"(fxbit-field #b1011011 0 4)", 11);
		eqi  (l,"(fxbit-field #b1011011 4 8)", 5);
		eqi  (l,"(fxbit-field #b1011011 0 31)", 91);
		eqi  (l,"(fxbit-field #b1011011 1 1)", 0);
		lperr(l,"(fxbit-field 72 -1 31)");
		lperr(l,"(fxbit-field 72 0 -1)");
		lperr(l,"(fxbit-field 72 0 32)");
		lperr(l,"(fxbit-field 72 3 2)");

		eqi  (l,"(fxcopy-bit-field #b1011011 1 4 3)", 87);
		eqi  (l,"(fxcopy-bit-field #b1011011 1 4 0)", 81);
		eqi  (l,"(fxcopy-bit-field #b1011011 1 4 7)", 95);
		eqi  (l,"(fxcopy-bit-field #b1011011 0 3 4)", 92);
		lperr(l,"(fxcopy-bit-field 72 -1 31 1)");
		lperr(l,"(fxcopy-bit-field 72 0 -1 1)");
		lperr(l,"(fxcopy-bit-field 72 0 32 1)");
		lperr(l,"(fxcopy-bit-field 72 3 2 1)");
	}

	public void testFxShift() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fxarithmetic-shift 1 7)", 128);
		eqi  (l,"(fxarithmetic-shift 128 -7)", 1);
		eqi  (l,"(fxarithmetic-shift 128 -8)", 0);
//		eqi  (l,"(fxarithmetic-shift 1 31)", -2147483648);
		eqi  (l,"(fxarithmetic-shift 1 0)", 1);
		eqi  (l,"(fxarithmetic-shift -1 7)", -128);
		eqi  (l,"(fxarithmetic-shift -128 -6)", -2);
		eqi  (l,"(fxarithmetic-shift -128 -7)", -1);
		lperr(l,"(fxarithmetic-shift 1 31)");
		lperr(l,"(fxarithmetic-shift 1 32)");
		lperr(l,"(fxarithmetic-shift 1 -32)");
		lperr(l,"(fxarithmetic-shift -2147483648 1)");

		eqi  (l,"(fxarithmetic-shift-left 1 7)", 128);
//		eqi  (l,"(fxarithmetic-shift-left 1 31)", -2147483648);
		eqi  (l,"(fxarithmetic-shift-left 1 0)", 1);
		eqi  (l,"(fxarithmetic-shift-left -1 7)", -128);
		lperr(l,"(fxarithmetic-shift-left 1 31)");
		lperr(l,"(fxarithmetic-shift-left 1 32)");
		lperr(l,"(fxarithmetic-shift-left 1 -1)");
		lperr(l,"(fxarithmetic-shift-left -2147483648 1)");

		eqi  (l,"(fxarithmetic-shift-right 128 7)", 1);
		eqi  (l,"(fxarithmetic-shift-right 128 8)", 0);
		eqi  (l,"(fxarithmetic-shift-right -128 6)", -2);
		eqi  (l,"(fxarithmetic-shift-right -128 7)", -1);
		lperr(l,"(fxarithmetic-shift-right 1 32)");
		lperr(l,"(fxarithmetic-shift-right 1 -1)");
	}

	public void testFxRotateReverse() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(fxrotate-bit-field #b1011011 1 6 1)", 117);
		eqi  (l,"(fxrotate-bit-field #b1011011 1 6 3)", 87);
		eqi  (l,"(fxrotate-bit-field #b1011011 1 6 0)", 91);
		eqi  (l,"(fxrotate-bit-field #b1011011 1 6 4)", 109);
		lperr(l,"(fxrotate-bit-field #b1011011 1 1 0)");
		lperr(l,"(fxrotate-bit-field 72 0 -1 1)");
		lperr(l,"(fxrotate-bit-field 72 0 32 1)");
		lperr(l,"(fxrotate-bit-field 72 4 3 0)");
		lperr(l,"(fxrotate-bit-field 72 1 6 -1)");
		lperr(l,"(fxrotate-bit-field 72 1 6 5)");

		eqi  (l,"(fxreverse-bit-field #b1011011 1 5)", 87);
		eqi  (l,"(fxreverse-bit-field #b1011011 0 4)", 93);
		lperr(l,"(fxreverse-bit-field 72 0 -1 1)");
		lperr(l,"(fxreverse-bit-field 72 0 32 1)");
		lperr(l,"(fxreverse-bit-field 72 4 3 0)");
	}

}
