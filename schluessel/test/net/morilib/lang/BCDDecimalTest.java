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
package net.morilib.lang;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/11/04
 */
public class BCDDecimalTest extends TC {

	private void ernum(String s) {
		try {
			BCDDecimal.parseBCD(s);
			fail();
		} catch(NumberFormatException e) {}
	}

	public void testParseBCD() {
		eq(BCDDecimal.parseBCD("72"), BCDDecimal.valueOf(72, 0));
		eq(BCDDecimal.parseBCD("7.2"), BCDDecimal.valueOf(72, 1));
		eq(BCDDecimal.parseBCD("0.72"), BCDDecimal.valueOf(72, 2));
		eq(BCDDecimal.parseBCD(".72"), BCDDecimal.valueOf(72, 2));
		eq(BCDDecimal.parseBCD("0.0072"), BCDDecimal.valueOf(72, 4));
		eq(BCDDecimal.parseBCD("-72"), BCDDecimal.valueOf(-72, 0));
		eq(BCDDecimal.parseBCD("+72"), BCDDecimal.valueOf(72, 0));
		eq(BCDDecimal.parseBCD("72e-1"), BCDDecimal.valueOf(72, 1));
		eq(BCDDecimal.parseBCD("72e+1"), BCDDecimal.valueOf(72, -1));
		eq(BCDDecimal.parseBCD("72e1"), BCDDecimal.valueOf(72, -1));
		eq(BCDDecimal.parseBCD("7.2e-1"), BCDDecimal.valueOf(72, 2));
		eq(BCDDecimal.parseBCD("7.2e+1"), BCDDecimal.valueOf(72, 0));
		eq(BCDDecimal.parseBCD("7.65e+1"), BCDDecimal.valueOf(765, 1));
		eq(BCDDecimal.parseBCD("00.0"), BCDDecimal.valueOf(0, 0));
		eq(BCDDecimal.parseBCD("00.0e72"), BCDDecimal.valueOf(0, 0));
		eq(BCDDecimal.parseBCD("00.0e-72"), BCDDecimal.valueOf(0, 0));

		ernum("");
		ernum("72.");
		ernum("7.6.5");
		ernum("7e6e5");
		ernum("++765");
		ernum("+7+65");
		ernum("+-765");
		ernum("+765e++72");
		ernum("72e9.1");
	}

	public void testAdd() {
		eq(BCDDecimal.valueOf(7.2).add(BCDDecimal.valueOf(91)),
				BCDDecimal.valueOf(98.2));
		eq(BCDDecimal.valueOf(91).add(BCDDecimal.valueOf(7.2)),
				BCDDecimal.valueOf(98.2));
		eq(BCDDecimal.valueOf(7.2).add(BCDDecimal.valueOf(0)),
				BCDDecimal.valueOf(7.2));
		eq(BCDDecimal.valueOf(0).add(BCDDecimal.valueOf(7.2)),
				BCDDecimal.valueOf(7.2));
		eq(BCDDecimal.valueOf(0).add(BCDDecimal.valueOf(0)),
				BCDDecimal.valueOf(0));
	}

	public void testSubtract() {
		eq(BCDDecimal.valueOf(7.2).subtract(BCDDecimal.valueOf(-91)),
				BCDDecimal.valueOf(98.2));
		eq(BCDDecimal.valueOf(91).subtract(BCDDecimal.valueOf(-7.2)),
				BCDDecimal.valueOf(98.2));
		eq(BCDDecimal.valueOf(7.2).subtract(BCDDecimal.valueOf(0)),
				BCDDecimal.valueOf(7.2));
		eq(BCDDecimal.valueOf(0).subtract(BCDDecimal.valueOf(-7.2)),
				BCDDecimal.valueOf(7.2));
		eq(BCDDecimal.valueOf(0).subtract(BCDDecimal.valueOf(0)),
				BCDDecimal.valueOf(0));
	}

	public void testMultiply() {
		eq(BCDDecimal.valueOf(7.2).multiply(BCDDecimal.valueOf(91)),
				BCDDecimal.valueOf(91*7.2));
		eq(BCDDecimal.valueOf(91).multiply(BCDDecimal.valueOf(7.2)),
				BCDDecimal.valueOf(91*7.2));
		eq(BCDDecimal.valueOf(7.2).multiply(BCDDecimal.valueOf(0)),
				BCDDecimal.valueOf(0));
		eq(BCDDecimal.valueOf(0).multiply(BCDDecimal.valueOf(7.2)),
				BCDDecimal.valueOf(0));
		eq(BCDDecimal.valueOf(0).multiply(BCDDecimal.valueOf(0)),
				BCDDecimal.valueOf(0));
	}

	public void testDivide() {
		eq(BCDDecimal.valueOf(91).divide(BCDDecimal.valueOf(13)),
				BCDDecimal.valueOf(7));
		eq(BCDDecimal.valueOf(91).divide(BCDDecimal.valueOf(0.7)),
				BCDDecimal.valueOf(130));
		eq(BCDDecimal.valueOf(100).divide(BCDDecimal.valueOf(2)),
				BCDDecimal.valueOf(50));
		eq(BCDDecimal.valueOf(0).divide(BCDDecimal.valueOf(7.2)),
				BCDDecimal.valueOf(0));
		try {
			BCDDecimal.valueOf(7.2).divide(BCDDecimal.valueOf(0));
			fail();
		} catch(ArithmeticException e) {}
		try {
			BCDDecimal.valueOf(0).divide(BCDDecimal.valueOf(0));
			fail();
		} catch(ArithmeticException e) {}
	}

	public void testRound() {
		eq(BCDDecimal.valueOf(12.24).round(1), BCDDecimal.valueOf(12.2));
		eq(BCDDecimal.valueOf(12.25).round(1), BCDDecimal.valueOf(12.3));
		eq(BCDDecimal.valueOf(12.25).round(0), BCDDecimal.valueOf(12));
		eq(BCDDecimal.valueOf(12.25).round(-1), BCDDecimal.valueOf(1, -1));
	}

	public void testLongValue() {
		eq(BCDDecimal.valueOf(12.44).longValue(), 12);
		eq(BCDDecimal.valueOf(12.54).longValue(), 13);
		eq(BCDDecimal.valueOf(1, -2).longValue(), 100);
	}

	public void testDoubleValue() {
		eq(BCDDecimal.valueOf(1, 1075).doubleValue(), 0);
		eq(BCDDecimal.valueOf(1, 11).doubleValue(), 1e-11);
		eq(BCDDecimal.valueOf(1, 10).doubleValue(), 1e-10);
		eq(BCDDecimal.valueOf(1, 9).doubleValue(), 1e-9);
		eq(BCDDecimal.valueOf(1.23).doubleValue(), 1.23);
		eq(BCDDecimal.valueOf(1, -1).doubleValue(), 10);
		eq(BCDDecimal.valueOf(1, -10).doubleValue(), 1e10);
	}

	public void testCompareTo() {
		eq(BCDDecimal.valueOf(72).compareTo(BCDDecimal.valueOf(9.1)), 1);
		eq(BCDDecimal.valueOf(9.1).compareTo(BCDDecimal.valueOf(72)), -1);
		eq(BCDDecimal.valueOf(7.2).compareTo(BCDDecimal.valueOf(9.1)), -1);
		eq(BCDDecimal.valueOf(9.1).compareTo(BCDDecimal.valueOf(7.2)), 1);
		eq(BCDDecimal.valueOf(7.2).compareTo(BCDDecimal.valueOf(7.2)), 0);
	}
}
