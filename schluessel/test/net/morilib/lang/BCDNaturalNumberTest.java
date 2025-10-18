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
 * @author MORIGUCHI, Yuichiro 2011/10/31
 */
public class BCDNaturalNumberTest extends TC {

	public void testBCDNaturalNumber() {
		eq(new BCDNaturalNumber(new byte[] { 1, 2, 3, 0 }),
				new BCDNaturalNumber(new byte[] { 1, 2, 3 }));
	}

	public void testAdd() {
		eq(BCDNaturalNumber.parseBCD("123").add(BCDNaturalNumber.parseBCD("234")),
				BCDNaturalNumber.parseBCD("357"));
		eq(BCDNaturalNumber.parseBCD("123").add(BCDNaturalNumber.parseBCD("237")),
				BCDNaturalNumber.parseBCD("360"));
		eq(BCDNaturalNumber.parseBCD("123").add(BCDNaturalNumber.parseBCD("877")),
				BCDNaturalNumber.parseBCD("1000"));
		eq(BCDNaturalNumber.parseBCD("12").add(BCDNaturalNumber.parseBCD("988")),
				BCDNaturalNumber.parseBCD("1000"));
		eq(BCDNaturalNumber.parseBCD("988").add(BCDNaturalNumber.parseBCD("12")),
				BCDNaturalNumber.parseBCD("1000"));
		eq(BCDNaturalNumber.parseBCD("123").add(BCDNaturalNumber.parseBCD("0")),
				BCDNaturalNumber.parseBCD("123"));
	}

	public void testSub() {
		eq(BCDNaturalNumber.parseBCD("234").subtract(BCDNaturalNumber.parseBCD("123")),
				BCDNaturalNumber.parseBCD("111"));
		eq(BCDNaturalNumber.parseBCD("234").subtract(BCDNaturalNumber.parseBCD("5")),
				BCDNaturalNumber.parseBCD("229"));
		eq(BCDNaturalNumber.parseBCD("1000").subtract(BCDNaturalNumber.parseBCD("988")),
				BCDNaturalNumber.parseBCD("12"));
		eq(BCDNaturalNumber.parseBCD("72").subtract(BCDNaturalNumber.parseBCD("0")),
				BCDNaturalNumber.parseBCD("72"));
		eq(BCDNaturalNumber.parseBCD("72").subtract(BCDNaturalNumber.parseBCD("72")),
				BCDNaturalNumber.parseBCD("0"));
		try {
			BCDNaturalNumber.parseBCD("72").subtract(BCDNaturalNumber.parseBCD("73"));
			fail();
		} catch(ArithmeticException e) {}
		try {
			BCDNaturalNumber.parseBCD("0").subtract(BCDNaturalNumber.parseBCD("73"));
			fail();
		} catch(ArithmeticException e) {}
	}

	public void testMultiply() {
		eq(BCDNaturalNumber.parseBCD("72").multiply(BCDNaturalNumber.parseBCD("91")).longValue(),
				72 * 91);
		eq(BCDNaturalNumber.parseBCD("86").multiply(BCDNaturalNumber.parseBCD("78")).longValue(),
				86 * 78);
		eq(BCDNaturalNumber.parseBCD("11111").multiply(
				BCDNaturalNumber.parseBCD("11")).longValue(), 11111 * 11);
		eq(BCDNaturalNumber.parseBCD("72").multiply(BCDNaturalNumber.parseBCD("0")).longValue(),
				0);
		eq(BCDNaturalNumber.parseBCD("0").multiply(BCDNaturalNumber.parseBCD("72")).longValue(),
				0);

		eq(BCDNaturalNumber.parseBCD("72").multiply(91).longValue(), 72 * 91);
		eq(BCDNaturalNumber.parseBCD("86").multiply(78).longValue(), 86 * 78);
		eq(BCDNaturalNumber.parseBCD("11111").multiply(11).longValue(), 11111 * 11);
		eq(BCDNaturalNumber.parseBCD("72").multiply(0).longValue(), 0);
		eq(BCDNaturalNumber.parseBCD("0").multiply(72).longValue(), 0);
		try {
			BCDNaturalNumber.parseBCD("72").multiply(-1);  fail();
		} catch(ArithmeticException e) {}
	}

	public void testDivide() {
		eq(BCDNaturalNumber.valueOf(72 * 91).divide(
				BCDNaturalNumber.valueOf(72)).intValue(), 91);
		eq(BCDNaturalNumber.valueOf(72 * 91).divide(
				BCDNaturalNumber.valueOf(91)).intValue(), 72);
		eq(BCDNaturalNumber.valueOf(86 * 78).divide(
				BCDNaturalNumber.valueOf(86)).intValue(), 78);
		eq(BCDNaturalNumber.valueOf(86 * 78).divide(
				BCDNaturalNumber.valueOf(78)).intValue(), 86);
		eq(BCDNaturalNumber.valueOf(86 * 19).divide(
				BCDNaturalNumber.valueOf(19)).intValue(), 86);
		eq(BCDNaturalNumber.valueOf(122221).divide(
				BCDNaturalNumber.valueOf(11)).intValue(), 11111);
		eq(BCDNaturalNumber.valueOf(122221).divide(
				BCDNaturalNumber.valueOf(11111)).intValue(), 11);
		eq(BCDNaturalNumber.valueOf(9999).divide(
				BCDNaturalNumber.valueOf(11)).intValue(), 9999 / 11);
		eq(BCDNaturalNumber.valueOf(100).divide(
				BCDNaturalNumber.valueOf(99)).intValue(), 1);
		eq(BCDNaturalNumber.valueOf(99).divide(
				BCDNaturalNumber.valueOf(33)).intValue(), 3);
		eq(BCDNaturalNumber.valueOf(99).divide(
				BCDNaturalNumber.valueOf(99)).intValue(), 1);
		eq(BCDNaturalNumber.valueOf(98).divide(
				BCDNaturalNumber.valueOf(99)).intValue(), 0);
		eq(BCDNaturalNumber.valueOf(0).divide(
				BCDNaturalNumber.valueOf(99)).intValue(), 0);
		try {
			BCDNaturalNumber.valueOf(2).divide(BCDNaturalNumber.valueOf(0));
			fail();
		} catch(ArithmeticException e) {}
		try {
			BCDNaturalNumber.valueOf(0).divide(BCDNaturalNumber.valueOf(0));
			fail();
		} catch(ArithmeticException e) {}
	}

	public void testRemainder() {
		eq(BCDNaturalNumber.valueOf(72 * 91 + 3).remainder(
				BCDNaturalNumber.valueOf(72)).intValue(), 3);
		eq(BCDNaturalNumber.valueOf(72 * 91).remainder(
				BCDNaturalNumber.valueOf(72)), BCDNaturalNumber.ZERO);
		eq(BCDNaturalNumber.valueOf(72 * 91 + 71).remainder(
				BCDNaturalNumber.valueOf(72)).intValue(), 71);
	}

	public void testSucc() {
		eq(BCDNaturalNumber.valueOf(72).succ().intValue(), 73);
		eq(BCDNaturalNumber.valueOf(799).succ().intValue(), 800);
		eq(BCDNaturalNumber.valueOf(99).succ().intValue(), 100);
		eq(BCDNaturalNumber.valueOf(0).succ().intValue(), 1);
	}

	public void testPrev() {
		eq(BCDNaturalNumber.valueOf(72).prev().intValue(), 71);
		eq(BCDNaturalNumber.valueOf(100).prev().intValue(), 99);
		eq(BCDNaturalNumber.valueOf(700).prev().intValue(), 699);
		eq(BCDNaturalNumber.valueOf(1).prev().intValue(), 0);
		try {
			BCDNaturalNumber.valueOf(0).prev(); fail();
		} catch(ArithmeticException e) {}
	}

	public void testCompareTo() {
		eq(BCDNaturalNumber.parseBCD("91").compareTo(BCDNaturalNumber.parseBCD("72")), 1);
		eq(BCDNaturalNumber.parseBCD("72").compareTo(BCDNaturalNumber.parseBCD("91")), -1);
		eq(BCDNaturalNumber.parseBCD("72").compareTo(BCDNaturalNumber.parseBCD("72")), 0);
		eq(BCDNaturalNumber.parseBCD("73").compareTo(BCDNaturalNumber.parseBCD("72")), 1);
		eq(BCDNaturalNumber.parseBCD("72").compareTo(BCDNaturalNumber.parseBCD("73")), -1);
		eq(BCDNaturalNumber.parseBCD("72").compareTo(BCDNaturalNumber.parseBCD("9")), 1);
		eq(BCDNaturalNumber.parseBCD("9").compareTo(BCDNaturalNumber.parseBCD("72")), -1);
		eq(BCDNaturalNumber.parseBCD("72").compareTo(BCDNaturalNumber.parseBCD("0")), 1);
		eq(BCDNaturalNumber.parseBCD("0").compareTo(BCDNaturalNumber.parseBCD("72")), -1);
	}

	public void testLongValue() {
		eq(BCDNaturalNumber.parseBCD("72").longValue(), 72);
		eq(BCDNaturalNumber.parseBCD("765").longValue(), 765);
		eq(BCDNaturalNumber.parseBCD("0").longValue(), 0);
	}

	public void testDoubleValue() {
		eq(BCDNaturalNumber.parseBCD("72").doubleValue(), 72.0);
		eq(BCDNaturalNumber.parseBCD("765").doubleValue(), 765.0);
		eq(BCDNaturalNumber.parseBCD("0").doubleValue(), 0.0);
		eq(BCDNaturalNumber.parseBCD("1000000000000000000").doubleValue(), 1e18);
	}

	public void testParseBCD() {
		try {
			BCDNaturalNumber.parseBCD("-1");  fail();
		} catch(NumberFormatException e) {}
		try {
			BCDNaturalNumber.parseBCD("aaa");  fail();
		} catch(NumberFormatException e) {}
	}

	public void testToEbcdicZoneBCD() {
		eq(BCDNaturalNumber.parseBCD("72").toEbcdicZoneBCD(),
				new byte[] { (byte)0xf7, (byte)0xc2 });
		eq(BCDNaturalNumber.parseBCD("765").toEbcdicZoneBCD(),
				new byte[] { (byte)0xf7, (byte)0xf6, (byte)0xc5 });
		eq(BCDNaturalNumber.parseBCD("5").toEbcdicZoneBCD(),
				new byte[] { (byte)0xc5 });
		eq(BCDNaturalNumber.parseBCD("0").toEbcdicZoneBCD(),
				new byte[] { (byte)0xc0 });
	}

	public void testToPackedBCD() {
		eq(BCDNaturalNumber.parseBCD("765").toPackedBCD(),
				new byte[] { (byte)0x76, (byte)0x5c });
		eq(BCDNaturalNumber.parseBCD("72").toPackedBCD(),
				new byte[] { (byte)0x07, (byte)0x2c });
		eq(BCDNaturalNumber.parseBCD("5").toPackedBCD(),
				new byte[] { (byte)0x5c });
		eq(BCDNaturalNumber.parseBCD("915986").toPackedBCD(),
				new byte[] { (byte)0x09, (byte)0x15, (byte)0x98, (byte)0x6c });
		eq(BCDNaturalNumber.parseBCD("0").toPackedBCD(),
				new byte[] { (byte)0x0c });
	}

	public void testShift() {
		eq(BCDNaturalNumber.parseBCD("72").shift(2).longValue(), 7200);
		eq(BCDNaturalNumber.parseBCD("7200").shift(-2).longValue(), 72);
		eq(BCDNaturalNumber.parseBCD("72").shift(0).longValue(), 72);
		eq(BCDNaturalNumber.parseBCD("7200").shift(-3).longValue(), 7);
		eq(BCDNaturalNumber.parseBCD("7200").shift(-4).longValue(), 0);
		eq(BCDNaturalNumber.parseBCD("7200").shift(-5).longValue(), 0);
		eq(BCDNaturalNumber.parseBCD("0").shift(2).longValue(), 0);
		eq(BCDNaturalNumber.parseBCD("0").shift(-2).longValue(), 0);
	}

	public void testGetDigit() {
		eq(BCDNaturalNumber.parseBCD("72").getDigit(0), 2);
		eq(BCDNaturalNumber.parseBCD("72").getDigit(1), 7);
		eq(BCDNaturalNumber.parseBCD("72").getDigit(2), 0);
		eq(BCDNaturalNumber.parseBCD("72").getDigit(-1), 0);
	}

	public void testValueOf() {
		eq(BCDNaturalNumber.valueOf(72).intValue(), 72);
		eq(BCDNaturalNumber.valueOf(765).intValue(), 765);
		eq(BCDNaturalNumber.valueOf(0).intValue(), 0);
		try {
			BCDNaturalNumber.valueOf(-1); fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testEquals() {
		ok(BCDNaturalNumber.parseBCD("72").equals(BCDNaturalNumber.parseBCD("72")));
		ng(BCDNaturalNumber.parseBCD("72").equals(BCDNaturalNumber.parseBCD("91")));
		ng(BCDNaturalNumber.parseBCD("72").equals(null));
		ng(BCDNaturalNumber.parseBCD("72").equals(Integer.valueOf(72)));
	}

	public void testToString() {
		eq(BCDNaturalNumber.parseBCD("72").toString(), "72");
		eq(BCDNaturalNumber.parseBCD("0").toString(), "0");
	}

}
