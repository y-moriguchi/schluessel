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
 * @author MORIGUCHI, Yuichiro 2011/11/05
 */
public class Decimal32Test extends TC {

	public void testGetExponent() {
		eq(Decimal32.getExponent(0x04000000), -95);
		eq(Decimal32.getExponent(0x06000000), -63);
		eq(Decimal32.getExponent(0x25f00000), 0);
		eq(Decimal32.getExponent(0x26000000), 1);
		eq(Decimal32.getExponent(0x46000000), 65);
		eq(Decimal32.getExponent(0x47f00000), 96);

		eq(Decimal32.getExponent(0x60000000), -95);
		eq(Decimal32.getExponent(0x66000000), -63);
		eq(Decimal32.getExponent(0x69fc0000), 0);
		eq(Decimal32.getExponent(0x6a000000), 1);
		eq(Decimal32.getExponent(0x72000000), 65);
		eq(Decimal32.getExponent(0x73fc0000), 96);
	}

	public void testGetFractionField() {
		eq(Decimal32.getFractionField(0x25f00000),
				BCDDecimal.parseBCD("1.000000"));
		eq(Decimal32.getFractionField(0x25f003e5),
				BCDDecimal.parseBCD("1.000765"));
		eq(Decimal32.getFractionField(0x25fffc00),
				BCDDecimal.parseBCD("1.999000"));
		eq(Decimal32.getFractionField(0x29f00000),
				BCDDecimal.parseBCD("2.000000"));
		eq(Decimal32.getFractionField(0x2df00000),
				BCDDecimal.parseBCD("3.000000"));
		eq(Decimal32.getFractionField(0x31f00000),
				BCDDecimal.parseBCD("4.000000"));
		eq(Decimal32.getFractionField(0x35f00000),
				BCDDecimal.parseBCD("5.000000"));
		eq(Decimal32.getFractionField(0x39f00000),
				BCDDecimal.parseBCD("6.000000"));
		eq(Decimal32.getFractionField(0x3df00000),
				BCDDecimal.parseBCD("7.000000"));
		eq(Decimal32.getFractionField(0x69f00000),
				BCDDecimal.parseBCD("8.000000"));
		eq(Decimal32.getFractionField(0x6df00000),
				BCDDecimal.parseBCD("9.000000"));
	}

	public void testIsNaN() {
		ok(Decimal32.isNaN(0x7c000000));
		ok(Decimal32.isNaN(0x7e000000));
		ng(Decimal32.isNaN(0x25f00000));
	}

	public void testIsZero() {
		ok(Decimal32.isZero(0x00000000));
		ok(Decimal32.isZero(0x80000000));
		ng(Decimal32.isZero(0x25f00000));
	}

	public void testGetSignum() {
		eq(Decimal32.getSignum(0x00000000), 0);
		eq(Decimal32.getSignum(0x25f00000), 1);
		eq(Decimal32.getSignum(0xa5f00000), -1);
	}

	public void testEncodeDecimal() {
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("1.000000"), 0),
				0x25f00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("1.000765"), 0),
				0x25f003e5);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("1.999000"), 0),
				0x25f3fc00);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("2.000000"), 0),
				0x29f00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("3.000000"), 0),
				0x2df00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("4.000000"), 0),
				0x31f00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("5.000000"), 0),
				0x35f00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("6.000000"), 0),
				0x39f00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("7.000000"), 0),
				0x3df00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("8.000000"), 0),
				0x69f00000);
		eq(Decimal32.encodeDecimal(BCDDecimal.parseBCD("9.000000"), 0),
				0x6df00000);
	}

	public void testToDecimal() {
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("0")), 0);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("1e-102")), 0);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("1e-101")), 1);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("765e-101")), 0x3e5);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("1e-95")), 0x04000000);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("1.000000")),
				0x25f00000);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("9.999999e96")),
				0x77F3FCFF);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("1e97")),
				Decimal32.POSITIVE_INFINITY_BY_INT);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("-1e-102")),   0x80000000);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("-1e-101")),   0x80000001);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("-1e-95")),    0x84000000);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("-1")),        0xa5f00000);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("-9.999999e96")),
				0xf7F3FCFF);
		eq(Decimal32.toDecimal(BCDDecimal.parseBCD("-1e97")),
				Decimal32.NEGATIVE_INFINITY_BY_INT);
	}

	public void testToBCDDecimal() {
		eq(Decimal32.toBCDDecimal(0x800003e5),
				BCDDecimal.parseBCD("-765e-101"));
		eq(Decimal32.toBCDDecimal(0x25f00000),
				BCDDecimal.parseBCD("1.000000"));
		nil(Decimal32.toBCDDecimal(Decimal32.POSITIVE_INFINITY_BY_INT));
		nil(Decimal32.toBCDDecimal(Decimal32.NEGATIVE_INFINITY_BY_INT));
		nil(Decimal32.toBCDDecimal(Decimal32.NaN_BY_INT));
	}

	private void eqp(String a, String b, String r) {
		eq(Decimal32.add(
				Decimal32.parseDecimal(a),
				Decimal32.parseDecimal(b)),
				Decimal32.parseDecimal(r));
	}

	public void testAdd() {
		eqp("1.111111", "1.111111", "2.222222");
		eqp("11.11111", "1.111111", "12.22222");
		eqp("11.11111", "1.111115", "12.22223");
		eqp("11.11111", "0", "11.11111");
		eqp("11.11111", "-11.11111", "0");
		eqp("111111e-101", "111111e-101", "222222e-101");
		eqp("111111e-101", "888889e-101", "1.000000e-95");
		eqp("9.999999e96", "1.111111e90", "Infinity");
		eqp("Infinity", "Infinity", "Infinity");
		eqp("Infinity", "0", "Infinity");
		eqp("1", "Infinity", "Infinity");
		eqp("-Infinity", "-Infinity", "-Infinity");
		eqp("Infinity", "-Infinity", "NaN");
		eqp("-Infinity", "Infinity", "NaN");
		eqp("NaN", "-Infinity", "NaN");
		eqp("Infinity", "NaN", "NaN");
		eqp("NaN", "NaN", "NaN");
	}

	private void eqm(String a, String b, String r) {
		eq(Decimal32.subtract(
				Decimal32.parseDecimal(a),
				Decimal32.parseDecimal(b)),
				Decimal32.parseDecimal(r));
	}

	public void testSubtract() {
		eqm("1.111111", "-1.111111", "2.222222");
		eqm("11.11111", "-1.111111", "12.22222");
		eqm("11.11111", "-1.111115", "12.22223");
		eqm("11.11111", "0", "11.11111");
		eqm("11.11111", "11.11111", "0");
		eqm("111111e-101", "-111111e-101", "222222e-101");
		eqm("111111e-101", "-888889e-101", "1.000000e-95");
		eqm("9.999999e96", "-1.111111e90", "Infinity");
		eqm("Infinity", "-Infinity", "Infinity");
		eqm("Infinity", "0", "Infinity");
		eqm("1", "-Infinity", "Infinity");
		eqm("-Infinity", "Infinity", "-Infinity");
		eqm("Infinity", "Infinity", "NaN");
		eqm("-Infinity", "-Infinity", "NaN");
		eqm("NaN", "Infinity", "NaN");
		eqm("Infinity", "NaN", "NaN");
		eqm("NaN", "NaN", "NaN");
	}

	private void eqk(String a, String b, String r) {
		eq(Decimal32.multiply(
				Decimal32.parseDecimal(a),
				Decimal32.parseDecimal(b)),
				Decimal32.parseDecimal(r));
	}

	public void testMultiply() {
		eqk("1.111111", "1.11111", "1.234567");
		eqk("1e96", "10", "Infinity");
		eqk("Infinity", "1", "Infinity");
		eqk("Infinity", "-1", "-Infinity");
		eqk("Infinity", "0", "0");
		eqk("Infinity", "Infinity", "Infinity");
		eqk("Infinity", "-Infinity", "-Infinity");
		eqk("1", "Infinity", "Infinity");
		eqk("-1", "Infinity", "-Infinity");
		eqk("0", "Infinity", "0");
		eqk("Infinity", "Infinity", "Infinity");
		eqk("-Infinity", "Infinity", "-Infinity");
		eqk("NaN", "Infinity", "NaN");
		eqk("Infinity", "NaN", "NaN");
		eqk("NaN", "NaN", "NaN");
	}

	private void eqw(String a, String b, String r) {
		eq(Decimal32.divide(
				Decimal32.parseDecimal(a),
				Decimal32.parseDecimal(b)),
				Decimal32.parseDecimal(r));
	}

	public void testDivide() {
		eqw("100", "2", "50");
		eqw("10", "3", "3.333333");
		eqw("20", "3", "6.666667");
		eqw("1e-95", "2", "500000e-101");
		eqw("2e-101", "2", "1e-101");
		eqw("222", "2", "111");
		eqw("222", "200", "1.11");
		eqw("2.222222", "2", "1.111111");
		eqw("2.222223", "2", "1.111112");
		eqw("1", "0", "Infinity");
		eqw("-1", "0", "-Infinity");
		eqw("Infinity", "0", "Infinity");
		eqw("-Infinity", "0", "-Infinity");
		eqw("0", "0", "NaN");
		eqw("1", "Infinity", "0");
		eqw("0", "Infinity", "0");
		eqw("Infinity", "Infinity", "NaN");
	}

	private void eqrnd(String s, int d, String x) {
		eq(Decimal32.round(Decimal32.parseDecimal(s), d), Decimal32.parseDecimal(x));
	}

	public void testRound() {
		eqrnd("1.2345", 2, "1.23");
		eqrnd("1.23456", 4, "1.2346");
		eqrnd("-1.2345", 2, "-1.23");
		eqrnd("-1.23456", 4, "-1.2346");
		eqrnd("1.999999e-10", 15, "2.000000e-10");
		eqrnd("1.999999e-10", 16, "1.999999e-10");
		eqrnd("1.999999e-10", 17, "1.999999e-10");
		eqrnd("1.999999e10", -5, "2.000000e10");
		eqrnd("1.999999e10", -4, "1.999999e10");
		eqrnd("1.999999e10", -3, "1.999999e10");
		eqrnd("199999e-101", 100, "200000e-101");
		eqrnd("199999e-101", 101, "199999e-101");
		eqrnd("199999e-101", 102, "199999e-101");
		eqrnd("1.2345", 0, "1");
		eqrnd("1.2345", -1, "0");
		eqrnd("1.2345", -2, "0");
		eqrnd("5.2345", 0, "5");
		eqrnd("5.2345", -1, "10");
		eqrnd("5.2345", -2, "0");
		eqrnd("-1.2345", 0, "-1");
		eqrnd("-1.2345", -1, "-0");
		eqrnd("-1.2345", -2, "-0");
		eqrnd("-5.2345", 0, "-5");
		eqrnd("-5.2345", -1, "-10");
		eqrnd("-5.2345", -2, "-0");
		eqrnd("Infinity", 0, "Infinity");
		eqrnd("-Infinity", 0, "-Infinity");
		eqrnd("NaN", 0, "NaN");
	}

	private void eqcel(String s, int d, String x) {
		eq(Decimal32.ceil(Decimal32.parseDecimal(s), d), Decimal32.parseDecimal(x));
	}

	public void testCeil() {
		eqcel("1.2345", 2, "1.24");
		eqcel("1.23456", 4, "1.2346");
		eqcel("-1.2345", 2, "-1.23");
		eqcel("-1.23456", 4, "-1.2345");
		eqcel("1.999999e-10", 15, "2.000000e-10");
		eqcel("1.999999e-10", 16, "2.000000e-10");
		eqcel("1.999999e-10", 17, "1.999999e-10");
		eqcel("1.999999e10", -5, "2.000000e10");
		eqcel("1.999999e10", -4, "2.000000e10");
		eqcel("1.999999e10", -3, "1.999999e10");
		eqcel("199999e-101", 100, "200000e-101");
		eqcel("199999e-101", 101, "200000e-101");
		eqcel("199999e-101", 102, "199999e-101");
		eqcel("1.2345", 0, "2");
		eqcel("1.2345", -1, "10");
		eqcel("1.2345", -2, "100");
		eqcel("5.2345", 0, "6");
		eqcel("5.2345", -1, "10");
		eqcel("5.2345", -2, "100");
		eqcel("-1.2345", 0, "-1");
		eqcel("-1.2345", -1, "0");
		eqcel("-1.2345", -2, "0");
		eqcel("-5.2345", 0, "-5");
		eqcel("-5.2345", -1, "0");
		eqcel("-5.2345", -2, "0");
		eqcel("Infinity", 0, "Infinity");
		eqcel("-Infinity", 0, "-Infinity");
		eqcel("NaN", 0, "NaN");
	}

	private void eqflr(String s, int d, String x) {
		eq(Decimal32.floor(Decimal32.parseDecimal(s), d), Decimal32.parseDecimal(x));
	}

	public void testFloor() {
		eqflr("1.2345", 2, "1.23");
		eqflr("1.23456", 4, "1.2345");
		eqflr("-1.2345", 2, "-1.24");
		eqflr("-1.23456", 4, "-1.2346");
		eqflr("1.999999e-10", 15, "1.999990e-10");
		eqflr("1.999999e-10", 16, "1.999999e-10");
		eqflr("1.999999e-10", 17, "1.999999e-10");
		eqflr("1.999999e10", -5, "1.999990e10");
		eqflr("1.999999e10", -4, "1.999999e10");
		eqflr("1.999999e10", -3, "1.999999e10");
		eqflr("199999e-101", 100, "199990e-101");
		eqflr("199999e-101", 101, "199999e-101");
		eqflr("199999e-101", 102, "199999e-101");
		eqflr("1.2345", 0, "1");
		eqflr("1.2345", -1, "0");
		eqflr("1.2345", -2, "0");
		eqflr("5.2345", 0, "5");
		eqflr("5.2345", -1, "0");
		eqflr("5.2345", -2, "0");
		eqflr("-1.2345", 0, "-2");
		eqflr("-1.2345", -1, "-10");
		eqflr("-1.2345", -2, "-100");
		eqflr("-5.2345", 0, "-6");
		eqflr("-5.2345", -1, "-10");
		eqflr("-5.2345", -2, "-100");
		eqflr("Infinity", 0, "Infinity");
		eqflr("-Infinity", 0, "-Infinity");
		eqflr("NaN", 0, "NaN");
	}

	private void eqcmp(String a, String b, int c) {
		eq(Decimal32.compare(Decimal32.parseDecimal(a), Decimal32.parseDecimal(b)), c);
	}

	public void testCompare() {
		eqcmp("NaN", "NaN", 0);
		eqcmp("NaN", "Infinity", 1);
		eqcmp("Infinity", "NaN", -1);
		eqcmp("Infinity", "Infinity", 0);
		eqcmp("Infinity", "9.999999e96", 1);
		eqcmp("9.999999e96", "Infinity", -1);
		eqcmp("-Infinity", "-9.999999e96", -1);
		eqcmp("-9.999999e96", "-Infinity", 1);
		eqcmp("11.1", "1.11", 1);
		eqcmp("1.11", "11.1", -1);
		eqcmp("1.11", "1.101", 1);
		eqcmp("1.101", "1.11", -1);
		eqcmp("1.11", "1.11", 0);
		eqcmp("1e-95", "999999e-101", 1);
		eqcmp("999999e-101", "1e-95", -1);
		eqcmp("2e-101", "1e-101", 1);
		eqcmp("1e-101", "2e-101", -1);
		eqcmp("1e-101", "0", 1);
		eqcmp("0", "1e-101", -1);
	}

}
