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
public class Decimal64Test extends TC {

	public void testGetExponent() {
		eq(Decimal64.getExponent(0x0400000000000000l), -383);
		eq(Decimal64.getExponent(0x0600000000000000l), -255);
		eq(Decimal64.getExponent(0x25fc000000000000l), 0);
		eq(Decimal64.getExponent(0x2600000000000000l), 1);
		eq(Decimal64.getExponent(0x4600000000000000l), 257);
		eq(Decimal64.getExponent(0x47fc000000000000l), 384);

		eq(Decimal64.getExponent(0x6000000000000000l), -383);
		eq(Decimal64.getExponent(0x6600000000000000l), -255);
		eq(Decimal64.getExponent(0x69fc000000000000l), 0);
		eq(Decimal64.getExponent(0x6a00000000000000l), 1);
		eq(Decimal64.getExponent(0x7200000000000000l), 257);
		eq(Decimal64.getExponent(0x73fc000000000000l), 384);
	}

	private int wpt(int p) {
		byte[] b = new byte[3];

		Decimal64.writedpd(p, b, 0);
		return new BCDNaturalNumber(b).intValue();
	}

	public void testWritepd() {
		eq(wpt(0x000), 0);
		eq(wpt(0x3e5), 765);
		eq(wpt(0x3f9), 779);
		eq(wpt(0x078),  78);
		eq(wpt(0x3fb), 797);
		eq(wpt(0x01b),  91);
		eq(wpt(0x37c), 876);
		eq(wpt(0x0ed), 961);
		eq(wpt(0x3df), 799);
		eq(wpt(0x05e),  98);
		eq(wpt(0x3bf), 979);
		eq(wpt(0x39f), 997);
		eq(wpt(0x08f), 981);
		eq(wpt(0x3ff), 999);
	}

	public void testGetFractionField() {
		eq(Decimal64.getFractionField(0x25fc000000000000l),
				BCDDecimal.parseBCD("1.000000000000000"));
		eq(Decimal64.getFractionField(0x25fc0000000003e5l),
				BCDDecimal.parseBCD("1.000000000000765"));
		eq(Decimal64.getFractionField(0x25fc0000000ffc00l),
				BCDDecimal.parseBCD("1.000000000999000"));
		eq(Decimal64.getFractionField(0x25fc00003e500000l),
				BCDDecimal.parseBCD("1.000000765000000"));
		eq(Decimal64.getFractionField(0x25fc00ffc0000000l),
				BCDDecimal.parseBCD("1.000999000000000"));
		eq(Decimal64.getFractionField(0x25ffe50000000000l),
				BCDDecimal.parseBCD("1.765000000000000"));
		eq(Decimal64.getFractionField(0x29fc000000000000l),
				BCDDecimal.parseBCD("2.000000000000000"));
		eq(Decimal64.getFractionField(0x2dfc000000000000l),
				BCDDecimal.parseBCD("3.000000000000000"));
		eq(Decimal64.getFractionField(0x31fc000000000000l),
				BCDDecimal.parseBCD("4.000000000000000"));
		eq(Decimal64.getFractionField(0x35fc000000000000l),
				BCDDecimal.parseBCD("5.000000000000000"));
		eq(Decimal64.getFractionField(0x39fc000000000000l),
				BCDDecimal.parseBCD("6.000000000000000"));
		eq(Decimal64.getFractionField(0x3dfc000000000000l),
				BCDDecimal.parseBCD("7.000000000000000"));
		eq(Decimal64.getFractionField(0x69fc000000000000l),
				BCDDecimal.parseBCD("8.000000000000000"));
		eq(Decimal64.getFractionField(0x6dfc000000000000l),
				BCDDecimal.parseBCD("9.000000000000000"));
	}

	public void testIsNaN() {
		ok(Decimal64.isNaN(0x7c00000000000000l));
		ok(Decimal64.isNaN(0x7e00000000000000l));
		ng(Decimal64.isNaN(0x25fc000000000000l));
	}

	public void testIsZero() {
		ok(Decimal64.isZero(0x0000000000000000l));
		ok(Decimal64.isZero(0x8000000000000000l));
		ng(Decimal64.isZero(0x25fc000000000000l));
	}

	public void testGetSignum() {
		eq(Decimal64.getSignum(0x0000000000000000l), 0);
		eq(Decimal64.getSignum(0x25fc000000000000l), 1);
		eq(Decimal64.getSignum(0xa5fc000000000000l), -1);
	}

	private int rpt(int p) {
		return (int)Decimal64.encodedec1(BCDDecimal.valueOf(p, 0), 0);
	}

	public void testEncodedec1() {
		eq(rpt(  0), 0x000);
		eq(rpt(765), 0x3e5);
		eq(rpt(779), 0x3f9);
		eq(rpt( 78), 0x078);
		eq(rpt(797), 0x3fb);
		eq(rpt( 91), 0x01b);
		eq(rpt(876), 0x37c);
		eq(rpt(961), 0x0ed);
		eq(rpt(799), 0x3df);
		eq(rpt( 98), 0x05e);
		eq(rpt(979), 0x3bf);
		eq(rpt(997), 0x39f);
		eq(rpt(981), 0x08f);
		eq(rpt(999), 0x0ff);
	}

	public void testEncodeDecimal() {
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("1.000000000000000"), 0),
				0x25fc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("1.000000000000765"), 0),
				0x25fc0000000003e5l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("1.000000000999000"), 0),
				0x25fc00000003fc00l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("1.000000765000000"), 0),
				0x25fc00003e500000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("1.000999000000000"), 0),
				0x25fc003fc0000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("1.765000000000000"), 0),
				0x25ffe50000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("2.000000000000000"), 0),
				0x29fc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("3.000000000000000"), 0),
				0x2dfc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("4.000000000000000"), 0),
				0x31fc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("5.000000000000000"), 0),
				0x35fc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("6.000000000000000"), 0),
				0x39fc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("7.000000000000000"), 0),
				0x3dfc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("8.000000000000000"), 0),
				0x69fc000000000000l);
		eq(Decimal64.encodeDecimal(BCDDecimal.parseBCD("9.000000000000000"), 0),
				0x6dfc000000000000l);
	}

	public void testToDecimal() {
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("0")), 0l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("1e-399")), 0l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("1e-398")), 1l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("765e-398")), 0x3e5l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("765e-392")), 0x3e500000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("765e-386")), 0x3e50000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("1e-383")), 0x0400000000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("1.000000000000000")),
				0x25fc000000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("9.999999999999999e384")),
				0x77FCFF3FCFF3FCFFl);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("1e385")),
				Decimal64.POSITIVE_INFINITY_BY_LONG);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-1e-399")),   0x8000000000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-1e-398")),   0x8000000000000001l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-765e-386")), 0x8003e50000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-1e-383")),   0x8400000000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-1")),        0xa5fc000000000000l);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-9.999999999999999e384")),
				0xf7FCFF3FCFF3FCFFl);
		eq(Decimal64.toDecimal(BCDDecimal.parseBCD("-1e385")),
				Decimal64.NEGATIVE_INFINITY_BY_LONG);
	}

	public void testToBCDDecimal() {
		eq(Decimal64.toBCDDecimal(0x8003e50000000000l),
				BCDDecimal.parseBCD("-765000000000000e-398"));
		eq(Decimal64.toBCDDecimal(0x25fc000000000000l),
				BCDDecimal.parseBCD("1.000000000000000"));
		nil(Decimal64.toBCDDecimal(Decimal64.POSITIVE_INFINITY_BY_LONG));
		nil(Decimal64.toBCDDecimal(Decimal64.NEGATIVE_INFINITY_BY_LONG));
		nil(Decimal64.toBCDDecimal(Decimal64.NaN_BY_LONG));
	}

	private void eqp(String a, String b, String r) {
		eq(Decimal64.add(
				Decimal64.parseDecimal(a),
				Decimal64.parseDecimal(b)),
				Decimal64.parseDecimal(r));
	}

	public void testAdd() {
		eqp("1.111111111111111", "1.111111111111111", "2.222222222222222");
		eqp("11.11111111111111", "1.111111111111111", "12.22222222222222");
		eqp("11.11111111111111", "1.111111111111115", "12.22222222222223");
		eqp("11.11111111111111", "0", "11.11111111111111");
		eqp("11.11111111111111", "-11.11111111111111", "0");
		eqp("111111111111111e-398", "111111111111111e-398", "222222222222222e-398");
		eqp("111111111111111e-398", "888888888888889e-398", "1.000000000000000e-383");
		eqp("9.999999999999999e384", "1.111111111111111e379", "Infinity");
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
		eq(Decimal64.subtract(
				Decimal64.parseDecimal(a),
				Decimal64.parseDecimal(b)),
				Decimal64.parseDecimal(r));
	}

	public void testSubtract() {
		eqm("1.111111111111111", "-1.111111111111111", "2.222222222222222");
		eqm("11.11111111111111", "-1.111111111111111", "12.22222222222222");
		eqm("11.11111111111111", "-1.111111111111115", "12.22222222222223");
		eqm("11.11111111111111", "0", "11.11111111111111");
		eqm("11.11111111111111", "11.11111111111111", "0");
		eqm("111111111111111e-398", "-111111111111111e-398", "222222222222222e-398");
		eqm("111111111111111e-398", "-888888888888889e-398", "1.000000000000000e-383");
		eqm("9.999999999999999e384", "-1.111111111111111e379", "Infinity");
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
		eq(Decimal64.multiply(
				Decimal64.parseDecimal(a),
				Decimal64.parseDecimal(b)),
				Decimal64.parseDecimal(r));
	}

	public void testMultiply() {
		eqk("1.111111111111111", "1.11111", "1.234566666666667");
		eqk("1e384", "10", "Infinity");
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
		eq(Decimal64.divide(
				Decimal64.parseDecimal(a),
				Decimal64.parseDecimal(b)),
				Decimal64.parseDecimal(r));
	}

	public void testDivide() {
		eqw("100", "2", "50");
		eqw("10", "3", "3.333333333333333");
		eqw("20", "3", "6.666666666666667");
		eqw("1e-383", "2", "500000000000000e-398");
		eqw("2e-398", "2", "1e-398");
		eqw("222", "2", "111");
		eqw("222", "200", "1.11");
		eqw("2.222222222222222", "2", "1.111111111111111");
		eqw("2.222222222222223", "2", "1.111111111111112");
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
		eq(Decimal64.round(Decimal64.parseDecimal(s), d), Decimal64.parseDecimal(x));
	}

	public void testRound() {
		eqrnd("1.2345", 2, "1.23");
		eqrnd("1.23456", 4, "1.2346");
		eqrnd("-1.2345", 2, "-1.23");
		eqrnd("-1.23456", 4, "-1.2346");
		eqrnd("1.999999999999999e-10", 24, "2.000000000000000e-10");
		eqrnd("1.999999999999999e-10", 25, "1.999999999999999e-10");
		eqrnd("1.999999999999999e-10", 26, "1.999999999999999e-10");
		eqrnd("1.999999999999999e10", 4, "2.000000000000000e10");
		eqrnd("1.999999999999999e10", 5, "1.999999999999999e10");
		eqrnd("1.999999999999999e10", 6, "1.999999999999999e10");
		eqrnd("199999999999999e-398", 397, "200000000000000e-398");
		eqrnd("199999999999999e-398", 398, "199999999999999e-398");
		eqrnd("199999999999999e-398", 399, "199999999999999e-398");
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
		eq(Decimal64.ceil(Decimal64.parseDecimal(s), d), Decimal64.parseDecimal(x));
	}

	public void testCeil() {
		eqcel("1.2345", 2, "1.24");
		eqcel("1.23456", 4, "1.2346");
		eqcel("-1.2345", 2, "-1.23");
		eqcel("-1.23456", 4, "-1.2345");
		eqcel("1.999999999999999e-10", 24, "2.000000000000000e-10");
		eqcel("1.999999999999999e-10", 25, "2.000000000000000e-10");
		eqcel("1.999999999999999e-10", 26, "1.999999999999999e-10");
		eqcel("1.999999999999999e10", 4, "2.000000000000000e10");
		eqcel("1.999999999999999e10", 5, "2.000000000000000e10");
		eqcel("1.999999999999999e10", 6, "1.999999999999999e10");
		eqcel("199999999999999e-398", 397, "200000000000000e-398");
		eqcel("199999999999999e-398", 398, "200000000000000e-398");
		eqcel("199999999999999e-398", 399, "199999999999999e-398");
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
		eq(Decimal64.floor(Decimal64.parseDecimal(s), d), Decimal64.parseDecimal(x));
	}

	public void testFloor() {
		eqflr("1.2345", 2, "1.23");
		eqflr("1.23456", 4, "1.2345");
		eqflr("-1.2345", 2, "-1.24");
		eqflr("-1.23456", 4, "-1.2346");
		eqflr("1.999999999999999e-10", 24, "1.999999999999990e-10");
		eqflr("1.999999999999999e-10", 25, "1.999999999999999e-10");
		eqflr("1.999999999999999e-10", 26, "1.999999999999999e-10");
		eqflr("1.999999999999999e10", 4, "1.999999999999990e10");
		eqflr("1.999999999999999e10", 5, "1.999999999999999e10");
		eqflr("1.999999999999999e10", 6, "1.999999999999999e10");
		eqflr("199999999999999e-398", 397, "199999999999990e-398");
		eqflr("199999999999999e-398", 398, "199999999999999e-398");
		eqflr("199999999999999e-398", 399, "199999999999999e-398");
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
		eq(Decimal64.compare(Decimal64.parseDecimal(a), Decimal64.parseDecimal(b)), c);
	}

	public void testCompare() {
		eqcmp("NaN", "NaN", 0);
		eqcmp("NaN", "Infinity", 1);
		eqcmp("Infinity", "NaN", -1);
		eqcmp("Infinity", "Infinity", 0);
		eqcmp("Infinity", "9.999999999999999e384", 1);
		eqcmp("9.999999999999999e384", "Infinity", -1);
		eqcmp("-Infinity", "-9.999999999999999e384", -1);
		eqcmp("-9.999999999999999e384", "-Infinity", 1);
		eqcmp("11.1", "1.11", 1);
		eqcmp("1.11", "11.1", -1);
		eqcmp("1.11", "1.101", 1);
		eqcmp("1.101", "1.11", -1);
		eqcmp("1.11", "1.11", 0);
		eqcmp("1e-383", "999999999999999e-398", 1);
		eqcmp("999999999999999e-398", "1e-383", -1);
		eqcmp("2e-398", "1e-398", 1);
		eqcmp("1e-398", "2e-398", -1);
	}

}
