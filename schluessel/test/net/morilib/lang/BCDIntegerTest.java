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
 * @author MORIGUCHI, Yuichiro 2011/11/01
 */
public class BCDIntegerTest extends TC {

	public void testAdd() {
		eq(BCDInteger.valueOf(72).add(BCDInteger.valueOf(91)).intValue(), 163);
		eq(BCDInteger.valueOf(-72).add(BCDInteger.valueOf(-91)).intValue(), -163);
		eq(BCDInteger.valueOf(72).add(BCDInteger.valueOf(-91)).intValue(), -19);
		eq(BCDInteger.valueOf(-72).add(BCDInteger.valueOf(91)).intValue(), 19);
		eq(BCDInteger.valueOf(-72).add(BCDInteger.valueOf(72)).intValue(), 0);
		eq(BCDInteger.valueOf(72).add(BCDInteger.valueOf(-72)).intValue(), 0);
	}

	public void testSubtract() {
		eq(BCDInteger.valueOf(72).subtract(BCDInteger.valueOf(91)).intValue(), -19);
		eq(BCDInteger.valueOf(-72).subtract(BCDInteger.valueOf(-91)).intValue(), 19);
		eq(BCDInteger.valueOf(72).subtract(BCDInteger.valueOf(-91)).intValue(), 163);
		eq(BCDInteger.valueOf(-72).subtract(BCDInteger.valueOf(91)).intValue(), -163);
		eq(BCDInteger.valueOf(-72).subtract(BCDInteger.valueOf(-72)).intValue(), 0);
		eq(BCDInteger.valueOf(72).subtract(BCDInteger.valueOf(72)).intValue(), 0);
	}

	public void testMultiply() {
		eq(BCDInteger.valueOf(72).multiply(BCDInteger.valueOf(91)).intValue(), 72*91);
		eq(BCDInteger.valueOf(-72).multiply(BCDInteger.valueOf(-91)).intValue(), 72*91);
		eq(BCDInteger.valueOf(72).multiply(BCDInteger.valueOf(-91)).intValue(), -72*91);
		eq(BCDInteger.valueOf(-72).multiply(BCDInteger.valueOf(91)).intValue(), -72*91);
		eq(BCDInteger.valueOf(-72).multiply(BCDInteger.valueOf(0)).intValue(), 0);
		eq(BCDInteger.valueOf(0).multiply(BCDInteger.valueOf(72)).intValue(), 0);
	}

	public void testDivide() {
		eq(BCDInteger.valueOf(72*91).divide(BCDInteger.valueOf(91)).intValue(), 72);
		eq(BCDInteger.valueOf(-72*91).divide(BCDInteger.valueOf(-91)).intValue(), 72);
		eq(BCDInteger.valueOf(72*91).divide(BCDInteger.valueOf(-91)).intValue(), -72);
		eq(BCDInteger.valueOf(-72*91).divide(BCDInteger.valueOf(91)).intValue(), -72);
		eq(BCDInteger.valueOf(0).divide(BCDInteger.valueOf(72)).intValue(), 0);
		try {
			BCDInteger.valueOf(2).divide(BCDInteger.ZERO); fail();
		} catch(ArithmeticException e) {}
	}

	public void testRemainder() {
		eq(BCDInteger.valueOf(72*91+2).remainder(BCDInteger.valueOf(91)).intValue(), 2);
		eq(BCDInteger.valueOf(-72*91+2).remainder(BCDInteger.valueOf(-91)).intValue(),
				(-72*91+2)%91);
		eq(BCDInteger.valueOf(72*91+2).remainder(BCDInteger.valueOf(-91)).intValue(),
				(72*91+2)%(-91));
		eq(BCDInteger.valueOf(-72*91+2).remainder(BCDInteger.valueOf(91)).intValue(),
				(-72*91+2)%(-91));
		eq(BCDInteger.valueOf(0).remainder(BCDInteger.valueOf(72)).intValue(), 0);
		try {
			BCDInteger.valueOf(2).divide(BCDInteger.ZERO); fail();
		} catch(ArithmeticException e) {}
	}

	public void testCompareTo() {
		eq(BCDInteger.valueOf(10).compareTo(BCDInteger.valueOf(-5)), 1);
		eq(BCDInteger.valueOf(-5).compareTo(BCDInteger.valueOf(10)), -1);
		eq(BCDInteger.valueOf(-10).compareTo(BCDInteger.valueOf(-5)), -1);
		eq(BCDInteger.valueOf(5).compareTo(BCDInteger.valueOf(10)), -1);
	}

	public void testToEbcdicZoneBCD() {
		eq(BCDInteger.parseBCD("72").toEbcdicZoneBCD(),
				new byte[] { (byte)0xf7, (byte)0xc2 });
		eq(BCDInteger.parseBCD("765").toEbcdicZoneBCD(),
				new byte[] { (byte)0xf7, (byte)0xf6, (byte)0xc5 });
		eq(BCDInteger.parseBCD("5").toEbcdicZoneBCD(),
				new byte[] { (byte)0xc5 });
		eq(BCDInteger.parseBCD("0").toEbcdicZoneBCD(),
				new byte[] { (byte)0xc0 });
		eq(BCDInteger.parseBCD("-72").toEbcdicZoneBCD(),
				new byte[] { (byte)0xf7, (byte)0xd2 });
		eq(BCDInteger.parseBCD("-1").toEbcdicZoneBCD(),
				new byte[] { (byte)0xd1 });
	}

	public void testToPackedBCD() {
		eq(BCDInteger.parseBCD("765").toPackedBCD(),
				new byte[] { (byte)0x76, (byte)0x5c });
		eq(BCDInteger.parseBCD("72").toPackedBCD(),
				new byte[] { (byte)0x07, (byte)0x2c });
		eq(BCDInteger.parseBCD("5").toPackedBCD(),
				new byte[] { (byte)0x5c });
		eq(BCDInteger.parseBCD("915986").toPackedBCD(),
				new byte[] { (byte)0x09, (byte)0x15, (byte)0x98, (byte)0x6c });
		eq(BCDInteger.parseBCD("0").toPackedBCD(),
				new byte[] { (byte)0x0c });
		eq(BCDInteger.parseBCD("-765").toPackedBCD(),
				new byte[] { (byte)0x76, (byte)0x5d });
		eq(BCDInteger.parseBCD("-72").toPackedBCD(),
				new byte[] { (byte)0x07, (byte)0x2d });
	}

	public void testSucc() {
		eq(BCDInteger.valueOf(72).succ().intValue(), 73);
		eq(BCDInteger.valueOf(-72).succ().intValue(), -71);
		eq(BCDInteger.valueOf(0).succ().intValue(), 1);
		eq(BCDInteger.valueOf(-1).succ().intValue(), 0);
	}

	public void testPrev() {
		eq(BCDInteger.valueOf(72).prev().intValue(), 71);
		eq(BCDInteger.valueOf(-72).prev().intValue(), -73);
		eq(BCDInteger.valueOf(1).prev().intValue(), 0);
		eq(BCDInteger.valueOf(0).prev().intValue(), -1);
	}

	public void testEquals() {
		ok(BCDInteger.valueOf(72).equals(BCDInteger.valueOf(72)));
		ok(BCDInteger.valueOf(-72).equals(BCDInteger.valueOf(-72)));
		ng(BCDInteger.valueOf(72).equals(BCDInteger.valueOf(-72)));
		ng(BCDInteger.valueOf(72).equals(null));
	}

}
