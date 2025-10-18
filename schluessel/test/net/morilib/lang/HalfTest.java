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
 * @author MORIGUCHI, Yuichiro 2011/10/26
 */
public class HalfTest extends TC {

	public void testGetExponent() {
		eq(Half.getExponent((short)0x7bff), 15);
		eq(Half.getExponent((short)0x0001), -24);
		eq(Half.getExponent((short)0x0300), -15);
	}

	public void testGetFractionField() {
		eq(Half.getFractionField((short)0x7bff), 0x3ff);
		eq(Half.getFractionField((short)0x7fff), 0x3ff);
	}

	public void testIsNaN() {
		ok(Half.isNaN((short)0x7fff));
		ok(Half.isNaN((short)0xffff));
		ng(Half.isNaN(Half.NEGATIVE_INFINITY_BY_SHORT));
		ng(Half.isNaN((short)0x7bff));
		ng(Half.isNaN((short)0x0000));
		ng(Half.isNaN((short)0x0001));
	}

	public void testIsInfinity() {
		ok(Half.isInfinite(Half.POSITIVE_INFINITY_BY_SHORT));
		ok(Half.isInfinite(Half.NEGATIVE_INFINITY_BY_SHORT));
		ng(Half.isInfinite((short)0x7fff));
		ng(Half.isInfinite((short)0x7bff));
		ng(Half.isInfinite((short)0x0001));
	}

	public void testIsZero() {
		ok(Half.isZero((short)0x0000));
		ok(Half.isZero((short)0x8000));
		ng(Half.isZero((short)0x0001));
	}

	public void testNeg() {
		eq(Half.toDouble(Half.neg((short)0xbc00)), 1.0);
		eq(Half.toDouble(Half.neg((short)0x3c00)), -1.0);
	}

	public void testInclement() {
		//out(Integer.toString(Half.inclement((short)0x3bff), 16));
		eq(Half.toDouble(Half.inclement((short)0x3bff)), 1.0);
	}

	public void testToDouble() {
		eq(Half.toDouble((short)0x7bff), 65504.0);
		eq(Half.toDouble((short)0xfbff), -65504.0);
		eq(Half.toDouble((short)0x0000), 0.0);
		eq(Half.toDouble((short)0x8000), -0.0);
		eq(Half.toDouble(Half.POSITIVE_INFINITY_BY_SHORT), Double.POSITIVE_INFINITY);
		eq(Half.toDouble(Half.NEGATIVE_INFINITY_BY_SHORT), Double.NEGATIVE_INFINITY);
		eq(Half.toDouble((short)0x0001), 5.9604644775390625E-8);
		eq(Half.toDouble((short)0x0400), 6.103515625E-5);
	}

	public void testAdd() {
		eq(Half.toDouble(Half.add((short)0x3c00, (short)0xbc00)), 0.0);
		eq(Half.toDouble(Half.add((short)0x3c00, (short)0x3c00)), 2.0);
		eq(Half.toDouble(Half.add((short)0x4200, (short)0x4200)), 6.0);
		eq(Half.toDouble(Half.add((short)0x3c00, (short)0x3d00)),
				Half.toDouble((short)0x3c00) + Half.toDouble((short)0x3d00));
		eq(Half.toDouble(Half.add((short)0xbc00, (short)0xbd00)),
				Half.toDouble((short)0xbc00) + Half.toDouble((short)0xbd00));
		eq(Half.toDouble(Half.add((short)0x3c01, (short)0x3bff)), 2.0);
		eq(Half.toDouble(Half.add((short)0x3c00, (short)0x3800)), 1.5);
		eq(Half.toDouble(Half.add((short)0x3c00, (short)0x3400)), 1.25);
		eq(Half.toDouble(Half.add((short)0x3c00, (short)0xb800)), 0.5);
		eq(Half.toDouble(Half.add((short)0xbc00, (short)0x3800)), -0.5);
		eq(Half.toDouble(Half.add((short)0x0800, (short)0x0300)),
				Half.toDouble((short)0x0980));
		eq(Half.toDouble(Half.add((short)0x0980, (short)0x8300)),
				Half.toDouble((short)0x0800));
		eq(Half.toDouble(Half.add((short)0x0010, (short)0x0020)),
				Half.toDouble((short)0x0030));
		eq(Half.toDouble(Half.add((short)0x0100, (short)0x0300)),
				Half.toDouble((short)0x0400));
		eq(Half.toDouble(Half.add((short)0x8100, (short)0x8300)),
				Half.toDouble((short)0x8400));
		eq(Half.toDouble(Half.add((short)0x0300, (short)0x0200)),
				Half.toDouble((short)0x0500));
		eq(Half.toDouble(Half.add((short)0x0500, (short)0x8200)),
				Half.toDouble((short)0x0300));
		eq(Half.toDouble(Half.add((short)0x8500, (short)0x0200)),
				Half.toDouble((short)0x8300));
		eq(Half.toDouble(Half.add((short)0x0030, (short)0x8020)),
				Half.toDouble((short)0x0010));
		eq(Half.toDouble(Half.add((short)0x0500, (short)0x8200)),
				Half.toDouble((short)0x0300));
		eq(Half.add(Half.POSITIVE_INFINITY_BY_SHORT, (short)0x3c00),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add(Half.POSITIVE_INFINITY_BY_SHORT, (short)0xbc00),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0x3c00, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0xbc00, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add(Half.POSITIVE_INFINITY_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0x3c00),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.add(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0xbc00),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0x3c00, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0xbc00, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.add(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		ok(Half.isNaN(Half.add(Half.POSITIVE_INFINITY_BY_SHORT,
				Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.add(Half.NaN_BY_SHORT, (short)0x3c00)));
		ok(Half.isNaN(Half.add(Half.NaN_BY_SHORT, (short)0xbc00)));
		ok(Half.isNaN(Half.add(Half.NaN_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.add(Half.NaN_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.add((short)0x3c00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.add((short)0xbc00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.add(Half.POSITIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.add(Half.POSITIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.add(Half.NaN_BY_SHORT, Half.NaN_BY_SHORT)));
		eq(Half.add((short)0x7bff, (short)0x77ff), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0x7bff, (short)0x7800), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0x7bff, (short)0x0300), (short)0x7bff);
		eq(Half.add((short)0xfbff, (short)0xf7ff), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0xfbff, (short)0xf800), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.add((short)0xfbff, (short)0x8300), (short)0xfbff);
	}

	public void testSub() {
		eq(Half.toDouble(Half.sub((short)0x3c00, (short)0x3c00)), 0.0);
		eq(Half.toDouble(Half.sub((short)0x3c00, (short)0xbc00)), 2.0);
		eq(Half.toDouble(Half.sub((short)0x4200, (short)0x3c00)), 2.0);
		eq(Half.toDouble(Half.sub((short)0x3c00, (short)0xbd00)),
				Half.toDouble((short)0x3c00) - Half.toDouble((short)0xbd00));
		eq(Half.toDouble(Half.sub((short)0xbc00, (short)0x3d00)),
				Half.toDouble((short)0xbc00) - Half.toDouble((short)0x3d00));
		//out(Integer.toString(Half.add((short)0x3c01, (short)0x3bff), 16));
		eq(Half.toDouble(Half.sub((short)0x3c01, (short)0xbbff)), 2.0);
		eq(Half.toDouble(Half.sub((short)0x3c00, (short)0xb800)), 1.5);
		eq(Half.toDouble(Half.sub((short)0x3c00, (short)0xb400)), 1.25);
		eq(Half.toDouble(Half.sub((short)0x3c00, (short)0x3800)), 0.5);
		eq(Half.toDouble(Half.sub((short)0xbc00, (short)0xb800)), -0.5);
		eq(Half.toDouble(Half.sub((short)0x0800, (short)0x8300)),
				Half.toDouble((short)0x0980));
		eq(Half.toDouble(Half.sub((short)0x0980, (short)0x0300)),
				Half.toDouble((short)0x0800));
		eq(Half.toDouble(Half.sub((short)0x0010, (short)0x8020)),
				Half.toDouble((short)0x0030));
		eq(Half.toDouble(Half.sub((short)0x0300, (short)0x8200)),
				Half.toDouble((short)0x0500));
		eq(Half.toDouble(Half.sub((short)0x0500, (short)0x0200)),
				Half.toDouble((short)0x0300));
		eq(Half.toDouble(Half.sub((short)0x8500, (short)0x8200)),
				Half.toDouble((short)0x8300));
		eq(Half.toDouble(Half.sub((short)0x0030, (short)0x0020)),
				Half.toDouble((short)0x0010));
		eq(Half.toDouble(Half.sub((short)0x0500, (short)0x0200)),
				Half.toDouble((short)0x0300));
		eq(Half.sub(Half.POSITIVE_INFINITY_BY_SHORT, (short)0x3c00),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub(Half.POSITIVE_INFINITY_BY_SHORT, (short)0xbc00),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0x3c00, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0xbc00, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub(Half.POSITIVE_INFINITY_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0x3c00),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.sub(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0xbc00),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0x3c00, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0xbc00, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.sub(Half.NEGATIVE_INFINITY_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		ok(Half.isNaN(Half.sub(Half.POSITIVE_INFINITY_BY_SHORT,
				Half.POSITIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.sub(Half.NEGATIVE_INFINITY_BY_SHORT,
				Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.sub(Half.NaN_BY_SHORT, (short)0x3c00)));
		ok(Half.isNaN(Half.sub(Half.NaN_BY_SHORT, (short)0xbc00)));
		ok(Half.isNaN(Half.sub(Half.NaN_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.sub(Half.NaN_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.sub((short)0x3c00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.sub((short)0xbc00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.sub(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.sub(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.sub(Half.NaN_BY_SHORT, Half.NaN_BY_SHORT)));
		eq(Half.sub((short)0x7bff, (short)0xf7ff), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0x7bff, (short)0xf800), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0x7bff, (short)0x8300), (short)0x7bff);
		eq(Half.sub((short)0xfbff, (short)0x77ff), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0xfbff, (short)0x7800), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.sub((short)0xfbff, (short)0x8300), (short)0xfbff);
	}

	public void testMul() {
		eq(Half.toDouble(Half.mul((short)0x4000, (short)0x4000)), 4.0);
		eq(Half.toDouble(Half.mul((short)0x4000, (short)0x3c00)), 2.0);
		eq(Half.toDouble(Half.mul((short)0x4000, (short)0xc000)), -4.0);
		eq(Half.toDouble(Half.mul((short)0x4000, (short)0xbc00)), -2.0);
		eq(Half.toDouble(Half.mul((short)0x4200, (short)0x4200)), 9.0);
		eq(Half.toDouble(Half.mul((short)0x7800, (short)0x0200)), 1);
		eq(Half.toDouble(Half.mul((short)0x7a00, (short)0x0300)), 1.5*1.5);
		eq(Half.toDouble(Half.mul((short)0x0300, (short)0x7a00)), 1.5*1.5);
		eq(Half.mul((short)0x0200, (short)0x0200), (short)0x0000);
		eq(Half.mul((short)0x5c00, (short)0x5c00), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.mul((short)0x5c00, (short)0x5800), (short)0x7800);
		eq(Half.toDouble(Half.mul((short)0xc000, (short)0x4000)), -4.0);
		eq(Half.toDouble(Half.mul((short)0xc000, (short)0x3c00)), -2.0);
		eq(Half.toDouble(Half.mul((short)0xc000, (short)0xc000)), 4.0);
		eq(Half.toDouble(Half.mul((short)0xc000, (short)0xbc00)), 2.0);
		eq(Half.toDouble(Half.mul((short)0xc200, (short)0x4200)), -9.0);
		eq(Half.toDouble(Half.mul((short)0xf800, (short)0x0200)), -1);
		eq(Half.toDouble(Half.mul((short)0xfa00, (short)0x0300)), -1.5*1.5);
		eq(Half.toDouble(Half.mul((short)0x8300, (short)0x7a00)), -1.5*1.5);
		eq(Half.mul((short)0x0200, (short)0x8200), (short)0x8000);
		eq(Half.mul((short)0xdc00, (short)0x5c00), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul((short)0xdc00, (short)0x5800), (short)0xf800);
		eq(Half.mul(Half.POSITIVE_INFINITY_BY_SHORT, (short)0x3c00),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.POSITIVE_INFINITY_BY_SHORT, (short)0xbc00),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul((short)0x3c00, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.mul((short)0xbc00, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.POSITIVE_INFINITY_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.POSITIVE_INFINITY_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0x3c00),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0xbc00),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.mul((short)0x3c00, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul((short)0xbc00, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.NEGATIVE_INFINITY_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT),
				Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.mul(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT),
				Half.POSITIVE_INFINITY_BY_SHORT);
		ok(Half.isNaN(Half.mul(Half.NaN_BY_SHORT, (short)0x3c00)));
		ok(Half.isNaN(Half.mul(Half.NaN_BY_SHORT, (short)0xbc00)));
		ok(Half.isNaN(Half.mul(Half.NaN_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.mul(Half.NaN_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.mul((short)0x3c00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.mul((short)0xbc00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.mul(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.mul(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.mul(Half.NaN_BY_SHORT, Half.NaN_BY_SHORT)));
	}

	public void testDiv() {
		eq(Half.toDouble(Half.div((short)0x4000, (short)0x4000)), 1.0);
		eq(Half.toDouble(Half.div((short)0x4400, (short)0x4000)), 2.0);
		eq(Half.toDouble(Half.div((short)0x4600, (short)0x4000)), 3.0);
		eq(Half.toDouble(Half.div((short)0xc600, (short)0x4000)), -3.0);
		eq(Half.toDouble(Half.div((short)0x0400, (short)0x0200)), 2.0);
		eq(Half.toDouble(Half.div((short)0x0600, (short)0x0100)), 6.0);
		eq(Half.toDouble(Half.div((short)0x0200, (short)0x0400)), 0.5);
		eq(Half.toDouble(Half.div((short)0x0300, (short)0x0400)), 0.75);
		eq(Half.toDouble(Half.div((short)0x0100, (short)0x0200)), 0.5);
		eq(Half.toDouble(Half.div((short)0x8100, (short)0x0200)), -0.5);
		eq(Half.toDouble(Half.div((short)0x0200, (short)0x0200)), 1.0);
		eq(Half.toDouble(Half.div((short)0x0200, (short)0x0100)), 2.0);
		eq(Half.toDouble(Half.div((short)0x0200, (short)0x0040)), 8.0);
		eq(Half.toDouble(Half.div((short)0x0300, (short)0x0040)), 12.0);
		eq(Half.toDouble(Half.div((short)0x0300, (short)0x0200)), 1.5);
		eq(Half.toDouble(Half.div((short)0x00c0, (short)0x0200)), 1.5/4);
		eq(Half.div((short)0x1800, (short)0x7800), (short)0x0001);  // -9  / 15
		eq(Half.div((short)0x1400, (short)0x7800), (short)0x0000);  // -10 / 15
		eq(Half.div((short)0x1800, (short)0x5400), (short)0x0200);  // -9  / 6
		eq(Half.div((short)0x1c00, (short)0x5400), (short)0x0400);  // -8  / 6
		eq(Half.div((short)0x9800, (short)0x7800), (short)0x8001);  // -9  / 15
		eq(Half.div((short)0x9400, (short)0x7800), (short)0x8000);  // -10 / 15
		eq(Half.div((short)0x9800, (short)0x5400), (short)0x8200);  // -9  / 6
		eq(Half.div((short)0x9c00, (short)0x5400), (short)0x8400);  // -8  / 6
		eq(Half.div((short)0x3c00, (short)0x0200), (short)0x7800);
		eq(Half.div((short)0x4000, (short)0x0200), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0x4000, (short)0x0400), (short)0x7800);
		eq(Half.div((short)0x4400, (short)0x0400), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0xbc00, (short)0x0200), (short)0xf800);
		eq(Half.div((short)0xc000, (short)0x0200), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0xc000, (short)0x0400), (short)0xf800);
		eq(Half.div((short)0xc400, (short)0x0400), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0x3c00, (short)0x0000), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0x3c00, (short)0x8000), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0xbc00, (short)0x0000), Half.NEGATIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0xbc00, (short)0x8000), Half.POSITIVE_INFINITY_BY_SHORT);
		eq(Half.div((short)0x3c00, Half.POSITIVE_INFINITY_BY_SHORT), (short)0x0000);
		eq(Half.div((short)0x3c00, Half.NEGATIVE_INFINITY_BY_SHORT), (short)0x8000);
		eq(Half.div((short)0xbc00, Half.POSITIVE_INFINITY_BY_SHORT), (short)0x8000);
		eq(Half.div((short)0xbc00, Half.NEGATIVE_INFINITY_BY_SHORT), (short)0x0000);
		ok(Half.isNaN(Half.div(Half.POSITIVE_INFINITY_BY_SHORT,
				Half.POSITIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.div(Half.POSITIVE_INFINITY_BY_SHORT,
				Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.div((short)0x3f00, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.div(Half.POSITIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.div(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT)));
		ok(Half.isNaN(Half.div(Half.NaN_BY_SHORT, (short)0x3c00)));
		ok(Half.isNaN(Half.div(Half.NaN_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.div(Half.NaN_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT)));
		ok(Half.isNaN(Half.div(Half.NaN_BY_SHORT, Half.NaN_BY_SHORT)));
	}

	public void testCompare() {
		eq(Half.compare(Half.NaN_BY_SHORT, Half.NaN_BY_SHORT), 0);
		eq(Half.compare(Half.NaN_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT), 1);
		eq(Half.compare(Half.POSITIVE_INFINITY_BY_SHORT, Half.NaN_BY_SHORT), -1);
		eq(Half.compare(Half.POSITIVE_INFINITY_BY_SHORT, Half.POSITIVE_INFINITY_BY_SHORT), 0);
		eq(Half.compare(Half.POSITIVE_INFINITY_BY_SHORT, (short)0x7bff), 1);
		eq(Half.compare((short)0x7bff, Half.POSITIVE_INFINITY_BY_SHORT), -1);
		eq(Half.compare((short)0x7bff, (short)0x7bff), 0);
		eq(Half.compare((short)0x7bff, (short)0x7bfe), 1);
		eq(Half.compare((short)0x7bfe, (short)0x7bff), -1);
		eq(Half.compare((short)0x7bff, (short)0x77ff), 1);
		eq(Half.compare((short)0x77ff, (short)0x7bff), -1);
		eq(Half.compare((short)0x0400, (short)0x03ff), 1);
		eq(Half.compare((short)0x03ff, (short)0x0400), -1);
		eq(Half.compare((short)0x0080, (short)0x0001), 1);
		eq(Half.compare((short)0x0001, (short)0x0080), -1);
		eq(Half.compare((short)0x0001, (short)0x0001), 0);
		eq(Half.compare((short)0x0001, (short)0x0000), 1);
		eq(Half.compare((short)0x0000, (short)0x0001), -1);
		eq(Half.compare((short)0x0001, (short)0x8000), 1);
		eq(Half.compare((short)0x8000, (short)0x0001), -1);
		eq(Half.compare((short)0x0000, (short)0x0000), 0);
		eq(Half.compare((short)0x0000, (short)0x8000), 0);
		eq(Half.compare((short)0x8000, (short)0x0000), 0);
		eq(Half.compare((short)0x8000, (short)0x8000), 0);
		eq(Half.compare((short)0x8001, (short)0x0000), -1);
		eq(Half.compare((short)0x0000, (short)0x8001), 1);
		eq(Half.compare((short)0x8001, (short)0x0001), -1);
		eq(Half.compare((short)0x0001, (short)0x8001), 1);
		eq(Half.compare((short)0x8001, (short)0x8002), -1);
		eq(Half.compare((short)0x8002, (short)0x8001), 1);
		eq(Half.compare((short)0x8200, (short)0x8001), 1);
		eq(Half.compare((short)0x8001, (short)0x8200), -1);
		eq(Half.compare((short)0x8400, (short)0x83ff), 1);
		eq(Half.compare((short)0x83ff, (short)0x8400), -1);
		eq(Half.compare((short)0xfbff, (short)0xf7ff), 1);
		eq(Half.compare((short)0xf7ff, (short)0xfbff), -1);
		eq(Half.compare((short)0xfbff, (short)0xfbfe), 1);
		eq(Half.compare((short)0xfbfe, (short)0xfbff), -1);
		eq(Half.compare((short)0xfbff, (short)0xfbff), 0);
		eq(Half.compare((short)0x7bff, Half.NEGATIVE_INFINITY_BY_SHORT), 1);
		eq(Half.compare(Half.NEGATIVE_INFINITY_BY_SHORT, (short)0x7bff), -1);
		eq(Half.compare(Half.NEGATIVE_INFINITY_BY_SHORT, Half.NEGATIVE_INFINITY_BY_SHORT), 0);
	}

	public void testHalf() {
		eq(new Half(2.0).toShortValue(), (short)0x4000);
		eq(new Half(3.0).toShortValue(), (short)0x4200);
		eq(new Half(32768).toShortValue(), (short)0x7800);
		eq(new Half(65504).toShortValue(), (short)0x7bff);
		eq(new Half(65505).toShortValue(), (short)0x7c00);
		eq(new Half(6.103515625E-5).toShortValue(), (short)0x0400);
		eq(new Half(5.9604644775390625E-8).toShortValue(), (short)0x0001);
		eq(new Half(5.960464477539062E-8).toShortValue(), (short)0x0000);
		eq(new Half(-2.0).toShortValue(), (short)0xc000);
		eq(new Half(-3.0).toShortValue(), (short)0xc200);
		eq(new Half(-32768).toShortValue(), (short)0xf800);
		eq(new Half(-65504).toShortValue(), (short)0xfbff);
		eq(new Half(-65505).toShortValue(), (short)0xfc00);
		eq(new Half(-6.103515625E-5).toShortValue(), (short)0x8400);
		eq(new Half(-5.9604644775390625E-8).toShortValue(), (short)0x8001);
		eq(new Half(-5.960464477539062E-8).toShortValue(), (short)0x8000);
		ok(new Half(Double.NaN).isNaN());
	}

	public void testUlp() {
		eq(Half.ulp((short)0x0000), Half.MIN_VALUE_BY_SHORT);
		eq(Half.ulp((short)0x0001), (short)0x0001);
		eq(Half.ulp((short)0x0010), (short)0x0001);
		eq(Half.ulp((short)0x03ff), (short)0x0001);
		eq(Half.ulp((short)0x0400), (short)0x0001);
		eq(Half.ulp((short)0x07ff), (short)0x0001);
		eq(Half.ulp((short)0x0800), (short)0x0002);
		eq(Half.ulp((short)0x2800), (short)0x0200);
		eq(Half.ulp((short)0x2c00), (short)0x0400);
		eq(Half.ulp((short)0x7bff), (short)0x5000);
		eq(Half.ulp((short)0x7c00), (short)0x7c00);
		eq(Half.ulp((short)0x8000), Half.MIN_VALUE_BY_SHORT);
		eq(Half.ulp((short)0x8001), (short)0x0001);
		eq(Half.ulp((short)0x8010), (short)0x0001);
		eq(Half.ulp((short)0x83ff), (short)0x0001);
		eq(Half.ulp((short)0x8400), (short)0x0001);
		eq(Half.ulp((short)0x87ff), (short)0x0001);
		eq(Half.ulp((short)0x8800), (short)0x0002);
		eq(Half.ulp((short)0xa800), (short)0x0200);
		eq(Half.ulp((short)0xac00), (short)0x0400);
		eq(Half.ulp((short)0xfbff), (short)0x5000);
		eq(Half.ulp((short)0xfc00), (short)0x7c00);
		eq(Half.ulp((short)0x7fff), (short)0x7fff);
	}

	public void testToHalf() {
		eq(DoubleUtils.toHalf(3.0), (short)0x4200);
	}

}
