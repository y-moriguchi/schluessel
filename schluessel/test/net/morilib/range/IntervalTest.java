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

import java.util.HashMap;
import java.util.SortedSet;

import net.morilib.lisp.test.TC;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2008/01/01
 */
public class IntervalTest extends TC {
	
	private static Integer O = Integer.valueOf(0);
	private static Integer ONE = Integer.valueOf(1);
	private static Integer valueOf(int i) {
		return Integer.valueOf(i);
	}
	
	//
	private Object newInt(int x) {
		return valueOf(x);
	}
	
	//
	static final Interval IV0030 = IntervalsInt.newClosedInterval( 0, 30);
	static final Interval IV0205 = IntervalsInt.newClosedInterval( 2,  5);
	static final Interval IV0510 = IntervalsInt.newClosedInterval( 5, 10);
	static final Interval IV0515 = IntervalsInt.newClosedInterval( 5, 15);
	static final Interval IV0525 = IntervalsInt.newClosedInterval( 5, 25);
	static final Interval IV1015 = IntervalsInt.newClosedInterval(10, 15);
	static final Interval IV1020 = IntervalsInt.newClosedInterval(10, 20);
	static final Interval IV1025 = IntervalsInt.newClosedInterval(10, 25);
	static final Interval IV1218 = IntervalsInt.newClosedInterval(12, 18);
	static final Interval IV1520 = IntervalsInt.newClosedInterval(15, 20);
	static final Interval IV1525 = IntervalsInt.newClosedInterval(15, 25);
	static final Interval IV2025 = IntervalsInt.newClosedInterval(20, 25);
	static final Interval IV2225 = IntervalsInt.newClosedInterval(22, 25);
	static final Interval IV2030 = IntervalsInt.newClosedInterval(20, 30);
	
	//
	static final Interval IV0030o = IntervalsInt.newOpenInterval( 0, 30);
	static final Interval IV0205o = IntervalsInt.newOpenInterval( 2,  5);
	static final Interval IV0510o = IntervalsInt.newOpenInterval( 5, 10);
	static final Interval IV0515o = IntervalsInt.newOpenInterval( 5, 15);
	static final Interval IV0525o = IntervalsInt.newOpenInterval( 5, 25);
	static final Interval IV1015o = IntervalsInt.newOpenInterval(10, 15);
	static final Interval IV1020o = IntervalsInt.newOpenInterval(10, 20);
	static final Interval IV1025o = IntervalsInt.newOpenInterval(10, 25);
	static final Interval IV1218o = IntervalsInt.newOpenInterval(12, 18);
	static final Interval IV1520o = IntervalsInt.newOpenInterval(15, 20);
	static final Interval IV1525o = IntervalsInt.newOpenInterval(15, 25);
	static final Interval IV2025o = IntervalsInt.newOpenInterval(20, 25);
	static final Interval IV2225o = IntervalsInt.newOpenInterval(22, 25);
	static final Interval IV2030o = IntervalsInt.newOpenInterval(20, 30);
	
	//
	static final Interval IVoo15 =
		IntervalsInt.newClosedInfimumlessInterval(15);
	static final Interval IV05oo =
		IntervalsInt.newClosedSupremumlessInterval(5);
	
	//
	static final Interval IVoo15o =
		IntervalsInt.newOpenInfimumlessInterval(15);
	static final Interval IV05ooo =
		IntervalsInt.newOpenSupremumlessInterval(5);
	
	//
	private void isInfimumClosed(Interval nw) {
		ok(nw.isInfimumClosed());
		ng(nw.isInfimumOpen());
		ok(nw.isInfimumFinite());
	}
	
	//
	private void isInfimumInfinite(Interval nw) {
		ng(nw.isInfimumFinite());
		ok(nw.isInfimumClosed());
		ok(nw.isInfimumOpen());
	}
	
	//
	private void isSupremumClosed(Interval nw) {
		ok(nw.isSupremumClosed());
		ng(nw.isSupremumOpen());
		ok(nw.isSupremumFinite());
	}
	
	//
	private void isSupremumInfinite(Interval nw) {
		ng(nw.isSupremumFinite());
		ok(nw.isSupremumClosed());
		ok(nw.isSupremumOpen());
	}
	
	//
	private void assertBound(Interval nw,
			Object o1, boolean b1, Object o2, boolean b2) {
		eq(nw.getInfimumBound(),  o1);
		eq(nw.getSupremumBound(), o2);
		eq(nw.isInfimumOpen(),  b1);
		eq(nw.isSupremumOpen(), b2);
	}
	
	//
	private void assertBound(Interval nw,
			int o1, boolean b1, int o2, boolean b2) {
		eq(nw.getInfimumBound(),  newInt(o1));
		eq(nw.getSupremumBound(), newInt(o2));
		eq(nw.isInfimumOpen(),  b1);
		eq(nw.isSupremumOpen(), b2);
	}
	
	//
	private void assertBound(Interval nw, int o1, int o2) {
		eq(nw.getInfimumBound(),  newInt(o1));
		eq(nw.getSupremumBound(), newInt(o2));
		ok(nw.isInfimumClosed());
		ok(nw.isSupremumClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testNewInstanceObOb() {
		Interval nw;
		
		// create [0, 1]
		nw = Interval.newInstance(O, false, ONE, false);
		assertBound(nw, O, false, ONE, false);
		
		// create [0, 0] - error
		nw = Interval.newInstance(
				O, false, O, false);
		assertBound(nw, O, false, O, false);
		
		// null pointers
		try {
			nw = Interval.newInstance(null, false, O, false);
			fail();
		} catch(NullPointerException e) {}
		
		// null pointers
		try {
			nw = Interval.newInstance(O, false, null, false);
			fail();
		} catch(NullPointerException e) {}
	}
	
	public void testNewPointO() {
		Interval i;
		
		// [0, 0]
		i = Interval.newPoint(O);
		assertBound(i, O, false, O, false);
	}
	
	/**
	 * 
	 *
	 */
	/*public void testIn_IntervalInterval() {
		Interval i0, i1, i2, i3, i4, i5;
		i0 = Intervals.newClosedInterval(10, 20);
		
		ok(Interval.in(i0, IV0525));
		ng(Interval.in(i0, IV0515));
		ng(Interval.in(i0, IV0205));
		ng(Interval.in(i0, IV2225));
		ok(Interval.in(i0, IV1020));
	}*/
	
	/**
	 * 
	 *
	 */
	public void testIndependentOf_IntervalInterval() {
		Interval i0;
		i0 = IntervalsInt.newClosedInterval(10, 20);
		
		ng(Interval.independentOf(i0, IV0525));
		ng(Interval.independentOf(i0, IV0515));
		ng(Interval.independentOf(i0, IV0510));
		ng(Interval.independentOf(i0, IV2025));
		ok(Interval.independentOf(i0, IV0205));
		ok(Interval.independentOf(i0, IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testInteriorIndependentOf_IntervalInterval() {
		Interval i0;
		i0 = IntervalsInt.newClosedInterval(10, 20);
		
		ng(Interval.interiorIndependentOf(i0, IV0525));
		ng(Interval.interiorIndependentOf(i0, IV0515));
		ok(Interval.interiorIndependentOf(i0, IV0510));
		ok(Interval.interiorIndependentOf(i0, IV2025));
		ok(Interval.interiorIndependentOf(i0, IV0205));
		ok(Interval.interiorIndependentOf(i0, IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testCompareInfimum() {
		// O
		ok(Interval.compareInfimum(Range.O, Range.O) == 0);
		ok(Interval.compareInfimum(Range.O, IV1020)  < 0);
		ok(Interval.compareInfimum(IV1020, Range.O) > 0);
		
		// (-oo, 15]
		ok(Interval.compareInfimum(IVoo15, IV1020) < 0);
		ok(Interval.compareInfimum(IV1020, IVoo15) > 0);
		ok(Interval.compareInfimum(IVoo15, 
				IntervalsInt.newClosedInfimumlessInterval(10)) == 0);
		
		// [10, 20]
		ok(Interval.compareInfimum(IV1020, IV0205) > 0);
		ok(Interval.compareInfimum(IV1020, IV1015) == 0);
		ok(Interval.compareInfimum(IV1020, IV1020o) < 0);
		ok(Interval.compareInfimum(IV1020, IV1525) < 0);
		
		// (10, 20)
		ok(Interval.compareInfimum(IV1020o, IV0205) > 0);
		ok(Interval.compareInfimum(IV1020o, IV1015) > 0);
		ok(Interval.compareInfimum(IV1020o, IV1020o) == 0);
		ok(Interval.compareInfimum(IV1020o, IV1525) < 0);
	}
	
	/**
	 * 
	 *
	 */
	public void testCompareSupremum() {
		// O
		ok(Interval.compareSupremum(Range.O, Range.O) == 0);
		ok(Interval.compareSupremum(Range.O, IV1020)  < 0);
		ok(Interval.compareSupremum(IV1020, Range.O) > 0);
		
		// [5, +oo)
		ok(Interval.compareSupremum(IV05oo, IV1020) > 0);
		ok(Interval.compareSupremum(IV1020, IV05oo) < 0);
		ok(Interval.compareSupremum(IV05oo, 
				IntervalsInt.newClosedSupremumlessInterval(10)) == 0);
		
		// [10, 20]
		ok(Interval.compareSupremum(IV1020, IV0205) > 0);
		ok(Interval.compareSupremum(IV1020, IV1520) == 0);
		ok(Interval.compareSupremum(IV1020, IV1520o) > 0);
		ok(Interval.compareSupremum(IV1020, IV1525) < 0);
		
		// (10, 20)
		ok(Interval.compareSupremum(IV1020o, IV0205) > 0);
		ok(Interval.compareSupremum(IV1020o, IV1520) < 0);
		ok(Interval.compareSupremum(IV1020o, IV1520o) == 0);
		ok(Interval.compareSupremum(IV1020o, IV1525) < 0);
	}
	
	/**
	 * 
	 *
	 */
	public void testCompare() {
		Interval iv10o20i = IntervalsInt.newLeftOpenInterval(10, 20);
		Interval iv1020oi = IntervalsInt.newRightOpenInterval(10, 20);
		
		ok(Interval.compare(IV1020, IV0515 ) > 0);
		ok(Interval.compare(IV1020, iv10o20i) < 0);
		ok(Interval.compare(IV1020, IV1520 ) < 0);
		ok(Interval.compare(IV1020, IV1015 ) > 0);
		ok(Interval.compare(IV1020, iv1020oi) > 0);
		ok(Interval.compare(IV1020, IV1025 ) < 0);
		ok(Interval.compare(IV1020, IV1020) == 0);
	}
	
	/**
	 * 
	 *
	 */
	public void testCloneInterval() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(0, 1);
		
		// create [0, 1]
		nw = i0.cloneInterval();
		assertBound(nw, O, false, ONE, false);
	}
	
	/**
	 * 
	 *
	 */
	public void testCloneInterval_IntervalInterval() {
		assertBound(Interval.newInstance(IV0205, IV0510), 2, 10);
	}
	
	/**
	 * 
	 *
	 */
	public void testCloneInterval_InfimumMarkerInterval() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(0, 10);
		
		// clone [0, 10]
		nw = i0.cloneInterval(Interval.FROM_INFIMUM, IV0510);
		assertBound(nw, 0, 10);
		
		// clone [10, 25]
		nw = i0.cloneInterval(Interval.ABOVE_SUPREMUM, IV0525);
		assertBound(nw, 10, true, 25, false);
	}
	
	/**
	 * 
	 *
	 */
	public void testCloneInterval_IntervalSupremumMarker() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(7, 10);
		
		// clone [0, 10]
		nw = i0.cloneInterval(IV0525, Interval.TO_SUPREMUM);
		assertBound(nw, 5, 10);
		
		// clone [10, 25]
		nw = i0.cloneInterval(IV0525, Interval.BELOW_INFIMUM);
		assertBound(nw, 5, false, 7, true);
	}
	
	/**
	 * 
	 *
	 */
	public void testClosureInterval() {
		Interval i0;
		i0 = IntervalsInt.newOpenInterval(0, 1);
		
		// (0, 1) -> [0, 1]
		Interval nw = (Interval)i0.closure();
		assertBound(nw, 0, false, 1, false);
	}
	
	/**
	 * 
	 *
	 */
	public void testInteriorInterval() {
		Interval i0;
		i0 = IntervalsInt.newClosedInterval(0, 1);
		
		// (0, 1) <- [0, 1]
		Interval nw = (Interval)i0.interior();
		assertBound(nw, 0, true, 1, true);
	}
	
	/**
	 * 
	 *
	 */
	public void testCoveredInterval() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(5, 25);
		
		// covers [5, 25], [10, 20] -> [5, 25]
		nw = i0.coveredInterval(IV1020);
		ok(nw == i0);
		
		// covers [5, 25], [0, 30] -> [0, 30]
		nw = i0.coveredInterval(IV0030);
		ok(nw == IV0030);
		
		// covers [5, 25], [2, 5] -> [2, 25]
		nw = i0.coveredInterval(IV0205);
		assertBound(nw, 2, 25);
		
		// covers [5, 25], [20, 30] -> [5, 30]
		nw = i0.coveredInterval(IV2030);
		assertBound(nw, 5, 30);
	}
	
	/**
	 * 
	 *
	 */
	public void testHideUpperHalfIntervalOf() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(10, 20);
		
		// below infimum
		nw = i0.hideUpperHalfIntervalOf(IV2225);
		ok(nw == i0);
		
		// above infimum all
		nw = i0.hideUpperHalfIntervalOf(IV1020);
		ok(nw == Range.O);
		
		// contact at i0's infimum
		nw = i0.hideUpperHalfIntervalOf(IV2030);
		assertBound(nw, 10, false, 20, true);
		
		// overlap
		nw = i0.hideUpperHalfIntervalOf(IV1520);
		assertBound(nw, 10, false, 15, true);
	}
	
	/**
	 * 
	 *
	 */
	public void testHideLowerHalfIntervalOf() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(10, 20);
		
		// below infimum
		nw = i0.hideLowerHalfIntervalOf(IV2225);
		ok(nw == Range.O);
		
		// above infimum all
		nw = i0.hideLowerHalfIntervalOf(IV0205);
		ok(nw == i0);
		
		// contact at i0's supremum
		nw = i0.hideLowerHalfIntervalOf(IV0510);
		assertBound(nw, 10, true, 20, false);
		
		// overlap
		nw = i0.hideLowerHalfIntervalOf(IV0515);
		assertBound(nw, 15, true, 20, false);
	}
	
	/**
	 * 
	 *
	 */
	public void testLowerHalfInterval() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(0, 1);
		
		// clone [0, +oo)
		nw = i0.lowerHalfInterval();
		eq(nw.getInfimumBound(),  Limit.MINIMUM);
		eq(nw.getSupremumBound(), ONE);
		isInfimumInfinite(nw);  isSupremumClosed(nw);
	}
	
	/**
	 * 
	 *
	 */
	public void testUpperHalfInterval() {
		Interval i0, nw;
		i0 = IntervalsInt.newClosedInterval(0, 1);
		
		// clone [0, +oo)
		nw = i0.upperHalfInterval();
		eq(nw.getInfimumBound(),  O);
		eq(nw.getSupremumBound(), Limit.MAXIMUM);
		isInfimumClosed(nw);  isSupremumInfinite(nw);
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumFinite() {
		// test [0, 1], (-oo, 0], [0, +oo), O
		ok(IntervalsInt.newClosedInterval(0, 1).isInfimumFinite());
		ng(IntervalsInt.newClosedInfimumlessInterval(0).isInfimumFinite());
		ok(IntervalsInt.newClosedSupremumlessInterval(0).isInfimumFinite());
		ng(Range.O.isInfimumFinite());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumFinite() {
		// test [0, 1], (0, 1), (-oo, 0], O
		ok(IntervalsInt.newClosedInterval(0, 1).isSupremumFinite());
		ok(IntervalsInt.newClosedInfimumlessInterval(0).isSupremumFinite());
		ng(IntervalsInt.newClosedSupremumlessInterval(0).isSupremumFinite());
		ng(Range.O.isSupremumFinite());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumOpen() {
		// test [0, 1], (0, 1), (-oo, 0]
		ng(IntervalsInt.newClosedInterval(0, 1).isInfimumOpen());
		ok(IntervalsInt.newOpenInterval  (0, 1).isInfimumOpen());
		ok(IntervalsInt.newClosedInfimumlessInterval(0).isInfimumOpen());
		ok(Range.O.isInfimumOpen());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumOpen() {
		// test [0, 1], (0, 1), [0, +oo)
		ng(IntervalsInt.newClosedInterval(0, 1).isSupremumOpen());
		ok(IntervalsInt.newOpenInterval  (0, 1).isSupremumOpen());
		ok(IntervalsInt.newClosedSupremumlessInterval(0).isSupremumOpen());
		ok(Range.O.isSupremumOpen());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumClosed() {
		// test [0, 1], (0, 1), (-oo, 0]
		ok(IntervalsInt.newClosedInterval(0, 1).isInfimumClosed());
		ng(IntervalsInt.newOpenInterval  (0, 1).isInfimumClosed());
		ok(IntervalsInt.newClosedInfimumlessInterval(0).isInfimumClosed());
		ok(Range.O.isInfimumClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumClosed() {
		// test [0, 1], (0, 1), [0, +oo)
		ok(IntervalsInt.newClosedInterval(0, 1).isSupremumClosed());
		ng(IntervalsInt.newOpenInterval  (0, 1).isSupremumClosed());
		ok(IntervalsInt.newClosedSupremumlessInterval(0).isSupremumClosed());
		ok(Range.O.isSupremumClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsOpen() {
		// test [0, 1], (0, 1], [0, 1)
		//    (-oo, 0], [0, +oo), (-oo, +oo), O
		ng(IntervalsInt.newClosedInterval   (0, 1).isOpen());
		ng(IntervalsInt.newLeftOpenInterval (0, 1).isOpen());
		ng(IntervalsInt.newRightOpenInterval(0, 1).isOpen());
		ok(IntervalsInt.newOpenInterval     (0, 1).isOpen());
		ok(Intervals.newInfimumlessInterval(O, true).isOpen());
		ok(Range.O.isOpen());
		ok(Range.U.isOpen());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsClosed() {
		// test [0, 1], (0, 1], [0, 1)
		//    (-oo, 0], [0, +oo), (-oo, +oo), O
		ok(IntervalsInt.newClosedInterval   (0, 1).isClosed());
		ng(IntervalsInt.newLeftOpenInterval (0, 1).isClosed());
		ng(IntervalsInt.newRightOpenInterval(0, 1).isClosed());
		ng(IntervalsInt.newOpenInterval     (0, 1).isClosed());
		ok(Intervals.newInfimumlessInterval(O, false).isClosed());
		ok(Range.O.isClosed());
		ok(Range.U.isClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumBound() {
		// test [5, 15]
		ng(IV0515.isInfimumBoundAbove(newInt(30)));
		ng(IV0515.isInfimumBoundAbove(newInt(15)));
		ng(IV0515.isInfimumBoundAbove(newInt(10)));
		ng(IV0515.isInfimumBoundAbove(newInt( 5)));
		ok(IV0515.isInfimumBoundAbove(newInt( 2)));
		
		// test (5, 15)
		ng(IV0515o.isInfimumBoundAbove(newInt(30)));
		ng(IV0515o.isInfimumBoundAbove(newInt(15)));
		ng(IV0515o.isInfimumBoundAbove(newInt(10)));
		ng(IV0515o.isInfimumBoundAbove(newInt( 5)));
		ok(IV0515o.isInfimumBoundAbove(newInt( 2)));
		
		// test (-oo, 15]
		ng(IVoo15.isInfimumBoundAbove(newInt(30)));
		ng(IVoo15.isInfimumBoundAbove(newInt(15)));
		ng(IVoo15.isInfimumBoundAbove(newInt(10)));
		ng(IVoo15.isInfimumBoundAbove(newInt( 5)));
		ng(IVoo15.isInfimumBoundAbove(newInt( 2)));
		
		// test [5, +oo)
		ng(IV05oo.isInfimumBoundAbove(newInt(30)));
		ng(IV05oo.isInfimumBoundAbove(newInt(15)));
		ng(IV05oo.isInfimumBoundAbove(newInt(10)));
		ng(IV05oo.isInfimumBoundAbove(newInt( 5)));
		ok(IV05oo.isInfimumBoundAbove(newInt( 2)));
		
		// test O
		ok(Range.O.isInfimumBoundAbove(newInt(30)));
		ok(Range.O.isInfimumBoundAbove(newInt(15)));
		ok(Range.O.isInfimumBoundAbove(newInt(10)));
		ok(Range.O.isInfimumBoundAbove(newInt( 5)));
		ok(Range.O.isInfimumBoundAbove(newInt( 2)));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimumBound() {
		// test [5, 15]
		ok(IV0515.isInfimumBoundBelow(newInt(30)));
		ok(IV0515.isInfimumBoundBelow(newInt(15)));
		ok(IV0515.isInfimumBoundBelow(newInt(10)));
		ng(IV0515.isInfimumBoundBelow(newInt( 5)));
		ng(IV0515.isInfimumBoundBelow(newInt( 2)));
		
		// test (5, 15)
		ok(IV0515o.isInfimumBoundBelow(newInt(30)));
		ok(IV0515o.isInfimumBoundBelow(newInt(15)));
		ok(IV0515o.isInfimumBoundBelow(newInt(10)));
		ng(IV0515o.isInfimumBoundBelow(newInt( 5)));
		ng(IV0515o.isInfimumBoundBelow(newInt( 2)));
		
		// test (-oo, 15]
		ok(IVoo15.isInfimumBoundBelow(newInt(30)));
		ok(IVoo15.isInfimumBoundBelow(newInt(15)));
		ok(IVoo15.isInfimumBoundBelow(newInt(10)));
		ok(IVoo15.isInfimumBoundBelow(newInt( 5)));
		ok(IVoo15.isInfimumBoundBelow(newInt( 2)));
		
		// test [5, +oo)
		ok(IV05oo.isInfimumBoundBelow(newInt(30)));
		ok(IV05oo.isInfimumBoundBelow(newInt(15)));
		ok(IV05oo.isInfimumBoundBelow(newInt(10)));
		ng(IV05oo.isInfimumBoundBelow(newInt( 5)));
		ng(IV05oo.isInfimumBoundBelow(newInt( 2)));
		
		// test O
		ng(Range.O.isInfimumBoundBelow(newInt(30)));
		ng(Range.O.isInfimumBoundBelow(newInt(15)));
		ng(Range.O.isInfimumBoundBelow(newInt(10)));
		ng(Range.O.isInfimumBoundBelow(newInt( 5)));
		ng(Range.O.isInfimumBoundBelow(newInt( 2)));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsInfimumBound() {
		// test [5, 15]
		ng(IV0515.isInfimumBoundEqualTo(newInt(30)));
		ng(IV0515.isInfimumBoundEqualTo(newInt(15)));
		ng(IV0515.isInfimumBoundEqualTo(newInt(10)));
		ok(IV0515.isInfimumBoundEqualTo(newInt( 5)));
		ng(IV0515.isInfimumBoundEqualTo(newInt( 2)));
		
		// test (5, 15)
		ng(IV0515o.isInfimumBoundEqualTo(newInt(30)));
		ng(IV0515o.isInfimumBoundEqualTo(newInt(15)));
		ng(IV0515o.isInfimumBoundEqualTo(newInt(10)));
		ok(IV0515o.isInfimumBoundEqualTo(newInt( 5)));
		ng(IV0515o.isInfimumBoundEqualTo(newInt( 2)));
		
		// test (-oo, 15]
		ng(IVoo15.isInfimumBoundEqualTo(newInt(30)));
		ng(IVoo15.isInfimumBoundEqualTo(newInt(15)));
		ng(IVoo15.isInfimumBoundEqualTo(newInt(10)));
		ng(IVoo15.isInfimumBoundEqualTo(newInt( 5)));
		ng(IVoo15.isInfimumBoundEqualTo(newInt( 2)));
		
		// test [5, +oo)
		ng(IV05oo.isInfimumBoundEqualTo(newInt(30)));
		ng(IV05oo.isInfimumBoundEqualTo(newInt(15)));
		ng(IV05oo.isInfimumBoundEqualTo(newInt(10)));
		ok(IV05oo.isInfimumBoundEqualTo(newInt( 5)));
		ng(IV05oo.isInfimumBoundEqualTo(newInt( 2)));
		
		// test O
		ng(Range.O.isInfimumBoundEqualTo(newInt(30)));
		ng(Range.O.isInfimumBoundEqualTo(newInt(15)));
		ng(Range.O.isInfimumBoundEqualTo(newInt(10)));
		ng(Range.O.isInfimumBoundEqualTo(newInt( 5)));
		ng(Range.O.isInfimumBoundEqualTo(newInt( 2)));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremumBound() {
		// test [5, 15]
		ng(IV0515.isSupremumBoundAbove(newInt(30)));
		ng(IV0515.isSupremumBoundAbove(newInt(15)));
		ok(IV0515.isSupremumBoundAbove(newInt(10)));
		ok(IV0515.isSupremumBoundAbove(newInt( 5)));
		ok(IV0515.isSupremumBoundAbove(newInt( 2)));
		
		// test (5, 15)
		ng(IV0515o.isSupremumBoundAbove(newInt(30)));
		ng(IV0515o.isSupremumBoundAbove(newInt(15)));
		ok(IV0515o.isSupremumBoundAbove(newInt(10)));
		ok(IV0515o.isSupremumBoundAbove(newInt( 5)));
		ok(IV0515o.isSupremumBoundAbove(newInt( 2)));
		
		// test (-oo, 15]
		ng(IVoo15.isSupremumBoundAbove(newInt(30)));
		ng(IVoo15.isSupremumBoundAbove(newInt(15)));
		ok(IVoo15.isSupremumBoundAbove(newInt(10)));
		ok(IVoo15.isSupremumBoundAbove(newInt( 5)));
		ok(IVoo15.isSupremumBoundAbove(newInt( 2)));
		
		// test [5, +oo)
		ok(IV05oo.isSupremumBoundAbove(newInt(30)));
		ok(IV05oo.isSupremumBoundAbove(newInt(15)));
		ok(IV05oo.isSupremumBoundAbove(newInt(10)));
		ok(IV05oo.isSupremumBoundAbove(newInt( 5)));
		ok(IV05oo.isSupremumBoundAbove(newInt( 2)));
		
		// test O
		ng(Range.O.isSupremumBoundAbove(newInt(30)));
		ng(Range.O.isSupremumBoundAbove(newInt(15)));
		ng(Range.O.isSupremumBoundAbove(newInt(10)));
		ng(Range.O.isSupremumBoundAbove(newInt( 5)));
		ng(Range.O.isSupremumBoundAbove(newInt( 2)));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremumBound() {
		// test [5, 15]
		ok(IV0515.isSupremumBoundBelow(newInt(30)));
		ng(IV0515.isSupremumBoundBelow(newInt(15)));
		ng(IV0515.isSupremumBoundBelow(newInt(10)));
		ng(IV0515.isSupremumBoundBelow(newInt( 5)));
		ng(IV0515.isSupremumBoundBelow(newInt( 2)));
		
		// test (5, 15)
		ok(IV0515o.isSupremumBoundBelow(newInt(30)));
		ng(IV0515o.isSupremumBoundBelow(newInt(15)));
		ng(IV0515o.isSupremumBoundBelow(newInt(10)));
		ng(IV0515o.isSupremumBoundBelow(newInt( 5)));
		ng(IV0515o.isSupremumBoundBelow(newInt( 2)));
		
		// test (-oo, 15]
		ok(IVoo15.isSupremumBoundBelow(newInt(30)));
		ng(IVoo15.isSupremumBoundBelow(newInt(15)));
		ng(IVoo15.isSupremumBoundBelow(newInt(10)));
		ng(IVoo15.isSupremumBoundBelow(newInt( 5)));
		ng(IVoo15.isSupremumBoundBelow(newInt( 2)));
		
		// test [5, +oo)
		ng(IV05oo.isSupremumBoundBelow(newInt(30)));
		ng(IV05oo.isSupremumBoundBelow(newInt(15)));
		ng(IV05oo.isSupremumBoundBelow(newInt(10)));
		ng(IV05oo.isSupremumBoundBelow(newInt( 5)));
		ng(IV05oo.isSupremumBoundBelow(newInt( 2)));
		
		// test O
		ok(Range.O.isSupremumBoundBelow(newInt(30)));
		ok(Range.O.isSupremumBoundBelow(newInt(15)));
		ok(Range.O.isSupremumBoundBelow(newInt(10)));
		ok(Range.O.isSupremumBoundBelow(newInt( 5)));
		ok(Range.O.isSupremumBoundBelow(newInt( 2)));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsSupremumBound() {
		// test [5, 15]
		ng(IV0515.isSupremumBoundEqualTo(newInt(30)));
		ok(IV0515.isSupremumBoundEqualTo(newInt(15)));
		ng(IV0515.isSupremumBoundEqualTo(newInt(10)));
		ng(IV0515.isSupremumBoundEqualTo(newInt( 5)));
		ng(IV0515.isSupremumBoundEqualTo(newInt( 2)));
		
		// test (5, 15)
		ng(IV0515o.isSupremumBoundEqualTo(newInt(30)));
		ok(IV0515o.isSupremumBoundEqualTo(newInt(15)));
		ng(IV0515o.isSupremumBoundEqualTo(newInt(10)));
		ng(IV0515o.isSupremumBoundEqualTo(newInt( 5)));
		ng(IV0515o.isSupremumBoundEqualTo(newInt( 2)));
		
		// test (-oo, 15]
		ng(IVoo15.isSupremumBoundEqualTo(newInt(30)));
		ok(IVoo15.isSupremumBoundEqualTo(newInt(15)));
		ng(IVoo15.isSupremumBoundEqualTo(newInt(10)));
		ng(IVoo15.isSupremumBoundEqualTo(newInt( 5)));
		ng(IVoo15.isSupremumBoundEqualTo(newInt( 2)));
		
		// test [5, +oo)
		ng(IV05oo.isSupremumBoundEqualTo(newInt(30)));
		ng(IV05oo.isSupremumBoundEqualTo(newInt(15)));
		ng(IV05oo.isSupremumBoundEqualTo(newInt(10)));
		ng(IV05oo.isSupremumBoundEqualTo(newInt( 5)));
		ng(IV05oo.isSupremumBoundEqualTo(newInt( 2)));
		
		// test O
		ng(Range.O.isSupremumBoundEqualTo(newInt(30)));
		ng(Range.O.isSupremumBoundEqualTo(newInt(15)));
		ng(Range.O.isSupremumBoundEqualTo(newInt(10)));
		ng(Range.O.isSupremumBoundEqualTo(newInt( 5)));
		ng(Range.O.isSupremumBoundEqualTo(newInt( 2)));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimum() {
		// test [5, 15]
		ng(IV0515.isInfimumAbove(30));
		ng(IV0515.isInfimumAbove(15));
		ng(IV0515.isInfimumAbove(10));
		ng(IV0515.isInfimumAbove( 5));
		ok(IV0515.isInfimumAbove( 2));
		
		// test (5, 15)
		ng(IV0515o.isInfimumAbove(30));
		ng(IV0515o.isInfimumAbove(15));
		ng(IV0515o.isInfimumAbove(10));
		ok(IV0515o.isInfimumAbove( 5));
		ok(IV0515o.isInfimumAbove( 2));
		
		// test (-oo, 15]
		ng(IVoo15.isInfimumAbove(30));
		ng(IVoo15.isInfimumAbove(15));
		ng(IVoo15.isInfimumAbove(10));
		ng(IVoo15.isInfimumAbove( 5));
		ng(IVoo15.isInfimumAbove( 2));
		
		// test [5, +oo)
		ng(IV05oo.isInfimumAbove(30));
		ng(IV05oo.isInfimumAbove(15));
		ng(IV05oo.isInfimumAbove(10));
		ng(IV05oo.isInfimumAbove( 5));
		ok(IV05oo.isInfimumAbove( 2));
		
		// test O
		ok(Range.O.isInfimumAbove(30));
		ok(Range.O.isInfimumAbove(15));
		ok(Range.O.isInfimumAbove(10));
		ok(Range.O.isInfimumAbove( 5));
		ok(Range.O.isInfimumAbove( 2));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimum() {
		// test [5, 15]
		ok(IV0515.isInfimumBelow(30));
		ok(IV0515.isInfimumBelow(15));
		ok(IV0515.isInfimumBelow(10));
		ng(IV0515.isInfimumBelow( 5));
		ng(IV0515.isInfimumBelow( 2));
		
		// test (5, 15)
		ok(IV0515o.isInfimumBelow(30));
		ok(IV0515o.isInfimumBelow(15));
		ok(IV0515o.isInfimumBelow(10));
		ng(IV0515o.isInfimumBelow( 5));
		ng(IV0515o.isInfimumBelow( 2));
		
		// test (-oo, 15]
		ok(IVoo15.isInfimumBelow(30));
		ok(IVoo15.isInfimumBelow(15));
		ok(IVoo15.isInfimumBelow(10));
		ok(IVoo15.isInfimumBelow( 5));
		ok(IVoo15.isInfimumBelow( 2));
		
		// test [5, +oo)
		ok(IV05oo.isInfimumBelow(30));
		ok(IV05oo.isInfimumBelow(15));
		ok(IV05oo.isInfimumBelow(10));
		ng(IV05oo.isInfimumBelow( 5));
		ng(IV05oo.isInfimumBelow( 2));
		
		// test O
		ng(Range.O.isInfimumBelow(30));
		ng(Range.O.isInfimumBelow(15));
		ng(Range.O.isInfimumBelow(10));
		ng(Range.O.isInfimumBelow( 5));
		ng(Range.O.isInfimumBelow( 2));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsInfimum() {
		// test [5, 15]
		ng(IV0515.isInfimumEqualTo(30));
		ng(IV0515.isInfimumEqualTo(15));
		ng(IV0515.isInfimumEqualTo(10));
		ok(IV0515.isInfimumEqualTo( 5));
		ng(IV0515.isInfimumEqualTo( 2));
		
		// test (5, 15)
		ng(IV0515o.isInfimumEqualTo(30));
		ng(IV0515o.isInfimumEqualTo(15));
		ng(IV0515o.isInfimumEqualTo(10));
		ng(IV0515o.isInfimumEqualTo( 5));
		ng(IV0515o.isInfimumEqualTo( 2));
		
		// test (-oo, 15]
		ng(IVoo15.isInfimumEqualTo(30));
		ng(IVoo15.isInfimumEqualTo(15));
		ng(IVoo15.isInfimumEqualTo(10));
		ng(IVoo15.isInfimumEqualTo( 5));
		ng(IVoo15.isInfimumEqualTo( 2));
		
		// test [5, +oo)
		ng(IV05oo.isInfimumEqualTo(30));
		ng(IV05oo.isInfimumEqualTo(15));
		ng(IV05oo.isInfimumEqualTo(10));
		ok(IV05oo.isInfimumEqualTo( 5));
		ng(IV05oo.isInfimumEqualTo( 2));
		
		// test O
		ng(Range.O.isInfimumEqualTo(30));
		ng(Range.O.isInfimumEqualTo(15));
		ng(Range.O.isInfimumEqualTo(10));
		ng(Range.O.isInfimumEqualTo( 5));
		ng(Range.O.isInfimumEqualTo( 2));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremum() {
		// test [5, 15]
		ng(IV0515.isSupremumAbove(30));
		ng(IV0515.isSupremumAbove(15));
		ok(IV0515.isSupremumAbove(10));
		ok(IV0515.isSupremumAbove( 5));
		ok(IV0515.isSupremumAbove( 2));
		
		// test (5, 15)
		ng(IV0515o.isSupremumAbove(30));
		ng(IV0515o.isSupremumAbove(15));
		ok(IV0515o.isSupremumAbove(10));
		ok(IV0515o.isSupremumAbove( 5));
		ok(IV0515o.isSupremumAbove( 2));
		
		// test (-oo, 15]
		ng(IVoo15.isSupremumAbove(30));
		ng(IVoo15.isSupremumAbove(15));
		ok(IVoo15.isSupremumAbove(10));
		ok(IVoo15.isSupremumAbove( 5));
		ok(IVoo15.isSupremumAbove( 2));
		
		// test [5, +oo)
		ok(IV05oo.isSupremumAbove(30));
		ok(IV05oo.isSupremumAbove(15));
		ok(IV05oo.isSupremumAbove(10));
		ok(IV05oo.isSupremumAbove( 5));
		ok(IV05oo.isSupremumAbove( 2));
		
		// test O
		ng(Range.O.isSupremumAbove(30));
		ng(Range.O.isSupremumAbove(15));
		ng(Range.O.isSupremumAbove(10));
		ng(Range.O.isSupremumAbove( 5));
		ng(Range.O.isSupremumAbove( 2));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremum() {
		// test [5, 15]
		ok(IV0515.isSupremumBelow(30));
		ng(IV0515.isSupremumBelow(15));
		ng(IV0515.isSupremumBelow(10));
		ng(IV0515.isSupremumBelow( 5));
		ng(IV0515.isSupremumBelow( 2));
		
		// test (5, 15)
		ok(IV0515o.isSupremumBelow(30));
		ok(IV0515o.isSupremumBelow(15));
		ng(IV0515o.isSupremumBelow(10));
		ng(IV0515o.isSupremumBelow( 5));
		ng(IV0515o.isSupremumBelow( 2));
		
		// test (-oo, 15]
		ok(IVoo15.isSupremumBelow(30));
		ng(IVoo15.isSupremumBelow(15));
		ng(IVoo15.isSupremumBelow(10));
		ng(IVoo15.isSupremumBelow( 5));
		ng(IVoo15.isSupremumBelow( 2));
		
		// test [5, +oo)
		ng(IV05oo.isSupremumBelow(30));
		ng(IV05oo.isSupremumBelow(15));
		ng(IV05oo.isSupremumBelow(10));
		ng(IV05oo.isSupremumBelow( 5));
		ng(IV05oo.isSupremumBelow( 2));
		
		// test O
		ok(Range.O.isSupremumBelow(30));
		ok(Range.O.isSupremumBelow(15));
		ok(Range.O.isSupremumBelow(10));
		ok(Range.O.isSupremumBelow( 5));
		ok(Range.O.isSupremumBelow( 2));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsSupremum() {
		// test [5, 15]
		ng(IV0515.isSupremumEqualTo(30));
		ok(IV0515.isSupremumEqualTo(15));
		ng(IV0515.isSupremumEqualTo(10));
		ng(IV0515.isSupremumEqualTo( 5));
		ng(IV0515.isSupremumEqualTo( 2));
		
		// test (5, 15)
		ng(IV0515o.isSupremumEqualTo(30));
		ng(IV0515o.isSupremumEqualTo(15));
		ng(IV0515o.isSupremumEqualTo(10));
		ng(IV0515o.isSupremumEqualTo( 5));
		ng(IV0515o.isSupremumEqualTo( 2));
		
		// test (-oo, 15]
		ng(IVoo15.isSupremumEqualTo(30));
		ok(IVoo15.isSupremumEqualTo(15));
		ng(IVoo15.isSupremumEqualTo(10));
		ng(IVoo15.isSupremumEqualTo( 5));
		ng(IVoo15.isSupremumEqualTo( 2));
		
		// test [5, +oo)
		ng(IV05oo.isSupremumEqualTo(30));
		ng(IV05oo.isSupremumEqualTo(15));
		ng(IV05oo.isSupremumEqualTo(10));
		ng(IV05oo.isSupremumEqualTo( 5));
		ng(IV05oo.isSupremumEqualTo( 2));
		
		// test O
		ng(Range.O.isSupremumEqualTo(30));
		ng(Range.O.isSupremumEqualTo(15));
		ng(Range.O.isSupremumEqualTo(10));
		ng(Range.O.isSupremumEqualTo( 5));
		ng(Range.O.isSupremumEqualTo( 2));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimumBoundAll() {
		// compare to [10, 20]
		ng(IV1020.isInfimumBoundBelowAllClosureOf(IV0205));
		ng(IV1020.isInfimumBoundBelowAllClosureOf(IV0510));
		ng(IV1020.isInfimumBoundBelowAllClosureOf(IV0515));
		ng(IV1020.isInfimumBoundBelowAllClosureOf(IV1015));
		ok(IV1020.isInfimumBoundBelowAllClosureOf(IV1520));
		ok(IV1020.isInfimumBoundBelowAllClosureOf(IV1525));
		ok(IV1020.isInfimumBoundBelowAllClosureOf(IV2025));
		ok(IV1020.isInfimumBoundBelowAllClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumBoundAll() {
		// compare to [10, 20]
		ok(IV1020.isInfimumBoundAboveAllClosureOf(IV0205));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV0510));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV0515));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV1015));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV1520));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV1525));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV2025));
		ng(IV1020.isInfimumBoundAboveAllClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimumBoundAny() {
		// compare to [10, 20]
		ng(IV1020.isInfimumBoundBelowAnyClosureOf(IV0205));
		ng(IV1020.isInfimumBoundBelowAnyClosureOf(IV0510));
		ok(IV1020.isInfimumBoundBelowAnyClosureOf(IV0515));
		ok(IV1020.isInfimumBoundBelowAnyClosureOf(IV1015));
		ok(IV1020.isInfimumBoundBelowAnyClosureOf(IV1520));
		ok(IV1020.isInfimumBoundBelowAnyClosureOf(IV1525));
		ok(IV1020.isInfimumBoundBelowAnyClosureOf(IV2025));
		ok(IV1020.isInfimumBoundBelowAnyClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumBoundAny() {
		// compare to [10, 20]
		ok(IV1020.isInfimumBoundAboveAnyClosureOf(IV0205));
		ok(IV1020.isInfimumBoundAboveAnyClosureOf(IV0510));
		ok(IV1020.isInfimumBoundAboveAnyClosureOf(IV0515));
		ng(IV1020.isInfimumBoundAboveAnyClosureOf(IV1015));
		ng(IV1020.isInfimumBoundAboveAnyClosureOf(IV1520));
		ng(IV1020.isInfimumBoundAboveAnyClosureOf(IV1525));
		ng(IV1020.isInfimumBoundAboveAnyClosureOf(IV2025));
		ng(IV1020.isInfimumBoundAboveAnyClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testContactInfimumBound() {
		// compare to [10, 20]
		ng(IV1020.contactInfimumBound(IV0205));
		ok(IV1020.contactInfimumBound(IV0510));
		ng(IV1020.contactInfimumBound(IV0515));
		ng(IV1020.contactInfimumBound(IV1015));
		ng(IV1020.contactInfimumBound(IV1520));
		ng(IV1020.contactInfimumBound(IV1525));
		ng(IV1020.contactInfimumBound(IV2025));
		ng(IV1020.contactInfimumBound(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testCommonInfimumBoundTo() {
		// compare to [10, 20]
		ng(IV1020.commonInfimumBoundTo(IV0205));
		ng(IV1020.commonInfimumBoundTo(IV0510));
		ng(IV1020.commonInfimumBoundTo(IV0515));
		ok(IV1020.commonInfimumBoundTo(IV1015));
		ng(IV1020.commonInfimumBoundTo(IV1520));
		ng(IV1020.commonInfimumBoundTo(IV1525));
		ng(IV1020.commonInfimumBoundTo(IV2025));
		ng(IV1020.commonInfimumBoundTo(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremumBoundAll() {
		// compare to [10, 20]
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV0205));
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV0510));
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV0515));
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV1015));
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV1520));
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV1525));
		ng(IV1020.isSupremumBoundBelowAllClosureOf(IV2025));
		ok(IV1020.isSupremumBoundBelowAllClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremumBoundAll() {
		// compare to [10, 20]
		ok(IV1020.isSupremumBoundAboveAllClosureOf(IV0205));
		ok(IV1020.isSupremumBoundAboveAllClosureOf(IV0510));
		ok(IV1020.isSupremumBoundAboveAllClosureOf(IV0515));
		ok(IV1020.isSupremumBoundAboveAllClosureOf(IV1015));
		ng(IV1020.isSupremumBoundAboveAllClosureOf(IV1520));
		ng(IV1020.isSupremumBoundAboveAllClosureOf(IV1525));
		ng(IV1020.isSupremumBoundAboveAllClosureOf(IV2025));
		ng(IV1020.isSupremumBoundAboveAllClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremumBoundAny() {
		// compare to [10, 20]
		ng(IV1020.isSupremumBoundBelowAnyClosureOf(IV0205));
		ng(IV1020.isSupremumBoundBelowAnyClosureOf(IV0510));
		ng(IV1020.isSupremumBoundBelowAnyClosureOf(IV0515));
		ng(IV1020.isSupremumBoundBelowAnyClosureOf(IV1015));
		ng(IV1020.isSupremumBoundBelowAnyClosureOf(IV1520));
		ok(IV1020.isSupremumBoundBelowAnyClosureOf(IV1525));
		ok(IV1020.isSupremumBoundBelowAnyClosureOf(IV2025));
		ok(IV1020.isSupremumBoundBelowAnyClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremumBoundAny() {
		// compare to [10, 20]
		ok(IV1020.isSupremumBoundAboveAnyClosureOf(IV0205));
		ok(IV1020.isSupremumBoundAboveAnyClosureOf(IV0510));
		ok(IV1020.isSupremumBoundAboveAnyClosureOf(IV0515));
		ok(IV1020.isSupremumBoundAboveAnyClosureOf(IV1015));
		ok(IV1020.isSupremumBoundAboveAnyClosureOf(IV1520));
		ok(IV1020.isSupremumBoundAboveAnyClosureOf(IV1525));
		ng(IV1020.isSupremumBoundAboveAnyClosureOf(IV2025));
		ng(IV1020.isSupremumBoundAboveAnyClosureOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testContactSupremumBound() {
		// compare to [10, 20]
		ng(IV1020.contactSupremumBound(IV0205));
		ng(IV1020.contactSupremumBound(IV0510));
		ng(IV1020.contactSupremumBound(IV0515));
		ng(IV1020.contactSupremumBound(IV1015));
		ng(IV1020.contactSupremumBound(IV1520));
		ng(IV1020.contactSupremumBound(IV1525));
		ok(IV1020.contactSupremumBound(IV2025));
		ng(IV1020.contactSupremumBound(IV2225));
		
		ok(IV1020o.contactSupremumBound(IV2025));
	}
	
	/**
	 * 
	 *
	 */
	public void testCommonSupremumBoundTo() {
		// compare to [10, 20]
		ng(IV1020.commonSupremumBoundTo(IV0205));
		ng(IV1020.commonSupremumBoundTo(IV0510));
		ng(IV1020.commonSupremumBoundTo(IV0515));
		ng(IV1020.commonSupremumBoundTo(IV1015));
		ok(IV1020.commonSupremumBoundTo(IV1520));
		ng(IV1020.commonSupremumBoundTo(IV1525));
		ng(IV1020.commonSupremumBoundTo(IV2025));
		ng(IV1020.commonSupremumBoundTo(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testContains() {
		// test [5, 15]
		ng(IV0515.contains( 0));  ok(IV0515.contains( 5));
		ok(IV0515.contains(10));  ok(IV0515.contains(15));
		ng(IV0515.contains(20));
		
		// test (5, 15)
		ng(IV0515o.contains( 0));  ng(IV0515o.contains( 5));
		ok(IV0515o.contains(10));  ng(IV0515o.contains(15));
		ng(IV0515o.contains(20));
		
		// test (-oo, 15]
		ok(IVoo15.contains( 0));  ok(IVoo15.contains( 5));
		ok(IVoo15.contains(10));  ok(IVoo15.contains(15));
		ng(IVoo15.contains(20));
		
		// test [5, +oo)
		ng(IV05oo.contains( 0));  ok(IV05oo.contains( 5));
		ok(IV05oo.contains(10));  ok(IV05oo.contains(15));
		ok(IV05oo.contains(20));
		
		// test (-oo, 15)
		ok(IVoo15o.contains( 0));  ok(IVoo15o.contains( 5));
		ok(IVoo15o.contains(10));  ng(IVoo15o.contains(15));
		ng(IVoo15o.contains(20));
		
		// test (5, +oo)
		ng(IV05ooo.contains( 0));  ng(IV05ooo.contains( 5));
		ok(IV05ooo.contains(10));  ok(IV05ooo.contains(15));
		ok(IV05ooo.contains(20));
		
		// test O
		ng(Range.O.contains( 0));  ng(Range.O.contains( 5));
		ng(Range.O.contains(10));  ng(Range.O.contains(15));
		ng(Range.O.contains(20));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsNeighborhoodOf() {
		// test [5, 15]
		ng(IV0515.isNeighborhoodOf( 0));
		ng(IV0515.isNeighborhoodOf( 5));
		ok(IV0515.isNeighborhoodOf(10));
		ng(IV0515.isNeighborhoodOf(15));
		ng(IV0515.isNeighborhoodOf(20));
		
		// test (5, 15)
		ng(IV0515o.isNeighborhoodOf( 0));
		ng(IV0515o.isNeighborhoodOf( 5));
		ok(IV0515o.isNeighborhoodOf(10));
		ng(IV0515o.isNeighborhoodOf(15));
		ng(IV0515o.isNeighborhoodOf(20));
		
		// test (-oo, 15]
		ok(IVoo15.isNeighborhoodOf( 0));
		ok(IVoo15.isNeighborhoodOf( 5));
		ok(IVoo15.isNeighborhoodOf(10));
		ng(IVoo15.isNeighborhoodOf(15));
		ng(IVoo15.isNeighborhoodOf(20));
		
		// test [5, +oo)
		ng(IV05oo.isNeighborhoodOf( 0));
		ng(IV05oo.isNeighborhoodOf( 5));
		ok(IV05oo.isNeighborhoodOf(10));
		ok(IV05oo.isNeighborhoodOf(15));
		ok(IV05oo.isNeighborhoodOf(20));
		
		// test (-oo, 15)
		ok(IVoo15o.isNeighborhoodOf( 0));
		ok(IVoo15o.isNeighborhoodOf( 5));
		ok(IVoo15o.isNeighborhoodOf(10));
		ng(IVoo15o.isNeighborhoodOf(15));
		ng(IVoo15o.isNeighborhoodOf(20));
		
		// test (5, +oo)
		ng(IV05ooo.isNeighborhoodOf( 0));
		ng(IV05ooo.isNeighborhoodOf( 5));
		ok(IV05ooo.isNeighborhoodOf(10));
		ok(IV05ooo.isNeighborhoodOf(15));
		ok(IV05ooo.isNeighborhoodOf(20));
		
		// test O
		ng(Range.O.isNeighborhoodOf( 0));
		ng(Range.O.isNeighborhoodOf( 5));
		ng(Range.O.isNeighborhoodOf(10));
		ng(Range.O.isNeighborhoodOf(15));
		ng(Range.O.isNeighborhoodOf(20));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsClosure() {
		// test [5, 15]
		ng(IV0515.containsClosure( 0));
		ok(IV0515.containsClosure( 5));
		ok(IV0515.containsClosure(10));
		ok(IV0515.containsClosure(15));
		ng(IV0515.containsClosure(20));
		
		// test (5, 15)
		ng(IV0515o.containsClosure( 0));
		ok(IV0515o.containsClosure( 5));
		ok(IV0515o.containsClosure(10));
		ok(IV0515o.containsClosure(15));
		ng(IV0515o.containsClosure(20));
		
		// test (-oo, 15]
		ok(IVoo15.containsClosure( 0));
		ok(IVoo15.containsClosure( 5));
		ok(IVoo15.containsClosure(10));
		ok(IVoo15.containsClosure(15));
		ng(IVoo15.containsClosure(20));
		
		// test [5, +oo)
		ng(IV05oo.containsClosure( 0));
		ok(IV05oo.containsClosure( 5));
		ok(IV05oo.containsClosure(10));
		ok(IV05oo.containsClosure(15));
		ok(IV05oo.containsClosure(20));
		
		// test (-oo, 15)
		ok(IVoo15o.containsClosure( 0));
		ok(IVoo15o.containsClosure( 5));
		ok(IVoo15o.containsClosure(10));
		ok(IVoo15o.containsClosure(15));
		ng(IVoo15o.containsClosure(20));
		
		// test (5, +oo)
		ng(IV05ooo.containsClosure( 0));
		ok(IV05ooo.containsClosure( 5));
		ok(IV05ooo.containsClosure(10));
		ok(IV05ooo.containsClosure(15));
		ok(IV05ooo.containsClosure(20));
		
		// test O
		ng(Range.O.containsClosure( 0));
		ng(Range.O.containsClosure( 5));
		ng(Range.O.containsClosure(10));
		ng(Range.O.containsClosure(15));
		ng(Range.O.containsClosure(20));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsBound() {
		// test [5, 15]
		ng(IV0515.containsBound( 0));  ok(IV0515.containsBound( 5));
		ng(IV0515.containsBound(10));  ok(IV0515.containsBound(15));
		ng(IV0515.containsBound(20));
		
		// test (5, 15)
		ng(IV0515o.containsBound( 0));  ok(IV0515o.containsBound( 5));
		ng(IV0515o.containsBound(10));  ok(IV0515o.containsBound(15));
		ng(IV0515o.containsBound(20));
		
		// test (-oo, 15]
		ng(IVoo15.containsBound( 0));  ng(IVoo15.containsBound( 5));
		ng(IVoo15.containsBound(10));  ok(IVoo15.containsBound(15));
		ng(IVoo15.containsBound(20));
		
		// test [5, +oo)
		ng(IV05oo.containsBound( 0));  ok(IV05oo.containsBound( 5));
		ng(IV05oo.containsBound(10));  ng(IV05oo.containsBound(15));
		ng(IV05oo.containsBound(20));
		
		// test (-oo, 15)
		ng(IVoo15o.containsBound( 0));  ng(IVoo15o.containsBound( 5));
		ng(IVoo15o.containsBound(10));  ok(IVoo15o.containsBound(15));
		ng(IVoo15o.containsBound(20));
		
		// test (5, +oo)
		ng(IV05ooo.containsBound( 0));  ok(IV05ooo.containsBound( 5));
		ng(IV05ooo.containsBound(10));  ng(IV05ooo.containsBound(15));
		ng(IV05ooo.containsBound(20));
		
		// test O
		ng(Range.O.containsBound( 0));  ng(Range.O.containsBound( 5));
		ng(Range.O.containsBound(10));  ng(Range.O.containsBound(15));
		ng(Range.O.containsBound(20));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsClosureOf() {
		// compare to [10, 20]
		ng(IV1020.closureContains(IV0205));
		ng(IV1020.closureContains(IV0510));
		ng(IV1020.closureContains(IV0510o));
		ng(IV1020.closureContains(IV0515));
		ok(IV1020.closureContains(IV1015));
		ok(IV1020.closureContains(IV1015o));
		ok(IV1020.closureContains(IV1218));
		ok(IV1020.closureContains(IV1520o));
		ok(IV1020.closureContains(IV1520));
		ng(IV1020.closureContains(IV1525));
		ng(IV1020.closureContains(IV2025o));
		ng(IV1020.closureContains(IV2025));
		ng(IV1020.closureContains(IV2225));
		
		// compare to (10, 20)
		ng(IV1020o.closureContains(IV0205));
		ng(IV1020o.closureContains(IV0510));
		ng(IV1020o.closureContains(IV0510o));
		ng(IV1020o.closureContains(IV0515));
		ok(IV1020o.closureContains(IV1015));
		ok(IV1020o.closureContains(IV1015o));
		ok(IV1020o.closureContains(IV1218));
		ok(IV1020o.closureContains(IV1520o));
		ok(IV1020o.closureContains(IV1520));
		ng(IV1020o.closureContains(IV1525));
		ng(IV1020o.closureContains(IV2025o));
		ng(IV1020o.closureContains(IV2025));
		ng(IV1020o.closureContains(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsInteriorOf() {
		// compare to [10, 20]
		ng(IV1020.interiorContains(IV0205));
		ng(IV1020.interiorContains(IV0510));
		ng(IV1020.interiorContains(IV0510o));
		ng(IV1020.interiorContains(IV0515));
		ng(IV1020.interiorContains(IV1015));
		ok(IV1020.interiorContains(IV1015o));
		ok(IV1020.interiorContains(IV1218));
		ok(IV1020.interiorContains(IV1520o));
		ng(IV1020.interiorContains(IV1520));
		ng(IV1020.interiorContains(IV1525));
		ng(IV1020.interiorContains(IV2025o));
		ng(IV1020.interiorContains(IV2025));
		ng(IV1020.interiorContains(IV2225));
		
		// compare to (10, 20)
		ng(IV1020o.interiorContains(IV0205));
		ng(IV1020o.interiorContains(IV0510));
		ng(IV1020o.interiorContains(IV0510o));
		ng(IV1020o.interiorContains(IV0515));
		ng(IV1020o.interiorContains(IV1015));
		ok(IV1020o.interiorContains(IV1015o));
		ok(IV1020o.interiorContains(IV1218));
		ok(IV1020o.interiorContains(IV1520o));
		ng(IV1020o.interiorContains(IV1520));
		ng(IV1020o.interiorContains(IV1525));
		ng(IV1020o.interiorContains(IV2025o));
		ng(IV1020o.interiorContains(IV2025));
		ng(IV1020o.interiorContains(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsAll() {
		// compare to [10, 20]
		ng(IV1020.containsAll(IV0205));
		ng(IV1020.containsAll(IV0510));
		ng(IV1020.containsAll(IV0510o));
		ng(IV1020.containsAll(IV0515));
		ok(IV1020.containsAll(IV1015));
		ok(IV1020.containsAll(IV1015o));
		ok(IV1020.containsAll(IV1218));
		ok(IV1020.containsAll(IV1520o));
		ok(IV1020.containsAll(IV1520));
		ng(IV1020.containsAll(IV1525));
		ng(IV1020.containsAll(IV2025o));
		ng(IV1020.containsAll(IV2025));
		ng(IV1020.containsAll(IV2225));
		
		// compare to (10, 20)
		ng(IV1020o.containsAll(IV0205));
		ng(IV1020o.containsAll(IV0510));
		ng(IV1020o.containsAll(IV0510o));
		ng(IV1020o.containsAll(IV0515));
		ng(IV1020o.containsAll(IV1015));
		ok(IV1020o.containsAll(IV1015o));
		ok(IV1020o.containsAll(IV1218));
		ok(IV1020o.containsAll(IV1520o));
		ng(IV1020o.containsAll(IV1520));
		ng(IV1020o.containsAll(IV1525));
		ng(IV1020o.containsAll(IV2025o));
		ng(IV1020o.containsAll(IV2025));
		ng(IV1020o.containsAll(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsAny() {
		// compare to [10, 20]
		ng(IV1020.containsAny(IV0205));
		ok(IV1020.containsAny(IV0510));
		ng(IV1020.containsAny(IV0510o));
		ok(IV1020.containsAny(IV0515));
		ok(IV1020.containsAny(IV1015));
		ok(IV1020.containsAny(IV1015o));
		ok(IV1020.containsAny(IV1218));
		ok(IV1020.containsAny(IV1520o));
		ok(IV1020.containsAny(IV1520));
		ok(IV1020.containsAny(IV1525));
		ng(IV1020.containsAny(IV2025o));
		ok(IV1020.containsAny(IV2025));
		ng(IV1020.containsAny(IV2225));
		
		// compare to (10, 20)
		ng(IV1020o.containsAny(IV0205));
		ng(IV1020o.containsAny(IV0510));
		ng(IV1020o.containsAny(IV0510o));
		ok(IV1020o.containsAny(IV0515));
		ok(IV1020o.containsAny(IV1015));
		ok(IV1020o.containsAny(IV1015o));
		ok(IV1020o.containsAny(IV1218));
		ok(IV1020o.containsAny(IV1520o));
		ok(IV1020o.containsAny(IV1520));
		ok(IV1020o.containsAny(IV1525));
		ng(IV1020o.containsAny(IV2025o));
		ng(IV1020o.containsAny(IV2025));
		ng(IV1020o.containsAny(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIntervals() {
		SortedSet<Interval> r = IV1020.intervals();
		
		eq(r.size(), 1);
		ok(r.contains(IV1020));
	}
	
	/**
	 * 
	 *
	 */
	public void testBound() {
		SortedSet<Interval> r = IV1020.bound().intervals();
		
		eq(r.size(), 2);
		ok(r.contains(IntervalsInt.newPoint(10)));
		ok(r.contains(IntervalsInt.newPoint(20)));
	}
	
	/**
	 * 
	 *
	 */
	public void testBoundElements() {
		SortedSet<?> r = IV1020.boundElements();
		
		eq(r.size(), 2);
		ok(r.contains(newInt(10)));
		ok(r.contains(newInt(20)));
	}
	
	/**
	 * 
	 *
	 */
	public void testInfimumBoundIn() {
		// closed
		ng(IV1020.infimumBoundIn(IV0205));
		ng(IV1020.infimumBoundIn(IV0510o));
		ok(IV1020.infimumBoundIn(IV0510));
		ok(IV1020.infimumBoundIn(IV0515));
		ok(IV1020.infimumBoundIn(IV1020));
		ng(IV1020.infimumBoundIn(IV1020o));
		ng(IV1020.infimumBoundIn(IV1525));
		ng(IV1020.infimumBoundIn(IV2030));
		ng(IV1020.infimumBoundIn(IV2030o));
		ng(IV1020.infimumBoundIn(IV2225));
		
		// open
		ng(IV1020o.infimumBoundIn(IV0205));
		ng(IV1020o.infimumBoundIn(IV0510o));
		ok(IV1020o.infimumBoundIn(IV0510));
		ok(IV1020o.infimumBoundIn(IV0515));
		ok(IV1020o.infimumBoundIn(IV1020));
		ng(IV1020o.infimumBoundIn(IV1020o));
		ng(IV1020o.infimumBoundIn(IV1525));
		ng(IV1020o.infimumBoundIn(IV2030));
		ng(IV1020o.infimumBoundIn(IV2030o));
		ng(IV1020o.infimumBoundIn(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testSupremumBoundIn() {
		// closed
		ng(IV1020.supremumBoundIn(IV0205));
		ng(IV1020.supremumBoundIn(IV0510o));
		ng(IV1020.supremumBoundIn(IV0510));
		ng(IV1020.supremumBoundIn(IV0515));
		ng(IV1020.supremumBoundIn(IV1020o));
		ok(IV1020.supremumBoundIn(IV1020));
		ok(IV1020.supremumBoundIn(IV1525));
		ok(IV1020.supremumBoundIn(IV2030));
		ng(IV1020.supremumBoundIn(IV2030o));
		ng(IV1020.supremumBoundIn(IV2225));
		
		// open
		ng(IV1020o.supremumBoundIn(IV0205));
		ng(IV1020o.supremumBoundIn(IV0510o));
		ng(IV1020o.supremumBoundIn(IV0510));
		ng(IV1020o.supremumBoundIn(IV0515));
		ng(IV1020o.supremumBoundIn(IV1020o));
		ok(IV1020o.supremumBoundIn(IV1020));
		ok(IV1020o.supremumBoundIn(IV1525));
		ok(IV1020o.supremumBoundIn(IV2030));
		ng(IV1020o.supremumBoundIn(IV2030o));
		ng(IV1020o.supremumBoundIn(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testBoundsIn() {
		// closed
		ng(IV1020.boundsIn(IV0205));  ng(IV1020.boundsIn(IV0510o));
		ng(IV1020.boundsIn(IV0510));  ng(IV1020.boundsIn(IV0515));
		ng(IV1020.boundsIn(IV1020o)); ok(IV1020.boundsIn(IV1020));
		ng(IV1020.boundsIn(IV1525));  ng(IV1020.boundsIn(IV2030));
		ng(IV1020.boundsIn(IV2030o)); ng(IV1020.boundsIn(IV2225));
		ok(IV1020.boundsIn(IV0030));
		
		// open
		ng(IV1020o.boundsIn(IV0205));  ng(IV1020o.boundsIn(IV0510o));
		ng(IV1020o.boundsIn(IV0510));  ng(IV1020o.boundsIn(IV0515));
		ng(IV1020o.boundsIn(IV1020o)); ok(IV1020o.boundsIn(IV1020));
		ng(IV1020o.boundsIn(IV1525));  ng(IV1020o.boundsIn(IV2030));
		ng(IV1020o.boundsIn(IV2030o)); ng(IV1020o.boundsIn(IV2225));
		ok(IV1020o.boundsIn(IV0030));
	}
	
	/**
	 * 
	 *
	 */
	public void testBoundsIndependentOf() {
		// closed
		ok(IV1020.boundsIndependentOf(IV0205));
		ok(IV1020.boundsIndependentOf(IV0510o));
		ng(IV1020.boundsIndependentOf(IV0510));
		ng(IV1020.boundsIndependentOf(IV0515));
		ok(IV1020.boundsIndependentOf(IV1020o));
		ng(IV1020.boundsIndependentOf(IV1020));
		ng(IV1020.boundsIndependentOf(IV1525));
		ng(IV1020.boundsIndependentOf(IV2030));
		ok(IV1020.boundsIndependentOf(IV2030o));
		ok(IV1020.boundsIndependentOf(IV2225));
		
		// open
		ok(IV1020o.boundsIndependentOf(IV0205));
		ok(IV1020o.boundsIndependentOf(IV0510o));
		ng(IV1020o.boundsIndependentOf(IV0510));
		ng(IV1020o.boundsIndependentOf(IV0515));
		ok(IV1020o.boundsIndependentOf(IV1020o));
		ng(IV1020o.boundsIndependentOf(IV1020));
		ng(IV1020o.boundsIndependentOf(IV1525));
		ng(IV1020o.boundsIndependentOf(IV2030));
		ok(IV1020o.boundsIndependentOf(IV2030o));
		ok(IV1020o.boundsIndependentOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIn() {
		// closed
		ng(IV1020.in(IV0205));   ng(IV1020.in(IV0510o));
		ng(IV1020.in(IV0510));   ng(IV1020.in(IV0515));
		ng(IV1020.in(IV1020o));
		ok(IV1020.in(IV1020));
		ng(IV1020.in(IV1525));   ng(IV1020.in(IV2030));
		ng(IV1020.in(IV2030o));  ng(IV1020.in(IV2225));
		ok(IV1020.in(IV0030));
		
		// open
		ng(IV1020o.in(IV0205));   ng(IV1020o.in(IV0510o));
		ng(IV1020o.in(IV0510));   ng(IV1020o.in(IV0515));
		ok(IV1020o.in(IV1020o));
		ok(IV1020o.in(IV1020));
		ng(IV1020o.in(IV1525));   ng(IV1020o.in(IV2030));
		ng(IV1020o.in(IV2030o));  ng(IV1020o.in(IV2225));
		ok(IV1020o.in(IV0030));
	}
	
	/**
	 * 
	 *
	 */
	public void testIndependentOf() {
		// closed
		ok(IV1020.independentOf(IV0205));
		ok(IV1020.independentOf(IV0510o));
		ng(IV1020.independentOf(IV0510));
		ng(IV1020.independentOf(IV0515));
		ng(IV1020.independentOf(IV1020o));
		ng(IV1020.independentOf(IV1020));
		ng(IV1020.independentOf(IV1525));
		ng(IV1020.independentOf(IV2030));
		ok(IV1020.independentOf(IV2030o));
		ok(IV1020.independentOf(IV2225));
		ng(IV1020.independentOf(IV0030));
		
		// open
		ok(IV1020o.independentOf(IV0205));
		ok(IV1020o.independentOf(IV0510o));
		ok(IV1020o.independentOf(IV0510));
		ng(IV1020o.independentOf(IV0515));
		ng(IV1020o.independentOf(IV1020o));
		ng(IV1020o.independentOf(IV1020));
		ng(IV1020o.independentOf(IV1525));
		ok(IV1020o.independentOf(IV2030));
		ok(IV1020o.independentOf(IV2030o));
		ok(IV1020o.independentOf(IV2225));
		ng(IV1020o.independentOf(IV0030));
	}
	
	/**
	 * 
	 *
	 */
	public void testInteriorIndependentOf() {
		// closed
		ok(IV1020.interiorIndependentOf(IV0205));
		ok(IV1020.interiorIndependentOf(IV0510o));
		ok(IV1020.interiorIndependentOf(IV0510));
		ng(IV1020.interiorIndependentOf(IV0515));
		ng(IV1020.interiorIndependentOf(IV1020o));
		ng(IV1020.interiorIndependentOf(IV1020));
		ng(IV1020.interiorIndependentOf(IV1525));
		ok(IV1020.interiorIndependentOf(IV2030));
		ok(IV1020.interiorIndependentOf(IV2030o));
		ok(IV1020.interiorIndependentOf(IV2225));
		ng(IV1020.interiorIndependentOf(IV0030));
		
		// open
		ok(IV1020o.interiorIndependentOf(IV0205));
		ok(IV1020o.interiorIndependentOf(IV0510o));
		ok(IV1020o.interiorIndependentOf(IV0510));
		ng(IV1020o.interiorIndependentOf(IV0515));
		ng(IV1020o.interiorIndependentOf(IV1020o));
		ng(IV1020o.interiorIndependentOf(IV1020));
		ng(IV1020o.interiorIndependentOf(IV1525));
		ok(IV1020o.interiorIndependentOf(IV2030));
		ok(IV1020o.interiorIndependentOf(IV2030o));
		ok(IV1020o.interiorIndependentOf(IV2225));
		ng(IV1020o.interiorIndependentOf(IV0030));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmpty() {
		ng(IV1020.isEmpty());  ng(IVoo15.isEmpty());
		ng(IV05oo.isEmpty());  ng(Range.U.isEmpty());
		ok(Range.O.isEmpty());
	}
	
	/**
	 * 
	 *
	 */
	public void testMeetInterval() {
		eq(IV1020.meetInterval(IV0205),  Range.O);
		eq(IV1020.meetInterval(IV0510o), Range.O);
		eq(IV1020.meetInterval(IV0510),  IntervalsInt.newPoint(10));
		eq(IV1020.meetInterval(IV0515),  IV1015);
		eq(IV1020.meetInterval(IV1020),  IV1020);
		eq(IV1020.meetInterval(IV1020o), IV1020o);
		eq(IV1020.meetInterval(IV1525),  IV1520);
		eq(IV1020.meetInterval(IV2025),  IntervalsInt.newPoint(20));
		eq(IV1020.meetInterval(IV2025o), Range.O);
		eq(IV1020.meetInterval(IV2225),  Range.O);
		eq(IV1020.meetInterval(IV0030),  IV1020);
	}
	
	/**
	 * 
	 *
	 */
	public void testcComplimentIntervals() {
		SortedSet<Interval> s;
		
		s = IV1020.complementIntervals(IV0205);
		ok(s.contains(IV1020));
		
		s = IV1020.complementIntervals(IV0510o);
		ok(s.contains(IV1020));
		
		s = IV1020.complementIntervals(IV0510);
		ok(s.contains(IntervalsInt.newInstance(10, true, 20, false)));
		
		s = IV1020.complementIntervals(IV0515);
		ok(s.contains(IntervalsInt.newInstance(15, true, 20, false)));
		
		s = IV1020.complementIntervals(IV1020);
		ok(s.isEmpty());
		
		s = IV1020.complementIntervals(IV1020o);
		ok(s.contains(IntervalsInt.newInstance(10, false, 10, false)));
		ok(s.contains(IntervalsInt.newInstance(20, false, 20, false)));
		
		s = IV1020.complementIntervals(IV1525);
		ok(s.contains(IntervalsInt.newInstance(10, false, 15, true)));
		
		s = IV1020.complementIntervals(IV2025);
		ok(s.contains(IntervalsInt.newInstance(10, false, 20, true)));
		
		s = IV1020.complementIntervals(IV2025o);
		ok(s.contains(IV1020));
		
		s = IV1020.complementIntervals(IV2225);
		ok(s.contains(IV1020));
		
		s = IV1020.complementIntervals(IV0030);
		ok(s.isEmpty());
	}
	
	/**
	 * 
	 *
	 */
	public void testEquals() {
		Interval i0, i1, i2, i3;
		i0 = IntervalsInt.newClosedInterval(100, 200);
		i1 = IntervalsInt.newClosedInterval(100, 250);
		i2 = IntervalsInt.newClosedInterval( 50, 200);
		i3 = IntervalsInt.newClosedInterval(100, 200);
		
		ng(i0.equals(i1));
		ng(i0.equals(i2));
		ok(i0.equals(i3));
		ok(i3.equals(i0));
		ng(i0.equals(null));
	}
	
	/**
	 * 
	 *
	 */
	public void testHashCode() {
		Interval i0;
		i0 = IntervalsInt.newClosedInterval(0, 30);
		HashMap<Interval, String> mp = new HashMap<Interval, String>();
		
		mp.put(i0, "Id");
		eq(mp.get(i0), "Id");
	}

}
