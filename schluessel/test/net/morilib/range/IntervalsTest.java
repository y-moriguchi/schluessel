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

import net.morilib.lisp.test.TC;


/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2008/01/01
 */
public class IntervalsTest extends TC {
	
	private static Integer O = Integer.valueOf(0);
	private static Integer ONE = Integer.valueOf(1);
	private static Integer valueOf(int i) {
		return Integer.valueOf(i);
	}
	
	
	//
	private void isInfimumClosed(Interval nw) {
		ok(nw.isInfimumClosed());
		ng(nw.isInfimumOpen());
		ok(nw.isInfimumFinite());
	}
	
	//
	//private void isInfimumOpen(Interval nw) {
	//	ng(nw.isInfimumClosed());
	//	ok(nw.isInfimumOpen());
	//	ok(nw.isInfimumFinite());
	//}
	
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
	//private void isSupremumOpen(Interval nw) {
	//	ng(nw.isSupremumClosed());
	//	ok(nw.isSupremumOpen());
	//	ok(nw.isSupremumFinite());
	//}
	
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
	private void assertBound(Interval nw, int o1, int o2) {
		eq(nw.getInfimumBound(),  valueOf(o1));
		eq(nw.getSupremumBound(), valueOf(o2));
		ok(nw.isInfimumClosed());
		ok(nw.isSupremumClosed());
	}

	/**
	 * 
	 *
	 */
	public void testNewLowerHalfInterval() {
		Interval nw;
		
		// create (-oo, 0]
		nw = Intervals.newInfimumlessInterval(O, false);
		eq(nw.getInfimumBound(),  Limit.MINIMUM);
		eq(nw.getSupremumBound(), O);
		isInfimumInfinite(nw);  isSupremumClosed(nw);
	}

	/**
	 * 
	 *
	 */
	public void testNewUpperHalfInterval() {
		Interval nw;
		
		// create [0, +oo)
		nw = Intervals.newSupremumlessInterval(O, false);
		eq(nw.getInfimumBound(),  O);
		eq(nw.getSupremumBound(), Limit.MAXIMUM);
		isInfimumClosed(nw);  isSupremumInfinite(nw);
	}

	/**
	 * 
	 *
	 */
	public void testNewClosedIntervalOO() {
		Interval i;
		
		// [0, 1]
		i = Intervals.newClosedInterval(O, ONE);
		assertBound(i, O, false, ONE, false);
	}

	/**
	 * 
	 *
	 */
	public void testNewOpenIntervalOO() {
		Interval i;
		
		// (0, 1)
		i = Intervals.newOpenInterval(O, ONE);
		assertBound(i, O, true, ONE, true);
	}

	/**
	 * 
	 *
	 */
	public void testNewLeftOpenIntervalOO() {
		Interval i;
		
		// (0, 1]
		i = Intervals.newLeftOpenInterval(O, ONE);
		assertBound(i, O, true, ONE, false);
	}

	/**
	 * 
	 *
	 */
	public void testNewRightOpenIntervalOO() {
		Interval i;
		
		// [0, 1)
		i = Intervals.newRightOpenInterval(O, ONE);
		assertBound(i, O, false, ONE, true);
	}

	/**
	 * 
	 *
	 */
	public void testNewClosedIntervalII() {
		Interval i;
		
		i = IntervalsInt.newClosedInterval(100, 200);
		assertBound(i, 100, 200);
	}

	/**
	 * 
	 *
	 */
	public void testNewClosedLowerHalfIntervalI() {
		Interval nw;
		
		// create (-oo, 0]
		nw = IntervalsInt.newClosedInfimumlessInterval(0);
		eq(nw.getInfimumBound(),  Limit.MINIMUM);
		eq(nw.getSupremumBound(), O);
		isInfimumInfinite(nw);  isSupremumClosed(nw);
	}

	/**
	 * 
	 *
	 */
	public void testNewClosedUpperHalfInterval() {
		Interval nw;
		
		// create [0, +oo)
		nw = IntervalsInt.newClosedSupremumlessInterval(0);
		eq(nw.getInfimumBound(),  O);
		eq(nw.getSupremumBound(), Limit.MAXIMUM);
		isInfimumClosed(nw);  isSupremumInfinite(nw);
	}

}
