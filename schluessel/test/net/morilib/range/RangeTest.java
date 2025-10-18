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


import net.morilib.lisp.test.TC;
import net.morilib.range.Interval;
import net.morilib.range.IntervalsInt;
import net.morilib.range.Range;
import net.morilib.range.Ranges;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2008/01/01
 */
public class RangeTest extends TC {
	
	//
	static final Interval IV1030 = IntervalsInt.newClosedInterval(10, 30);
	static final Interval IV5070 = IntervalsInt.newClosedInterval(50, 70);
	static final Interval IV6080 = IntervalsInt.newClosedInterval(60, 80);
	
	//
	static final Interval IV0030 = IntervalsInt.newClosedInterval( 0, 30);
	static final Interval IV0205 = IntervalsInt.newClosedInterval( 2,  5);
	static final Interval IV0510 = IntervalsInt.newClosedInterval( 5, 10);
	static final Interval IV0515 = IntervalsInt.newClosedInterval( 5, 15);
	static final Interval IV0525 = IntervalsInt.newClosedInterval( 5, 25);
	static final Interval IV1015 = IntervalsInt.newClosedInterval(10, 15);
	static final Interval IV1020 = IntervalsInt.newClosedInterval(10, 20);
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
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumBelowAllOf() {
		// compare to [10, 20]
		ng(IV1020.isInfimumBelowAllOf(IV0205));
		ng(IV1020.isInfimumBelowAllOf(IV0510));
		ng(IV1020.isInfimumBelowAllOf(IV0515));
		ng(IV1020.isInfimumBelowAllOf(IV1015));
		ok(IV1020.isInfimumBelowAllOf(IV1015o));
		ok(IV1020.isInfimumBelowAllOf(IV1520));
		ok(IV1020.isInfimumBelowAllOf(IV1525));
		ok(IV1020.isInfimumBelowAllOf(IV2025));
		ok(IV1020.isInfimumBelowAllOf(IV2225));
		
		ng(IV1020o.isInfimumBelowAllOf(IV0205));
		ng(IV1020o.isInfimumBelowAllOf(IV0510));
		ng(IV1020o.isInfimumBelowAllOf(IV0515));
		ng(IV1020o.isInfimumBelowAllOf(IV1015));
		ok(IV1020o.isInfimumBelowAllOf(IV1015o));
		ok(IV1020o.isInfimumBelowAllOf(IV1520));
		ok(IV1020o.isInfimumBelowAllOf(IV1525));
		ok(IV1020o.isInfimumBelowAllOf(IV2025));
		ok(IV1020o.isInfimumBelowAllOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumAboveAllOf() {
		// compare to [10, 20]
		ok(IV1020.isInfimumAboveAllOf(IV0205));
		ok(IV1020.isInfimumAboveAllOf(IV0510o));
		ng(IV1020.isInfimumAboveAllOf(IV0510));
		ng(IV1020.isInfimumAboveAllOf(IV0515));
		ng(IV1020.isInfimumAboveAllOf(IV1015));
		ng(IV1020.isInfimumAboveAllOf(IV1520));
		ng(IV1020.isInfimumAboveAllOf(IV1525));
		ng(IV1020.isInfimumAboveAllOf(IV2025));
		ng(IV1020.isInfimumAboveAllOf(IV2225));
		
		ok(IV1020o.isInfimumAboveAllOf(IV0205));
		ok(IV1020o.isInfimumAboveAllOf(IV0510o));
		ok(IV1020o.isInfimumAboveAllOf(IV0510));
		ng(IV1020o.isInfimumAboveAllOf(IV0515));
		ng(IV1020o.isInfimumAboveAllOf(IV1015));
		ng(IV1020o.isInfimumAboveAllOf(IV1520));
		ng(IV1020o.isInfimumAboveAllOf(IV1525));
		ng(IV1020o.isInfimumAboveAllOf(IV2025));
		ng(IV1020o.isInfimumAboveAllOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumBelowAnyOf() {
		// compare to [10, 20]
		ng(IV1020.isInfimumBelowAnyOf(IV0205));
		ng(IV1020.isInfimumBelowAnyOf(IV0510o));
		ng(IV1020.isInfimumBelowAnyOf(IV0510));
		ok(IV1020.isInfimumBelowAnyOf(IV0515));
		ok(IV1020.isInfimumBelowAnyOf(IV1015));
		ok(IV1020.isInfimumBelowAnyOf(IV1520));
		ok(IV1020.isInfimumBelowAnyOf(IV1525));
		ok(IV1020.isInfimumBelowAnyOf(IV2025));
		ok(IV1020.isInfimumBelowAnyOf(IV2225));
		
		ng(IV1020o.isInfimumBelowAnyOf(IV0205));
		ng(IV1020o.isInfimumBelowAnyOf(IV0510o));
		ng(IV1020o.isInfimumBelowAnyOf(IV0510));
		ok(IV1020o.isInfimumBelowAnyOf(IV0515));
		ok(IV1020o.isInfimumBelowAnyOf(IV1015));
		ok(IV1020o.isInfimumBelowAnyOf(IV1520));
		ok(IV1020o.isInfimumBelowAnyOf(IV1525));
		ok(IV1020o.isInfimumBelowAnyOf(IV2025));
		ok(IV1020o.isInfimumBelowAnyOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumAboveAnyOf() {
		// compare to [10, 20]
		ok(IV1020.isInfimumAboveAnyOf(IV0205));
		ok(IV1020.isInfimumAboveAnyOf(IV0510));
		ok(IV1020.isInfimumAboveAnyOf(IV0515));
		ng(IV1020.isInfimumAboveAnyOf(IV1015));
		ng(IV1020.isInfimumAboveAnyOf(IV1015o));
		ng(IV1020.isInfimumAboveAnyOf(IV1520));
		ng(IV1020.isInfimumAboveAnyOf(IV1525));
		ng(IV1020.isInfimumAboveAnyOf(IV2025));
		ng(IV1020.isInfimumAboveAnyOf(IV2225));
		
		ok(IV1020o.isInfimumAboveAnyOf(IV0205));
		ok(IV1020o.isInfimumAboveAnyOf(IV0510));
		ok(IV1020o.isInfimumAboveAnyOf(IV0515));
		ok(IV1020o.isInfimumAboveAnyOf(IV1015));
		ng(IV1020o.isInfimumAboveAnyOf(IV1015o));
		ng(IV1020o.isInfimumAboveAnyOf(IV1520));
		ng(IV1020o.isInfimumAboveAnyOf(IV1525));
		ng(IV1020o.isInfimumAboveAnyOf(IV2025));
		ng(IV1020o.isInfimumAboveAnyOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumBelowAllOf() {
		// compare to [10, 20]
		ng(IV1020.isSupremumBelowAllOf(IV0205));
		ng(IV1020.isSupremumBelowAllOf(IV0510));
		ng(IV1020.isSupremumBelowAllOf(IV0515));
		ng(IV1020.isSupremumBelowAllOf(IV1015));
		ng(IV1020.isSupremumBelowAllOf(IV1520));
		ng(IV1020.isSupremumBelowAllOf(IV1525));
		ng(IV1020.isSupremumBelowAllOf(IV2025));
		ok(IV1020.isSupremumBelowAllOf(IV2025o));
		ok(IV1020.isSupremumBelowAllOf(IV2225));
		
		ng(IV1020o.isSupremumBelowAllOf(IV0205));
		ng(IV1020o.isSupremumBelowAllOf(IV0510));
		ng(IV1020o.isSupremumBelowAllOf(IV0515));
		ng(IV1020o.isSupremumBelowAllOf(IV1015));
		ng(IV1020o.isSupremumBelowAllOf(IV1520));
		ng(IV1020o.isSupremumBelowAllOf(IV1525));
		ok(IV1020o.isSupremumBelowAllOf(IV2025));
		ok(IV1020o.isSupremumBelowAllOf(IV2025o));
		ok(IV1020o.isSupremumBelowAllOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumAboveAll() {
		// compare to [10, 20]
		ok(IV1020.isSupremumAboveAllOf(IV0205));
		ok(IV1020.isSupremumAboveAllOf(IV0510));
		ok(IV1020.isSupremumAboveAllOf(IV0515));
		ok(IV1020.isSupremumAboveAllOf(IV1015));
		ok(IV1020.isSupremumAboveAllOf(IV1520o));
		ng(IV1020.isSupremumAboveAllOf(IV1520));
		ng(IV1020.isSupremumAboveAllOf(IV1525));
		ng(IV1020.isSupremumAboveAllOf(IV2025));
		ng(IV1020.isSupremumAboveAllOf(IV2225));
		
		ok(IV1020o.isSupremumAboveAllOf(IV0205));
		ok(IV1020o.isSupremumAboveAllOf(IV0510));
		ok(IV1020o.isSupremumAboveAllOf(IV0515));
		ok(IV1020o.isSupremumAboveAllOf(IV1015));
		ok(IV1020o.isSupremumAboveAllOf(IV1520o));
		ng(IV1020o.isSupremumAboveAllOf(IV1520));
		ng(IV1020o.isSupremumAboveAllOf(IV1525));
		ng(IV1020o.isSupremumAboveAllOf(IV2025));
		ng(IV1020o.isSupremumAboveAllOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumBelowAnyOf() {
		// compare to [10, 20]
		ng(IV1020.isSupremumBelowAnyOf(IV0205));
		ng(IV1020.isSupremumBelowAnyOf(IV0510));
		ng(IV1020.isSupremumBelowAnyOf(IV0515));
		ng(IV1020.isSupremumBelowAnyOf(IV1015));
		ng(IV1020.isSupremumBelowAnyOf(IV1520o));
		ng(IV1020.isSupremumBelowAnyOf(IV1520));
		ok(IV1020.isSupremumBelowAnyOf(IV1525));
		ok(IV1020.isSupremumBelowAnyOf(IV2025));
		ok(IV1020.isSupremumBelowAnyOf(IV2225));
		
		ng(IV1020o.isSupremumBelowAnyOf(IV0205));
		ng(IV1020o.isSupremumBelowAnyOf(IV0510));
		ng(IV1020o.isSupremumBelowAnyOf(IV0515));
		ng(IV1020o.isSupremumBelowAnyOf(IV1015));
		ng(IV1020o.isSupremumBelowAnyOf(IV1520o));
		ok(IV1020o.isSupremumBelowAnyOf(IV1520));
		ok(IV1020o.isSupremumBelowAnyOf(IV1525));
		ok(IV1020o.isSupremumBelowAnyOf(IV2025));
		ok(IV1020o.isSupremumBelowAnyOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumAboveAnyOf() {
		// compare to [10, 20]
		ok(IV1020.isSupremumAboveAnyOf(IV0205));
		ok(IV1020.isSupremumAboveAnyOf(IV0510));
		ok(IV1020.isSupremumAboveAnyOf(IV0515));
		ok(IV1020.isSupremumAboveAnyOf(IV1015));
		ok(IV1020.isSupremumAboveAnyOf(IV1520));
		ok(IV1020.isSupremumAboveAnyOf(IV1525));
		ng(IV1020.isSupremumAboveAnyOf(IV2025));
		ng(IV1020.isSupremumAboveAnyOf(IV2025o));
		ng(IV1020.isSupremumAboveAnyOf(IV2225));
		
		ok(IV1020o.isSupremumAboveAnyOf(IV0205));
		ok(IV1020o.isSupremumAboveAnyOf(IV0510));
		ok(IV1020o.isSupremumAboveAnyOf(IV0515));
		ok(IV1020o.isSupremumAboveAnyOf(IV1015));
		ok(IV1020o.isSupremumAboveAnyOf(IV1520));
		ok(IV1020o.isSupremumAboveAnyOf(IV1525));
		ng(IV1020o.isSupremumAboveAnyOf(IV2025));
		ng(IV1020o.isSupremumAboveAnyOf(IV2025o));
		ng(IV1020o.isSupremumAboveAnyOf(IV2225));
	}
	
	/**
	 * 
	 *
	 */
	public void testEquals() {
		Interval iv1030l = IntervalsInt.newClosedInterval(10, 30);
		Interval iv1040l = IntervalsInt.newClosedInterval(10, 40);
		Interval iv3040i = IntervalsInt.newClosedInterval(30, 40);
		Interval iv3050i = IntervalsInt.newClosedInterval(30, 50);
		Interval iv1050i = IntervalsInt.newClosedInterval(10, 50);
		Interval iv5070i = IntervalsInt.newClosedInterval(50, 70);
		Interval iv6080i = IntervalsInt.newClosedInterval(60, 80);
		Interval iv5070oi = IntervalsInt.newOpenInterval(50, 70);
		Range r1 = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		Range r2 = Ranges.sum(Ranges.sum(iv1030l, iv5070i), iv6080i);
		Range r3 = Ranges.sum(Ranges.sum(iv1030l, iv6080i), iv5070oi);
		Range r4 = Ranges.sum(IV1030, iv5070i);
		Range r5 = Ranges.sum(iv1030l, iv3050i);
		Range r8 = Ranges.overlap(iv1040l, iv3050i);
		
		//
		ok(r1.equals(r2));
		ng(r1.equals(r3));
		ng(r1.equals(r4));
		ok(r5.equals(iv1050i));
		ok(r8.equals(iv3040i));
		
		//
		ok(r1.equals(r1));
		ok(r2.equals(r1));
		ng(r1.equals(null));
	}
	
	/**
	 * 
	 *
	 */
	public void testHashCode() {
		Interval iv1030l = IntervalsInt.newClosedInterval(10, 30);
		Interval iv1040l = IntervalsInt.newClosedInterval(10, 40);
		Interval iv3040i = IntervalsInt.newClosedInterval(30, 40);
		Interval iv3050i = IntervalsInt.newClosedInterval(30, 50);
		Interval iv1050i = IntervalsInt.newClosedInterval(10, 50);
		Interval iv5070i = IntervalsInt.newClosedInterval(50, 70);
		Interval iv6080i = IntervalsInt.newClosedInterval(60, 80);
		Interval iv9099i = IntervalsInt.newClosedInterval(90, 99);
		Interval iv9099l = IntervalsInt.newClosedInterval(90, 99);
		Interval iv5070oi = IntervalsInt.newOpenInterval(50, 70);
		Range r1 = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		Range r2 = Ranges.sum(Ranges.sum(iv1030l, iv5070i), iv6080i);
		Range r3 = Ranges.sum(Ranges.sum(iv1030l, iv6080i), iv5070oi);
		Range r4 = Ranges.sum(iv1040l, iv5070i);
		Range r5 = Ranges.sum(iv1030l, iv3050i);
		Range r6 = Ranges.sum(r5, iv9099i);
		Range r7 = Ranges.sum(iv1050i, iv9099l);
		Range r8 = Ranges.overlap(iv1040l, iv3050i);
		HashMap<Range, String> mp = new HashMap<Range, String>();
		
		mp.put(r1, "r1");
		mp.put(r5, "r5");
		mp.put(r6, "r6");
		mp.put(r8, "r8");
		
		//
		eq(mp.get(r2), "r1");
		ng(mp.containsKey(r3));
		ng(mp.containsKey(r4));
		eq(mp.get(iv1050i), "r5");
		eq(mp.get(r7), "r6");
		eq(mp.get(iv3040i), "r8");
	}
	
}
