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

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import net.morilib.lisp.test.TC;

/**
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2008/01/01
 */
public class MergedRangeTest extends TC {
	
	//private static Integer O = new Integer(0);
	//private static Integer ONE = new Integer(1);
	private static Integer valueOf(int i) {
		return Integer.valueOf(i);
	}
	
	//
	//Interval IV2030 = Intervals.newClosedInterval(20, 70);
	//Interval IV5070 = Intervals.newClosedInterval(50, 70);
	
	//
	static final Interval IV1030 = IntervalsInt.newClosedInterval(10, 30);
	static final Interval IV5070 = IntervalsInt.newClosedInterval(50, 70);
	static final Interval IV6080 = IntervalsInt.newClosedInterval(60, 80);
	static final Interval IVoo00 =
		IntervalsInt.newClosedInfimumlessInterval (0);
	static final Interval IVoo99 =
		IntervalsInt.newClosedInfimumlessInterval (99);
	static final Interval IV00oo =
		IntervalsInt.newClosedSupremumlessInterval(0);
	static final Interval IV99oo =
		IntervalsInt.newClosedSupremumlessInterval(99);
	
	//
	static final Interval IV1030o = IntervalsInt.newOpenInterval(10, 30);
	static final Interval IV5070o = IntervalsInt.newOpenInterval(50, 70);
	static final Interval IV6080o = IntervalsInt.newOpenInterval(60, 80);
	static final Interval IVoo00o =
		IntervalsInt.newOpenInfimumlessInterval (0);
	static final Interval IVoo99o =
		IntervalsInt.newOpenInfimumlessInterval (99);
	static final Interval IV00ooo =
		IntervalsInt.newOpenSupremumlessInterval(0);
	static final Interval IV99ooo =
		IntervalsInt.newOpenSupremumlessInterval(99);
	
	//
	static final Interval IV0005 = IntervalsInt.newClosedInterval( 0,  5);
	static final Interval IV0010 = IntervalsInt.newClosedInterval( 0, 10);
	static final Interval IV0020 = IntervalsInt.newClosedInterval( 0, 20);
	static final Interval IV1020 = IntervalsInt.newClosedInterval(10, 20);
	static final Interval IV1525 = IntervalsInt.newClosedInterval(15, 25);
	static final Interval IV2030 = IntervalsInt.newClosedInterval(20, 30);
	static final Interval IV3040 = IntervalsInt.newClosedInterval(30, 40);
	static final Interval IV3545 = IntervalsInt.newClosedInterval(35, 45);
	static final Interval IV4050 = IntervalsInt.newClosedInterval(40, 50);
	static final Interval IV4060 = IntervalsInt.newClosedInterval(40, 60);
	static final Interval IV7080 = IntervalsInt.newClosedInterval(70, 80);
	static final Interval IV7090 = IntervalsInt.newClosedInterval(70, 90);
	static final Interval IV5060 = IntervalsInt.newClosedInterval(50, 60);
	static final Interval IV5080 = IntervalsInt.newClosedInterval(50, 80);
	static final Interval IV1080 = IntervalsInt.newClosedInterval(10, 80);
	static final Interval IV8090 = IntervalsInt.newClosedInterval(80, 90);
	static final Interval IV8590 = IntervalsInt.newClosedInterval(85, 90);
	
	//
	static final Interval IV0005o = IntervalsInt.newOpenInterval( 0,  5);
	static final Interval IV0010o = IntervalsInt.newOpenInterval( 0, 10);
	static final Interval IV0020o = IntervalsInt.newOpenInterval( 0, 20);
	static final Interval IV1020o = IntervalsInt.newOpenInterval(10, 20);
	static final Interval IV2030o = IntervalsInt.newOpenInterval(20, 30);
	static final Interval IV3040o = IntervalsInt.newOpenInterval(30, 40);
	static final Interval IV3545o = IntervalsInt.newOpenInterval(35, 45);
	static final Interval IV4050o = IntervalsInt.newOpenInterval(40, 50);
	static final Interval IV4060o = IntervalsInt.newOpenInterval(40, 60);
	static final Interval IV7080o = IntervalsInt.newOpenInterval(70, 80);
	static final Interval IV7090o = IntervalsInt.newOpenInterval(70, 90);
	static final Interval IV5060o = IntervalsInt.newOpenInterval(50, 60);
	static final Interval IV5080o = IntervalsInt.newOpenInterval(50, 80);
	static final Interval IV1080o = IntervalsInt.newOpenInterval(10, 80);
	static final Interval IV8090o = IntervalsInt.newOpenInterval(80, 90);
	static final Interval IV8590o = IntervalsInt.newOpenInterval(85, 90);
	
	/**
	 * 
	 *
	 */
	public void testAdd() {
		Range r;
		Interval i1 = IntervalsInt.newClosedInterval(200, 300);
		Interval i2 = IntervalsInt.newClosedInterval(500, 700);
		Interval i3 = IntervalsInt.newClosedInterval(400, 600);
		SortedSet<Interval> s;
		
		r = Ranges.sum(i1, i2);
		s = new TreeSet<Interval>();
		s.add(i1);  s.add(i2);
		ok(r.intervals().contains(i1));
		eq(r.intervals(), s);
		
		r = Ranges.sum(Ranges.sum(i1, i2), i3);
		s = new TreeSet<Interval>();
		s.add(i1);
		s.add(IntervalsInt.newClosedInterval(400, 700));
		eq(r.intervals(), s);
	}
	
	/**
	 * 
	 *
	 */
	public void testContains() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.contains(10));  ok(r.contains(20));
		ok(r.contains(25));  ok(r.contains(30));
		ng(r.contains(35));  ok(r.contains(40));
		ok(r.contains(45));  ok(r.contains(50));
		ok(r.contains(55));  ok(r.contains(60));
		ok(r.contains(65));  ok(r.contains(70));
		ng(r.contains(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimum() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ok(r.isInfimumAbove(10));  ng(r.isInfimumAbove(20));
		ng(r.isInfimumAbove(25));  ng(r.isInfimumAbove(30));
		ng(r.isInfimumAbove(35));  ng(r.isInfimumAbove(40));
		ng(r.isInfimumAbove(45));  ng(r.isInfimumAbove(50));
		ng(r.isInfimumAbove(55));  ng(r.isInfimumAbove(60));
		ng(r.isInfimumAbove(65));  ng(r.isInfimumAbove(70));
		ng(r.isInfimumAbove(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimum() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.isInfimumBelow(10));  ng(r.isInfimumBelow(20));
		ok(r.isInfimumBelow(25));  ok(r.isInfimumBelow(30));
		ok(r.isInfimumBelow(35));  ok(r.isInfimumBelow(40));
		ok(r.isInfimumBelow(45));  ok(r.isInfimumBelow(50));
		ok(r.isInfimumBelow(55));  ok(r.isInfimumBelow(60));
		ok(r.isInfimumBelow(65));  ok(r.isInfimumBelow(70));
		ok(r.isInfimumBelow(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsInfimum() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.isInfimumEqualTo(10));  ok(r.isInfimumEqualTo(20));
		ng(r.isInfimumEqualTo(25));  ng(r.isInfimumEqualTo(30));
		ng(r.isInfimumEqualTo(35));  ng(r.isInfimumEqualTo(40));
		ng(r.isInfimumEqualTo(45));  ng(r.isInfimumEqualTo(50));
		ng(r.isInfimumEqualTo(55));  ng(r.isInfimumEqualTo(60));
		ng(r.isInfimumEqualTo(65));  ng(r.isInfimumEqualTo(70));
		ng(r.isInfimumEqualTo(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremum() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ok(r.isSupremumAbove(10));  ok(r.isSupremumAbove(20));
		ok(r.isSupremumAbove(25));  ok(r.isSupremumAbove(30));
		ok(r.isSupremumAbove(35));  ok(r.isSupremumAbove(40));
		ok(r.isSupremumAbove(45));  ok(r.isSupremumAbove(50));
		ok(r.isSupremumAbove(55));  ok(r.isSupremumAbove(60));
		ok(r.isSupremumAbove(65));  ng(r.isSupremumAbove(70));
		ng(r.isSupremumAbove(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremum() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.isSupremumBelow(10));  ng(r.isSupremumBelow(20));
		ng(r.isSupremumBelow(25));  ng(r.isSupremumBelow(30));
		ng(r.isSupremumBelow(35));  ng(r.isSupremumBelow(40));
		ng(r.isSupremumBelow(45));  ng(r.isSupremumBelow(50));
		ng(r.isSupremumBelow(55));  ng(r.isSupremumBelow(60));
		ng(r.isSupremumBelow(65));  ng(r.isSupremumBelow(70));
		ok(r.isSupremumBelow(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsSupremum() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.isSupremumEqualTo(10));  ng(r.isSupremumEqualTo(20));
		ng(r.isSupremumEqualTo(25));  ng(r.isSupremumEqualTo(30));
		ng(r.isSupremumEqualTo(35));  ng(r.isSupremumEqualTo(40));
		ng(r.isSupremumEqualTo(45));  ng(r.isSupremumEqualTo(50));
		ng(r.isSupremumEqualTo(55));  ng(r.isSupremumEqualTo(60));
		ng(r.isSupremumEqualTo(65));  ok(r.isSupremumEqualTo(70));
		ng(r.isSupremumEqualTo(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testIn() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.in(IV0005));  ng(r.in(IV0010));
		ng(r.in(IV0020));  ng(r.in(IV1020));
		ng(r.in(IV2030));  ng(r.in(IV3040));
		ng(r.in(IV3545));  ng(r.in(IV4050));
		ng(r.in(IV4060));  ng(r.in(IV7080));
		ng(r.in(IV7090));  ng(r.in(IV5080));
		ok(r.in(IV1080));  ng(r.in(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testIndependentOf() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.independentOf(IV0005));  ng(r.independentOf(IV0010));
		ng(r.independentOf(IV0020));  ng(r.independentOf(IV1020));
		ng(r.independentOf(IV2030));  ng(r.independentOf(IV3040));
		ok(r.independentOf(IV3545));  ng(r.independentOf(IV4050));
		ng(r.independentOf(IV4060));  ng(r.independentOf(IV7080));
		ng(r.independentOf(IV7090));  ng(r.independentOf(IV5080));
		ng(r.independentOf(IV1080));  ok(r.independentOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testInteriorIndependentOf() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.interiorIndependentOf(IV0005));
		ok(r.interiorIndependentOf(IV0010));
		ng(r.interiorIndependentOf(IV0020));
		ng(r.interiorIndependentOf(IV1020));
		ng(r.interiorIndependentOf(IV2030));
		ok(r.interiorIndependentOf(IV3040));
		ok(r.interiorIndependentOf(IV3545));
		ok(r.interiorIndependentOf(IV4050));
		ng(r.interiorIndependentOf(IV4060));
		ng(r.interiorIndependentOf(IV7080));
		ng(r.interiorIndependentOf(IV7090));
		ng(r.interiorIndependentOf(IV5080));
		ng(r.interiorIndependentOf(IV1080));
		ok(r.interiorIndependentOf(IV8090));
		ok(r.interiorIndependentOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsEmpty() {
		ng(Ranges.sum(IV1030, IV6080).isEmpty());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumFinite() {
		ok(Ranges.sum(IV1030, IV6080).isInfimumFinite());
		ng(Ranges.sum(IV1030, IVoo00).isInfimumFinite());
		ok(Ranges.sum(IV1030, IV99oo).isInfimumFinite());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumFinite() {
		ok(Ranges.sum(IV1030, IV6080).isSupremumFinite());
		ok(Ranges.sum(IV1030, IVoo00).isSupremumFinite());
		ng(Ranges.sum(IV1030, IV99oo).isSupremumFinite());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumOpen() {
		ng(Ranges.sum(IV1030,  IV6080).isInfimumOpen());
		ok(Ranges.sum(IV1030,  IVoo00).isInfimumOpen());
		ng(Ranges.sum(IV1030,  IV99oo).isInfimumOpen());
		ok(Ranges.sum(IV1030o, IV6080).isInfimumOpen());
		ok(Ranges.sum(IV1030o, IVoo00).isInfimumOpen());
		ok(Ranges.sum(IV1030o, IV99oo).isInfimumOpen());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumOpen() {
		ng(Ranges.sum(IV1030,  IV0010).isSupremumOpen());
		ng(Ranges.sum(IV1030,  IVoo00).isSupremumOpen());
		ok(Ranges.sum(IV1030,  IV99oo).isSupremumOpen());
		ok(Ranges.sum(IV1030o, IV0010).isSupremumOpen());
		ok(Ranges.sum(IV1030o, IVoo00).isSupremumOpen());
		ok(Ranges.sum(IV1030o, IV99oo).isSupremumOpen());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsInfimumClosed() {
		ok(Ranges.sum(IV1030,  IV6080).isInfimumClosed());
		ok(Ranges.sum(IV1030,  IVoo00).isInfimumClosed());
		ok(Ranges.sum(IV1030,  IV99oo).isInfimumClosed());
		ng(Ranges.sum(IV1030o, IV6080).isInfimumClosed());
		ok(Ranges.sum(IV1030o, IVoo00).isInfimumClosed());
		ng(Ranges.sum(IV1030o, IV99oo).isInfimumClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsSupremumClosed() {
		ok(Ranges.sum(IV1030,  IV0010).isSupremumClosed());
		ok(Ranges.sum(IV1030,  IVoo00).isSupremumClosed());
		ok(Ranges.sum(IV1030,  IV99oo).isSupremumClosed());
		ng(Ranges.sum(IV1030o, IV0010).isSupremumClosed());
		ng(Ranges.sum(IV1030o, IVoo00).isSupremumClosed());
		ok(Ranges.sum(IV1030o, IV99oo).isSupremumClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testBound() {
		Range r;  Collection<Interval> c;
		
		r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		c = r.bound().intervals();
		eq(c.size(), 4);
		ok(c.contains(IntervalsInt.newPoint(10)));
		ok(c.contains(IntervalsInt.newPoint(30)));
		ok(c.contains(IntervalsInt.newPoint(50)));
		ok(c.contains(IntervalsInt.newPoint(80)));
		
		r = Ranges.sum(Ranges.sum(IV1030, IV6080), IVoo99);
		c = r.bound().intervals();
		eq(c.size(), 1);
		ok(c.contains(IntervalsInt.newPoint(99)));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsNeighborhoodOf() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.isNeighborhoodOf(10));  ng(r.isNeighborhoodOf(20));
		ok(r.isNeighborhoodOf(25));  ng(r.isNeighborhoodOf(30));
		ng(r.isNeighborhoodOf(35));  ng(r.isNeighborhoodOf(40));
		ok(r.isNeighborhoodOf(45));  ok(r.isNeighborhoodOf(50));
		ok(r.isNeighborhoodOf(55));  ok(r.isNeighborhoodOf(60));
		ok(r.isNeighborhoodOf(65));  ng(r.isNeighborhoodOf(70));
		ng(r.isNeighborhoodOf(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsClosure() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.containsClosure(10));  ok(r.containsClosure(20));
		ok(r.containsClosure(25));  ok(r.containsClosure(30));
		ng(r.containsClosure(35));  ok(r.containsClosure(40));
		ok(r.containsClosure(45));  ok(r.containsClosure(50));
		ok(r.containsClosure(55));  ok(r.containsClosure(60));
		ok(r.containsClosure(65));  ok(r.containsClosure(70));
		ng(r.containsClosure(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060);
		
		ng(r.containsBound(10));  ok(r.containsBound(20));
		ng(r.containsBound(25));  ok(r.containsBound(30));
		ng(r.containsBound(35));  ok(r.containsBound(40));
		ng(r.containsBound(45));  ng(r.containsBound(50));
		ng(r.containsBound(55));  ng(r.containsBound(60));
		ng(r.containsBound(65));  ok(r.containsBound(70));
		ng(r.containsBound(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testBoundsIn() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.boundsIn(IV0005));  ng(r.boundsIn(IV0010));
		ng(r.boundsIn(IV0020));  ng(r.boundsIn(IV1020));
		ng(r.boundsIn(IV2030));  ng(r.boundsIn(IV3040));
		ng(r.boundsIn(IV3545));  ng(r.boundsIn(IV4050));
		ng(r.boundsIn(IV4060));  ng(r.boundsIn(IV7080));
		ng(r.boundsIn(IV7090));  ng(r.boundsIn(IV5080));
		ok(r.boundsIn(IV1080));  ng(r.boundsIn(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testClosureContains() {
		Range r = Ranges.sum(Ranges.sum(IV1030o, IV6080o), IV5070o);
		
		ng(r.closureContains(IV0005));
		ng(r.closureContains(IV0010));
		ng(r.closureContains(IV0020));
		ok(r.closureContains(IV1020));
		ok(r.closureContains(IV2030)); 
		ng(r.closureContains(IV3040));
		ng(r.closureContains(IV3545));
		ng(r.closureContains(IV4050));
		ng(r.closureContains(IV4060));
		ok(r.closureContains(IV5060));
		ok(r.closureContains(IV5080));
		ok(r.closureContains(IV7080));
		ng(r.closureContains(IV7090));
		ng(r.closureContains(IV1080));
		ng(r.closureContains(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testInteriorContains() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.interiorContains(IV0005));
		ng(r.interiorContains(IV0010));
		ng(r.interiorContains(IV0020));
		ng(r.interiorContains(IV1020));
		ng(r.interiorContains(IV2030)); 
		ng(r.interiorContains(IV3040));
		ng(r.interiorContains(IV3545));
		ng(r.interiorContains(IV4050));
		ng(r.interiorContains(IV4060));
		ng(r.interiorContains(IV5060));
		ng(r.interiorContains(IV5080));
		ng(r.interiorContains(IV7080));
		ng(r.interiorContains(IV7090));
		ng(r.interiorContains(IV1080));
		ng(r.interiorContains(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsAll() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.containsAll(IV0005));  ng(r.containsAll(IV0010));
		ng(r.containsAll(IV0020));  ok(r.containsAll(IV1020));
		ok(r.containsAll(IV2030));  ng(r.containsAll(IV3040));
		ng(r.containsAll(IV3545));  ng(r.containsAll(IV4050));
		ng(r.containsAll(IV4060));  ok(r.containsAll(IV5060));
		ok(r.containsAll(IV5080));  ok(r.containsAll(IV7080));
		ng(r.containsAll(IV7090));  ng(r.containsAll(IV1080));
		ng(r.containsAll(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testContainsAny() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.containsAny(IV0005));  ok(r.containsAny(IV0010));
		ok(r.containsAny(IV0020));  ok(r.containsAny(IV1020));
		ok(r.containsAny(IV2030));  ok(r.containsAny(IV3040));
		ng(r.containsAny(IV3545));  ok(r.containsAny(IV4050));
		ok(r.containsAny(IV4060));  ok(r.containsAny(IV5060));
		ok(r.containsAny(IV5080));  ok(r.containsAny(IV7080));
		ok(r.containsAny(IV7090));  ok(r.containsAny(IV1080));
		ng(r.containsAny(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumAll() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.isInfimumAboveAllOf(IV0005));
		ok(r.isInfimumAboveAllOf(IV0010o));
		ng(r.isInfimumAboveAllOf(IV0010));
		ng(r.isInfimumAboveAllOf(IV0020));
		ng(r.isInfimumAboveAllOf(IV1020));
		ng(r.isInfimumAboveAllOf(IV2030));
		ng(r.isInfimumAboveAllOf(IV3040));
		ng(r.isInfimumAboveAllOf(IV3545));
		ng(r.isInfimumAboveAllOf(IV4050));
		ng(r.isInfimumAboveAllOf(IV4060));
		ng(r.isInfimumAboveAllOf(IV5060));
		ng(r.isInfimumAboveAllOf(IV5080));
		ng(r.isInfimumAboveAllOf(IV7080));
		ng(r.isInfimumAboveAllOf(IV7090));
		ng(r.isInfimumAboveAllOf(IV1080));
		ng(r.isInfimumAboveAllOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testContactInfimum() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.contactInfimum(IV0005));  ok(r.contactInfimum(IV0010));
		ng(r.contactInfimum(IV0020));  ng(r.contactInfimum(IV1020));
		ng(r.contactInfimum(IV2030));  ng(r.contactInfimum(IV3040));
		ng(r.contactInfimum(IV3545));  ng(r.contactInfimum(IV4050));
		ng(r.contactInfimum(IV4060));  ng(r.contactInfimum(IV5060));
		ng(r.contactInfimum(IV5080));  ng(r.contactInfimum(IV7080));
		ng(r.contactInfimum(IV7090));  ng(r.contactInfimum(IV1080));
		ng(r.contactInfimum(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testBoundElements() {
		Range r;  Collection<?> c;
		
		r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		c = r.boundElements();
		eq(c.size(), 4);
		ok(c.contains(valueOf(10)));
		ok(c.contains(valueOf(30)));
		ok(c.contains(valueOf(50)));
		ok(c.contains(valueOf(80)));
		
		r = Ranges.sum(Ranges.sum(IV1030, IV6080), IVoo99);
		c = r.boundElements();
		eq(c.size(), 1);
		ok(c.contains(valueOf(99)));
	}
	
	/**
	 * 
	 *
	 */
	public void testBoundsIndependentOf() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.boundsIndependentOf(IV0005));
		ng(r.boundsIndependentOf(IV0010));
		ng(r.boundsIndependentOf(IV0020));
		ng(r.boundsIndependentOf(IV1020));
		ok(r.boundsIndependentOf(IV1525));
		ng(r.boundsIndependentOf(IV2030));
		ng(r.boundsIndependentOf(IV3040));
		ok(r.boundsIndependentOf(IV3545));
		ng(r.boundsIndependentOf(IV4050));
		ng(r.boundsIndependentOf(IV4060));
		ng(r.boundsIndependentOf(IV7080));
		ng(r.boundsIndependentOf(IV7090));
		ng(r.boundsIndependentOf(IV5080));
		ng(r.boundsIndependentOf(IV1080));
		ok(r.boundsIndependentOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o);
		
		ok(r.isInfimumBoundAbove(10));  ng(r.isInfimumBoundAbove(20));
		ng(r.isInfimumBoundAbove(25));  ng(r.isInfimumBoundAbove(30));
		ng(r.isInfimumBoundAbove(35));  ng(r.isInfimumBoundAbove(40));
		ng(r.isInfimumBoundAbove(45));  ng(r.isInfimumBoundAbove(50));
		ng(r.isInfimumBoundAbove(55));  ng(r.isInfimumBoundAbove(60));
		ng(r.isInfimumBoundAbove(65));  ng(r.isInfimumBoundAbove(70));
		ng(r.isInfimumBoundAbove(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimumBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o);
		
		ng(r.isInfimumBoundBelow(10));  ng(r.isInfimumBoundBelow(20));
		ok(r.isInfimumBoundBelow(25));  ok(r.isInfimumBoundBelow(30));
		ok(r.isInfimumBoundBelow(35));  ok(r.isInfimumBoundBelow(40));
		ok(r.isInfimumBoundBelow(45));  ok(r.isInfimumBoundBelow(50));
		ok(r.isInfimumBoundBelow(55));  ok(r.isInfimumBoundBelow(60));
		ok(r.isInfimumBoundBelow(65));  ok(r.isInfimumBoundBelow(70));
		ok(r.isInfimumBoundBelow(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsInfimumBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o);
		
		ng(r.isInfimumBoundEqualTo(10));  ok(r.isInfimumBoundEqualTo(20));
		ng(r.isInfimumBoundEqualTo(25));  ng(r.isInfimumBoundEqualTo(30));
		ng(r.isInfimumBoundEqualTo(35));  ng(r.isInfimumBoundEqualTo(40));
		ng(r.isInfimumBoundEqualTo(45));  ng(r.isInfimumBoundEqualTo(50));
		ng(r.isInfimumBoundEqualTo(55));  ng(r.isInfimumBoundEqualTo(60));
		ng(r.isInfimumBoundEqualTo(65));  ng(r.isInfimumBoundEqualTo(70));
		ng(r.isInfimumBoundEqualTo(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremumBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o);
		
		ok(r.isSupremumBoundAbove(10));  ok(r.isSupremumBoundAbove(20));
		ok(r.isSupremumBoundAbove(25));  ok(r.isSupremumBoundAbove(30));
		ok(r.isSupremumBoundAbove(35));  ok(r.isSupremumBoundAbove(40));
		ok(r.isSupremumBoundAbove(45));  ok(r.isSupremumBoundAbove(50));
		ok(r.isSupremumBoundAbove(55));  ok(r.isSupremumBoundAbove(60));
		ok(r.isSupremumBoundAbove(65));  ng(r.isSupremumBoundAbove(70));
		ng(r.isSupremumBoundAbove(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremumBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o);
		
		ng(r.isSupremumBoundBelow(10));
		ng(r.isSupremumBoundBelow(20));
		ng(r.isSupremumBoundBelow(25));
		ng(r.isSupremumBoundBelow(30));
		ng(r.isSupremumBoundBelow(35));
		ng(r.isSupremumBoundBelow(40));
		ng(r.isSupremumBoundBelow(45));
		ng(r.isSupremumBoundBelow(50));
		ng(r.isSupremumBoundBelow(55));
		ng(r.isSupremumBoundBelow(60));
		ng(r.isSupremumBoundBelow(65));
		ng(r.isSupremumBoundBelow(70));
		ok(r.isSupremumBoundBelow(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testEqualsSupremumBound() {
		Range r = Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o);
		
		ng(r.isSupremumBoundEqualTo(10));
		ng(r.isSupremumBoundEqualTo(20));
		ng(r.isSupremumBoundEqualTo(25));
		ng(r.isSupremumBoundEqualTo(30));
		ng(r.isSupremumBoundEqualTo(35));
		ng(r.isSupremumBoundEqualTo(40));
		ng(r.isSupremumBoundEqualTo(45));
		ng(r.isSupremumBoundEqualTo(50));
		ng(r.isSupremumBoundEqualTo(55)); 
		ng(r.isSupremumBoundEqualTo(60));
		ng(r.isSupremumBoundEqualTo(65));
		ok(r.isSupremumBoundEqualTo(70));
		ng(r.isSupremumBoundEqualTo(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testIsOpen() {
		ok(Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060o).isOpen());
		ng(Ranges.sum(Ranges.sum(IV2030o, IV5070o), IV4060).isOpen());
	}
	
	/**
	 * 
	 *
	 */
	public void testIsClosed() {
		ok(Ranges.sum(Ranges.sum(IV2030, IV5070), IV4060).isClosed());
		ng(Ranges.sum(Ranges.sum(IV2030, IV5070o), IV4060).isClosed());
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumBoundAll() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.isInfimumBoundAboveAllClosureOf(IV0005));
		ng(r.isInfimumBoundAboveAllClosureOf(IV0010));
		ng(r.isInfimumBoundAboveAllClosureOf(IV0020));
		ng(r.isInfimumBoundAboveAllClosureOf(IV1020));
		ng(r.isInfimumBoundAboveAllClosureOf(IV1525));
		ng(r.isInfimumBoundAboveAllClosureOf(IV2030));
		ng(r.isInfimumBoundAboveAllClosureOf(IV3040));
		ng(r.isInfimumBoundAboveAllClosureOf(IV3545));
		ng(r.isInfimumBoundAboveAllClosureOf(IV4050));
		ng(r.isInfimumBoundAboveAllClosureOf(IV4060));
		ng(r.isInfimumBoundAboveAllClosureOf(IV7080));
		ng(r.isInfimumBoundAboveAllClosureOf(IV7090));
		ng(r.isInfimumBoundAboveAllClosureOf(IV5080));
		ng(r.isInfimumBoundAboveAllClosureOf(IV1080));
		ng(r.isInfimumBoundAboveAllClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimumBoundAll() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.isInfimumBoundBelowAllClosureOf(IV0005));
		ng(r.isInfimumBoundBelowAllClosureOf(IV0010));
		ng(r.isInfimumBoundBelowAllClosureOf(IV0020));
		ng(r.isInfimumBoundBelowAllClosureOf(IV1020));
		ok(r.isInfimumBoundBelowAllClosureOf(IV1525));
		ok(r.isInfimumBoundBelowAllClosureOf(IV2030));
		ok(r.isInfimumBoundBelowAllClosureOf(IV3040));
		ok(r.isInfimumBoundBelowAllClosureOf(IV3545));
		ok(r.isInfimumBoundBelowAllClosureOf(IV4050));
		ok(r.isInfimumBoundBelowAllClosureOf(IV4060));
		ok(r.isInfimumBoundBelowAllClosureOf(IV7080));
		ok(r.isInfimumBoundBelowAllClosureOf(IV7090));
		ok(r.isInfimumBoundBelowAllClosureOf(IV5080));
		ng(r.isInfimumBoundBelowAllClosureOf(IV1080));
		ok(r.isInfimumBoundBelowAllClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowInfimumBoundAny() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.isInfimumBoundAboveAnyClosureOf(IV0005));
		ok(r.isInfimumBoundAboveAnyClosureOf(IV0010));
		ok(r.isInfimumBoundAboveAnyClosureOf(IV0020));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV1020));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV1525));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV2030));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV3040));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV3545));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV4050));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV4060));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV7080));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV7090));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV5080));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV1080));
		ng(r.isInfimumBoundAboveAnyClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveInfimumBoundAny() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.isInfimumBoundBelowAnyClosureOf(IV0005));
		ng(r.isInfimumBoundBelowAnyClosureOf(IV0010));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV0020));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV1020));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV1525));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV2030));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV3040));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV3545));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV4050));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV4060));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV7080));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV7090));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV5080));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV1080));
		ok(r.isInfimumBoundBelowAnyClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testContactInfimumBound() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.contactInfimumBound(IV0005));
		ok(r.contactInfimumBound(IV0010));
		ng(r.contactInfimumBound(IV0020));
		ng(r.contactInfimumBound(IV1020));
		ng(r.contactInfimumBound(IV1525));
		ng(r.contactInfimumBound(IV2030));
		ng(r.contactInfimumBound(IV3040));
		ng(r.contactInfimumBound(IV3545));
		ng(r.contactInfimumBound(IV4050));
		ng(r.contactInfimumBound(IV4060));
		ng(r.contactInfimumBound(IV7080));
		ng(r.contactInfimumBound(IV7090));
		ng(r.contactInfimumBound(IV5080));
		ng(r.contactInfimumBound(IV1080));
		ng(r.contactInfimumBound(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testCommonInfimumBoundTo() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.commonInfimumBoundTo(IV0005));
		ng(r.commonInfimumBoundTo(IV0010));
		ng(r.commonInfimumBoundTo(IV0020));
		ok(r.commonInfimumBoundTo(IV1020));
		ng(r.commonInfimumBoundTo(IV1525));
		ng(r.commonInfimumBoundTo(IV2030));
		ng(r.commonInfimumBoundTo(IV3040));
		ng(r.commonInfimumBoundTo(IV3545));
		ng(r.commonInfimumBoundTo(IV4050));
		ng(r.commonInfimumBoundTo(IV4060));
		ng(r.commonInfimumBoundTo(IV7080));
		ng(r.commonInfimumBoundTo(IV7090));
		ng(r.commonInfimumBoundTo(IV5080));
		ok(r.commonInfimumBoundTo(IV1080));
		ng(r.commonInfimumBoundTo(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremumBoundAll() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.isSupremumBoundAboveAllClosureOf(IV0005));
		ok(r.isSupremumBoundAboveAllClosureOf(IV0010));
		ok(r.isSupremumBoundAboveAllClosureOf(IV0020));
		ok(r.isSupremumBoundAboveAllClosureOf(IV1020));
		ok(r.isSupremumBoundAboveAllClosureOf(IV1525));
		ok(r.isSupremumBoundAboveAllClosureOf(IV2030));
		ok(r.isSupremumBoundAboveAllClosureOf(IV3040));
		ok(r.isSupremumBoundAboveAllClosureOf(IV3545));
		ok(r.isSupremumBoundAboveAllClosureOf(IV4050));
		ok(r.isSupremumBoundAboveAllClosureOf(IV4060));
		ng(r.isSupremumBoundAboveAllClosureOf(IV7080));
		ng(r.isSupremumBoundAboveAllClosureOf(IV7090));
		ng(r.isSupremumBoundAboveAllClosureOf(IV5080));
		ng(r.isSupremumBoundAboveAllClosureOf(IV1080));
		ng(r.isSupremumBoundAboveAllClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremumBoundAll() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.isSupremumBoundBelowAllClosureOf(IV0005));
		ng(r.isSupremumBoundBelowAllClosureOf(IV0010));
		ng(r.isSupremumBoundBelowAllClosureOf(IV0020));
		ng(r.isSupremumBoundBelowAllClosureOf(IV1020));
		ng(r.isSupremumBoundBelowAllClosureOf(IV1525));
		ng(r.isSupremumBoundBelowAllClosureOf(IV2030));
		ng(r.isSupremumBoundBelowAllClosureOf(IV3040));
		ng(r.isSupremumBoundBelowAllClosureOf(IV3545));
		ng(r.isSupremumBoundBelowAllClosureOf(IV4050));
		ng(r.isSupremumBoundBelowAllClosureOf(IV4060));
		ng(r.isSupremumBoundBelowAllClosureOf(IV7080));
		ng(r.isSupremumBoundBelowAllClosureOf(IV7090));
		ng(r.isSupremumBoundBelowAllClosureOf(IV5080));
		ng(r.isSupremumBoundBelowAllClosureOf(IV1080));
		ok(r.isSupremumBoundBelowAllClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testBelowSupremumBoundAny() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ok(r.isSupremumBoundAboveAnyClosureOf(IV0005));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV0010));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV0020));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV1020));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV1525));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV2030));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV3040));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV3545));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV4050));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV4060));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV7080));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV7090));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV5080));
		ok(r.isSupremumBoundAboveAnyClosureOf(IV1080));
		ng(r.isSupremumBoundAboveAnyClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testAboveSupremumBoundAny() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.isSupremumBoundBelowAnyClosureOf(IV0005));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV0010));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV0020));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV1020));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV1525));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV2030));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV3040));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV3545));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV4050));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV4060));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV7080));
		ok(r.isSupremumBoundBelowAnyClosureOf(IV7090));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV5080));
		ng(r.isSupremumBoundBelowAnyClosureOf(IV1080));
		ok(r.isSupremumBoundBelowAnyClosureOf(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testContactSupremumBound() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.contactSupremumBound(IV0005));
		ng(r.contactSupremumBound(IV0010));
		ng(r.contactSupremumBound(IV0020));
		ng(r.contactSupremumBound(IV1020));
		ng(r.contactSupremumBound(IV1525));
		ng(r.contactSupremumBound(IV2030));
		ng(r.contactSupremumBound(IV3040));
		ng(r.contactSupremumBound(IV3545));
		ng(r.contactSupremumBound(IV4050));
		ng(r.contactSupremumBound(IV4060));
		ng(r.contactSupremumBound(IV7080));
		ng(r.contactSupremumBound(IV7090));
		ng(r.contactSupremumBound(IV5080));
		ng(r.contactSupremumBound(IV1080));
		ok(r.contactSupremumBound(IV8090));
		ng(r.contactSupremumBound(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testCommonSupremumBoundTo() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.commonSupremumBoundTo(IV0005));
		ng(r.commonSupremumBoundTo(IV0010));
		ng(r.commonSupremumBoundTo(IV0020));
		ng(r.commonSupremumBoundTo(IV1020));
		ng(r.commonSupremumBoundTo(IV1525));
		ng(r.commonSupremumBoundTo(IV2030));
		ng(r.commonSupremumBoundTo(IV3040));
		ng(r.commonSupremumBoundTo(IV3545));
		ng(r.commonSupremumBoundTo(IV4050));
		ng(r.commonSupremumBoundTo(IV4060));
		ok(r.commonSupremumBoundTo(IV7080));
		ng(r.commonSupremumBoundTo(IV7090));
		ok(r.commonSupremumBoundTo(IV5080));
		ok(r.commonSupremumBoundTo(IV1080));
		ng(r.commonSupremumBoundTo(IV8090));
		ng(r.commonSupremumBoundTo(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testInfimumBoundIn() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.infimumBoundIn(IV0005));  ok(r.infimumBoundIn(IV0010));
		ok(r.infimumBoundIn(IV0020));  ok(r.infimumBoundIn(IV1020));
		ng(r.infimumBoundIn(IV1525));  ng(r.infimumBoundIn(IV2030));
		ng(r.infimumBoundIn(IV3040));  ng(r.infimumBoundIn(IV3545));
		ng(r.infimumBoundIn(IV4050));  ng(r.infimumBoundIn(IV4060));
		ng(r.infimumBoundIn(IV7080));  ng(r.infimumBoundIn(IV7090));
		ng(r.infimumBoundIn(IV5080));  ok(r.infimumBoundIn(IV1080));
		ng(r.infimumBoundIn(IV8090));  ng(r.infimumBoundIn(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testSupremumBoundIn() {
		Range r = Ranges.sum(Ranges.sum(IV1030, IV6080), IV5070);
		
		ng(r.supremumBoundIn(IV0005));  ng(r.supremumBoundIn(IV0010));
		ng(r.supremumBoundIn(IV0020));  ng(r.supremumBoundIn(IV1020));
		ng(r.supremumBoundIn(IV1525));  ng(r.supremumBoundIn(IV2030));
		ng(r.supremumBoundIn(IV3040));  ng(r.supremumBoundIn(IV3545));
		ng(r.supremumBoundIn(IV4050));  ng(r.supremumBoundIn(IV4060));
		ok(r.supremumBoundIn(IV7080));  ok(r.supremumBoundIn(IV7090));
		ok(r.supremumBoundIn(IV5080));  ok(r.supremumBoundIn(IV1080));
		ok(r.supremumBoundIn(IV8090));  ng(r.supremumBoundIn(IV8590));
	}
	
	/**
	 * 
	 *
	 */
	public void testClosure() {
		Range r = Ranges.sum(
				Ranges.sum(IV2030o, IV5070o), IV4060o).closure();
		
		ng(r.contains(10));  ok(r.contains(20));
		ok(r.contains(25));  ok(r.contains(30));
		ng(r.contains(35));  ok(r.contains(40));
		ok(r.contains(45));  ok(r.contains(50));
		ok(r.contains(55));  ok(r.contains(60));
		ok(r.contains(65));  ok(r.contains(70));
		ng(r.contains(90));
	}
	
	/**
	 * 
	 *
	 */
	public void testInterior() {
		Range r = Ranges.sum(
				Ranges.sum(IV2030o, IV5070o), IV4060o).interior();
		
		ng(r.contains(10));  ng(r.contains(20));
		ok(r.contains(25));  ng(r.contains(30));
		ng(r.contains(35));  ng(r.contains(40));
		ok(r.contains(45));  ok(r.contains(50));
		ok(r.contains(55));  ok(r.contains(60));
		ok(r.contains(65));  ng(r.contains(70));
		ng(r.contains(90));
	}

}
