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
package net.morilib.util.bit;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/27
 */
public class TestPackedBitVector extends TC {

	static PackedBitVector BVEC0 = new PackedBitVector(0x00000000);
	static PackedBitVector BVEC1 = new PackedBitVector(0xf0f0f0f0);
	static PackedBitVector BVECA = new PackedBitVector(0xffffffff);
	static PackedBitVector BVEC2;

	static {
		BVEC2 = new PackedBitVector();
		BVEC2.add(false);
		BVEC2.add(true);
		BVEC2.add(false);
	}

	public void testGetBoolean() {
		eq(BVEC1.getBoolean(0), false);
		eq(BVEC1.getBoolean(4), true);
		try {
			BVEC1.getBoolean(-1);
		} catch(IndexOutOfBoundsException e) {}
		try {
			BVEC1.getBoolean(32);
		} catch(IndexOutOfBoundsException e) {}
	}

	public void testSize() {
		BitVector v = new PackedBitVector(BVEC1);

		eq(v.size(), 32);
		v.add(false);
		eq(v.size(), 33);
	}

	public void testAddV() {
		BitVector v = new PackedBitVector(BVEC1);

		v.add(false);
		eq(v.getBoolean(32), false);
		v.add(true);
		eq(v.getBoolean(33), true);
	}

	public void testAddAllV() {
		BitVector v = new PackedBitVector(BVEC1);

		v.addAllBoolean(BVEC2);
		eq(v.getBoolean(32), false);
		eq(v.getBoolean(33), true);
		eq(v.getBoolean(34), false);
	}

	public void testClear() {
		BitVector v = new PackedBitVector(BVEC1);

		v.clear();
		ok(v.isEmpty());
	}

	public void testAny() {
		eq(BVEC0.any(true), false);
		eq(BVEC1.any(true), true);
		eq(BVECA.any(true), true);
	}

	public void testEvery() {
		eq(BVEC0.every(true), false);
		eq(BVEC1.every(true), false);
		eq(BVECA.every(true), true);
	}

	public void testToBooleanArray() {
		BitVector v = new PackedBitVector();
		boolean[] a;

		v.add(true);  v.add(false);  v.add(true);
		a = v.toBooleanArray();
		eq(a[0], true);  eq(a[1], false);  eq(a[2], true);
		a = v.toBooleanArray(new boolean[5]);
		eq(a[0], true);  eq(a[1], false);  eq(a[2], true);
	}

	public void testAddI() {
		BitVector v = new PackedBitVector(BVECA);

		v.add(0, false);
		v.add(0, true);
		eq(v.getBoolean(0), true);
		eq(v.getBoolean(1), false);
		for(int i = 2; i < 34; i++) {
			eq(v.getBoolean(i), true);
		}
	}

	public void testAddAllBooleanI() {
		BitVector v = new PackedBitVector(BVECA);

		v.addAllBoolean(0, BVEC0);
		v.add(0, true);
		eq(v.getBoolean(0), true);
		for(int i = 1; i < 33; i++) {
			eq(v.getBoolean(i), false);
		}
		for(int i = 33; i < 65; i++) {
			eq(v.getBoolean(i), true);
		}
	}

	public void testIndexOf() {
		eq(BVEC1.indexOf(true), 4);
	}

	public void testLastIndexOf() {
		eq(BVEC1.lastIndexOf(false), 27);
	}

	public void testBitVectorIterator() {
		BitVector v = new PackedBitVector();
		BitVectorIterator i;

		v.add(true);  v.add(false);  v.add(true);
		i = v.bitVectorIterator();
		eq(i.hasNext(), true);
		eq(i.nextIndex(), 0);
		eq(i.next(), true);
		eq(i.next(), false);
		eq(i.next(), true);
		eq(i.nextIndex(), 3);
		eq(i.hasNext(), false);

		i = v.bitVectorIterator();
		eq(i.hasPrevious(), true);
		eq(i.previousIndex(), 2);
		eq(i.previous(), true);
		eq(i.previous(), false);
		eq(i.previous(), true);
		eq(i.previousIndex(), -1);
		eq(i.hasPrevious(), false);

		i = v.bitVectorIterator();
		i.add(true);
		i.next();  i.previous();  i.set(false);
		i.next();  i.remove();
		eq(v.getBoolean(0), true);
		eq(v.getBoolean(1), false);
		eq(v.getBoolean(2), true);
	}

	public void testRemoveAt() {
		BitVector v = new PackedBitVector(BVECA);

		v.add(0, false);
		eq(v.removeAt(0), false);
		eq(v.getBoolean(31), true);
		eq(v.removeAt(0), true);
		eq(v.getBoolean(30), true);
	}

	public void testRemoveAt2() {
		BitVector v = new PackedBitVector();

		v.add(true);  v.add(false);  v.add(false);  v.add(true);
		v.removeAt(2);
		eq(v.getBoolean(0), true);
		eq(v.getBoolean(1), false);
		eq(v.getBoolean(2), true);
	}

	public void testSet() {
		BitVector v = new PackedBitVector(BVECA);

		v.set(0, false);
		eq(v.removeAt(0), false);
	}

	public void testSubVector() {
		BitVector v = new PackedBitVector(BVEC1), w;

		w = v.subVector(3, 7);
		eq(w.getBoolean(0), false);
		eq(w.getBoolean(1), true);
		eq(w.size(), 4);
		try {
			w.getBoolean(-1);
		} catch(IndexOutOfBoundsException e) {}
		try {
			w.getBoolean(4);
		} catch(IndexOutOfBoundsException e) {}
		w.removeAt(0);
		w.add(3, false);
		w.set(1, false);
		eq(v.getBoolean(3), true);
		eq(v.getBoolean(4), false);
		eq(v.getBoolean(5), true);
		eq(v.getBoolean(6), false);
	}

	public void testContains() {
		eq(BVEC1.contains(true), true);
		eq(BVEC1.contains(null), false);
	}

	public void testToArray() {
		BitVector v = new PackedBitVector();
		Boolean[] a;

		v.add(true);  v.add(false);  v.add(true);
		a = v.toArray(new Boolean[0]);
		eq(a[0], Boolean.TRUE);
		eq(a[1], Boolean.FALSE);
		eq(a[2], Boolean.TRUE);
		a = v.toArray(new Boolean[5]);
		eq(a[0], Boolean.TRUE);
		eq(a[1], Boolean.FALSE);
		eq(a[2], Boolean.TRUE);
	}

	public void testRemove() {
		PackedBitVector v = new PackedBitVector(BVEC1);

		v.remove(false);
		eq(v.size(), 16);
		eq(v.every(true), true);
	}

	public void testContainsAll() {
		ok(BVEC1.containsAll(BVEC1));
		ok(BVEC1.containsAll(BVEC0));
		ok(BVEC1.containsAll(BVECA));
		ng(BVEC0.containsAll(BVECA));
		ng(BVECA.containsAll(BVEC0));
		ng(BVEC1.containsAll(new PackedBitVector()));
	}

	public void testAddAllI() {
		BitVector v = new PackedBitVector(BVECA);

		v.addAll(0, BVEC0);
		v.add(0, true);
		eq(v.getBoolean(0), true);
		for(int i = 1; i < 33; i++) {
			eq(v.getBoolean(i), false);
		}
		for(int i = 33; i < 65; i++) {
			eq(v.getBoolean(i), true);
		}
	}

	public void testRemoveAll() {
		PackedBitVector v;

		v = new PackedBitVector(BVEC1);
		v.removeAll(BVEC1);
		ok(v.isEmpty());
		v = new PackedBitVector(BVEC1);
		v.removeAll(BVEC0);
		eq(v.size(), 16);
		eq(v.every(true), true);
		v = new PackedBitVector(BVEC1);
		v.removeAll(BVECA);
		eq(v.size(), 16);
		eq(v.every(false), true);
		v = new PackedBitVector(BVEC1);
		v.removeAll(new PackedBitVector());
		eq(v, BVEC1);
	}

	public void testRetainAll() {
		BitVector v;

		v = new PackedBitVector(BVEC1);
		v.retainAll(BVEC1);
		eq(v, BVEC1);
		v.retainAll(BVEC0);
		eq(v.size(), 16);
		eq(v.every(false), true);
		v = new PackedBitVector(BVEC1);
		v.retainAll(BVECA);
		eq(v.size(), 16);
		eq(v.every(true), true);
		v = new PackedBitVector(BVEC1);
		v.retainAll(new PackedBitVector());
		ok(v.isEmpty());
		v = new PackedBitVector(BVEC1);
	}

	public void testNegate() {
		PackedBitVector v = new PackedBitVector(BVEC1);

		v.add(false);  v.add(true);  v.add(false);
		v.negate();
		eq(v.vector.getInt(0), 0x0f0f0f0f);
		eq(v.vector.getInt(1), 0x00000005);
	}

	public void testEquals() {
		BitVector v = new PackedBitVector(BVEC1);
		BitVector w = new PackedBitVector(BVEC1);

		ok(v.equals(BVEC1));
		ok(BVEC1.equals(v));
		ok(v.equals(w));
		ng(v.equals(null));
		ng(v.equals(BVEC0));
	}

	public void testHashCode() {
		BitVector v = new PackedBitVector(BVEC1);
		BitVector w = new PackedBitVector(BVEC1);

		eq(v.hashCode(), BVEC1.hashCode());
		eq(v.hashCode(), w.hashCode());
	}

}
