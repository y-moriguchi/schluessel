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
package net.morilib.math;

import net.morilib.lisp.test.TC;

public class ArrayIntSymmetricGroupTest extends TC {
	
	public void testGetCardinal() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		
		eq(g.getCardinal(), 4);
	}
	
	public void testGetIdentity() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntPermutation e = g.getIdentity();
		IntPermutation x = g.newElement(2, 3, 4, 1);
		
		for(int i = 0; i < e.getCardinal(); i++) {
			eq(e.get(i + 1), i + 1);
			eq(e.getInverse(i + 1), i + 1);
		}
		ok(e.isEqualTo(e));
		ok(e.invert().isEqualTo(e));
		ok(x.map(e).isEqualTo(x));
		ok(e.map(x).isEqualTo(x));
	}
	
	public void testNewElementI() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		
		try {
			g.newElement(1);
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			g.newElement(null);
			fail();
		} catch(NullPointerException e) {}
		
		try {
			g.newElement(1, 2, 5, 4);
			fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			g.newElement(1, 2, 2, 4);
			fail();
		} catch(IllegalArgumentException e) {}
		
		g.newElement(1, 3, 2, 4);
	}
	
	public void testNewElementII() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		
		try {
			g.newElement(new int[] { 1 }, new int[] { 1, 2, 3, 4 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			g.newElement(new int[] { 1, 2, 3, 4 }, new int[] { 1 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			g.newElement(new int[] { 1, 2, 3, 4 }, null);  fail();
		} catch(NullPointerException e) {}
		
		try {
			g.newElement(null, new int[] { 1, 2, 3, 4 });  fail();
		} catch(NullPointerException e) {}
		
		try {
			g.newElement(
					new int[] { 1, 2, 5, 4 },
					new int[] { 1, 2, 3, 4 });
			fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			g.newElement(
					new int[] { 1, 2, 3, 4 },
					new int[] { 1, 2, 0, 4 });
			fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			g.newElement(
					new int[] { 1, 2, 2, 4 },
					new int[] { 1, 2, 3, 4 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		try {
			g.newElement(
					new int[] { 1, 2, 3, 4 },
					new int[] { 1, 2, 2, 4 });
			fail();
		} catch(IllegalArgumentException e) {}
		
		IntPermutation x = g.newElement(
				new int[] { 2, 3, 4, 1 },
				new int[] { 3, 4, 1, 2 });
		eq(x.get(1), 2);  eq(x.get(2), 3);
		eq(x.get(3), 4);  eq(x.get(4), 1);
	}
	
	public void testEleGet() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		
		eq(x.get(1), 2);  eq(x.get(2), 3);
		eq(x.get(3), 4);  eq(x.get(4), 1);
		
		try {
			x.get(0);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			x.get(5);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testEleGetInverse() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		
		eq(x.getInverse(1), 4);  eq(x.getInverse(2), 1);
		eq(x.getInverse(3), 2);  eq(x.getInverse(4), 3);
		
		try {
			x.getInverse(0);  fail();
		} catch(IndexOutOfBoundsException e) {}
		
		try {
			x.getInverse(5);  fail();
		} catch(IndexOutOfBoundsException e) {}
	}
	
	public void testEleInverse() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		IntPermutation y = g.newElement(4, 1, 2, 3);
		
		eq(x.invert(), y);
		eq(x.invert().invert(), x);
	}
	
	public void testEleOp() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntSymmetricGroup h = ArrayIntSymmetricGroup.getInstance(2);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		IntPermutation y = g.newElement(3, 4, 1, 2);
		IntPermutation z = g.newElement(4, 1, 2, 3);
		
		eq(x.map(y), z);  eq(y.map(z), x);
		eq(x.map(x.invert()), g.getIdentity());
		eq(x.invert().map(x), g.getIdentity());
		
		try {
			x.map(null);  fail();
		} catch(NullPointerException e) {}
		
		try {
			x.map(h.getIdentity());  fail();
		} catch(IllegalArgumentException e) {}
	}
	
	public void testEleIsEqualTo() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntSymmetricGroup h = ArrayIntSymmetricGroup.getInstance(2);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		IntPermutation y = g.newElement(3, 4, 1, 2);
		IntPermutation z = g.newElement(4, 1, 2, 3);
		IntPermutation w = g.newElement(2, 3, 4, 1);
		IntPermutation v = g.newElement(2, 3, 4, 1);
		
		ok(x.map(y).isEqualTo(z));  ok(y.map(z).isEqualTo(x));
		ok(x.map(x.invert()).isEqualTo(g.getIdentity()));
		ok(x.invert().map(x).isEqualTo(g.getIdentity()));
		ok(x.isEqualTo(x));
		ok(x.isEqualTo(v));  ok(x.isEqualTo(w));  ok(v.isEqualTo(w));
		ng(x.isEqualTo(z));
		ng(x.isEqualTo(h.getIdentity()));
		
		try {
			x.map(null);  fail();
		} catch(NullPointerException e) {}
	}
	
	public void testEleEquals() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntSymmetricGroup h = ArrayIntSymmetricGroup.getInstance(2);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		IntPermutation y = g.newElement(3, 4, 1, 2);
		IntPermutation z = g.newElement(4, 1, 2, 3);
		IntPermutation w = g.newElement(2, 3, 4, 1);
		IntPermutation v = g.newElement(2, 3, 4, 1);
		
		ok(x.map(y).equals(z));  ok(y.map(z).equals(x));
		ok(x.map(x.invert()).equals(g.getIdentity()));
		ok(x.invert().map(x).equals(g.getIdentity()));
		ok(x.equals(x));
		ok(x.equals(v));  ok(x.equals(w));  ok(v.equals(w));
		ng(x.equals(z));
		ng(x.equals(h.getIdentity()));
		ng(x.equals(null));
		ng(x.equals(Integer.valueOf(1)));
	}
	
	public void testEleHashCode() {
		IntSymmetricGroup g = ArrayIntSymmetricGroup.getInstance(4);
		IntPermutation x = g.newElement(2, 3, 4, 1);
		IntPermutation y = g.newElement(3, 4, 1, 2);
		IntPermutation z = g.newElement(4, 1, 2, 3);
		
		eq(x.map(y).hashCode(), z.hashCode());
		eq(y.map(z).hashCode(), x.hashCode());
		eq(x.map(x.invert()).hashCode(), g.getIdentity().hashCode());
		eq(x.invert().map(x).hashCode(), g.getIdentity().hashCode());
		eq(x.hashCode(), x.hashCode());
	}
	
}
