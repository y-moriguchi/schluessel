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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * An implementation of symmetric group by an array of int.
 * <p>int型の配列により実装された対称群である。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class ArrayIntSymmetricGroup
implements IntSymmetricGroup {
	
	//
	private class Ele implements IntPermutation {
		
		//
		private int[] permutation;
		private int[] inverted;
		
		private Ele(int[] e, int[] inv) {
			permutation = e;
			inverted    = inv;
		}
		
		public int get(int i) {
			if(i <= 0 || i > cardinal) {
				throw new IndexOutOfBoundsException(i + "");
			}
			return permutation[i - 1] + 1;
		}

		public int getCardinal() {
			return cardinal;
		}

		public int getInverse(int i) {
			if(i <= 0 || i > cardinal) {
				throw new IndexOutOfBoundsException(i + "");
			}
			return inverted[i - 1] + 1;
		}

		public IntPermutation invert() {
			int[] fw = new int[cardinal];
			int[] in = new int[cardinal];
			
			for(int i = 0; i < cardinal; i++) {
				fw[i] = inverted[i];
				in[inverted[i]] = i;
			}
			return new Ele(fw, in);
		}

		public IntPermutation map(IntPermutation e) {
			int[] fw = new int[cardinal];
			int[] in = new int[cardinal];
			
			if(e == null) {
				throw new NullPointerException();
			} else if(e.getCardinal() != cardinal) {
				throw new IllegalArgumentException(
						"cardinality is not equal");
			}
			
			for(int i = 0; i < cardinal; i++) {
				fw[i] = e.get(permutation[i] + 1) - 1;
				in[e.get(permutation[i] + 1) - 1] = i;
			}
			return new Ele(fw, in);
		}
		
		public boolean isEqualTo(IntPermutation e) {
			int card = getCardinal();
			
			if(e == null) {
				throw new NullPointerException();
			} else if(card != e.getCardinal()) {
				return false;
			} else {
				for(int i = 0; i < card; i++) {
					if(permutation[i] != e.get(i + 1) - 1) {
						return false;
					}
				}
				return true;
			}
		}
		
		public boolean equals(Object o) {
			if(o instanceof Ele) {
				int[] fw2 = ((Ele)o).permutation;
				
				return Arrays.equals(permutation, fw2);
			}
			return false;
		}
		
		public int hashCode() {
			return Arrays.hashCode(permutation);
		}
		
		public String toString() {
			StringBuilder b = new StringBuilder();
			
			b.append("(");
			for(int i = 0; i < permutation.length; i++) {
				b.append(" ");
				b.append(i + 1).append("->");
				b.append(permutation[i] + 1);
			}
			b.append(" )");
			return b.toString();
		}
		
	}
	
	//
	private static Map<Integer, IntSymmetricGroup> flyweight =
		new HashMap<Integer, IntSymmetricGroup>();
	
	private int cardinal;
	private IntPermutation identity;
	
	//
	private ArrayIntSymmetricGroup(int card) {
		int[] id = new int[card];
		
		cardinal = card;
		for(int i = 0; i < card; i++) {
			id[i] = i;
		}
		identity = new Ele(id, id);
	}
	
	/**
	 * gets an instance of symmetric group
	 * whose cardinality is the given value.
	 * <p>与えられた基数の対称群を表すインスタンスを取得します。
	 * 
	 * @throws IllegalArgumentException cardinality is not positive
	 * @param card cardinality
	 */
	public static IntSymmetricGroup getInstance(int card) {
		IntSymmetricGroup res;
		
		if(card <= 0) {
			throw new IllegalArgumentException(
					"cardinality must be positive");
		}
		
		synchronized(ArrayIntSymmetricGroup.class) {
			if((res = flyweight.get(card)) == null) {
				res = new ArrayIntSymmetricGroup(card);
			}
			flyweight.put(card, res);
		}
		return res;
	}
	
	/**
	 * gets cardinality of this group.
	 * <p>この群の基数を取得します。
	 * 
	 * @see net.morilib.math.IntSymmetricGroup#getCardinal()
	 */
	public int getCardinal() {
		return cardinal;
	}

	/**
	 * gets the identity permutation of this group.
	 * <p>この群の恒等置換を取得します。
	 * 
	 * @see net.morilib.math.IntSymmetricGroup#getIdentity()
	 */
	public IntPermutation getIdentity() {
		return identity;
	}

	/**
	 * gets a permutation which maps (1, 2, ..., n)
	 * to the given arguments,
	 * the elements of the array must be unique.
	 * <p>(1, 2, ..., n)から与えられた引数で表される番号に写像する
	 * 置換を取得する。引数の要素は一意である必要がある。
	 * 
	 * @throws NullPointerException if given array is null
	 * @throws IllegalArgumentException if number of arguments is not equal to cardinality or not unique
	 * @throws IndexOutOfBoundsException if one of arguments is not from 0 to cardinality - 1
	 * @see net.morilib.math.IntSymmetricGroup#newElement(int[])
	 */
	public IntPermutation newElement(int... ps) {
		int[] fw = new int[cardinal];
		int[] in = new int[cardinal];
		
		if(ps == null) {
			throw new NullPointerException();
		} else if(ps.length != cardinal) {
			throw new IllegalArgumentException(
					"cardinality is not equal");
		}
		
		for(int i = 0; i < cardinal; i++) {
			in[i] = -1;
		}
		
		for(int i = 0; i < cardinal; i++) {
			if(ps[i] <= 0 || ps[i] > cardinal) {
				throw new IndexOutOfBoundsException(ps[i] + "");
			}
			
			fw[i] = ps[i] - 1;
			in[ps[i] - 1] = i;
		}
		
		for(int i = 0; i < cardinal; i++) {
			if(in[i] == -1) {
				throw new IllegalArgumentException(
						"argument must be unique integer");
			}
		}
		return new Ele(fw, in);
	}

	/**
	 * gets a permutation which maps the first array
	 * to the second array,
	 * the elements of each array must be unique.
	 * <p>第1引数で与えられた配列の順序から
	 * 第2引数で表される配列の順序に写像する置換を取得する。
	 * それぞれの引数の要素は一意である必要がある。
	 * 
	 * @throws NullPointerException if given array is null
	 * @throws IllegalArgumentException if number of array is not equal to cardinality or not unique
	 * @throws IndexOutOfBoundsException if one of array is not from 0 to cardinality - 1
	 * @see net.morilib.math.IntSymmetricGroup#newElement(int[], int[])
	 */
	public IntPermutation newElement(int[] i1, int[] i2) {
		int[] fw = new int[cardinal];
		int[] in = new int[cardinal];
		
		if(i1 == null || i2 == null) {
			throw new NullPointerException();
		} else if(i1.length != cardinal) {
			throw new IllegalArgumentException(
					"cardinality is not equal");
		} else if(i2.length != cardinal) {
			throw new IllegalArgumentException(
					"cardinality is not equal");
		}
		
		for(int i = 0; i < cardinal; i++) {
			fw[i] = in[i] = -1;
		}
		
		for(int i = 0; i < cardinal; i++) {
			if(i1[i] <= 0 || i1[i] > cardinal) {
				throw new IndexOutOfBoundsException(i1[i] + "");
			}
			
			fw[i1[i] - 1] = i2[i] - 1;
			in[i2[i] - 1] = i1[i] - 1;
		}
		
		for(int i = 0; i < cardinal; i++) {
			if(in[i] == -1 || fw[i] == -1) {
				throw new IllegalArgumentException(
						"argument must be unique integer");
			}
		}
		return new Ele(fw, in);
	}
	
}
