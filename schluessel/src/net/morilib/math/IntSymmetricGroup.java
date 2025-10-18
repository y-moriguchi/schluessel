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

/**
 * A symmetric group of int values.
 * <p>int型による対称群である。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public interface IntSymmetricGroup {
	
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
	 */
	public IntPermutation newElement(int... ps);
	
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
	 */
	public IntPermutation newElement(int[] i1, int[] i2);
	
	/**
	 * gets the identity permutation of this group.
	 * <p>この群の恒等置換を取得します。
	 */
	public IntPermutation getIdentity();
	
	/**
	 * gets cardinality of this group.
	 * <p>この群の基数を取得します。
	 */
	public int getCardinal();
	
}
