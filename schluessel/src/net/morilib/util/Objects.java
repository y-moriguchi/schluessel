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
package net.morilib.util;

import java.util.Comparator;

import net.morilib.range.Limit;

/**
 * <i>USEful Implements</i> for general objects.<br>
 * <p>オブジェクトに関する便利な関数である.
 * 
 * @author MORIGUCHI, Yuichiro 2004/12/11
 */
public final class Objects {
	
	/**
	 * An empty array of Object.
	 * <p>空のObject型配列である.
	 */
	public static final Object[] OBJECT_EMPTY = new Object[0];
	
	
	/**
	 * Returns the second argument if the first argument is null,
	 * otherwise returns the first one.<br>
	 * <p>第1引数がnullのときは第2引数、そうでないときは第1引数を返す.
	 * 
	 * @param ifnull  return value in case that o is null
	 * @return return ifnull if o is null, otherwise returns o
	 */
	public static /*inline*/ Object nvl(Object o, Object ifnull) {
		return (o != null) ? o : ifnull;
	}

	/**
	 * Returns true if the given objects are equal.<br>
	 * <p>与えられた引数が等しいときにtrueを得る.
	 * 
	 * @return  true if obj1 equals obj2, otherwise false
	 */
	public static /*inline*/ boolean equals(Object obj1, Object obj2) {
		return (obj1 == null) ? obj2 == null : obj1.equals(obj2);
	}
	
	/**
	 * Transform an object to its string representation:
	 * If the object is null, returns an empty string.<br>
	 * <p>与えられたオブジェクトの文字列表現を返す.オブジェクトがnullの
	 * ときは空文字列を返す.
	 * 
	 * @param attr  an object to transform
	 * @return  string representation
	 * @see  java.lang.Object
	 */
	public static /*inline*/ String toString(Object attr) {
		return (attr == null) ? "" : attr.toString();
	}

	/**
	 * gets the maximum value of given arguments.
	 * <p>引数のうち最大のオブジェクトを返す.
	 * 
	 * @return  the maximum value of the given arguments
	 */
	public static<T extends Comparable<? super T>> T max(T i1, T i2) {
		return (i1.compareTo(i2) > 0) ? i1 : i2;
	}

	/**
	 * gets the minimum value of the arguments.
	 * <p>引数のうち最小のオブジェクトを返す.
	 * 
	 * @return  the minimum value of the given arguments
	 */
	public static<T extends Comparable<? super T>> T min(T i1, T i2) {
		return (i1.compareTo(i2) > 0) ? i2 : i1;
	}

	/**
	 * gets the maximum value of given arguments.
	 * <p>引数のうち最大のオブジェクトを返す.
	 * 
	 * @return  the maximum value of the given arguments
	 */
	/*public static Object max(Object[] objs) {
		Object res = null;
		
		for(int i = 0; i < objs.length; i++) {
			res = maxNull(res, objs[i]);
		}
		return res;
	}*/

	/**
	 * get the minimum value of the arguments.
	 * <p>引数のうち最小のオブジェクトを返す.
	 * 
	 * @return  the minimum value of the given arguments
	 */
	/*public static Object min(Object[] objs) {
		Object res = null;
		
		for(int i = 0; i < objs.length; i++) {
			res = minNull(res, objs[i]);
		}
		return res;
	}*/

	/**
	 * gets the maximum value of the given arguments.
	 * <p>引数のうち最大のオブジェクトを返す.
	 * 
	 * @return  the maximum value of the given arguments
	 */
	public static<T> T max(T i1, T i2, Comparator<T> cp) {
		return (compare(i1, i2, cp) > 0) ? i1 : i2;
	}

	/**
	 * get the minimum value of the arguments.
	 * <p>引数のうち最小のオブジェクトを返す.
	 * 
	 * @return  the minimum value of the given arguments
	 */
	public static<T> T min(T i1, T i2, Comparator<T> cp) {
		return (compare(i1, i2, cp) > 0) ? i2 : i1;
	}

	/**
	 * get the maximum value of the given arguments.
	 * <p>引数のうち最大のオブジェクトを返す.nullは「最小の値」と解釈される.
	 * 
	 * @return  the maximum value of the given arguments
	 */
	public static<T extends Comparable<? super T>> T maxNull(
			T i1, T i2) {
		return (i1 == null) ? i2 : ((i2 == null) ? i1 : max(i1, i2));
	}

	/**
	 * get the minimum value of the arguments.
	 * <p>引数のうち最小のオブジェクトを返す.nullは「最大の値」と解釈される.
	 * 
	 * @return  the minimum value of the given arguments
	 */
	public static<T extends Comparable<? super T>> T minNull(
			T i1, T i2) {
		return (i1 == null) ? i2 : ((i2 == null) ? i1 : min(i1, i2));
	}

	/**
	 * compares the given objects each other with the given Comparator.
	 * <p>与えられたComparatorを使用してオブジェクトを比較する.
	 * 
	 * @param c  a comparator
	 * @see java.lang.Comparable
	 * @see java.util.Comparator
	 */
	public static<T> int compare(T o, T p, Comparator<T> c) {
		//return (c == null) ? compare(o, p) : c.compare(o, p);
		return c.compare(o, p);
	}
	
	/**
	 * returns true if the first argument is between the second
	 * argument and the third.
	 * <p>第1の引数が第2の引数と第3の引数の間にあればtrueとする.
	 * 
	 * @param src   the first argument
	 * @param from  the second argument
	 * @param to    the third argument
	 */
	public static boolean betweenBound(
			Object src, Object from, Object to) {
		return (Limit.compareBound(src, from) >= 0 &&
				Limit.compareBound(src, to)   <= 0);
	}
	
	/**
	 * returns true if the first argument is between the second
	 * argument and the third; the given comparator is used for compare.
	 * <p>第1の引数が第2の引数と第3の引数の間にあればtrueとする.
	 * 比較には引数のComparatorを使用する.
	 * 
	 * @param src   the first argument
	 * @param from  the second argument
	 * @param to    the third argument
	 * @param comp  the comparator to be used
	 */
	public static<T> boolean between(
			T src, T from, T to, Comparator<T> comp) {
		return (compare(src, from, comp) >= 0 &&
				compare(src, to,   comp) <= 0);
	}
	
	/**
	 * returns string representation of hash code
	 * of the given object.
	 * <p>与えられたオブジェクトのハッシュ値の文字列表現を得ます。
	 * 
	 * @return string representation of hash code
	 */
	public static String printHash(Object o) {
		return (o == null) ? "0" : Integer.toString(o.hashCode(), 16);
	}
	
}
