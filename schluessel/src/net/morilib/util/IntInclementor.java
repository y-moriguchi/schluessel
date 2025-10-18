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

import java.math.BigInteger;

/**
 * An Inclementor by int value.
 * <p>int型の値によるInclementorである。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class IntInclementor implements Inclementor<Integer> {
	
	//
	private int index;
	
	/**
	 * constructs an Inclementor.
	 * <p>Inclementorを生成する。
	 */
	public IntInclementor() {
		index = 0;
	}
	
	/**
	 * constructs an Inclementor which begins from i.
	 * <p>iから始まるInclementorを生成する。
	 * 
	 * @param i an int value from which Inclementor begins
	 */
	public IntInclementor(int i) {
		index = i;
	}

	/**
	 * returns true if this instance represents &quot;zero&quot;.
	 * <p>インスタンスが「ゼロ」を表現するときにtrueを得る。
	 * 
	 * @see org.usei.math.Inclimental#isZero()
	 */
	public boolean isZero() {
		return index == 0;
	}

	/**
	 * returns the successor of this instance.
	 * <p>このインスタンスの「次者」を取得する。
	 * 
	 * @see org.usei.math.Inclimental#suc()
	 */
	public Inclementor<Integer> suc() {
		if(++index > Integer.MAX_VALUE) {
			throw new InclementorBoundsException();
		}
		return this;
	}

	/**
	 * gets the n's successor of this instance.
	 * <p>このインスタンスのn個先の「次者」を取得する。
	 * 
	 * @see org.usei.math.Inclimental#suc(int)
	 */
	public Inclementor<Integer> suc(int step) {
		if((index += step) > Integer.MAX_VALUE) {
			throw new InclementorBoundsException();
		}
		return this;
	}

	/**
	 * returns true if this object is equal to the given object.
	 * <p>与えられたオブジェクトと等しいときtrueとなる。
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof IntInclementor) {
			return index == ((IntInclementor)o).index;
		}
		return false;
	}
	
	/**
	 * returns hash code of this object.
	 * <p>ハッシュコードを得る。
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return index;
	}

	/**
	 * returns true if this number is equal to the given number.
	 * <p>与えられた「自然数」とこれが等しいときにtrueを得る。
	 * 
	 * @see net.morilib.util.Inclementor#equalIncliment(net.morilib.util.Inclementor)
	 */
	public boolean equalIncliment(Inclementor<?> i) {
		return index == ((Inclementor<?>)i).toInt();
	}

	/**
	 * returns true if the &quot;int value&quot; representation
	 * of this number is equal to the given int.
	 * <p>このインスタンスのintによる表現が与えられたintと等しいとき
	 * trueを得る。
	 * 
	 * @see net.morilib.util.Inclementor#equalInt(int)
	 */
	public boolean equalInt(int i) {
		return index == i;
	}

	/**
	 * returns the &quot;int value&quot; representation
	 * of this number.
	 * <p>この「数」のint型による表現を取得する。
	 * 
	 * @see net.morilib.util.Inclementor#toInt()
	 */
	public int toInt() {
		return index;
	}

	/**
	 * gets the object which represents this number.
	 * <p>数を表現するオブジェクトをえる。
	 * 
	 * @see net.morilib.util.Inclementor#getObject()
	 */
	public Integer getObject() {
		return Integer.valueOf(index);
	}

	/**
	 * return true if successor is.
	 * <p>「次者」が存在するときtrueを得る。
	 * 
	 * @see net.morilib.util.Inclementor#hasNext()
	 */
	public boolean hasNext() {
		return index < Integer.MAX_VALUE;
	}

	/**
	 * gets zero of this number system.
	 * <p>この自然数系の「ゼロ」を取得する。
	 * 
	 * @see net.morilib.util.Inclementor#getZero()
	 */
	public Inclementor<Integer> getZero() {
		return new IntInclementor(0);
	}

	/**
	 * returns true if the &quot;BigInteger value&quot; representation
	 * of this number is equal to the given BigInteger.
	 * <p>このインスタンスのBigIntegerによる表現が
	 * 与えられたBigIntegerと等しいときtrueを得る。
	 * 
	 * @see net.morilib.util.Inclementor#equalInt(java.math.BigInteger)
	 */
	public boolean equalInt(BigInteger i) {
		return i.equals(BigInteger.valueOf(index));
	}

}
