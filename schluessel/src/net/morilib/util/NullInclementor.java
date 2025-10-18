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
 * An Inclementor which has only zero.
 * <p>値として0のみをもつInclementorです。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class NullInclementor implements Inclementor<Void> {
	
	private NullInclementor() {
		// do nothing
	}
	
	/**
	 * The singleton instance of this class.
	 * <p>このクラスのsingletonインスタンスです。
	 */
	public static final NullInclementor INSTANCE =
		new NullInclementor();
	
	/**
	 * returns true if the given Inclementor is zero.
	 * <p>与えられたInclemntorが0元のときtrueを得ます。
	 * 
	 * @see net.morilib.util.Inclementor#equalIncliment(net.morilib.util.Inclementor)
	 */
	public boolean equalIncliment(Inclementor<?> i) {
		return i.isZero();
	}

	/**
	 * returns true if the given value is zero.
	 * <p>与えられた値が0のときtrueを得ます。
	 * 
	 * @see net.morilib.util.Inclementor#equalInt(int)
	 */
	public boolean equalInt(int i) {
		return i == 0;
	}

	/**
	 * always returns null.
	 * <p>常にnullを返します。
	 * 
	 * @see net.morilib.util.Inclementor#getObject()
	 */
	public Void getObject() {
		return null;
	}

	/**
	 * always returns true.
	 * <p>常にtrueを返します。
	 * 
	 * @see net.morilib.util.Inclementor#isZero()
	 */
	public boolean isZero() {
		return true;
	}

	/**
	 * always throws InclementorBoundsException.
	 * <p>常にInclementorBoundsExceptionをスローします。
	 * 
	 * @throws InclementorBoundsException
	 * @see net.morilib.util.Inclementor#suc()
	 */
	public Inclementor<Void> suc() {
		throw new InclementorBoundsException();
	}

	/**
	 * always throws InclementorBoundsException.
	 * <p>常にInclementorBoundsExceptionをスローします。
	 * 
	 * @throws InclementorBoundsException
	 * @see net.morilib.util.Inclementor#suc(int)
	 */
	public Inclementor<Void> suc(int step) {
		throw new InclementorBoundsException();
	}

	/**
	 * always returns 0.
	 * <p>常に0を返します。
	 * 
	 * @see net.morilib.util.Inclementor#toInt()
	 */
	public int toInt() {
		return 0;
	}

	/**
	 * always returns false.
	 * <p>常にfalseを返します。
	 * 
	 * @see net.morilib.util.Inclementor#hasNext()
	 */
	public boolean hasNext() {
		return false;
	}

	/**
	 * always returns this instance.
	 * <p>常に同じインスタンスを返します。
	 * 
	 * @see net.morilib.util.Inclementor#getZero()
	 */
	public Inclementor<Void> getZero() {
		return INSTANCE;
	}

	/**
	 * returns true if the given value is zero.
	 * <p>与えられた値が0のときtrueを得ます。
	 * 
	 * @see net.morilib.util.Inclementor#equalInt(java.math.BigInteger)
	 */
	public boolean equalInt(BigInteger i) {
		return i.equals(BigInteger.ZERO);
	}

}
