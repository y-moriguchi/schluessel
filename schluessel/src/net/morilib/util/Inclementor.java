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
 * Representation of natural numbers by Peano System.
 * <p>ペアノの公理系による自然数を表現したクラスである。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public interface Inclementor<A> {
	
	/**
	 * returns true if this instance represents &quot;zero&quot;.
	 * <p>インスタンスが「ゼロ」を表現するときにtrueを得る。
	 */
	public boolean isZero();
	
	/**
	 * returns the successor of this instance.
	 * <p>このインスタンスの「次者」を取得する。
	 * 
	 * @throws InclementorBoundsException
	 * @return the successor
	 */
	public Inclementor<A> suc();
	
	/**
	 * gets the n's successor of this instance.
	 * <p>このインスタンスのn個先の「次者」を取得する。
	 * 
	 * @throws InclementorBoundsException
	 */
	public Inclementor<A> suc(int step);
	
	/**
	 * returns the &quot;int value&quot; representation
	 * of this number.
	 * <p>この「数」のint型による表現を取得する。
	 */
	public int toInt();
	
	/**
	 * returns true if the &quot;int value&quot; representation
	 * of this number is equal to the given int.
	 * <p>このインスタンスのintによる表現が与えられたintと等しいとき
	 * trueを得る。
	 * 
	 * @param i value to be tested
	 */
	public boolean equalInt(int i);
	
	/**
	 * returns true if the &quot;BigInteger value&quot; representation
	 * of this number is equal to the given BigInteger.
	 * <p>このインスタンスのBigIntegerによる表現が
	 * 与えられたBigIntegerと等しいときtrueを得る。
	 * 
	 * @param i value to be tested
	 */
	public boolean equalInt(BigInteger i);
	
	/**
	 * returns true if this number is equal to the given number.
	 * <p>与えられた「自然数」とこれが等しいときにtrueを得る。
	 * 
	 * @param i number to be tested
	 */
	public boolean equalIncliment(Inclementor<?> i);
	
	/**
	 * gets the object which represents this number.
	 * <p>数を表現するオブジェクトをえる。
	 */
	public A getObject();
	
	/**
	 * return true if successor is.
	 * <p>「次者」が存在するときtrueを得る。
	 */
	public boolean hasNext();
	
	/**
	 * gets zero of this number system.
	 * <p>この自然数系の「ゼロ」を取得する。
	 */
	public Inclementor<A> getZero();
	
}
