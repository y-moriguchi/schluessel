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
 * An interface represents permutations which map int values.
 * <p>int型の値を扱う置換を表現するインターフェースです。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public interface IntPermutation extends GroupElement<IntPermutation> {

	/**
	 * maps the given int.
	 * <p>与えられたint値を写像します。
	 * 
	 * @param i an int value to be mapped
	 */
	public int get(int i);

	/**
	 * maps the given int inversely.
	 * <p>与えられたint値の逆写像を得ます。
	 * 
	 * @param i an int value to be mapped
	 */
	public int getInverse(int i);

	/**
	 * get the cardinality.
	 * <p>この置換の基数を得ます。
	 */
	public int getCardinal();

}
