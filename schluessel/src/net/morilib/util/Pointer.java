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
package net.morilib.util;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/27
 */
public interface Pointer<R, A>
extends Cloneable, Comparable<Pointer<R, A>> {

	/**
	 * 
	 * @return
	 */
	public Pointer<R, A> inclement();

	/**
	 * 
	 * @return
	 */
	public Pointer<R, A> declement();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Pointer<R, A> add(int x);

	/**
	 * 
	 * @return
	 */
	public boolean isValid();

	/**
	 * 
	 * @return
	 */
	public boolean isValid(int x);

	/**
	 * 
	 * @return
	 */
	public A refer();

	/**
	 * 
	 * @return
	 */
	public A referSafe();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public A indexOf(int x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public A indexOfSafe(int x);

	/**
	 * 
	 * @param p
	 * @return
	 */
	public int subtract(Pointer<R, A> p);

	/**
	 * 
	 * @return
	 */
	public int getIndex();

	/**
	 * 
	 * @return
	 */
	public Pointer<R, A> clone();

	/**
	 * 
	 * @param p
	 * @return
	 */
	public Pointer<R, A> overwrite(Pointer<R, A> p);

	/**
	 * 
	 * @return
	 */
	public R getReferent();

	/**
	 * 
	 * @return
	 */
	public Pointer<R, A> moveStart();

	/**
	 * 
	 * @return
	 */
	public Pointer<R, A> moveEnd();

}
