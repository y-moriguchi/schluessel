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
package net.morilib.lisp.array;

import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public interface ILispArray {

	/**
	 * @return
	 */
	public int rank();

	/**
	 * @param dim
	 * @return
	 */
	public int startIndex(int dim);

	/**
	 * @param dim
	 * @return
	 */
	public int endIndex(int dim);

	/**
	 * @return
	 */
	public LispVector toVector();

	/**
	 * @param i
	 * @return
	 */
	public Datum getFromArray(int... is);

	/**
	 * @param d
	 * @param i
	 * @return
	 */
	public void setToArray(Datum d, int... is);

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isIndexEqualTo(ILispArray a);

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isEqualTo(ILispArray a);

	/**
	 * 
	 * @return
	 */
	public String getTypeSpecifier();

	/**
	 * 
	 * @param itr
	 */
	public void fill(Iterator<Datum> itr);

	/**
	 * 
	 * @return
	 */
	public LispArrayShape getShape();

}
