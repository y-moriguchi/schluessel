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
package net.morilib.lisp.r6rs.hash;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/21
 */
public interface ILispR6RSHashtable {

	/**
	 * 
	 * @return
	 */
	public int size();

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum get(Datum k);

	/**
	 * 
	 * @param k
	 * @param v
	 */
	public void setBang(Datum k, Datum v);

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum remove(Datum k);

	/**
	 * 
	 * @param k
	 * @return
	 */
	public boolean containsKey(Datum k);

	/**
	 * 
	 * @return
	 */
	public ILispR6RSHashtable duplicate();

	/**
	 * 
	 * @return
	 */
	public void clearBang(int k);

	/**
	 * 
	 * @return
	 */
	public LispVector keysToVector();

	/**
	 * 
	 * @return
	 */
	public LispVector valuesToVector();

	/**
	 * 
	 * @return
	 */
	public LispVector[] entriesToVector();

	/**
	 * 
	 * @return
	 */
	public Procedure keyEquivalence();

	/**
	 * 
	 * @return
	 */
	public Procedure hashFunction();

	/**
	 * 
	 * @return
	 */
	public boolean isMutableHashtable();

}
