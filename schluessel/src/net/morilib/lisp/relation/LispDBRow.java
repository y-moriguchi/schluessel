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
package net.morilib.lisp.relation;

import net.morilib.lisp.Datum;
import net.morilib.relation.DBRow;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/06
 */
public abstract class LispDBRow extends Datum implements DBRow<Datum> {

	/**
	 * 
	 * @param column
	 * @return
	 */
	public abstract Datum get(
			Object column) throws LispRelationException;

	/**
	 * 
	 * @param column
	 * @param value
	 */
	public abstract void set(Object column,
			Datum value) throws LispRelationException;

	/**
	 * 
	 * @return
	 * @throws LispRelationException
	 */
	public abstract Datum getByList() throws LispRelationException;

}
