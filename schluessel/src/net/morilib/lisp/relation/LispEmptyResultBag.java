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
import net.morilib.relation.RelationException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/02
 */
public class LispEmptyResultBag extends LispResultBag {

	/**
	 * 
	 */
	public static final LispEmptyResultBag INSTANCE =
		new LispEmptyResultBag();

	//
	private LispEmptyResultBag() {}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#referRow(int)
	 */
	@Override
	public DBRow<Datum> referRow(int row) throws RelationException {
		throw new LispRelationException("empty set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#isDeletable()
	 */
	@Override
	public boolean isDeletable() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#deleteRow(int)
	 */
	@Override
	public void deleteRow(int row) throws RelationException {
		throw new LispRelationException("empty set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#isInsertable()
	 */
	@Override
	public boolean isInsertable() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#insertRow(T[])
	 */
	@Override
	public void insertRow(Datum... objects) throws RelationException {
		throw new LispRelationException("empty set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#getRow()
	 */
	@Override
	public LispDBRow getRow() throws LispRelationException {
		throw new LispRelationException("empty set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.ResultBag#rest()
	 */
	@Override
	public LispResultBag rest() throws LispRelationException {
		throw new LispRelationException("empty set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.relation.LispResultBag#isAfterLast()
	 */
	@Override
	public boolean isAfterLast() throws LispRelationException {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<empty-result-bag>");
	}

}
