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
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/21
 */
public class LispImmutableR6RSHashtable extends Datum2
implements ILispR6RSHashtable {

	//
	private ILispR6RSHashtable wrapee;

	/**
	 * 
	 * @param wrapee
	 */
	public LispImmutableR6RSHashtable(ILispR6RSHashtable wrapee) {
		this.wrapee = wrapee;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#size()
	 */
	@Override
	public int size() {
		return wrapee.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#get(net.morilib.lisp.Datum)
	 */
	@Override
	public Datum get(Datum k) {
		return wrapee.get(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#setBang(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public void setBang(Datum k, Datum v) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#remove(net.morilib.lisp.Datum)
	 */
	@Override
	public Datum remove(Datum k) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#containsKey(net.morilib.lisp.Datum)
	 */
	@Override
	public boolean containsKey(Datum k) {
		return wrapee.containsKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<immutable-hashtable>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#duplicate()
	 */
	@Override
	public ILispR6RSHashtable duplicate() {
		return new LispImmutableR6RSHashtable(wrapee.duplicate());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#clear()
	 */
	@Override
	public void clearBang(int k) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#keysToVector()
	 */
	@Override
	public LispVector keysToVector() {
		return wrapee.keysToVector();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#valuesToVector()
	 */
	@Override
	public LispVector valuesToVector() {
		return wrapee.valuesToVector();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#entriesToVector()
	 */
	@Override
	public LispVector[] entriesToVector() {
		return new LispVector[] {
				keysToVector(), valuesToVector()
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#keyEquivalence()
	 */
	@Override
	public Procedure keyEquivalence() {
		return wrapee.keyEquivalence();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#hashFunction()
	 */
	@Override
	public Procedure hashFunction() {
		return wrapee.hashFunction();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#isMutableHashtable()
	 */
	@Override
	public boolean isMutableHashtable() {
		return false;
	}

}
