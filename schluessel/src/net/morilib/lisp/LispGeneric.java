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
package net.morilib.lisp;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.morilib.lisp.sos.LispClass;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.sos.LispTypeList;
import net.morilib.lisp.subr.BinaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class LispGeneric extends Settable implements Procedure {

	//
	/*package*/ static final String APPLY_GENERIC_STR =
		"apply-generic";

	//
	/*package*/ static final Symbol APPLY_GENERIC =
		Symbol.getSymbol(APPLY_GENERIC_STR);

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010
	 */
	public static final class Remove extends BinaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			List<LispType> lst = new ArrayList<LispType>();
			ConsIterator   it  = new ConsIterator(c2a);

			if(!(c1a instanceof LispGeneric)) {
				throw mesg.getError("err.require.generic");
			}

			while(it.hasNext()) {
				Datum d = it.next();

				if(d instanceof LispClass) {
					lst.add(((LispClass)d).getObjectType());
				} else {
					throw mesg.getError("err.require.class");
				}
			}
			if(it.getTerminal() != Nil.NIL) {
				throw mesg.getError("err.list");
			}

			((LispGeneric)c1a).remove(new LispTypeList(lst));
			return c1a;
		}

	}

	//
	private class NoNext extends LispNextMethod {

		private Datum prms;

		private NoNext(Datum prms) {
			this.prms = prms;
		}

		@Override
		public Closure get() {
			return null;
		}

		@Override
		Datum getDefaultParams() {
			return prms;
		}

		@Override
		LispGeneric getMethod() {
			return LispGeneric.this;
		}

		@Override
		public LispNextMethod getNextMethod() {
			return this;
		}

	}

	//
	private MethodMap mmap = new MethodMap();
	private LispType  typ  = LispType.GENERIC;

	//
	/*package*/ LispGeneric() {
		// do nothing
	}

	//
	/*package*/ LispGeneric(String name) {
		if(name == null) {
			throw new NullPointerException();
		}
		setName(name);
	}

	//
	/*package*/ void put(LispTypeList lst, Closure cl) {
		mmap.put(lst, cl);
	}

	//
	private boolean remove(LispTypeList lst) {
		return mmap.remove(lst);
	}

	/**
	 * 
	 * @param lst
	 * @return
	 */
	public Closure get(LispTypeList lst) {
		Iterator<LispTypeList> it = mmap.select(lst).iterator();

		return it.hasNext() ? mmap.get(it.next()) : null;
	}

	/**
	 * 
	 * @param lst
	 * @param prms
	 * @return
	 */
	public LispNextMethod getNextMethod(
			LispTypeList lst, Datum prms) {
		Iterator<LispTypeList> it = mmap.select(lst).iterator();

		if(it.hasNext()) {
			it.next();
			return new LispNextMethodImpl(this, it, prms);
		} else {
			//return null;
			return new NoNext(prms);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public Procedure mul(Procedure y) {
		return LispUtils.mul(this, y);
	}

	//
	/*package*/ void setType(LispType lt) {
		typ = lt;
	}

	//
	/*package*/ boolean isApplyGeneric() {
		return APPLY_GENERIC_STR.equals(getName());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return typ;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.NamableDatum#display()
	 */
	@Override
	public String display() {
		return "#<generic " + printName() + "(" + mmap.size() + ")>";
	}

}
