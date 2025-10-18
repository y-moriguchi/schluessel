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

import net.morilib.lisp.math.ILispSquareIdempotence;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.topology.ILispTopology;
import net.morilib.lisp.topology.LispCardinality;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispBoolean extends Atom
implements JavaObjective, ILispAddable<LispBoolean>,
ILispMultipliable<LispBoolean>, ILispTopology, ILispSquareIdempotence,
java.io.Serializable {

	//
	private static final long serialVersionUID = 1583375250021807431L;

	//
	private boolean value;

	/**
	 * 
	 */
	public static final LispBoolean TRUE = new LispBoolean(true);

	/**
	 * 
	 */
	public static final LispBoolean FALSE = new LispBoolean(false);

	/**
	 * 
	 * @param value
	 * @return
	 */
	public static LispBoolean getInstance(boolean value) {
		return value ? TRUE : FALSE;
	}

	//
	private LispBoolean(boolean value) {
		this.value = value;
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isEqv(Atom a) {
		return equals(a);
	}

	/**
	 * 
	 * @return
	 */
	public boolean getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	public String print() {
		return (value ? "#t" : "#f");
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	public String getResult() {
		return (value ? "#t" : "#f");
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	public LispString toLispString() {
		return new LispString(value ? "#t" : "#f");
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeBoolean()
	 */
	public boolean isTypeBoolean() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTrue()
	 */
	public boolean isTrue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.BOOLEAN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toObject()
	 */
	public Object toObject() {
		return Boolean.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispBoolean mul(LispBoolean y) {
		return getInstance(value && y.value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispBoolean add(LispBoolean y) {
		return getInstance(value || y.value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNeighborOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#containsTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isContained(ILispTopology t) {
		return !value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		return !value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isNotContained(ILispTopology t) {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNotIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isNotIndependent(ILispTopology t) {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology unionTopology(ILispTopology t) {
		return value ? this : t;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#intersectTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology intersectionTopology(ILispTopology t) {
		return value ? t : this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#openKernel()
	 */
	public ILispTopology interior() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#closure()
	 */
	public ILispTopology closure() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isOpen()
	 */
	public boolean isOpen() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isClosed()
	 */
	public boolean isClosed() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#cardinarity()
	 */
	public LispCardinality cardinality() {
		return value ? LispCardinality.OVER_C : LispCardinality.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isEmpty()
	 */
	public boolean isEmpty() {
		return !value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispBoolean) {
			return value == ((LispBoolean)o).value;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return (value ? 1 : 0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return (value ? "#t" : "#f");
	}

}
