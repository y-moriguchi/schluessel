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
package net.morilib.lisp.topology;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispString;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/16
 */
public class LispAllAtomsTopology extends AbstractLispTopology {

	/**
	 * 
	 */
	public static final LispAllAtomsTopology ALL_CHARS =
		new LispAllAtomsTopology(LispCharacter.class, "chars");

	/**
	 * 
	 */
	public static final LispAllAtomsTopology ALL_STRINGS =
		new LispAllAtomsTopology(LispString.class, "strings");

	//
	private Class<?> atomklass;
	private String   name;

	/**
	 * name should be plural.
	 * 
	 * @param atomklass
	 * @param name
	 */
	public LispAllAtomsTopology(Class<?> atomklass, String name) {
		this.atomklass = atomklass;
		this.name      = name;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNeighborhoodOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		return atomklass.isInstance(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isContained(ILispTopology t) {
		if(t instanceof LispAllAtomsTopology) {
			return atomklass.isAssignableFrom(
					((LispAllAtomsTopology)t).atomklass);
		} else {
			return super.isContained(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		if(t instanceof LispAllAtomsTopology) {
			return !atomklass.isAssignableFrom(
					((LispAllAtomsTopology)t).atomklass);
		} else {
			return super.isIndependent(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#interior()
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
	 * @see net.morilib.lisp.topology.ILispTopology#cardinality()
	 */
	public LispCardinality cardinality() {
		return LispCardinality.A;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isEmpty()
	 */
	public boolean isEmpty() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<all " + name + ">");
	}

}
