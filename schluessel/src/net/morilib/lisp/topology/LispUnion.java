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

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/08
 */
public class LispUnion extends AbstractLispTopology {

	//
	/*package*/ ILispTopology[] topologies;

	//
	private LispUnion(ILispTopology[] ts, boolean b) {
		this.topologies = ts;
	}

	//
	private LispUnion(ILispTopology... ts) {
		this.topologies = new ILispTopology[ts.length];
		System.arraycopy(ts, 0, this.topologies, 0, ts.length);
	}

	//
	private LispUnion(List<ILispTopology> ts) {
		this.topologies = ts.toArray(new ILispTopology[0]);
	}

	/**
	 * 
	 * @param ts
	 * @return
	 */
	public static ILispTopology newInstance(ILispTopology... ts) {
		List<ILispTopology> l = new ArrayList<ILispTopology>();

		outer: for(int i = 0; i < ts.length; i++) {
			for(int j = i + 1; j < ts.length; j++) {
				if(ts[i].isContained(ts[j])) {
					continue outer;
				}
			}
			l.add(ts[i]);
		}

		switch(l.size()) {
		case 0:   return LispBoolean.FALSE;
		case 1:   return l.get(0);
		default:  return new LispUnion(l);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNeighborOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		for(ILispTopology t : topologies) {
			if(t.isNeighborhoodOf(d)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		for(ILispTopology s : topologies) {
			if(!s.isIndependent(t)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#interior()
	 */
	public ILispTopology interior() {
		//return null;
		//return LispBoolean.FALSE;
		ILispTopology[] ts = new ILispTopology[topologies.length];

		for(int i = 0; i < ts.length; i++) {
			if((ts[i] = topologies[i].interior()) == null) {
				//return null;
				return LispBoolean.FALSE;
			}
		}
		return new LispUnion(ts, false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#closure()
	 */
	public ILispTopology closure() {
		ILispTopology[] ts = new ILispTopology[topologies.length];

		for(int i = 0; i < ts.length; i++) {
			if((ts[i] = topologies[i].closure()) == null) {
				//return null;
				return LispBoolean.TRUE;
			}
		}
		return new LispUnion(ts, false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isOpen()
	 */
	public boolean isOpen() {
		//return null;
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isClosed()
	 */
	public boolean isClosed() {
		for(ILispTopology t : topologies) {
			if(!t.isClosed()) {
				//return null;
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#cardinality()
	 */
	public LispCardinality cardinality() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isEmpty()
	 */
	public boolean isEmpty() {
		for(ILispTopology t : topologies) {
			if(!t.isEmpty()) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		for(ILispTopology t : topologies) {
			if(t.isUniverse()) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#toDisplayString(java.lang.StringBuilder)
	 */
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<union of sets>");
	}

}
