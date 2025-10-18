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
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/08
 */
public class LispIntersection extends AbstractLispTopology
implements Iterable<ILispTopology> {

	//
	/*package*/ ILispTopology[] topologies;

	//
	private LispIntersection(ILispTopology[] ts, boolean b) {
		this.topologies = ts;
	}

	//
	private LispIntersection(ILispTopology... ts) {
		this.topologies = new ILispTopology[ts.length];
		System.arraycopy(ts, 0, this.topologies, 0, ts.length);
	}

	//
	private LispIntersection(List<ILispTopology> ts) {
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
				if(ts[j].isContained(ts[i])) {
					continue outer;
				}
			}
			l.add(ts[i]);
		}

		switch(l.size()) {
		case 0:   return LispBoolean.TRUE;
		case 1:   return l.get(0);
		default:  return new LispIntersection(l);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isNeighborOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		for(ILispTopology t : topologies) {
			if(!t.isNeighborhoodOf(d)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#interior()
	 */
	public ILispTopology interior() {
		ILispTopology[] ts = new ILispTopology[topologies.length];

		for(int i = 0; i < ts.length; i++) {
			if((ts[i] = topologies[i].interior()) == null) {
				//return null;
				return LispBoolean.FALSE;
			}
		}
		return new LispIntersection(ts, false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#closure()
	 */
	public ILispTopology closure() {
		//return null;
		//return LispBoolean.TRUE;
		ILispTopology[] ts = new ILispTopology[topologies.length];

		for(int i = 0; i < ts.length; i++) {
			if((ts[i] = topologies[i].closure()) == null) {
				//return null;
				return LispBoolean.TRUE;
			}
		}
		return new LispIntersection(ts, false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isOpen()
	 */
	public boolean isOpen() {
		for(ILispTopology t : topologies) {
			if(!t.isOpen()) {
				//return null;
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isClosed()
	 */
	public boolean isClosed() {
		//return null;
		return false;
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
			if(t.isEmpty()) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		for(ILispTopology t : topologies) {
			if(!t.isUniverse()) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<ILispTopology> iterator() {
		final int[] i = new int[1];

		i[0] = 0;
		return new Iterator<ILispTopology>() {

			public boolean hasNext() {
				return i[0] < topologies.length;
			}

			public ILispTopology next() {
				if(i[0] >= topologies.length) {
					throw new NoSuchElementException();
				}
				return topologies[i[0]++];
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<intersection of sets>");
	}

}
