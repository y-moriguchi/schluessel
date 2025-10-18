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
import net.morilib.lisp.LispDouble;
import net.morilib.range.Interval;
import net.morilib.range.Range;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/08
 */
public class LispRealNumberSet extends AbstractLispTopology
implements ILispOrderedSet {

	/**
	 * 
	 */
	public static LispRealNumberSet O = new LispRealNumberSet(Range.O);

	/**
	 * 
	 */
	public static LispRealNumberSet U = new LispRealNumberSet(Range.U);

	//
	private Range set;

	/**
	 * 
	 * @param set
	 */
	public LispRealNumberSet(Range set) {
		this.set = set;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNeighborOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		return set.contains(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#containsTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isContained(ILispTopology t) {
		if(t instanceof LispRealNumberSet) {
			return set.in(((LispRealNumberSet)t).set);
		} else if(t instanceof LispNumberClassTopology) {
			if(LispNumberClassTopology.R.isContained(t)) {
				return true;
			} else if(set.isFinite()) {
				for(Object o : set.getJavaSetIfFinite()) {
					if(!t.isNeighborhoodOf((Datum)o)) {
						return false;
					}
				}
				return true;
			} else {
				return false;
			}
		} else {
			return super.isContained(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		if(t instanceof LispRealNumberSet) {
			return set.independentOf(((LispRealNumberSet)t).set);
		} else if(t instanceof LispNumberClassTopology) {
			return LispNumberClassTopology.contained(
					(LispNumberClassTopology)t, this);
		} else {
			return super.isIndependent(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology unionTopology(ILispTopology t) {
		if(t instanceof LispRealNumberSet) {
			return new LispRealNumberSet(
					set.join(((LispRealNumberSet)t).set));
		} else {
			return super.unionTopology(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#intersectTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	public ILispTopology intersectionTopology(ILispTopology t) {
		if(t instanceof LispRealNumberSet) {
			return new LispRealNumberSet(
					set.meet(((LispRealNumberSet)t).set));
		} else {
			return super.intersectionTopology(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#openKernel()
	 */
	public ILispTopology interior() {
		return new LispRealNumberSet(set.interior());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#closure()
	 */
	public ILispTopology closure() {
		return new LispRealNumberSet(set.closure());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isOpen()
	 */
	public boolean isOpen() {
		return set.isOpen();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isClosed()
	 */
	public boolean isClosed() {
		return set.isClosed();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#cardinality()
	 */
	public LispCardinality cardinality() {
		if(set.isEmpty()) {
			return LispCardinality.ZERO;
		} else if(set.isFinite()) {
			return LispCardinality.finiteValueOf(
					set.getJavaSetIfFinite().size());
		} else {
			return LispCardinality.C;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isEmpty()
	 */
	public boolean isEmpty() {
		return set.isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public Range getRange() {
		return set;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#maximum()
	 */
	public Datum maximum() {
		Interval i;

		if(set.isEmpty()) {
			return null;
		} else {
			i = set.intervals().last();
			if(!i.isSupremumFinite()) {
				//return LispDouble.POSITIVE_INFINITY;
				return null;
			} else if(i.isSupremumClosed()) {
				return (Datum)i.getSupremumBound();
			} else {
				return null;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#minimum()
	 */
	public Datum minimum() {
		Interval i;

		if(set.isEmpty()) {
			return null;
		} else {
			i = set.intervals().first();
			if(!i.isInfimumFinite()) {
				//return LispDouble.NEGATIVE_INFINITY;
				return null;
			} else if(i.isInfimumClosed()) {
				return (Datum)i.getInfimumBound();
			} else {
				return null;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#supremum()
	 */
	public Datum supremum() {
		Interval i;

		if(set.isEmpty()) {
			return null;
		} else {
			i = set.intervals().last();
			if(!i.isSupremumFinite()) {
				return LispDouble.POSITIVE_INFINITY;
			} else {
				return (Datum)i.getSupremumBound();
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#infinum()
	 */
	public Datum infimum() {
		Interval i;

		if(set.isEmpty()) {
			return null;
		} else {
			i = set.intervals().first();
			if(!i.isInfimumFinite()) {
				return LispDouble.NEGATIVE_INFINITY;
			} else {
				return (Datum)i.getInfimumBound();
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return set.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof LispRealNumberSet) {
			return set.equals(((LispRealNumberSet)o).set);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<real number set ")
		.append(set.toString())
		.append(">");
	}

}
