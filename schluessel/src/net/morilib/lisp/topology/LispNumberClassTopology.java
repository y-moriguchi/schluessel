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
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispOctonion;
import net.morilib.lisp.LispQuaternion;
import net.morilib.lisp.LispReal;
import net.morilib.range.Interval;
import net.morilib.range.Intervals;
import net.morilib.range.Range;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/16
 */
public abstract class LispNumberClassTopology
extends AbstractLispTopology {

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	O = new LispNumberClassTopology(0x4008) {

		public boolean isNeighborhoodOf(Datum d) {
			return d instanceof LispOctonion;
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				return false;
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.C;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of octonions>");
		}

	};

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	H = new LispNumberClassTopology(0x4004) {

		public boolean isNeighborhoodOf(Datum d) {
			return d instanceof LispQuaternion;
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				return false;
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.C;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of quaternions>");
		}

	};

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	C = new LispNumberClassTopology(0x4002) {

		public boolean isNeighborhoodOf(Datum d) {
			return d instanceof LispComplex;
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				return false;
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.C;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of complex numbers>");
		}

	};

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	R = new LispNumberClassTopology(0x4001) {

		public boolean isNeighborhoodOf(Datum d) {
			return d instanceof LispReal;
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				return ((LispRealNumberSet)t).getRange().isUniverse();
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.C;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of real numbers>");
		}

	};

	/**
	 * Note: Inexact numbers are rational.
	 */
	public static final LispNumberClassTopology
	Q = new LispNumberClassTopology(0x2000) {

		public boolean isNeighborhoodOf(Datum d) {
			if(d instanceof LispReal) {
				return ((LispReal)d).isRational();
			} else {
				return false;
			}
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				return ((LispRealNumberSet)t).getRange().isUniverse();
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.A;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of rational numbers>");
		}

	};

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	Z = new LispNumberClassTopology(0x1000) {

		public boolean isNeighborhoodOf(Datum d) {
			if(d instanceof LispReal) {
				return ((LispReal)d).isInteger();
			} else {
				return false;
			}
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				return ((LispRealNumberSet)t).getRange().isUniverse();
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.A;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of integers>");
		}

	};

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	N = new LispNumberClassTopology(0x0801) {

		public boolean isNeighborhoodOf(Datum d) {
			if(d instanceof LispReal) {
				LispReal r = (LispReal)d;

				return r.isInteger() && r.signum() >= 0;
			} else {
				return false;
			}
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				Range r = ((LispRealNumberSet)t).getRange();

				return r.containsAll(Intervals.newSupremumlessInterval(
						LispInteger.ZERO, false));
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.A;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of natural numbers with 0>");
		}

	};

	/**
	 * 
	 */
	public static final LispNumberClassTopology
	NPLUS = new LispNumberClassTopology(0x0800) {

		public boolean isNeighborhoodOf(Datum d) {
			if(d instanceof LispReal) {
				LispReal r = (LispReal)d;

				return r.isInteger() && r.signum() > 0;
			} else {
				return false;
			}
		}

		@Override
		public boolean isContained(ILispTopology t) {
			if(t instanceof LispRealNumberSet) {
				Range r = ((LispRealNumberSet)t).getRange();

				return r.containsAll(Intervals.newSupremumlessInterval(
						LispInteger.ONE, false));
			} else {
				return super.isContained(t);
			}
		}

		public LispCardinality cardinality() {
			return LispCardinality.A;
		}

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<class of natural numbers>");
		}

	};

	//
	private int order;

	//
	private LispNumberClassTopology(int order) {
		this.order = order;
	}

	/*package*/ static boolean contained(
			LispNumberClassTopology t, LispRealNumberSet s) {
		Range set = s.getRange();

		if(LispNumberClassTopology.R.isContained(t)) {
			return false;
		} else if(LispNumberClassTopology.Q.isContained(t)) {
			return false;
		} else if(LispNumberClassTopology.N.equals(t) &&
				Intervals.newInfimumlessInterval(
						LispInteger.ZERO, true).containsAll(set)) {
			return true;
		} else if(LispNumberClassTopology.NPLUS.equals(t) &&
				Intervals.newInfimumlessInterval(
						LispInteger.ONE, true).containsAll(set)) {
			return true;
		} else {
			for(Interval i : set.intervals()) {
				Object inf = i.getInfimumBound();
				Object sup = i.getSupremumBound();
				LispReal ri, rs, si, ss;

				if(!(inf instanceof LispReal) ||
						!(sup instanceof LispReal)) {
					return false;
				} else {
					si = (LispReal)inf;
					ss = (LispReal)sup;
					ri = si.ceil();
					rs = ss.ceil();
					if(si.isInteger() && i.isInfimumOpen()) {
						ri = ri.add(LispInteger.ONE);
					}

					if(!ri.isEqualTo(rs)) {
						return false;
					} else if((si.isInteger() && !i.isInfimumOpen()) ||
							(ss.isInteger() && !i.isSupremumOpen())) {
						return false;
					}
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	@Override
	public boolean isContained(ILispTopology t) {
		if(t instanceof LispNumberClassTopology) {
			return order <= ((LispNumberClassTopology)t).order;
		} else {
			return super.isContained(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		if(t instanceof LispRealNumberSet) {
			return contained(this, (LispRealNumberSet)t);
		} else {
			return (!(t instanceof LispNumberClassTopology) &&
					super.isIndependent(t));
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

}
