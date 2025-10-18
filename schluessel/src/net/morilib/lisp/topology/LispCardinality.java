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

import java.math.BigInteger;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.util.IntMath;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/08
 */
public class LispCardinality extends Datum2
implements ILispAddable<LispCardinality>,
ILispMultipliable<LispCardinality>, Comparable<LispCardinality> {

	//
	/*package*/ static final int FINITE = 0;
	/*package*/ static final int INF_A = 1;
	/*package*/ static final int INF_C = 2;
	/*package*/ static final int INF_OVER_C = 3;

	/**
	 * 
	 */
	public static final LispCardinality ZERO =
		new LispCardinality(0, true);

	/**
	 * 
	 */
	public static final LispCardinality A =
		new LispCardinality(INF_A, false);

	/**
	 * 
	 */
	public static final LispCardinality C =
		new LispCardinality(INF_C, false);

	/**
	 * 
	 */
	public static final LispCardinality OVER_C =
		new LispCardinality(INF_OVER_C, false);

	//
	private BigInteger finite;
	private int infinite;

	//
	private LispCardinality(long n, boolean isfinite) {
		if(isfinite) {
			this.finite   = BigInteger.valueOf(n);
			this.infinite = FINITE;
		} else if(n > 0) {
			this.finite   = null;
			this.infinite = Math.min((int)n, INF_OVER_C);
		} else {
			throw new IllegalArgumentException();
		}
	}

	//
	private LispCardinality(BigInteger n) {
		this.finite   = n;
		this.infinite = FINITE;
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispCardinality finiteValueOf(int n) {
		if(n < 0) {
			throw new IllegalArgumentException();
		}
		return new LispCardinality(n, true);
	}

	/**
	 * @param cardinality
	 * @return
	 */
	public static LispCardinality finiteValueOf(long n) {
		if(n < 0) {
			throw new IllegalArgumentException();
		}
		return new LispCardinality(n, true);
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispCardinality finiteValueOf(BigInteger n) {
		if(n.signum() < 0) {
			throw new IllegalArgumentException();
		}
		return new LispCardinality(n);
	}

	//
	/*package*/ static LispCardinality infiniteValueOf(int n) {
		return new LispCardinality(n, false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispCardinality mul(LispCardinality y) {
		if(infinite == FINITE && finite.signum() == 0) {
			return ZERO;
		} else if(y.infinite == FINITE && y.finite.signum() == 0) {
			return ZERO;
		} else if(infinite == FINITE && y.infinite == FINITE) {
			return finiteValueOf(finite.multiply(y.finite));
		} else {
			return infiniteValueOf(Math.max(infinite, y.infinite));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispCardinality add(LispCardinality y) {
		if(infinite == FINITE && y.infinite == FINITE) {
			return finiteValueOf(finite.add(y.finite));
		} else {
			return infiniteValueOf(Math.max(infinite, y.infinite));
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isFinite() {
		return infinite == FINITE;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCountable() {
		return infinite < INF_C;
	}

	/**
	 * 
	 * @return
	 */
	public BigInteger toBigInteger() {
		return finite;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		switch(infinite) {
		case FINITE:
			buf.append("#<cardinarity ");
			buf.append(finite.toString());
			break;
		case INF_A:
			buf.append("#<cardinarity ");
			buf.append("countable");
			break;
		case INF_C:
			buf.append("#<cardinarity ");
			buf.append("real set");
			break;
		default:
			buf.append("#<cardinarity ");
			buf.append("over power of real set");
			break;
		}
		buf.append(">");
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(LispCardinality c) {
		if(infinite == FINITE && c.infinite == FINITE) {
			return finite.compareTo(c.finite);
		} else {
			return IntMath.compareTo(infinite, c.infinite);
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return (infinite == FINITE) ? finite.hashCode() : -infinite;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispCardinality) {
			LispCardinality c = (LispCardinality)o;

			if(infinite == FINITE && c.infinite == FINITE) {
				return finite.equals(c.finite);
			} else {
				return infinite == c.infinite;
			}
		}
		return false;
	}

}
