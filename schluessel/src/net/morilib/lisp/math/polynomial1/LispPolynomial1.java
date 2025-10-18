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
package net.morilib.lisp.math.polynomial1;

import java.util.Arrays;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/25
 */
public class LispPolynomial1 extends UnaryArgs
implements ILispPolynomial1, java.io.Serializable {

	/**
	 * 
	 */
	public static final LispPolynomial1 ZERO = new LispPolynomial1(
			new LispNumber[] { LispInteger.ZERO });

	//
	private LispNumber[] coefficients;

	/**
	 * 
	 * @param coeff
	 */
	public LispPolynomial1(LispNumber[] coeff) {
		int d = degree(coeff);

		coefficients = new LispNumber[d + 1];
		for(int i = 0; i <= d; i++) {
			coefficients[i] = coeff[i];
		}
	}

	/**
	 * 
	 * @param poly
	 */
	public LispPolynomial1(ILispPolynomial1 poly) {
		coefficients = new LispNumber[poly.degree() + 1];
		for(int i = 0; i <= poly.degree(); i++) {
			coefficients[i] = poly.coefficient(i);
		}
	}

	//
	private int degree(LispNumber[] coeff) {
		for(int i = coeff.length - 1; i >= 0; i--) {
			if(!coeff[i].isZero()) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * 
	 * @param coeffs
	 * @return
	 */
	public static LispPolynomial1 newInstance(LispNumber... coeffs) {
		LispNumber[] c2 = new LispNumber[coeffs.length];

		for(int i = 0; i < c2.length; i++) {
			c2[c2.length - i - 1] = coeffs[i];
		}
		return new LispPolynomial1(c2);
	}

	/**
	 * 
	 * @param coeffs
	 * @return
	 */
	public static LispPolynomial1 newInstance(double... coeffs) {
		LispNumber[] c2 = new LispNumber[coeffs.length];

		for(int i = 0; i < c2.length; i++) {
			c2[c2.length - i - 1] = new LispDouble(coeffs[i]);
		}
		return new LispPolynomial1(c2);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return (coefficients.length == 0 ||
				(coefficients.length == 1 &&
						coefficients[0].isZero()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#coefficient(int)
	 */
	public LispNumber coefficient(int n) {
		if(n < 0) {
			throw new IndexOutOfBoundsException();
		} else if(n >= coefficients.length) {
			return LispInteger.ZERO;
		}
		return coefficients[n];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#degree()
	 */
	public int degree() {
		return coefficients.length - 1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#substitute(net.morilib.lisp.LispNumber)
	 */
	public LispNumber substitute(LispNumber x) {
		LispNumber r = LispInteger.ZERO;

		for(int i = 0; i <= degree(); i++) {
			r = r.add(coefficients[i].mul(x.pow(i)));
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#multiply(net.morilib.lisp.LispNumber)
	 */
	public ILispPolynomial1 mul(LispNumber a) {
		LispPolynomial1 r = new LispPolynomial1(this);

		for(int i = 0; i <= r.degree(); i++) {
			r.coefficients[i] = r.coefficients[i].mul(a);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#divide(net.morilib.lisp.LispNumber)
	 */
	public ILispPolynomial1 div(LispNumber a) {
		LispPolynomial1 r = new LispPolynomial1(this);

		for(int i = 0; i <= r.degree(); i++) {
			r.coefficients[i] = r.coefficients[i].div(a);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#add(net.morilib.lisp.math.polynomial1.ILispPolynomial1)
	 */
	public ILispPolynomial1 add(ILispPolynomial1 b) {
		LispNumber[] r;

		r = new LispNumber[Math.max(degree(), b.degree()) + 1];
		for(int i = 0; i < r.length; i++) {
			r[i] = coefficient(i).add(b.coefficient(i));
		}
		return new LispPolynomial1(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#subtract(net.morilib.lisp.math.polynomial1.ILispPolynomial1)
	 */
	public ILispPolynomial1 sub(ILispPolynomial1 b) {
		LispNumber[] r;

		r = new LispNumber[Math.max(degree(), b.degree()) + 1];
		for(int i = 0; i < r.length; i++) {
			r[i] = coefficient(i).sub(b.coefficient(i));
		}
		return new LispPolynomial1(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#negate()
	 */
	public ILispPolynomial1 uminus() {
		LispPolynomial1 r = new LispPolynomial1(this);

		for(int i = 0; i <= r.degree(); i++) {
			r.coefficients[i] = r.coefficients[i].uminus();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#multiply(net.morilib.lisp.math.polynomial1.ILispPolynomial1)
	 */
	public ILispPolynomial1 mul(ILispPolynomial1 b) {
		LispNumber[] r = new LispNumber[degree() + b.degree() + 1];

		Arrays.fill(r, LispInteger.ZERO);
		for(int i = 0; i <= degree(); i++) {
			for(int j = 0; j <= b.degree(); j++) {
				r[i + j] = r[i + j].add(
						coefficient(i).mul(b.coefficient(j)));
			}
		}
		return new LispPolynomial1(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#divideAndRemainder(net.morilib.lisp.math.polynomial1.ILispPolynomial1)
	 */
	public ILispPolynomial1[] divAndRemainder(ILispPolynomial1 b) {
		LispNumber[] d, r;
		LispNumber   x;

		if(degree() < b.degree()) {
			return new LispPolynomial1[] { ZERO, this };
		} else if(b.degree() == 0) {
			return new ILispPolynomial1[] {
					div(b.coefficient(0)), ZERO };
		} else {
			d = new LispNumber[degree() - b.degree() + 1];
			r = new LispNumber[degree() + 1];
			System.arraycopy(coefficients, 0, r, 0, r.length);
			for(int i = degree() - b.degree(); i >= 0; i--) {
				x = r[i + b.degree()].div(b.coefficient(b.degree()));
				for(int j = i + b.degree(); j >= i; j--) {
					r[j] = r[j].sub(b.coefficient(j - i).mul(x));
				}
				d[i] = x;
			}
		}
		return new ILispPolynomial1[] {
				new LispPolynomial1(d), new LispPolynomial1(r)
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#differenciate()
	 */
	public ILispPolynomial1 differenciate() {
		if(coefficients.length > 0) {
			LispNumber[] r = new LispNumber[degree()];
	
			for(int i = 1; i <= r.length; i++) {
				r[i - 1] = coefficients[i].mul(LispInteger.valueOf(i));
			}
			return new LispPolynomial1(r);
		} else {
			return ZERO;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.polynomial1.ILispPolynomial1#integrate(net.morilib.lisp.LispNumber)
	 */
	public ILispPolynomial1 integrate(LispNumber constant) {
		LispNumber[] r = new LispNumber[degree() + 2];

		r[0] = constant;
		for(int i = 0; i <= degree(); i++) {
			r[i + 1] = coefficients[i].div(LispInteger.valueOf(i + 1));
		}
		return new LispPolynomial1(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.analysis.ILispIntegrable1#integrate(net.morilib.lisp.LispNumber, net.morilib.lisp.LispNumber)
	 */
	public LispReal integrate(LispReal a, LispReal b) {
		ILispPolynomial1 p = integrate(LispInteger.ZERO);

		return (LispReal)p.substitute(b).sub(p.substitute(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		return substitute(SubrUtils.getNumber(c1a, mesg));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		String pl = "";

		for(int i = degree(); i >= 0; i--) {
			if(coefficients[i].isZero()) {
				continue;
			} else if(coefficients[i].getReal().signum() > 0) {
				buf.append(pl);
			} else if(coefficients[i].getReal().signum() < 0) {
				// do nothing
			} else if(coefficients[i].getImag().signum() > 0) {
				buf.append(pl);
			} else if(coefficients[i].getImag().signum() < 0) {
				// do nothing
			}

			if(!(coefficients[i].isOne() ||
					coefficients[i].uminus().isOne()) || i == 0) {
				buf.append(LispUtils.print(coefficients[i]));
			}

			if(i > 1) {
				buf.append("x^").append(i);
			} else if(i > 0) {
				buf.append("x");
			}
			pl = "+";
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNumberEqual#isEqualTo(java.lang.Object)
	 */
	@Override
	public boolean isEqualTo(ILispPolynomial1 x) {
		if(degree() != x.degree()) {
			return false;
		} else {
			for(int i = 0; i <= degree(); i++) {
				if(!coefficient(i).isEqualTo(x.coefficient(i))) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(coefficients);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispPolynomial1) {
			return Arrays.equals(coefficients,
					((LispPolynomial1)o).coefficients);
		}
		return false;
	}

}
