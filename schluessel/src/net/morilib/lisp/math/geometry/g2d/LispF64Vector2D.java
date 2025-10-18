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
package net.morilib.lisp.math.geometry.g2d;

import java.util.Iterator;
import java.util.NoSuchElementException;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.matrix.ILispNumberVector;
import net.morilib.lisp.math.matrix.LispMatrixException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public class LispF64Vector2D extends Datum2 implements ILispVector2D {

	//
	private double xp, yp;

	/**
	 * 
	 * @param x
	 * @param y
	 * @param z
	 */
	public LispF64Vector2D(double x, double y) {
		if(Double.isNaN(x) || Double.isNaN(y)) {
			throw new IllegalArgumentException();
		} else if(Double.isInfinite(x) || Double.isInfinite(y)) {
			throw new IllegalArgumentException();
		}
		xp = x;  yp = y;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#get(int)
	 */
	@Override
	public LispNumber get(int index) {
		switch(index) {
		case 0:  return new LispDouble(xp);
		case 1:  return new LispDouble(yp);
		default:  throw new IndexOutOfBoundsException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#set(int, net.morilib.lisp.LispNumber)
	 */
	@Override
	public void set(int index,
			LispNumber x) throws LispMatrixException {
		if(!x.isReal()) {
			throw new ClassCastException();
		} else {
			switch(index) {
			case 0:  xp = x.getRealDouble();
			case 1:  yp = x.getRealDouble();
			default:  throw new IndexOutOfBoundsException();
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public ILispNumberVector mul(LispNumber x) {
		double a;

		if(x.isReal()) {
			a = x.getRealDouble();
			return new LispF64Vector2D(xp * a, yp * a);
		} else {
			throw new ClassCastException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#add(net.morilib.lisp.math.matrix.ILispNumberVector)
	 */
	@Override
	public ILispNumberVector add(ILispNumberVector v) {
		if(v.size() != 2) {
			throw new LispMatrixException();
		} else if(!v.isRealVector()) {
			throw new ClassCastException();
		} else {
			return new LispF64Vector2D(
					xp + v.get(0).getRealDouble(),
					yp + v.get(1).getRealDouble());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#sub(net.morilib.lisp.math.matrix.ILispNumberVector)
	 */
	@Override
	public ILispNumberVector sub(ILispNumberVector v) {
		if(v.size() != 2) {
			throw new LispMatrixException();
		} else if(!v.isRealVector()) {
			throw new ClassCastException();
		} else {
			return new LispF64Vector2D(
					xp - v.get(0).getRealDouble(),
					yp - v.get(1).getRealDouble());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#uminus()
	 */
	@Override
	public ILispNumberVector uminus() {
		return new LispF64Vector2D(-xp, -yp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#innerProduct(net.morilib.lisp.math.matrix.ILispNumberVector)
	 */
	@Override
	public LispNumber innerProduct(ILispNumberVector v) {
		if(v.size() != 2) {
			throw new LispMatrixException();
		} else if(!v.isRealVector()) {
			throw new ClassCastException();
		} else {
			return new LispDouble(
					xp * v.get(0).getRealDouble() +
					yp * v.get(1).getRealDouble());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#normSquared()
	 */
	@Override
	public LispNumber normSquared() {
		return new LispDouble(xp * xp + yp * yp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isEqualTo(net.morilib.lisp.math.matrix.ILispNumberVector)
	 */
	@Override
	public boolean isEqualTo(ILispNumberVector v) {
		if(v.size() != 2) {
			return false;
		} else if(!v.isRealVector()) {
			return false;
		} else {
			return (xp == v.get(0).getRealDouble() &&
					yp == v.get(1).getRealDouble());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isQuaternionVector()
	 */
	@Override
	public boolean isQuaternionVector() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isComplexVector()
	 */
	@Override
	public boolean isComplexVector() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isRealVector()
	 */
	@Override
	public boolean isRealVector() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#size()
	 */
	@Override
	public int size() {
		return 2;
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<LispNumber> iterator() {
		final double[] i = new double[2];

		i[0] = xp;  i[1] = yp;
		return new Iterator<LispNumber>() {

			@Override
			public boolean hasNext() {
				return !Double.isNaN(i[1]);
			}

			@Override
			public LispNumber next() {
				double r;

				if(!Double.isNaN(i[0])) {
					r = i[0];  i[0] = Double.NaN;
				} else if(!Double.isNaN(i[1])) {
					r = i[1];  i[1] = Double.NaN;
				} else {
					throw new NoSuchElementException();
				}
				return new LispDouble(r);
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNorm#norm()
	 */
	@Override
	public LispNumber norm() {
		return new LispDouble(Math.sqrt(xp * xp + yp * yp));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispVector2D#crossProduct(net.morilib.lisp.math.geometry.g2d.ILispVector2D)
	 */
	@Override
	public LispReal crossProduct(ILispVector2D v) {
		double xq, yq;

		if(v.size() != 2) {
			throw new LispMatrixException();
		} else if(!v.isRealVector()) {
			throw new ClassCastException();
		} else {
			xq = v.get(0).getRealDouble();
			yq = v.get(1).getRealDouble();
			return new LispDouble(xp * yq - yp * xq);
		}
	}

//	/* (non-Javadoc)
//	 * @see java.lang.Object#hashCode()
//	 */
//	public int hashCode() {
//		int r = Hashes.INIT;
//
//		r = Hashes.A * (r + (int)Double.doubleToRawLongBits(xp));
//		r = Hashes.A * (r + (int)Double.doubleToRawLongBits(yp));
//		r = Hashes.A * (r + (int)Double.doubleToRawLongBits(zp));
//		return r;
//	}

//	/* (non-Javadoc)
//	 * @see java.lang.Object#equals(java.lang.Object)
//	 */
//	public boolean equals(Object o) {
//		LispF64Vector3D x;
//
//		if(o instanceof LispF64Vector3D) {
//			return ((x = (LispF64Vector3D)o).xp == xp &&
//					x.yp == yp && x.zp == zp);
//		}
//		return false;
//	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#2Df(");
		buf.append(xp).append(" ");
		buf.append(yp).append(")");
	}

}
