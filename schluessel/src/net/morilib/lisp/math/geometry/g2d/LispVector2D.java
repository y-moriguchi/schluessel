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

import net.morilib.geometry.g2d.Vector2D;
import net.morilib.geometry.g2d.VectorFactory2D;
import net.morilib.lang.Hashes;
import net.morilib.lisp.Atom;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispString;
import net.morilib.lisp.math.algebra.ILispVectorSpace;

/**
 *
 *
 * @deprecated
 * @author MORIGUCHI, Yuichiro 2011/01/30
 */
public final class LispVector2D extends Atom
implements Vector2D<LispVector2D, LispReal>,
ILispVectorSpace<LispVector2D>, java.io.Serializable {

	//
	private static final VectorFactory2D<LispVector2D, LispReal>
	_FIELDR = new VectorFactory2D<LispVector2D, LispReal>() {

		public LispVector2D create(LispReal x, LispReal y) {
			return new LispVector2D(x, y);
		}

	};

	//
	private LispReal vx, vy;

	/**
	 * 
	 * @param x
	 * @param y
	 */
	public LispVector2D(LispReal x, LispReal y) {
		if(x == null || y == null) {
			throw new NullPointerException();
		}
		this.vx = x;
		this.vy = y;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 */
	public LispVector2D(double x, double y) {
		this(new LispDouble(x), new LispDouble(y));
	}

	/**
	 * 
	 * @param x
	 * @param y
	 */
	public LispVector2D(long x, long y) {
		this(LispInteger.valueOf(x), LispInteger.valueOf(y));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.VectorElement#multiply(java.lang.Object)
	 */
	public LispVector2D multiply(LispReal a) {
		return new LispVector2D(vx.multiply(a), vy.multiply(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public LispVector2D negate() {
		return new LispVector2D(vx.negate(), vy.negate());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public LispVector2D subtract(LispVector2D x) {
		return new LispVector2D(vx.subtract(x.vx), vy.subtract(x.vy));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispVector2D add(LispVector2D x) {
		return new LispVector2D(vx.add(x.vx), vy.add(x.vy));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispVector2D multiply(int n) {
		return new LispVector2D(vx.multiply(n), vy.multiply(n));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	public LispVector2D sub(LispVector2D y) {
		return subtract(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	public LispVector2D uminus() {
		return negate();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispScalarMultipliable#mul(net.morilib.lisp.LispNumber)
	 */
	public LispVector2D mul(LispNumber x) {
		return multiply((LispReal)x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.geometry.g2d.Vector2D#getX()
	 */
	public LispReal getX() {
		return vx;
	}

	/* (non-Javadoc)
	 * @see net.morilib.geometry.g2d.Vector2D#getY()
	 */
	public LispReal getY() {
		return vy;
	}

	/* (non-Javadoc)
	 * @see net.morilib.geometry.g2d.Vector2D#innerProduct(net.morilib.geometry.g2d.Vector2D)
	 */
	public LispReal innerProduct(LispVector2D p) {
		return vx.multiply(p.vx).add(vy.multiply(p.vy));
	}

	/* (non-Javadoc)
	 * @see net.morilib.geometry.g2d.Vector2D#crossProduct(net.morilib.geometry.g2d.Vector2D)
	 */
	public LispReal crossProduct(LispVector2D p) {
		return vx.multiply(p.vy).subtract(vy.multiply(p.vx));
	}

	/* (non-Javadoc)
	 * @see net.morilib.geometry.g2d.Vector2D#getFactory()
	 */
	public VectorFactory2D<LispVector2D, LispReal> getFactory() {
		return _FIELDR;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		return toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return ("(make-vector2d " + vx.getResult() + " " +
				vy.getResult() + ")");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	@Override
	public LispString toLispString() {
		return new LispString(print());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("(").append(vx.getResult());
		buf.append(", ").append(vy.getResult()).append(")");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + vx.hashCode()) * Hashes.A;
		r = (r + vy.hashCode()) * Hashes.A;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispVector2D) {
			return (vx.equals(((LispVector2D)obj).vx) &&
					vy.equals(((LispVector2D)obj).vy));
		}
		return false;
	}

}
