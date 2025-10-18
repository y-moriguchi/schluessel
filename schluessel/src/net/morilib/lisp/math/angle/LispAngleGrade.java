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
package net.morilib.lisp.math.angle;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.math.constants.LispPi;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public class LispAngleGrade extends Datum2 implements ILispAngle {

	//
	static final LispInteger RIGHT_ANGLE = LispInteger.valueOf(100);
	static final LispInteger STRAIGHT_ANGLE = LispInteger.valueOf(200);
	static final LispInteger FULL_ANGLE = LispInteger.valueOf(400);

	//
	private LispReal angle;

	/**
	 * 
	 * @param a
	 */
	public LispAngleGrade(LispReal a) {
		this.angle = a.remainder(FULL_ANGLE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	@Override
	public ILispAngle add(ILispAngle y) {
		return new LispAngleGrade(angle.add(y.byGrade()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	@Override
	public ILispAngle sub(ILispAngle y) {
		return new LispAngleGrade(angle.subtract(y.byGrade()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	@Override
	public ILispAngle uminus() {
		return new LispAngleGrade(angle.uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispScalarMultipliable#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public ILispAngle mul(LispNumber x) {
		if(x instanceof LispReal) {
			return new LispAngleGrade(angle.multiply((LispReal)x));
		} else {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#byRadian()
	 */
	@Override
	public LispReal byRadian() {
		return angle.divide(STRAIGHT_ANGLE).multiply(LispPi.PI);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#byDegree()
	 */
	@Override
	public LispReal byDegree() {
		return angle.divide(STRAIGHT_ANGLE).multiply(180);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#byGrade()
	 */
	@Override
	public LispReal byGrade() {
		return angle;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#sin()
	 */
	@Override
	public LispReal sin() {
		return new LispDouble(Math.sin(
				angle.doubleValue() / 200 * Math.PI));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#cos()
	 */
	@Override
	public LispReal cos() {
		return new LispDouble(Math.cos(
				angle.doubleValue() / 200 * Math.PI));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#tan()
	 */
	@Override
	public LispReal tan() {
		return new LispDouble(Math.tan(
				angle.doubleValue() / 200 * Math.PI));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#cot()
	 */
	@Override
	public LispReal cot() {
		return new LispDouble(1.0 / Math.tan(
				angle.doubleValue() / 200 * Math.PI));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#sec()
	 */
	@Override
	public LispReal sec() {
		return new LispDouble(1.0 / Math.cos(
				angle.doubleValue() / 200 * Math.PI));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#cosec()
	 */
	@Override
	public LispReal cosec() {
		return new LispDouble(1.0 / Math.sin(
				angle.doubleValue() / 200 * Math.PI));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isAcute()
	 */
	@Override
	public boolean isAcute() {
		return angle.compareTo(RIGHT_ANGLE) < 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isRight()
	 */
	@Override
	public boolean isRight() {
		return angle.isEqualTo(RIGHT_ANGLE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isObtuse()
	 */
	@Override
	public boolean isObtuse() {
		return (angle.compareTo(RIGHT_ANGLE) > 0 &&
				angle.compareTo(STRAIGHT_ANGLE) < 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isStraight()
	 */
	@Override
	public boolean isStraight() {
		return angle.isEqualTo(STRAIGHT_ANGLE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isReflex()
	 */
	@Override
	public boolean isReflex() {
		return angle.compareTo(STRAIGHT_ANGLE) > 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isSinExact()
	 */
	@Override
	public boolean isSinExact() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isCosExact()
	 */
	@Override
	public boolean isCosExact() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isTanExact()
	 */
	@Override
	public boolean isTanExact() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isExact()
	 */
	@Override
	public boolean isExact() {
		return angle.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispExactOrInexactQuantity#toExact()
	 */
	@Override
	public ILispAngle toExact() {
		return new LispAngleGrade(angle.toExact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispExactOrInexactQuantity#toInexact()
	 */
	@Override
	public ILispAngle toInexact() {
		return new LispAngleGrade(angle.toInexact());
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(ILispAngle o) {
		return angle.compareTo(o.byGrade());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNumberEqual#isEqualTo(java.lang.Object)
	 */
	@Override
	public boolean isEqualTo(ILispAngle x) {
		return angle.isEqualTo(x.byGrade());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return angle.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof ILispAngle) {
			return angle.isEqualTo(((ILispAngle)o).byGrade());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(LispUtils.print(angle)).append("#grad");
	}

}
