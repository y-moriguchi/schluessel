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
import net.morilib.lisp.LispMath;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public class LispAngleTrigonometric extends Datum2
implements ILispAngle {

	//
	private LispReal hypotenuse, opposite, adjacent;

	//
	LispAngleTrigonometric(LispReal h, LispReal o, LispReal a) {
		if(h.signum() <= 0 || o.signum() <= 0 || a.signum() <= 0) {
			throw new IllegalArgumentException();
		}
		this.hypotenuse = h;
		this.opposite   = o;
		this.adjacent   = a;
	}

	/**
	 * 
	 * @param hyp
	 * @param opp
	 * @return
	 */
	public static ILispAngle sin(LispReal hyp, LispReal opp) {
		if(opp.signum() == 0) {
			return new LispRadian(LispInteger.ZERO);
		} else if(!hyp.isExact() || !opp.isExact() ||
				opp.signum() < 0) {
			return new LispRadian(
					Math.asin(opp.doubleValue() / hyp.doubleValue()));
		}
		return new LispAngleTrigonometric(hyp, opp, pyt1(hyp, opp));
	}

	/**
	 * 
	 * @param hyp
	 * @param adj
	 * @return
	 */
	public static ILispAngle cos(LispReal hyp, LispReal adj) {
		if(adj.signum() == 0) {
			return new LispRadian(LispRadian.RIGHT_ANGLE);
		} else if(!hyp.isExact() || !adj.isExact() ||
				adj.signum() < 0) {
			return new LispRadian(
					Math.acos(adj.doubleValue() / hyp.doubleValue()));
		}
		return new LispAngleTrigonometric(hyp, pyt1(hyp, adj), adj);
	}

	/**
	 * 
	 * @param opp
	 * @param adj
	 * @return
	 */
	public static ILispAngle tan(LispReal adj, LispReal opp) {
		if(!adj.isExact() || !opp.isExact() ||
				adj.signum() <= 0 || opp.signum() <= 0) {
			return new LispRadian(
					Math.atan2(opp.doubleValue(), adj.doubleValue()));
		}
		return new LispAngleTrigonometric(pyt2(adj, opp), opp, adj);
	}

	//
	private static LispReal pyt1(LispReal hyp, LispReal x) {
		return LispMath.sqrt(
				hyp.power(2).subtract(x.power(2))).getReal();
	}

	//
	private static LispReal pyt2(LispReal x, LispReal y) {
		return LispMath.sqrt(x.power(2).add(y.power(2))).getReal();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	@Override
	public ILispAngle add(ILispAngle y) {
		return new LispRadian(byRadian()).add(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	@Override
	public ILispAngle sub(ILispAngle y) {
		return new LispRadian(byRadian()).sub(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	@Override
	public ILispAngle uminus() {
		return new LispRadian(byRadian()).uminus();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispScalarMultipliable#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public ILispAngle mul(LispNumber x) {
		return new LispRadian(byRadian()).mul(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#byRadian()
	 */
	@Override
	public LispReal byRadian() {
		return new LispDouble(Math.acos(cos().doubleValue()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#byDegree()
	 */
	@Override
	public LispReal byDegree() {
		return new LispDouble(
				Math.acos(cos().doubleValue() / Math.PI * 180));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#byGrade()
	 */
	@Override
	public LispReal byGrade() {
		return new LispDouble(
				Math.acos(cos().doubleValue() / Math.PI * 200));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#sin()
	 */
	@Override
	public LispReal sin() {
		return opposite.divide(hypotenuse);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#cos()
	 */
	@Override
	public LispReal cos() {
		return adjacent.divide(hypotenuse);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#tan()
	 */
	@Override
	public LispReal tan() {
		return opposite.divide(adjacent);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#cot()
	 */
	@Override
	public LispReal cot() {
		return adjacent.divide(opposite);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#sec()
	 */
	@Override
	public LispReal sec() {
		return hypotenuse.divide(adjacent);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#cosec()
	 */
	@Override
	public LispReal cosec() {
		return hypotenuse.divide(opposite);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isAcute()
	 */
	@Override
	public boolean isAcute() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isRight()
	 */
	@Override
	public boolean isRight() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isObtuse()
	 */
	@Override
	public boolean isObtuse() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isStraight()
	 */
	@Override
	public boolean isStraight() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isReflex()
	 */
	@Override
	public boolean isReflex() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isSinExact()
	 */
	@Override
	public boolean isSinExact() {
		return hypotenuse.isExact() && opposite.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isCosExact()
	 */
	@Override
	public boolean isCosExact() {
		return hypotenuse.isExact() && adjacent.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isTanExact()
	 */
	@Override
	public boolean isTanExact() {
		return adjacent.isExact() && opposite.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.angle.ILispAngle#isExact()
	 */
	@Override
	public boolean isExact() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispExactOrInexactQuantity#toExact()
	 */
	@Override
	public ILispAngle toExact() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispExactOrInexactQuantity#toInexact()
	 */
	@Override
	public ILispAngle toInexact() {
		return new LispRadian(byRadian().doubleValue());
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(ILispAngle o) {
		LispAngleTrigonometric t;

		if(!o.isAcute()) {
			return -1;
		} else if(!(o instanceof LispAngleTrigonometric)) {
			return byRadian().compareTo(o.byRadian());
		} else if((t = (LispAngleTrigonometric)o).isSinExact() &&
				isSinExact()) {
			return opposite.divide(hypotenuse).compareTo(
					t.opposite.divide(t.hypotenuse));
		} else if(isCosExact() && t.isCosExact()) {
			return adjacent.divide(hypotenuse).compareTo(
					t.adjacent.divide(t.hypotenuse));
		} else if(isTanExact() && t.isTanExact()) {
			return opposite.divide(adjacent).compareTo(
					t.opposite.divide(t.adjacent));
		} else {
			return byRadian().compareTo(o.byRadian());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNumberEqual#isEqualTo(java.lang.Object)
	 */
	@Override
	public boolean isEqualTo(ILispAngle x) {
		return compareTo(x) == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		if(isSinExact()) {
			buf.append("sin ");
			buf.append(opposite.divide(hypotenuse).print());
		} else if(isCosExact()) {
			buf.append("cos ");
			buf.append(adjacent.divide(hypotenuse).print());
		} else if(isTanExact()) {
			buf.append("tan ");
			buf.append(opposite.divide(adjacent).print());
		} else {
			buf.append("cos ");
			buf.append(opposite.divide(hypotenuse).print());
		}
	}

}
