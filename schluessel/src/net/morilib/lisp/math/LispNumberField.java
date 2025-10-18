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
package net.morilib.lisp.math;

import net.morilib.lang.algebra.FieldElement;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public class LispNumberField
implements FieldElement<LispNumberField> {

	/**
	 * 
	 */
	public static LispNumberField ZERO =
		new LispNumberField(LispInteger.ZERO);
	
	/**
	 * 
	 */
	public static LispNumberField ONE =
		new LispNumberField(LispInteger.ONE);
	
	//
	private LispNumber number;
	
	/**
	 * 
	 * @param number
	 */
	public LispNumberField(LispNumber number) {
		if(number == null) {
			throw new NullPointerException();
		}
		this.number = number;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return LispInteger.ONE.equals(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return LispInteger.ZERO.equals(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public LispNumberField negate() {
		return new LispNumberField(LispInteger.ZERO.sub(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public LispNumberField subtract(LispNumberField x) {
		return new LispNumberField(number.sub(x.number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispNumberField add(LispNumberField x) {
		return new LispNumberField(number.add(x.number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispNumberField multiply(int n) {
		return new LispNumberField(number.mul(LispInteger.valueOf(n)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public LispNumberField multiply(LispNumberField x) {
		return new LispNumberField(number.mul(x.number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispNumberField power(int n) {
		LispNumber x;
		
		if(n == 0) {
			return new LispNumberField(LispInteger.ONE);
		} else if(n == 1) {
			return this;
		} else if(n >= 2) {
			x = number;
			for(int i = 1; i < n; i++) {
				x = x.mul(number);
			}
			return new LispNumberField(x);
		} else {
			x = LispInteger.ONE;
			for(int i = 0; i < -n; i++) {
				x = x.div(number);
			}
			return new LispNumberField(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispNumberField invert() {
		return new LispNumberField(LispInteger.ONE.div(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public LispNumberField divide(LispNumberField x) {
		return new LispNumberField(number.div(x.number));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return number.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispNumberField) {
			return number.equals(((LispNumberField)obj).number);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return number.toString();
	}

}
