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
package net.morilib.lisp.phys.unit;

import net.morilib.lang.Hashes;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.AbstractLispQuantity;
import net.morilib.lisp.math.ILispQuantityFactory;
import net.morilib.lisp.math.algebra.ILispField;
import net.morilib.lisp.math.algebra.ILispScalarMultipliable;
import net.morilib.phys.unit.Unit;
import net.morilib.phys.unit.UnitSystem;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public class LispQuantity extends AbstractLispQuantity
implements ILispField<LispQuantity>,
ILispScalarMultipliable<LispQuantity>, java.io.Serializable {

	//
	private double value;
	private Unit unit;

	/**
	 * 
	 * @param value
	 * @param unit
	 */
	public LispQuantity(double value, Unit unit) {
		this.value = value;
		this.unit  = unit;
	}

	/**
	 * 
	 * @return
	 */
	public double getValue() {
		return value;
	}

	/**
	 * 
	 * @return
	 */
	public Unit getUnit() {
		return unit;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispQuantity add(LispQuantity y) {
		if(!unit.equals(y.unit)) {
			throw new ClassCastException();
		}
		return new LispQuantity(value + y.value, unit);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	public LispQuantity sub(LispQuantity y) {
		if(!unit.equals(y.unit)) {
			throw new ClassCastException();
		}
		return new LispQuantity(value - y.value, unit);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	public LispQuantity uminus() {
		return new LispQuantity(-value, unit);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispQuantity mul(LispQuantity y) {
		return new LispQuantity(value * y.value,
				unit.multiply(y.unit));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispDividable#div(net.morilib.lisp.math.algebra.ILispDividable)
	 */
	public LispQuantity div(LispQuantity y) {
		return new LispQuantity(value / y.value, unit.divide(y.unit));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispInvertable#inv()
	 */
	public LispQuantity inv() {
		return new LispQuantity(1.0 / value, unit.invert());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispScalarMultipliable#mul(net.morilib.lisp.LispNumber)
	 */
	public LispQuantity mul(LispNumber x) {
		if(!x.isReal()) {
			throw new ClassCastException();
		}
		return new LispQuantity(value * x.getRealDouble(), unit);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (Hashes.hashCode(value) + r);
		r = Hashes.A * (unit.hashCode() + r);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		return ((o instanceof LispQuantity) &&
				value == ((LispQuantity)o).value &&
				unit.equals(((LispQuantity)o).unit));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		UnitSystem sys = LispUnitSystem.defaultSystem.sys;
		double ms;

		ms = Math.pow(1000, unit.getIndex("mass"));
		buf.append(value / ms).append(sys.getDescription(unit));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.AbstractLispQuantity#getReal()
	 */
	@Override
	public LispReal getReal() {
		return new LispDouble(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispQuantity#factory()
	 */
	public ILispQuantityFactory factory() {
		return new LispUnit(unit);
	}

}
