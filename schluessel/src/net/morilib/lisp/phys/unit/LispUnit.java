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

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.ILispQuantityFactory;
import net.morilib.phys.unit.Unit;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public class LispUnit extends Datum2
implements ILispQuantityFactory, java.io.Serializable {

	/**
	 * 
	 */
	public static final LispUnit NONDIMENSION =
		new LispUnit(Unit.NONDIMENSION);

	//
	Unit unit;

	/**
	 * 
	 * @param unit
	 */
	public LispUnit(Unit unit) {
		this.unit = unit;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<unit ").append(unit.toString()).append(">");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispQuantityFactory#getInstance(net.morilib.lisp.LispReal)
	 */
	public LispQuantity getInstance(LispReal r) {
		return new LispQuantity(r.doubleValue(), unit);
	}

}
