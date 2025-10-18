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
package net.morilib.math.functions;

import net.morilib.lang.algebra.AbstractUnitaryRing;
import net.morilib.lang.algebra.Field;
import net.morilib.lang.algebra.FieldElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/16
 */
public class PolynomialRing
<F extends Comparable<F>, A extends Field<C>, C extends FieldElement<C>>
extends AbstractUnitaryRing<Polynomial<F, C>> {
	
	//
	private final Polynomial<F, C> unit, zero;
	
	
	public PolynomialRing(A field) {
		this.zero  = new Polynomial<F, C>();
		this.unit  = new Polynomial<F, C>(field.getUnit());
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRing#getUnit()
	 */
	public Polynomial<F, C> getUnit() {
		return unit;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#getZero()
	 */
	public Polynomial<F, C> getZero() {
		return zero;
	}

}
