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
package net.morilib.lang.number;

import net.morilib.lang.algebra.AbstractUnitaryRing;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Integer2Ring extends AbstractUnitaryRing<Integer2>
implements NumericalRing<Integer2> {

	//
	private static final Integer2Ring INSTANCE = new Integer2Ring();

	//
	private Integer2Ring() { }

	/**
	 * 
	 * @return
	 */
	public static Integer2Ring getInstance() {
		return INSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRing#getUnit()
	 */
	public Integer2 getUnit() {
		return Integer2.ONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#getZero()
	 */
	public Integer2 getZero() {
		return Integer2.ZERO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(byte)
	 */
	public Integer2 valueOf(byte v) {
		return Integer2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(short)
	 */
	public Integer2 valueOf(short v) {
		return Integer2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(int)
	 */
	public Integer2 valueOf(int v) {
		return Integer2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(long)
	 */
	public Integer2 valueOf(long v) {
		return Integer2.valueOf(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(net.morilib.lang.number.Integer2)
	 */
	public Integer2 valueOf(Integer2 v) {
		return v;
	}

}
