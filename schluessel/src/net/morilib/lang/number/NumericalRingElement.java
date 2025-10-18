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

import net.morilib.lang.algebra.UnitaryRingElement;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public interface NumericalRingElement
<E extends NumericalRingElement<E>>
extends UnitaryRingElement<E>, Comparable<E> {

	/**
	 * 
	 * @return
	 */
	public byte castByte();

	/**
	 * 
	 * @return
	 */
	public short castShort();

	/**
	 * 
	 * @return
	 */
	public int castInt();

	/**
	 * 
	 * @return
	 */
	public long castLong();

	/**
	 * 
	 * @return
	 */
	public Integer2 castInteger2();

	/**
	 * 
	 * @return
	 */
	public byte byteFloor();

	/**
	 * 
	 * @return
	 */
	public short shortFloor();

	/**
	 * 
	 * @return
	 */
	public int intFloor();

	/**
	 * 
	 * @return
	 */
	public long longFloor();

	/**
	 * 
	 * @return
	 */
	public Integer2 getInteger2Floor();

	/**
	 * 
	 * @return
	 */
	public byte byteCeil();

	/**
	 * 
	 * @return
	 */
	public short shortCeil();

	/**
	 * 
	 * @return
	 */
	public int intCeil();

	/**
	 * 
	 * @return
	 */
	public long longCeil();

	/**
	 * 
	 * @return
	 */
	public Integer2 getInteger2Ceil();

	/**
	 * 
	 * @return
	 */
	public Rational getRational();

	/**
	 * 
	 * @return
	 */
	public float floatValue();

	/**
	 * 
	 * @return
	 */
	public double doubleValue();

	/**
	 * 
	 * @return
	 */
	public boolean inByteRange();

	/**
	 * 
	 * @return
	 */
	public boolean inShortRange();

	/**
	 * 
	 * @return
	 */
	public boolean inIntRange();

	/**
	 * 
	 * @return
	 */
	public boolean inLongRange();

	/**
	 * 
	 * @return
	 */
	public boolean isInteger();

	/**
	 * 
	 * @return
	 */
	public NumericalRing<E> getUniverse();

}
