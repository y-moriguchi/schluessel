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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public abstract class AbstractNumerical<E extends NumericalRingElement<E>>
implements NumericalRingElement<E> {

	//
	private static final Integer2 _I_MAX =
		Integer2.valueOf(Integer.MAX_VALUE);
	private static final Integer2 _I_MIN =
		Integer2.valueOf(Integer.MIN_VALUE);
	private static final Integer2 _S_MAX =
		Integer2.valueOf(Short.MAX_VALUE);
	private static final Integer2 _S_MIN =
		Integer2.valueOf(Short.MIN_VALUE);
	private static final Integer2 _B_MAX =
		Integer2.valueOf(Byte.MAX_VALUE);
	private static final Integer2 _B_MIN =
		Integer2.valueOf(Byte.MIN_VALUE);
	private static final Integer2 _L_MAX =
		Integer2.valueOf(Long.MAX_VALUE);
	private static final Integer2 _L_MIN =
		Integer2.valueOf(Long.MIN_VALUE);

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castByte()
	 */
	public byte castByte() {
		return (byte)castInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castShort()
	 */
	public short castShort() {
		return (short)castInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#byteFloor()
	 */
	public byte byteFloor() {
		return (byte)intFloor();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#shortFloor()
	 */
	public short shortFloor() {
		return (short)intFloor();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#byteCeil()
	 */
	public byte byteCeil() {
		return (byte)intCeil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#shortCeil()
	 */
	public short shortCeil() {
		return (short)intCeil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isByteValue()
	 */
	public boolean inByteRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_B_MAX) <= 0 && f.compareTo(_B_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isShortValue()
	 */
	public boolean inShortRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_S_MAX) <= 0 && f.compareTo(_S_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isIntValue()
	 */
	public boolean inIntRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_I_MAX) <= 0 && f.compareTo(_I_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isLongValue()
	 */
	public boolean inLongRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_L_MAX) <= 0 && f.compareTo(_L_MIN) >= 0;
	}

}
