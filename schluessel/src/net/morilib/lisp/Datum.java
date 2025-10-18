/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.List;

import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class Datum {

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isEqv(Datum a) {
		return equals(a);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeNumber() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeString() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeCharacter() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypePort() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeBoolean() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeSymbol() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeList() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeVector() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTypeProcedure() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNil() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isDottedList() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public int getInt() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public long getLong() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public BigInteger getBigInteger() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public BigDecimal getBigDecimal() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public LispReal getReal() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public LispReal getImag() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public double getRealDouble() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public float getRealFloat() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public short getRealHalf() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public long getRealDecimal64() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public int getRealDecimal32() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public double getImagDouble() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public String getString() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public List<Datum> getList() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public Datum getDottedDatum() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public char getCharacter() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public int getCharacterCodePoint() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isTrue() {
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public LispType getType() {
		return LispType.TOP;
	}

	/**
	 * 
	 * @param buf
	 */
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<unknown-object>");
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNilUnit() {
		return false;
	}

	/**
	 * 
	 * @return
	 */
	public List<Datum> getValues() {
		return Collections.singletonList(this);
	}

	/**
	 * 
	 * @return
	 */
	public int howManyValues() {
		return 1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Datum getValueOf(int x) {
		if(x == 0) {
			return this;
		} else {
			throw new IndexOutOfBoundsException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isGrammarVariable() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();

		toDisplayString(b);
		return b.toString();
	}

}
