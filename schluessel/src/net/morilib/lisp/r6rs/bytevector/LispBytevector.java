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
package net.morilib.lisp.r6rs.bytevector;

import java.util.Arrays;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/16
 */
public class LispBytevector extends Datum2
implements Cloneable, ILispRef, java.io.Serializable {

	//
	byte[] vector;

	/**
	 * 
	 * @param vector
	 */
	protected LispBytevector(byte[] vector) {
		this.vector = vector;
	}

	/**
	 * 
	 * @param len
	 * @param fill
	 */
	public LispBytevector(int len, byte fill) {
		vector = new byte[len];
		Arrays.fill(vector, fill);
	}

	/**
	 * 
	 * @param v
	 */
	public LispBytevector(LispBytevector v) {
		this(Arrays.copyOf(v.vector, v.vector.length));
	}

	//
	static LispBytevector datumToBytevector(Datum d,
			LispMessage mesg) {
		if(d instanceof LispBytevector) {
			return (LispBytevector)d;
		} else {
			throw mesg.getError("err.r6rs.require.bytevector", d);
		}
	}

	//
	static byte datumToByteOrOctet(Datum d, LispMessage mesg) {
		int fl;

		if(!(d instanceof LispSmallInt)) {
			throw mesg.getError("err.r6rs.require.byteoroctet", d);
		} else if((fl = ((LispSmallInt)d).getInt()) < -128 ||
				fl > 255) {
			throw mesg.getError("err.r6rs.require.byteoroctet", d);
		} else if(fl > 127) {
			fl = fl - 256;
		}
		return (byte)fl;
	}

	/**
	 * 
	 * @return
	 */
	public int length() {
		return vector.length;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public boolean isEqualTo(LispBytevector v) {
		return Arrays.equals(vector, v.vector);
	}

	/**
	 * 
	 * @param b
	 * @param start
	 * @param end
	 */
	public void fill(byte b, int start, int end) {
		Arrays.fill(vector, start, end, b);
	}

	/**
	 * 
	 * @param srcPos
	 * @param dest
	 * @param destPos
	 * @param length
	 */
	public void copy(int srcPos, LispBytevector dest, int destPos,
			int length) {
		System.arraycopy(vector, srcPos, dest.vector, destPos, length);
	}

	/**
	 * 
	 * @param start
	 * @param end
	 */
	public LispBytevector duplicate(int start, int end) {
		return new LispBytevector(
				Arrays.copyOfRange(vector, start, end));
	}

	/**
	 * 
	 * @param k
	 * @return
	 */
	public byte get(int k) {
		return vector[k];
	}

	/**
	 * 
	 * @param k
	 * @param b
	 */
	public void set(int k, byte b) {
		vector[k] = b;
	}

	/**
	 * 
	 * @return
	 */
	public Datum toList() {
		ConsListBuilder d = new ConsListBuilder();

		for(byte b : vector) {
			d.append(LispInteger.valueOf((b < 0) ? b + 256 : b));
		}
		return d.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#clone()
	 */
	@Override
	public LispBytevector clone() {
		return new LispBytevector(this);
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toByteArray() {
		return Arrays.copyOf(vector, vector.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getSmallInt(arg, mesg);

		if(k < 0 || k >= vector.length) {
			throw mesg.getError("err.accessor.ref.outofrange", arg);
		}
		return LispInteger.valueOf((byte)vector[k]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#vb8(");
		for(int i = 0; i < vector.length; i++) {
			if(i > 0)  buf.append(" ");
			buf.append(vector[i]);
		}
		buf.append(")");
	}

}
