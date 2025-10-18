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

import java.nio.ByteOrder;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SyntaxSimple;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Endianness2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/16
 */
public class SynEndianness extends SyntaxSimple {

	/**
	 * 
	 */
	public static final Datum BIG_ENDIAN = Symbol.getSymbol("big");

	/**
	 * 
	 */
	public static final Datum LITTLE_ENDIAN =
			Symbol.getSymbol("little");

	//
	static Endianness2 getEndianness(Datum d, LispMessage mesg) {
		if(d.equals(BIG_ENDIAN)) {
			return Endianness2.BIG;
		} else if(d.equals(LITTLE_ENDIAN)) {
			return Endianness2.LITTLE;
		} else {
			throw mesg.getError("err.r6rs.endianness.invalid", d);
		}
	}

	//
	static Endianness2 getNative() {
		return ByteOrder.nativeOrder().equals(ByteOrder.BIG_ENDIAN) ?
				Endianness2.BIG : Endianness2.LITTLE;
	}

	/**
	 * 
	 * @return
	 */
	public static Datum getNativeSymbol() {
		return ByteOrder.nativeOrder().equals(ByteOrder.BIG_ENDIAN) ?
				BIG_ENDIAN : LITTLE_ENDIAN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SyntaxSimple#toDatum(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum toDatum(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextSymbolName(itr, mesg, body);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(s.equals("big")) {
			return BIG_ENDIAN;
		} else if(s.equals("little")) {
			return LITTLE_ENDIAN;
		} else {
			throw mesg.getError("err.r6rs.endianness.invalid", s);
		}
	}

}
