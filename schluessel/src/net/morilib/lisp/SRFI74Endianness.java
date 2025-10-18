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
package net.morilib.lisp;

import java.nio.ByteOrder;
import java.util.List;

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.LispCompiler.MiscInfo;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/11
 */
public class SRFI74Endianness extends Syntax {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/12
	 */
	public static class Endian extends Datum2 {

		//
		private ByteOrder bo;

		//
		private Endian(ByteOrder bo) {
			this.bo = bo;
		}

		/**
		 * 
		 * @return
		 */
		public ByteOrder getByteOrder() {
			return bo;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			if(bo.equals(ByteOrder.BIG_ENDIAN)) {
				buf.append("#<endianness big>");
			} else {
				buf.append("#<endianness little>");
			}
		}

	}

	/**
	 * 
	 */
	public static final Datum BIG_ENDIAN =
		new Endian(ByteOrder.BIG_ENDIAN);

	/**
	 * 
	 */
	public static final Datum LITTLE_ENDIAN =
		new Endian(ByteOrder.LITTLE_ENDIAN);

	/**
	 * 
	 * @return
	 */
	public static final Datum getNative() {
		return ByteOrder.BIG_ENDIAN.equals(ByteOrder.nativeOrder()) ?
				BIG_ENDIAN : LITTLE_ENDIAN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#compile(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.CompiledCode.Builder, boolean, net.morilib.lisp.Cons, boolean, net.morilib.lisp.LispMessage, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
	 */
	@Override
	/*package*/ void compile(Datum body, Environment env,
			LispCompiler comp, Builder build, boolean toplevel,
			Cons callsym, boolean istail, LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento,
			MiscInfo syncased) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextSymbolName(itr, mesg, body);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(s.equals("big")) {
			build.addPush(BIG_ENDIAN);
		} else if(s.equals("little")) {
			build.addPush(LITTLE_ENDIAN);
		} else if(s.equals("native")) {
			build.addPush(getNative());
		} else {
			throw mesg.getError("err.srfi74.endianness.invalid", s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#replaceLocalVals(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage, boolean, int)
	 */
	@Override
	/*package*/ Datum replaceLocalVals(Datum body, Environment env,
			LispCompiler comp, Environment ienv, LispMessage mesg,
			boolean toplv, int ttype) {
		return body;
	}

}
