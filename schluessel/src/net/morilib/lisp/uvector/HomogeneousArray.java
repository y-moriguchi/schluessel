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
package net.morilib.lisp.uvector;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Undef;
import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public interface HomogeneousArray extends ILispVector, ILispRef {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class VectorLength extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof HomogeneousArray) {
				return LispInteger.valueOf(
						((HomogeneousArray)c1a).size());
			} else {
				throw mesg.getError("err.uvector.require", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class VectorRef extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof HomogeneousArray) {
				HomogeneousArray v = (HomogeneousArray)c1a;
				int i = SubrUtils.getSmallInt(c2a, mesg);

				if(i < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", c2a);
				} else if(i >= v.size()) {
					throw mesg.getError("err.vector.outofrange");
				}
				return v.get(i);
			} else {
				throw mesg.getError("err.uvector.require", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class VectorSetS extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Datum c3a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof HomogeneousArray) {
				HomogeneousArray h = ((HomogeneousArray)c1a);
				int i = SubrUtils.getSmallInt(c2a, mesg);

				if(i < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", c2a);
				} else if(i >= h.size()) {
					throw mesg.getError("err.vector.outofrange");
				}

				h.checkRange((LispReal)c3a, mesg);
				h.set(i, (LispReal)c3a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.uvector.require", c1a);
			}
		}

	}

	/**
	 * 
	 * @param d1
	 * @param d2
	 * @return
	 */
	public boolean equalsArray(Datum d1, Datum d2);

	/**
	 * 
	 * @param index
	 * @param x
	 */
	public void set(int index, LispReal x);

	/**
	 * 
	 * @param index
	 * @param x
	 */
	public void set(int index, int x);

	/**
	 * 
	 * @param index
	 * @param x
	 */
	public void set(int index, long x);

	/**
	 * 
	 * @param index
	 * @param x
	 */
	public void set(int index, double x);

	/**
	 * 
	 * @param x
	 */
	public void checkRange(LispReal x, LispMessage mesg);

}
