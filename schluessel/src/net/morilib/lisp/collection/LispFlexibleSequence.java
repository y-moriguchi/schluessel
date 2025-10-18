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
package net.morilib.lisp.collection;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/05
 */
public interface LispFlexibleSequence
extends LispSequence, LispDirectionalCollection {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsFlexibleSequence extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispFlexibleSequence);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class FlexibleSequenceInsert extends TernaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public FlexibleSequenceInsert(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			int i = SubrUtils.getSmallInt(c2a, mesg);

			checkType(c1a, mesg);
			try {
				return ((LispFlexibleSequence)c1a).copyInsert(i, c3a);
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError(
						"err.srfi44.outofbounds",
						LispInteger.valueOf(i));
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), c3a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class FlexibleSequenceInsertS extends TernaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public FlexibleSequenceInsertS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			int i = SubrUtils.getSmallInt(c2a, mesg);

			checkType(c1a, mesg);
			try {
				return ((LispFlexibleSequence)c1a).insert(i, c3a);
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError(
						"err.srfi44.outofbounds",
						LispInteger.valueOf(i));
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), c3a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class FlexibleSequenceDeleteAt extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public FlexibleSequenceDeleteAt(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			int i = SubrUtils.getSmallInt(c2a, mesg);

			checkType(c1a, mesg);
			try {
				return ((LispFlexibleSequence)c1a).copyDelete(i);
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError(
						"err.srfi44.outofbounds",
						LispInteger.valueOf(i));
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class FlexibleSequenceDeleteAtS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public FlexibleSequenceDeleteAtS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			int i = SubrUtils.getSmallInt(c2a, mesg);

			checkType(c1a, mesg);
			try {
				return ((LispFlexibleSequence)c1a).delete(i);
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError(
						"err.srfi44.outofbounds",
						LispInteger.valueOf(i));
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			}
		}

	}

	/**
	 * 
	 * @param index
	 * @param d
	 * @return
	 */
	public Datum copyInsert(int index, Datum d);

	/**
	 * 
	 * @param index
	 * @param d
	 * @return
	 */
	public Datum insert(int index, Datum d) throws ImmutableException;

	/**
	 * 
	 * @param index
	 * @param d
	 * @return
	 */
	public Datum copyDelete(int index);

	/**
	 * 
	 * @param index
	 * @param d
	 * @return
	 */
	public Datum delete(int index) throws ImmutableException;

}
