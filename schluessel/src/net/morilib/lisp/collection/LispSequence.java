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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/05
 */
public interface LispSequence extends LispBag, ILispRef {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsSequence extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispSequence);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SequenceRef extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceRef(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			int   i  = SubrUtils.nextSmallInt(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);

			checkType(d, mesg);
			SubrUtils.checkTerminated(itr, body, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				try {
					return ((LispSequence)d).get(i);
				} catch(IndexOutOfBoundsException e) {
					if(th == null) {
						throw mesg.getError(
								"err.srfi44.outofbounds",
								LispInteger.valueOf(i));
					} else {
						return Scheme.callva(th, env, mesg);
					}
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SequenceGetLeft extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceGetLeft(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);

			checkType(d, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispSequence c = (LispSequence)d;

				if(c.size() > 0) {
					return c.first();
				} else if(th == null) {
					return LispBoolean.FALSE;
				} else {
					return Scheme.callva(th, env, mesg);
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SequenceGetRight extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceGetRight(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);

			checkType(d, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispSequence c = (LispSequence)d;

				if(c.size() > 0) {
					return c.last();
				} else if(c.size() < 0) {
					throw mesg.getError("err.srfi44.infinite", d);
				} else if(th == null) {
					return LispBoolean.FALSE;
				} else {
					return Scheme.callva(th, env, mesg);
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SequenceSet extends TernaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceSet(Class<?> cls, String err) {
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
				return ((LispSequence)c1a).copySet(i, c3a);
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
	public static class SequenceSetS extends TernaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceSetS(Class<?> cls, String err) {
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
				return ((LispSequence)c1a).set(i, c3a);
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError(
						"err.srfi44.outofbounds",
						LispInteger.valueOf(i));
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch(UnsupportedOperationException e) {
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
	public static class SequenceReplaceFrom extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceReplaceFrom(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum dd = SubrUtils.nextIf(itr, mesg, body);
			int   bd = SubrUtils.nextSmallInt(itr, mesg, body);
			Datum ss = SubrUtils.nextIf(itr, mesg, body);
			int   bs, es;

			checkType(dd, mesg);
			if(!(ss instanceof LispSequence)) {
				throw mesg.getError(
						"err.srfi44.require.sequence", ss);
			} else {
				LispSequence d = (LispSequence)dd;
				LispSequence s = (LispSequence)ss;

				bs = SubrUtils.nextSmallInt(itr, 0, mesg);
				es = SubrUtils.nextSmallInt(itr, s.size(), mesg);
				SubrUtils.checkTerminated(itr, body, mesg);
				try {
					return d.replace(s, bs, bd, es - bs);
				} catch(IndexOutOfBoundsException e) {
					throw mesg.getError(
							"err.srfi44.outofbounds", e.getMessage());
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), ss);
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SequenceReplaceFromS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceReplaceFromS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum dd = SubrUtils.nextIf(itr, mesg, body);
			int   bd = SubrUtils.nextSmallInt(itr, mesg, body);
			Datum ss = SubrUtils.nextIf(itr, mesg, body);
			int   bs, es;

			checkType(dd, mesg);
			if(!(ss instanceof LispSequence)) {
				throw mesg.getError(
						"err.srfi44.require.sequence", ss);
			} else {
				LispSequence d = (LispSequence)dd;
				LispSequence s = (LispSequence)ss;

				bs = SubrUtils.nextSmallInt(itr, 0, mesg);
				es = SubrUtils.nextSmallInt(itr, s.size(), mesg);
				SubrUtils.checkTerminated(itr, body, mesg);
				try {
					return d.arraycopy(s, bs, bd, es - bs);
				} catch(IndexOutOfBoundsException e) {
					throw mesg.getError(
							"err.srfi44.outofbounds", e.getMessage());
				} catch(ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", dd);
				} catch(UnsupportedOperationException e) {
					throw mesg.getError("err.srfi44.immutable", dd);
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), ss);
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SequenceCopy extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SequenceCopy(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum dd = SubrUtils.nextIf(itr, mesg, body);
			int   b, e;

			checkType(dd, mesg);
			LispSequence d = (LispSequence)dd;

			if(!itr.hasNext()) {
				return d.duplicate();
			}
			b = SubrUtils.nextSmallInt(itr, 0, mesg);
			e = SubrUtils.nextSmallInt(itr, d.size(), mesg);
			SubrUtils.checkTerminated(itr, body, mesg);
			try {
				if(e < b) {
					throw mesg.getError("err.range.invalid");
				}
				return d.copy(b, e);
			} catch(IndexOutOfBoundsException f) {
				throw mesg.getError(
						"err.srfi44.outofbounds", f.getMessage());
			}
		}

	}

	/**
	 * 
	 * @param index
	 * @return
	 */
	public Datum get(int index);

	/**
	 * @return
	 */
	public Datum first();

	/**
	 * @return
	 */
	public Datum last();

	/**
	 * 
	 * @param index
	 * @param d
	 * @return
	 */
	public Datum copySet(int index, Datum d);

	/**
	 * 
	 * @param index
	 * @param d
	 * @return
	 */
	public Datum set(int index, Datum d) throws ImmutableException;

	/**
	 * 
	 * @param src
	 * @param srcPos
	 * @param destPos
	 * @param len
	 * @return
	 */
	public Datum replace(LispSequence src, int srcPos,
			int destPos, int len);

	/**
	 * 
	 * @param src
	 * @param srcPos
	 * @param destPos
	 * @param len
	 * @return
	 */
	public Datum arraycopy(LispSequence src, int srcPos,
			int destPos, int len) throws ImmutableException;

	/**
	 * 
	 * @param b
	 * @param e
	 * @return
	 */
	public Datum copy(int b, int e);

}
