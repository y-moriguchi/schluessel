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
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.math.algebra.ILispRing;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.topology.ILispEnumerableTopology;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/05
 */
public interface LispSet extends LispCollection, ILispRing<LispSet>,
ILispEnumerableTopology {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsSet extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispSet);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetEquivalenceFunction extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetEquivalenceFunction(
				Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			checkType(c1a, mesg);
			return (Datum)((LispSet)c1a).equivalence();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsSubset extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public IsSubset(Class<?> cls, String err) {
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
			LispSet s = null;
			boolean r = true;

			while(itr.hasNext()) {
				Datum d = itr.next();

				if(s == null) {
					checkType(d, mesg);
					s = (LispSet)d;
				} else if(!(d instanceof LispSet)) {
					throw mesg.getError("err.srfi44.require.set", d);
				} else if(r) {
					r = s.subset((LispSet)d);
					//s = (LispSet)d;
				}
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			return LispBoolean.getInstance(r);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetAdd extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetAdd(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			return ((LispSet)c1a).copyAdd(c2a);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetAddS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetAddS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return ((LispSet)c1a).add(c2a);
			} catch (ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetDelete extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetDelete(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			return ((LispSet)c1a).copyDelete(c2a);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetDeleteS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetDeleteS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return ((LispSet)c1a).delete(c2a);
			} catch (ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/21
	 */
	public static abstract class SetOpBase extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetOpBase(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/**
		 * 
		 * @param x
		 * @return
		 */
		protected abstract LispSet operate(LispSet d,
				LispSet x) throws ImmutableException;

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = SubrUtils.nextIf(itr, mesg, body);
			LispSet r;

			checkType(d, mesg);
			r = (LispSet)d;
			while(itr.hasNext()) {
				Datum x = itr.next();

				if(x instanceof LispSet) {
					try {
						r = operate(r, (LispSet)x);
					} catch (ImmutableException e) {
						throw mesg.getError("err.srfi44.immutable", d);
					}
				} else {
					throw mesg.getError("err.srfi44.require.set", x);
				}
			}
			return (Datum)r;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetUnion extends SetOpBase {

		/**
		 * 
		 * @param cls
		 */
		public SetUnion(Class<?> cls, String err) {
			super(cls, err);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet.SetOpBase#operate(net.morilib.lisp.collection.LispSet)
		 */
		@Override
		protected LispSet operate(LispSet d, LispSet x) {
			return (LispSet)d.union(x);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetUnionS extends SetOpBase {

		/**
		 * 
		 * @param cls
		 */
		public SetUnionS(Class<?> cls, String err) {
			super(cls, err);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet.SetOpBase#operate(net.morilib.lisp.collection.LispSet)
		 */
		@Override
		protected LispSet operate(LispSet d,
				LispSet x) throws ImmutableException {
			return (LispSet)d.addAll(x);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetIntersection extends SetOpBase {

		/**
		 * 
		 * @param cls
		 */
		public SetIntersection(Class<?> cls, String err) {
			super(cls, err);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet.SetOpBase#operate(net.morilib.lisp.collection.LispSet)
		 */
		@Override
		protected LispSet operate(LispSet d, LispSet x) {
			return (LispSet)d.intersection(x);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetIntersectionS extends SetOpBase {

		/**
		 * 
		 * @param cls
		 */
		public SetIntersectionS(Class<?> cls, String err) {
			super(cls, err);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet.SetOpBase#operate(net.morilib.lisp.collection.LispSet)
		 */
		@Override
		protected LispSet operate(LispSet d,
				LispSet x) throws ImmutableException {
			return (LispSet)d.retainAll(x);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetDifference extends SetOpBase {

		/**
		 * 
		 * @param cls
		 */
		public SetDifference(Class<?> cls, String err) {
			super(cls, err);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet.SetOpBase#operate(net.morilib.lisp.collection.LispSet)
		 */
		@Override
		protected LispSet operate(LispSet d, LispSet x) {
			return (LispSet)d.difference(x);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetDifferenceS extends SetOpBase {

		/**
		 * 
		 * @param cls
		 */
		public SetDifferenceS(Class<?> cls, String err) {
			super(cls, err);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet.SetOpBase#operate(net.morilib.lisp.collection.LispSet)
		 */
		@Override
		protected LispSet operate(LispSet d,
				LispSet x) throws ImmutableException {
			return (LispSet)d.removeAll(x);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetSymmetricDifference extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetSymmetricDifference(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			if(!(c2a instanceof LispSet)) {
				throw mesg.getError("err.srfi44.require.set", c2a);
			} else {
				return ((LispSet)c1a).symmetricDifference(
						(LispSet)c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetSymmetricDifferenceS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetSymmetricDifferenceS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			if(!(c2a instanceof LispSet)) {
				throw mesg.getError("err.srfi44.require.set", c2a);
			} else {
				try {
					return ((LispSet)c1a).symmetricDifferenceModify(
							(LispSet)c2a);
				} catch (ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", c1a);
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
	public static class SetAddFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetAddFrom(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			if(!(c2a instanceof LispBag)) {
				throw mesg.getError("err.srfi44.require.bag", c2a);
			} else {
				return ((LispSet)c1a).copyAddFrom((LispBag)c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetAddFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetAddFromS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			if(!(c2a instanceof LispBag)) {
				throw mesg.getError("err.srfi44.require.bag", c2a);
			} else {
				try {
					return ((LispSet)c1a).addFrom((LispBag)c2a);
				} catch (ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", c1a);
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
	public static class SetDeleteFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetDeleteFrom(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			if(!(c2a instanceof LispBag)) {
				throw mesg.getError("err.srfi44.require.bag", c2a);
			} else {
				return ((LispSet)c1a).copyDeleteFrom((LispBag)c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class SetDeleteFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public SetDeleteFromS(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			checkType(c1a, mesg);
			if(!(c2a instanceof LispBag)) {
				throw mesg.getError("err.srfi44.require.bag", c2a);
			} else {
				try {
					return ((LispSet)c1a).deleteFrom((LispBag)c2a);
				} catch (ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", c1a);
				}
			}
		}

	}

	/**
	 * 
	 * @return
	 */
	public Procedure equivalence();

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean equivalence(Datum a, Datum b);

	/**
	 * 
	 * @param sets
	 * @return
	 */
	public boolean subset(LispSet set);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public Datum copyAdd(Datum d);

	/**
	 * 
	 * @param d
	 * @return
	 * @throws ImmutableException
	 */
	public Datum add(Datum d) throws ImmutableException;

	/**
	 * 
	 * @param d
	 * @return
	 */
	public Datum copyDelete(Datum d);

	/**
	 * 
	 * @param d
	 * @return
	 * @throws ImmutableException
	 */
	public Datum delete(Datum d) throws ImmutableException;

	/**
	 * 
	 * @param s
	 * @return
	 */
	public Datum union(LispSet s);

	/**
	 * 
	 * @param s
	 * @return
	 * @throws ImmutableException
	 */
	public Datum addAll(LispSet s) throws ImmutableException;

	/**
	 * 
	 * @param s
	 * @return
	 */
	public Datum intersection(LispSet s);

	/**
	 * 
	 * @param s
	 * @return
	 * @throws ImmutableException
	 */
	public Datum retainAll(LispSet s) throws ImmutableException;

	/**
	 * 
	 * @param s
	 * @return
	 */
	public Datum difference(LispSet s);

	/**
	 * 
	 * @param s
	 * @return
	 * @throws ImmutableException
	 */
	public Datum removeAll(LispSet s) throws ImmutableException;

	/**
	 * 
	 * @param s
	 * @return
	 */
	public Datum symmetricDifference(LispSet s);

	/**
	 * 
	 * @param s
	 * @return
	 * @throws ImmutableException
	 */
	public Datum symmetricDifferenceModify(
			LispSet s) throws ImmutableException;

	/**
	 * 
	 * @param d
	 * @return
	 */
	public Datum copyAddFrom(LispBag d);

	/**
	 * 
	 * @param d
	 * @return
	 * @throws ImmutableException
	 */
	public Datum addFrom(LispBag d) throws ImmutableException;

	/**
	 * 
	 * @param d
	 * @return
	 */
	public Datum copyDeleteFrom(LispBag d);

	/**
	 * 
	 * @param d
	 * @return
	 * @throws ImmutableException
	 */
	public Datum deleteFrom(LispBag d) throws ImmutableException;

}
