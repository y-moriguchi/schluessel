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
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/05
 */
public interface LispOrderedCollection extends LispCollection {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsOrderedCollection extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispOrderedCollection);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class OrderedCollectionOrderingFunction
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionOrderingFunction(
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
			return (Datum)((LispOrderedCollection)c1a).ordering();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class OrderedCollectionGetLeft extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionGetLeft(
				Class<?> cls, String err) {
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
			Datum th = Iterators.nextIf(itr);

			checkType(d, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispOrderedCollection c = (LispOrderedCollection)d;

				if(c.size() > 0) {
					try {
						return c.first();
					} catch (NoBoundsException e) {
						throw new RuntimeException(e);
					}
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
	public static class OrderedCollectionGetRight extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionGetRight(
				Class<?> cls, String err) {
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
			Datum th = Iterators.nextIf(itr);

			checkType(d, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispOrderedCollection c = (LispOrderedCollection)d;

				if(c.size() > 0) {
					try {
						return c.last();
					} catch (NoBoundsException e) {
						throw new RuntimeException(e);
					}
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
	public static class OrderedCollectionDeleteLeft
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionDeleteLeft(
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
			Datum r2, r3;

			checkType(c1a, mesg);
			try {
				r3 = ((LispOrderedCollection)c1a).first();
				r2 = ((LispOrderedCollection)c1a).copyWithoutFirst();
				return MultiValues.newValues(r2, r3);
			} catch (NoBoundsException e) {
				throw mesg.getError("err.srfi44.infinite", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class OrderedCollectionDeleteLeftS
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionDeleteLeftS(
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
			Datum r2, r3;

			checkType(c1a, mesg);
			try {
				r3 = ((LispOrderedCollection)c1a).first();
				r2 = ((LispOrderedCollection)c1a).removeFirst();
				return MultiValues.newValues(r2, r3);
			} catch (ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch (NoBoundsException e) {
				throw mesg.getError("err.srfi44.infinite", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class OrderedCollectionDeleteRight
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionDeleteRight(
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
			Datum r2, r3;

			checkType(c1a, mesg);
			try {
				r3 = ((LispOrderedCollection)c1a).last();
				r2 = ((LispOrderedCollection)c1a).copyWithoutLast();
				return MultiValues.newValues(r2, r3);
			} catch (NoBoundsException e) {
				throw mesg.getError("err.srfi44.infinite", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class OrderedCollectionDeleteRightS
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public OrderedCollectionDeleteRightS(
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
			Datum r2, r3;

			checkType(c1a, mesg);
			try {
				r3 = ((LispOrderedCollection)c1a).last();
				r2 = ((LispOrderedCollection)c1a).removeLast();
				return MultiValues.newValues(r2, r3);
			} catch (ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch (NoBoundsException e) {
				throw mesg.getError("err.srfi44.infinite", c1a);
			}
		}

	}

	/**
	 * 
	 * @return
	 */
	public Procedure ordering();

	/**
	 * 
	 * @return
	 */
	public Datum first() throws NoBoundsException;

	/**
	 * 
	 * @return
	 */
	public Datum last() throws NoBoundsException;

	/**
	 * 
	 * @return
	 */
	public Datum removeFirst(
			) throws ImmutableException, NoBoundsException;

	/**
	 * 
	 * @return
	 */
	public Datum removeLast(
			) throws ImmutableException, NoBoundsException;

	/**
	 * 
	 * @return
	 */
	public Datum copyWithoutFirst() throws NoBoundsException;

	/**
	 * 
	 * @return
	 */
	public Datum copyWithoutLast() throws NoBoundsException;

}
