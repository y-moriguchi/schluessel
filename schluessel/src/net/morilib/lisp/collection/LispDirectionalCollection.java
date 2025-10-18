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

import java.util.NoSuchElementException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/05
 */
public interface LispDirectionalCollection extends LispCollection {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsDirectedCollection extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispDirectionalCollection);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionGetLeft extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionGetLeft(
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
			Datum th = Iterators.nextIf(itr, (Datum)null);

			checkType(d, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispDirectionalCollection c = (LispDirectionalCollection)d;

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
	public static class DirectedCollectionGetRight extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionGetRight(
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
			Datum th = Iterators.nextIf(itr, (Datum)null);

			checkType(d, mesg);
			if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispDirectionalCollection c = (LispDirectionalCollection)d;

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
	public static class DirectedCollectionInsertLeft
	extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionInsertLeft(
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
		protected Datum execute(Datum c1a,
				Datum c2a, Environment env, LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return ((LispDirectionalCollection)
						c1a).copyInsertFirst(c2a);
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionInsertLeftS
	extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionInsertLeftS(
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
		protected Datum execute(Datum c1a,
				Datum c2a, Environment env, LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return ((LispDirectionalCollection)
						c1a).insertFirst(c2a);
			} catch (ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch (ClassCastException e) {
				throw mesg.getError(e.getMessage(), c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionInsertRight
	extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionInsertRight(
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
		protected Datum execute(Datum c1a,
				Datum c2a, Environment env, LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return ((LispDirectionalCollection)
						c1a).copyInsertLast(c2a);
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionInsertRightS
	extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionInsertRightS(
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
		protected Datum execute(Datum c1a,
				Datum c2a, Environment env, LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return ((LispDirectionalCollection)c1a).insertLast(
						c2a);
			} catch (ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch (ClassCastException e) {
				throw mesg.getError(e.getMessage(), c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionDeleteLeft
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionDeleteLeft(
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
			Datum[] r2;

			checkType(c1a, mesg);
			try {
				r2 = ((LispDirectionalCollection)
						c1a).copyWithoutFirst();
				return MultiValues.newValues(r2);
			} catch(NoSuchElementException e) {
				throw mesg.getError("err.srfi44.nosuchelement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionDeleteLeftS
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionDeleteLeftS(
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
			Datum[] r2;

			checkType(c1a, mesg);
			try {
				r2 = ((LispDirectionalCollection)c1a).removeFirst();
				return MultiValues.newValues(r2);
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			} catch(NoSuchElementException e) {
				throw mesg.getError("err.srfi44.nosuchelement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionDeleteRight
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionDeleteRight(
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
			Datum[] r2;

			checkType(c1a, mesg);
			try {
				r2 = ((LispDirectionalCollection)
						c1a).copyWithoutLast();
				return MultiValues.newValues(r2);
			} catch(NoSuchElementException e) {
				throw mesg.getError("err.srfi44.nosuchelement", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DirectedCollectionDeleteRightS
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DirectedCollectionDeleteRightS(
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
			Datum[] r2;

			checkType(c1a, mesg);
			try {
				r2 = ((LispDirectionalCollection)c1a).removeLast();
				return MultiValues.newValues(r2);
			} catch(NoSuchElementException e) {
				throw mesg.getError("err.srfi44.nosuchelement", c1a);
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", c1a);
			}
		}

	}

	/**
	 * 
	 * @return
	 */
	public Datum first();

	/**
	 * 
	 * @return
	 */
	public Datum last();

	/**
	 * 
	 * @return
	 */
	public Datum insertFirst(Datum d) throws ImmutableException;

	/**
	 * 
	 * @return
	 */
	public Datum insertLast(Datum d) throws ImmutableException;

	/**
	 * 
	 * @return
	 */
	public Datum copyInsertFirst(Datum d);

	/**
	 * 
	 * @return
	 */
	public Datum copyInsertLast(Datum d);

	/**
	 * 
	 * @return
	 */
	public Datum[] removeFirst() throws ImmutableException;

	/**
	 * 
	 * @return
	 */
	public Datum[] removeLast() throws ImmutableException;

	/**
	 * 
	 * @return
	 */
	public Datum[] copyWithoutFirst();

	/**
	 * 
	 * @return
	 */
	public Datum[] copyWithoutLast();

}
