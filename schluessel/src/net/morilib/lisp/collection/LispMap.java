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
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.SExpressionDatum;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/05
 */
public interface LispMap
extends LispCollection, LispEntryEnumeration, ILispRef {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsMap extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispMap);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapEquivalenceFunction
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapEquivalenceFunction(Class<?> cls, String err) {
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
			return (Datum)((LispMap)c1a).valueEquivalence();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapKeyEquivalenceFunction
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapKeyEquivalenceFunction(Class<?> cls, String err) {
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
			return (Datum)((LispMap)c1a).keyEquivalence();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapCount extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapCount(Class<?> cls, String err) {
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
			return LispInteger.valueOf(
					((LispMap)c1a).countValue(c2a));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsMapContainsKey extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public IsMapContainsKey(Class<?> cls, String err) {
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
		protected Datum execute(Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			checkType(c1a, mesg);
			try {
				return LispBoolean.getInstance(
						((LispMap)c1a).containsKey(c2a));
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
	public static class MapKeysToList extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapKeysToList(Class<?> cls, String err) {
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
			return ((LispMap)c1a).keysToList();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapEq extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapEq(Class<?> cls, String err) {
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
			Datum p = SubrUtils.nextIf(itr, mesg, body);
			LispMap d = null;
			boolean r = true;

			if(!(p instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", p);
			}

			while(itr.hasNext()) {
				Datum e = itr.next();

				checkType(e, mesg);
				if(d == null) {
					d = (LispMap)e;
				} else if(!(e instanceof LispCollection)) {
					throw mesg.getError(
							"err.srfi44.require.collection", e);
				} else if(r) {
					r = d.equalToMap((LispMap)e, (Procedure)p, env,
							mesg);
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
	public static class MapGet extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapGet(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);
			Datum r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			try {
				if((r = ((LispMap)d).get(k)) != null) {
					return r;
				} else if(th == null) {
					return LispBoolean.FALSE;
				} else if(th instanceof Procedure) {
					return Scheme.callva(th, env, mesg);
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), k);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapPut extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapPut(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum v  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);
			Datum[] r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			try {
				r = ((LispMap)d).copyPut(k, v);
				if(r[1] != null) {
					return MultiValues.newValues(r);
				} else if(th == null) {
					return MultiValues.newValues(
							r[0], LispBoolean.FALSE);
				} else if(th instanceof Procedure) {
					return MultiValues.newValues(r[0],
							Scheme.callva(th, env, mesg));
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), k);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapPutS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapPutS(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum v  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);
			Datum[] r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			try {
				r = ((LispMap)d).put(k, v);
				if(r[1] != null) {
					return MultiValues.newValues(r);
				} else if(th == null) {
					return MultiValues.newValues(
							r[0], LispBoolean.FALSE);
				} else if(th instanceof Procedure) {
					return MultiValues.newValues(r[0],
							Scheme.callva(th, env, mesg));
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), k);
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", d);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapUpdate extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapUpdate(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum f  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);
			Datum x, y;
			Datum[] r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			SubrUtils.checkType(f, Procedure.class, mesg,
					"err.require.procedure");
			try {
				x = ((LispMap)d).get(k);
				if(x != null) {
					y = Scheme.callva(f, env, mesg, x);
				} else if(th == null) {
					y = LispBoolean.FALSE;
				} else if(th instanceof Procedure) {
					y = Scheme.callva(th, env, mesg);
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
	
				r = ((LispMap)d).copyPut(k, y);
				return r[0];
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), k);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapUpdateS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapUpdateS(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum f  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);
			Datum x, y;
			Datum[] r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			SubrUtils.checkType(f, Procedure.class, mesg,
					"err.require.procedure");
			try {
				x = ((LispMap)d).get(k);
				if(x != null) {
					y = Scheme.callva(f, env, mesg, x);
				} else if(th == null) {
					y = LispBoolean.FALSE;
				} else if(th instanceof Procedure) {
					y = Scheme.callva(th, env, mesg);
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
	
				r = ((LispMap)d).put(k, y);
				return r[0];
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), k);
			} catch(ImmutableException e) {
				throw mesg.getError("err.srfi44.immutable", d);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapDelete extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapDelete(Class<?> cls, String err) {
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
				return ((LispMap)c1a).copyDeleteKey(c2a);
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
	public static class MapDeleteS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapDeleteS(Class<?> cls, String err) {
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
				return ((LispMap)c1a).deleteKey(c2a);
			} catch(ClassCastException e) {
				throw mesg.getError(e.getMessage(), c2a);
			} catch(ImmutableException e) {
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
	public static class MapDeleteFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapDeleteFrom(Class<?> cls, String err) {
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
			if(c2a instanceof LispBag) {
				try {
					return ((LispMap)c1a).copyDeleteFromKey(
							(LispBag)c2a);
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), c2a);
				}
			} else {
				throw mesg.getError("err.srfi44.require.bag", c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapDeleteFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapDeleteFromS(Class<?> cls, String err) {
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
			if(c2a instanceof LispBag) {
				try {
					return ((LispMap)c1a).deleteFromKey((LispBag)c2a);
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), c2a);
				} catch(ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", c2a);
				}
			} else {
				throw mesg.getError("err.srfi44.require.bag", c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapAddFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapAddFrom(Class<?> cls, String err) {
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
			if(c2a instanceof LispMap) {
				try {
					return ((LispMap)c1a).copyAddFrom((LispMap)c2a);
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), c2a);
				}
			} else {
				throw mesg.getError("err.srfi44.require.map", c2a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class MapAddFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public MapAddFromS(Class<?> cls, String err) {
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
			if(c2a instanceof LispMap) {
				try {
					return ((LispMap)c1a).addFrom((LispMap)c2a);
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), c2a);
				} catch(ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", c2a);
				}
			} else {
				throw mesg.getError("err.srfi44.require.map", c2a);
			}
		}

	}

	/**
	 * 
	 * @return
	 */
	public Procedure keyEquivalence();

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean equivalenceKey(Datum a, Datum b);

	/**
	 * 
	 * @return
	 */
	public Procedure valueEquivalence();

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean equivalenceValue(Datum a, Datum b);

	/**
	 * 
	 * @param k
	 * @return
	 */
	public boolean containsKey(Datum k);

	/**
	 * 
	 * @return
	 */
	public SExpressionDatum keysToList();

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum get(Datum k);

	/**
	 * 
	 * @param k
	 * @param v
	 * @return
	 */
	public Datum[] copyPut(Datum k, Datum v);

	/**
	 * 
	 * @param k
	 * @param v
	 * @return
	 */
	public Datum[] put(Datum k, Datum v) throws ImmutableException;

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum copyDeleteKey(Datum k);

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum deleteKey(Datum k) throws ImmutableException;

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum copyDeleteFromKey(LispBag k);

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum deleteFromKey(LispBag k) throws ImmutableException;

	/**
	 * 
	 * @param m
	 * @return
	 */
	public Datum copyAddFrom(LispMap m);

	/**
	 * 
	 * @param m
	 * @return
	 */
	public Datum addFrom(LispMap m) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public int countValue(Datum d);

	/**
	 * @param e
	 * @param p
	 * @param env
	 * @param mesg
	 * @return
	 */
	public boolean equalToMap(LispMap e, Procedure p, Environment env,
			LispMessage mesg);

}
