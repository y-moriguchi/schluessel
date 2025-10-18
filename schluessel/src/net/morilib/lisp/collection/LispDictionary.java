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
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.SExpressionDatum;
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
public interface LispDictionary
extends LispCollection, LispEntryEnumeration {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsDictionary extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispDictionary);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DictionaryEquivalenceFunction
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryEquivalenceFunction(
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
			return (Datum)((LispDictionary)c1a).valueEquivalence();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DictionaryKeyEquivalenceFunction
	extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryKeyEquivalenceFunction(
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
			return (Datum)((LispDictionary)c1a).keyEquivalence();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DictionaryCount extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryCount(Class<?> cls, String err) {
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
					((LispDictionary)c1a).countValue(c2a));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DictionaryKeyCount extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryKeyCount(Class<?> cls, String err) {
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
					((LispDictionary)c1a).countKey(c2a));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class IsDictionaryContainsKey extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public IsDictionaryContainsKey(Class<?> cls, String err) {
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
						((LispDictionary)c1a).containsKey(c2a));
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
	public static class DictionaryKeysToList extends UnaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryKeysToList(Class<?> cls, String err) {
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
			return ((LispDictionary)c1a).keysToList();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DictionaryEq extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryEq(Class<?> cls, String err) {
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
			LispDictionary d = null;
			boolean r = true;

			if(!(p instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", p);
			}

			while(itr.hasNext()) {
				Datum e = itr.next();

				checkType(e, mesg);
				if(d == null) {
					d = (LispDictionary)e;
				} else if(!(e instanceof LispCollection)) {
					throw mesg.getError(
							"err.srfi44.require.collection", e);
				} else if(r) {
					r = d.equalToDictionary((LispDictionary)e,
							(Procedure)p, env, mesg);
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
	public static class DictionaryGet extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryGet(Class<?> cls, String err) {
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
				if((r = ((LispDictionary)d).get(k)) != null) {
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
	public static class DictionaryGetAll extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryGetAll(Class<?> cls, String err) {
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
//			Datum th = Iterators.nextIf(itr, null);
//			Datum r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			return ((LispDictionary)d).getAllAsList(k);
//			try {
//				if((r = ((LispDictionary)d).getAllAsList(k)) != null) {
//					return r;
//				} else if(th == null) {
//					return LispBoolean.FALSE;
//				} else if(th instanceof Procedure) {
//					return Scheme.callva(th, env, mesg);
//				} else {
//					throw mesg.getError("err.require.procedure", th);
//				}
//			} catch(ClassCastException e) {
//				throw mesg.getError(e.getMessage(), k);
//			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/05
	 */
	public static class DictionaryPut extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryPut(Class<?> cls, String err) {
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
				r = ((LispDictionary)d).copyPut(k, v);
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
	public static class DictionaryPutS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryPutS(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum v  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);
			Datum[] r;

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			try {
				r = ((LispDictionary)d).put(k, v);
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
	public static class DictionaryReplaceAll extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryReplaceAll(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum v  = itr.rest();

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			try {
				return ((LispDictionary)d).copyReplaceAll(
						k, LispUtils.toArray(v, mesg));
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
	public static class DictionaryReplaceAllS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryReplaceAllS(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum v  = itr.rest();

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			try {
				return ((LispDictionary)d).replaceAll(
						k, LispUtils.toArray(v, mesg));
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
	public static class DictionaryUpdate extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryUpdate(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
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
				x = ((LispDictionary)d).get(k);
				if(x != null) {
					y = Scheme.callva(f, env, mesg, x);
				} else if(th == null) {
					y = LispBoolean.FALSE;
				} else if(th instanceof Procedure) {
					y = Scheme.callva(th, env, mesg);
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
	
				r = ((LispDictionary)d).copyPut(k, y);
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
	public static class DictionaryUpdateS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryUpdateS(Class<?> cls, String err) {
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
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
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
				x = ((LispDictionary)d).get(k);
				if(x != null) {
					y = Scheme.callva(f, env, mesg, x);
				} else if(th == null) {
					y = LispBoolean.FALSE;
				} else if(th instanceof Procedure) {
					y = Scheme.callva(th, env, mesg);
				} else {
					throw mesg.getError("err.require.procedure", th);
				}
	
				r = ((LispDictionary)d).put(k, y);
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
	public static class DictionaryUpdateAll extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryUpdateAll(Class<?> cls, String err) {
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

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			SubrUtils.checkType(f, Procedure.class, mesg,
					"err.require.procedure");
			try {
				if(th != null && !(th instanceof Procedure)) {
					throw mesg.getError("err,require.procedure", th);
				}
				return ((LispDictionary)d).copyUpdateAll(k,
						(Procedure)f, (Procedure)th, env, mesg);
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
	public static class DictionaryUpdateAllS extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryUpdateAllS(Class<?> cls, String err) {
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

			SubrUtils.checkTerminated(itr, body, mesg);
			checkType(d, mesg);
			SubrUtils.checkType(f, Procedure.class, mesg,
					"err.require.procedure");
			try {
				if(th != null && !(th instanceof Procedure)) {
					throw mesg.getError("err,require.procedure", th);
				}
				return ((LispDictionary)d).updateAll(k,
						(Procedure)f, (Procedure)th, env, mesg);
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
	public static class DictionaryDelete extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDelete(Class<?> cls, String err) {
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
				return ((LispDictionary)c1a).copyDeleteKey(c2a);
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
	public static class DictionaryDeleteS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteS(Class<?> cls, String err) {
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
				return ((LispDictionary)c1a).deleteKey(c2a);
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
	public static class DictionaryDeleteAll extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteAll(Class<?> cls, String err) {
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
				return ((LispDictionary)c1a).copyDeleteAllKey(c2a);
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
	public static class DictionaryDeleteAllS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteAllS(Class<?> cls, String err) {
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
				return ((LispDictionary)c1a).deleteAllKey(c2a);
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
	public static class DictionaryDeleteFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteFrom(Class<?> cls, String err) {
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
					return ((LispDictionary)
							c1a).copyDeleteFromKey((LispBag)c2a);
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
	public static class DictionaryDeleteFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteFromS(Class<?> cls, String err) {
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
					return ((LispDictionary)
							c1a).deleteFromKey((LispBag)c2a);
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
	public static class DictionaryDeleteAllFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteAllFrom(Class<?> cls, String err) {
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
					return ((LispDictionary)
							c1a).copyDeleteAllFromKey((LispBag)c2a);
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
	public static class DictionaryDeleteAllFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryDeleteAllFromS(Class<?> cls, String err) {
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
					return ((LispDictionary)
							c1a).deleteAllFromKey((LispBag)c2a);
				} catch(ClassCastException e) {
					throw mesg.getError(e.getMessage(), c2a);
				} catch(ImmutableException e) {
					throw mesg.getError("err.srfi44.immutable", c1a);
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
	public static class DictionaryAddFrom extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryAddFrom(Class<?> cls, String err) {
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
			if(c2a instanceof LispDictionary) {
				try {
					return ((LispDictionary)c1a).copyAddFrom(
							(LispDictionary)c2a);
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
	public static class DictionaryAddFromS extends BinaryArgs {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public DictionaryAddFromS(Class<?> cls, String err) {
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
			if(c2a instanceof LispDictionary) {
				try {
					return ((LispDictionary)c1a).addFrom(
							(LispDictionary)c2a);
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
	 * @param k
	 * @return
	 */
	public SExpressionDatum getAllAsList(Datum k);

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
	 * @param k
	 * @param f
	 * @param th
	 * @return
	 */
	public Datum copyUpdateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg);

	/**
	 * @param k
	 * @param f
	 * @param th
	 * @return
	 */
	public Datum updateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public Datum copyDeleteKey(Datum c2a);

	/**
	 * @param c2a
	 * @return
	 */
	public Datum deleteKey(Datum c2a) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public Datum copyDeleteAllKey(Datum c2a);

	/**
	 * @param c2a
	 * @return
	 */
	public Datum deleteAllKey(Datum c2a) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public Datum copyDeleteFromKey(LispBag c2a);

	/**
	 * @param c2a
	 * @return
	 */
	public Datum deleteFromKey(LispBag c2a) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public Datum copyDeleteAllFromKey(LispBag c2a);

	/**
	 * @param c2a
	 * @return
	 */
	public Datum deleteAllFromKey(
			LispBag c2a) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public Datum copyAddFrom(LispDictionary c2a);

	/**
	 * @param c2a
	 * @return
	 */
	public Datum addFrom(LispDictionary c2a) throws ImmutableException;

	/**
	 * @param k
	 * @param array
	 * @return
	 */
	public Datum copyReplaceAll(Datum k, Datum[] array);

	/**
	 * @param k
	 * @param array
	 * @return
	 */
	public Datum replaceAll(Datum k,
			Datum[] array) throws ImmutableException;

	/**
	 * @param c2a
	 * @return
	 */
	public int countValue(Datum d);

	/**
	 * @param c2a
	 * @return
	 */
	public int countKey(Datum d);

	/**
	 * @param e
	 * @param p
	 * @param env
	 * @param mesg
	 * @return
	 */
	public boolean equalToDictionary(LispDictionary e, Procedure p,
			Environment env,LispMessage mesg);

}
