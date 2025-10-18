/*
 * Copyright 2009 Yuichiro Moriguchi
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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.r6rs.LibraryID;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Environment {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/01
	 */
	public static class Apropos extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			String s = SubrUtils.nextSymbolName(itr, mesg, body);
			Datum  d = Iterators.nextIf(itr, (Datum)null);
			Environment e;

			SubrUtils.checkTerminated(itr, body, mesg);
			e = (d == null) ?
					env : LibraryID.getNamespace(new LibraryID(d));
			while(e != null) {
				for(SymbolName sym : e.binds.keySet()) {
					if(sym.getName().indexOf(s) >= 0) {
						System.out.println(sym.getName());
					}
				}
				e = e.rootenv;
			}
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/02
	 */
	public static class PreviousEnvironment extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			Environment e;
			int c = SubrUtils.getSmallInt(c2a, mesg), i;

			if(c < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						c2a);
			} else if(c1a instanceof EnvironmentObject) {
				e = ((EnvironmentObject)c1a).getEnvironment();
				for(i = 0; i < c && e != null; i++, e = e.rootenv);
				return (e != null) ?
						new EnvironmentObject(e, false) :
							LispBoolean.FALSE;
			} else {
				throw mesg.getError("err.environment", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/02
	 */
	public static class PreviousInteractionEnvironment
	extends UnaryArgs implements ILispDynamicSubr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.NoArgs#execute(net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			Environment e;
			int c = SubrUtils.getSmallInt(c1a, mesg), i;

			if(c < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						c1a);
			} else {
				e = env;
				for(i = 0; i < c && e != null; i++, e = e.rootenv);
				return (e != null) ?
						new EnvironmentObject(e, false) :
							LispBoolean.FALSE;
			}
		}

	}

	//
	private Environment rootenv;
	private Map<SymbolName, Datum> binds = Collections.synchronizedMap(
			new HashMap<SymbolName, Datum>());
	private Set<Datum> rdonly = Collections.synchronizedSet(
			new HashSet<Datum>());

	/**
	 * 
	 */
	public Environment() {
		rootenv = null;
	}

	//
	/*package*/ Environment(Environment rootenv) {
		this.rootenv = rootenv;
	}

	/**
	 * 
	 * @param sym
	 * @param d
	 */
	public void bindDatum(Datum sym, Datum d) {
		if(sym instanceof SymbolName) {
			binds.put((SymbolName)sym, d);
			if(d instanceof Subr) {
				((Subr)d).symbolName = ((SymbolName)sym).getName();
			}
		} else {
			//System.out.println(sym);
			throw new LispException("Parameter is not a symbol");
		}
	}

	//
	/*package*/ void bindDatumReadOnly(Datum sym, Datum d) {
		bindDatum(sym, d);
		rdonly.add(sym);
	}

	//
	/*package*/ void bindDatumWithoutScope(Datum sym, Datum d) {
		if(sym instanceof SymbolName) {
			binds.put(((SymbolName)sym).getSymbol(), d);
		} else {
			//System.out.println(sym);
			throw new LispException("Parameter is not a symbol");
		}
	}

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public Datum getDatum(Datum sym) {
		if(sym instanceof Symbol) {
			return binds.get(sym);
		} else {
			throw new LispException("Parameter is not a symbol");
		}
	}

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public Datum getDatumTop(Datum sym) {
		if(sym instanceof Symbol) {
			return getGlobal().binds.get(sym);
		} else {
			throw new LispException("Parameter is not a symbol");
		}
	}

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public Datum findDatum(Datum sym) {
		if(sym instanceof SymbolName) {
			Environment env = this;
			SymbolName s1 = (SymbolName)sym;

			while(env != null) {
				Datum f = env.binds.get(sym);

				if(f != null) {
					return f;
				} else if((f = env.binds.get(
						Symbol.DEFAULT_NAMESPACE.getSymbol(
								s1.getName()))) != null) {
					return f;
				}
				env = env.rootenv;
			}
			return null;
		} else {
			throw new LispException("Parameter is not a symbol");
		}
	}

	/**
	 * 
	 * @param sym
	 * @param d
	 * @return
	 * @throws ReadOnlyException
	 */
	public boolean setDatum(
			Datum sym, Datum d) throws ReadOnlyException {
		if(sym instanceof Symbol) {
			Environment env = this;
			Symbol s1 = (Symbol)sym;

			while(env != null) {
				Datum f = env.binds.get(sym);

				if(f != null) {
					if(rdonly.contains(sym)) {
						throw new ReadOnlyException();
					}
					env.binds.put((Symbol)sym, d);
					return true;
				} else {
					Symbol s0 = Symbol.DEFAULT_NAMESPACE.getSymbol(
							s1.getName());

					if((f = env.binds.get(s0)) != null) {
						if(rdonly.contains(sym)) {
							throw new ReadOnlyException();
						}
						env.binds.put(s0, d);
						return true;
					}
				}
				env = env.rootenv;
			}
			//throw new NotBoundException();
			return false;
		} else {
			throw new LispException("Parameter is not a symbol");
		}
	}

	/**
	 * 
	 * @return
	 */
	public Set<SymbolName> getBoundSymbols() {
		return Collections.unmodifiableSet(binds.keySet());
	}

	/**
	 * 
	 * @return
	 */
	public Map<SymbolName, Datum> getData() {
		return Collections.unmodifiableMap(binds);
	}

	//
	/*package*/ Environment getGlobal() {
		Environment env = this;

		while(env.rootenv != null) {
			env = env.rootenv;
		}
		return env;
	}

	//
	/*package*/ Environment getRootenv() {
		return rootenv;
	}

	//
	/*package*/ Environment copy() {
		Environment res = new Environment();

		res.rootenv = rootenv;
		res.binds   = new HashMap<SymbolName, Datum>(binds);
		return res;
	}

	//
	/*package*/ Environment copyNotRoot() {
		return (rootenv != null) ? copy() : this;
	}

	//
	private static Environment _copyExceptRoot(Environment e) {
		Environment er;

		if(e.rootenv != null) {
			er = new Environment(_copyExceptRoot(e.rootenv));
			er.binds = new HashMap<SymbolName, Datum>(e.binds);
			return er;
		} else {
			return e;
		}
	}

	//
	Environment copyExceptRoot() {
		return _copyExceptRoot(this);
	}

	//
	/*package*/ /*Environment copyChangeRoot(Environment rt) {
		Environment res = new Environment();

		res.rootenv = rt;
		res.binds   = new HashMap<Symbol, Datum>(binds);
		return res;
	}*/

	/**
	 * 
	 */
	public boolean isFreeVariable(Datum x) {
		return (x instanceof Symbol &&
				!((Symbol)x).isMacroBound() && findDatum(x) == null);
	}

	/**
	 * 
	 */
	public boolean isBoundVariable(Datum x) {
		return (x instanceof Symbol &&
				(((Symbol)x).isMacroBound() || findDatum(x) != null));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		Environment env = this;
		StringBuilder buf = new StringBuilder();

		while(env.rootenv != null) {
			buf.append(env.binds).append("->");
			env = env.rootenv;
		}
		buf.append("{Global}");
		return buf.toString();
	}

}
