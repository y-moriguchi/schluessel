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

import java.util.Map;

import net.morilib.lisp.r6rs.LibraryID;
import net.morilib.lisp.r6rs.LibraryIDException;
import net.morilib.lisp.r6rs.SymbolEnv;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class EnvironmentObject extends Datum {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/22
	 */
	public static class R6RSEnvironment extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			Environment env2 = new Environment();
			Map<Symbol, SymbolEnv> mp;

			try {
				mp = LibraryID.compileImport(body);
			} catch (LibraryIDException e) {
				throw e.toLispException(mesg);
			}

			for(Map.Entry<Symbol, SymbolEnv> e : mp.entrySet()) {
				Environment env3 = e.getValue().getEnvironment();

				env2.bindDatumReadOnly(
						e.getValue().getSymbol(),
						env3.getDatum(e.getKey()));
			}
			return new EnvironmentObject(env2, false);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/23
	 */
	public static class BindEnvironmentS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			Map<Datum, Datum> maps;
			Environment eo;

			maps = LispUtils.assocToMap(c2a);
			if(maps == null) {
				throw mesg.getError("err.require.assoc", c2a);
			} else if(!(c1a instanceof EnvironmentObject)) {
				throw mesg.getError("err.environment", c1a);
			}

			eo = ((EnvironmentObject)c1a).getEnvironment();
			for(Map.Entry<Datum, Datum> e : maps.entrySet()) {
				Symbol s;

				if(!(e.getKey() instanceof Symbol)) {
					throw mesg.getError(
							"err.require.symbol", e.getKey());
				}
				s = (Symbol)e.getKey();
				eo.bindDatumReadOnly(s, e.getValue());
			}
			return c1a;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/16
	 */
	public static class FindEnvironment extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof EnvironmentObject)) {
				throw mesg.getError("err.environment", c1a);
			} else if(!(c2a instanceof Symbol)) {
				throw mesg.getError("err.require.symbol", c2a);
			} else {
				Datum d = ((EnvironmentObject)
						c1a).environment.findDatum((Symbol)c2a);

				return (d == null) ? LispBoolean.FALSE : d;
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/16
	 */
	public static class SetEnvironmentS extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			if(!(c1a instanceof EnvironmentObject)) {
				throw mesg.getError("err.environment", c1a);
			} else if(!(c2a instanceof Symbol)) {
				throw mesg.getError("err.require.symbol", c2a);
			} else {
				Environment e2 =
					((EnvironmentObject)c1a).environment;

				try {
					if(!e2.setDatum((Symbol)c2a, c3a)) {
						e2.bindDatum((Symbol)c2a, c3a);
					}
				} catch (ReadOnlyException e) {
					return LispBoolean.FALSE;
				}
				return LispBoolean.TRUE;
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/02/28
	 */
	public static class MakeEnvironment extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = Iterators.nextIf(itr);
			Datum e = Iterators.nextIf(itr);
			Datum x;
			Environment e2;
			Cons c;

			SubrUtils.checkTerminated(itr, body, mesg);
			if(e == null) {
				e2 = new Environment();
			} else if(e instanceof EnvironmentObject) {
				e2 = ((EnvironmentObject)e).environment;
			} else {
				throw mesg.getError("err.environment", e);
			}

			if(d != null) {
				itr = new ConsIterator(d);
				while(itr.hasNext()) {
					if((x = itr.next()) instanceof Cons) {
						c = (Cons)x;
						e2.bindDatum(c.getCar(), c.getCdr());
					}
				}
			}
			return new EnvironmentObject(e2, false);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/02/28
	 */
	public static class NextEnvironment extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof EnvironmentObject)) {
				throw mesg.getError("err.environment", c1a);
			} else {
				Environment e2 =
					((EnvironmentObject)c1a).environment;

				e2 = new Environment(e2);
				return new EnvironmentObject(e2, false);
			}
		}

	}

	//
	private Environment environment;
	private boolean inherit = false;

	//
	EnvironmentObject(
			Environment environment, boolean inherit) {
		if(environment == null) {
			throw new NullPointerException();
		}
		this.environment = environment;
		this.inherit = inherit;
	}

	//
	/*package*/ static EnvironmentObject newNullEnv(int ver) {
		return new EnvironmentObject(Scheme.newNullEnv(ver), false);
	}

	//
	/*package*/ static EnvironmentObject newRnRSEnv(int ver) {
		return new EnvironmentObject(Scheme.newRnRSEnv(ver), false);
	}

	//
	/*package*/ static EnvironmentObject newInteractionEnv(
			Environment e) {
		//return new EnvironmentObject(Scheme.newInteractionEnv(e, msg));
		return new EnvironmentObject(e, true);
	}

	/**
	 * 
	 * @return
	 */
	public Environment getEnvironment() {
		return environment;
	}

	//
	/*package*/ boolean isInherit() {
		return inherit;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<environment>");
	}

}
