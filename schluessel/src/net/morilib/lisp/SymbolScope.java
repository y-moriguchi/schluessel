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

import net.morilib.lang.Hashes;
import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
/*package*/ class SymbolScope extends Datum implements SymbolName {

	//
	private Symbol symbol;
	private UserSyntax scope;
	private boolean captured = false;

	//
	/*package*/ SymbolScope(
			Symbol symbol, UserSyntax scope, boolean cap) {
		if(symbol == null || scope == null) {
			throw new NullPointerException();
		}
		this.symbol = symbol;
		this.scope = scope;
		this.captured = cap;
	}

	//
	/*package*/ SymbolScope(Symbol symbol, UserSyntax scope) {
		this(symbol, scope, false);
	}

	/**
	 * @return the symbol
	 */
	public Symbol getSymbol() {
		return symbol;
	}

	/**
	 * 
	 * @return
	 */
	public UserSyntax getUserSyntax() {
		return scope;
	}

	/**
	 * 
	 * @param eenv
	 * @return
	 */
	public Environment getExecuteEnv(Environment eenv) {
		if(scope.getRootSyntax() != null) {
			Environment s2 =
				scope.getRootSyntax().getExecuteEnv();

			if(s2 != null) {
				return s2;
			} else {
				throw new RuntimeException(
						"environment has not been initialized");
			}
			//return eenv;
		} else if(scope.getExecuteEnv() == null) {
			throw new RuntimeException(
					"environment has not been initialized");
		}
		return scope.getExecuteEnv();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return symbol + ":" + scope;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeSymbol()
	 */
	public boolean isTypeSymbol() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SymbolName#getName()
	 */
	public String getName() {
		return symbol.getName();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.SYMBOL;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCaptured() {
		return captured;
	}

	//
	/*package*/ boolean isSameScope(SymbolScope s2) {
		return scope.getCompileEnv() == s2.scope.getCompileEnv();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = r * Hashes.A + symbol.hashCode();
		r = r * Hashes.A + scope.hashCode();
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof SymbolScope) {
			SymbolScope s = (SymbolScope)o;

			return symbol.equals(s.symbol) && scope.equals(s.scope);
		}
		return false;
	}

}
