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
package net.morilib.lisp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/20
 */
/*package*/ class LispLazySubrSyn extends Datum2 {

	//
	private Environment cenv;
	private Symbol symbol;
	private String className;

	/**
	 * 
	 * @param env
	 * @param k
	 * @param cn
	 */
	LispLazySubrSyn(Environment env, Symbol k, String cn) {
		this.cenv = env;
		this.symbol = k;
		this.className = cn;
	}

	//
	Datum setup() {
		Datum r;

		r = IntLispUtils.getJavaSubr(symbol, className);
		cenv.bindDatum(symbol, r);

		cenv = null;
		symbol = null;
		className = null;
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<subr-or-syn>");
	}

}
