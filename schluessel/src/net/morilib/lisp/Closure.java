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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Closure extends Settable implements Procedure {

	//
	private Datum params;
	private CompiledCode body;
	private Environment env;

	//
	/*package*/ Closure(ClosureClass cl, Environment env) {
		if(cl == null || env == null) {
			throw new NullPointerException();
		}
		this.params  = cl.getParameterList();
		this.body    = cl.getCode();
		this.env     = env;
	}

	//
	/*package*/ Closure(ClosureClassMethod cl, Environment env) {
		if(cl == null || env == null) {
			throw new NullPointerException();
		}
		this.params  = cl.getParameterList();
		this.body    = cl.getCode();
		this.env     = env;
	}

	//
	/*package*/ Datum getParameterList() {
		return params;
	}

	//
	/*package*/ CompiledCode getCode() {
		return body;
	}

	//
	/*package*/ Environment getEnvironment() {
		return env;
	}

	/**
	 * 
	 * @return
	 */
	public int getArity() {
		ConsIterator itr = new ConsIterator(params);
		int ary = 0;

		while(itr.hasNext()) {
			itr.next();
			ary++;
		}
		return itr.getTerminal().isNil() ? ary : -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public Procedure mul(Procedure y) {
		return LispUtils.mul(this, y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeProcedure()
	 */
	public boolean isTypeProcedure() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return "Closure:" + printName();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.NamableDatum#display()
	 */
	@Override
	public String display() {
		return "#<closure " + printName() + ">";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<closure " + printName() + ">");
	}

}
