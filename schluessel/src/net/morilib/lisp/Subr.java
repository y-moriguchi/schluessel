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

import java.util.HashMap;
import java.util.Map;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class Subr extends Settable
implements ISubr, Procedure {

	//
	private Map<Environment, ClosureClass> code =
		new HashMap<Environment, ClosureClass>();

	/**
	 * 
	 */
	protected String symbolName;

	/**
	 * 
	 * @return
	 */
	public String getSymbolName() {
		return symbolName;
	}

	/**
	 * 
	 */
	public Subr() {
		// do nothing
	}

	/**
	 * 
	 * @param name
	 */
	public Subr(String name) {
		symbolName = name;
	}

	/**
	 * 
	 * @param body
	 * @param env
	 * @param mesg
	 * @return
	 */
	public abstract Datum eval(
			Datum body, Environment env, LispMessage mesg);

	//
//	/*package*/ Datum eval(
//			Datum body,
//			Environment env, IntStack m, LispMessage mesg) {
//		return eval(body, env, mesg);
//	}

	//
	/*package*/ final ClosureClass getClosureClass(Environment env) {
		ClosureClass cl1;

		synchronized(this) {
			if(!code.containsKey(env)) {
				cl1 = createClosureClass(env);
				code.put(env, cl1);
			} else {
				cl1 = code.get(env);
			}
		}
		return cl1;
	}

	//
	/*package*/ ClosureClass createClosureClass(Environment env) {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public Procedure mul(Procedure y) {
		return LispUtils.mul(this, y);
	}

	/**
	 * @see net.morilib.lisp.Datum#isTypeProcedure()
	 */
	@Override
	public boolean isTypeProcedure() {
		return true;
	}

	/**
	 * 
	 */
	public String toString() {
		return "Subr:" + symbolName;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof Subr) {
			return this.getClass().equals(o.getClass());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return this.getClass().hashCode();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.NamableDatum#display()
	 */
	@Override
	public String display() {
		return "#<subr " + printName() + ">";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<subr " + getSymbolName() + ">");
	}

}
