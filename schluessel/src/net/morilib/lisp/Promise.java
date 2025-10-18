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

import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Promise extends Datum {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/31
	 */
	public static class IsPromise extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof Promise);
		}

	}

	/**
	 * 
	 */
	public static final Promise STREAM_NULL = new Promise(false);

	//
	private CompiledCode code;
	private Environment  environment;
	private Datum memo = null;

	//
	private Promise(boolean x) {
		CompiledCode.Builder b = new CompiledCode.Builder();

		b.addPush(Nil.NIL);
		b.addReturnOp();
		this.code = b.getCodeRef();
		this.environment = null;
		this.memo = Nil.NIL;
	}

	//
	/*package*/ Promise(CompiledCode code) {
		if(code == null) {
			throw new NullPointerException();
		}
		this.code = code;
	}

	//
	/*package*/ void setMemo(Datum memo) {
		this.memo = memo;
	}

	//
	/*package*/ void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	/**
	 * @return the memo
	 */
	/*package*/ Datum getMemo() {
		return memo;
	}

	/**
	 * @return the code
	 */
	/*package*/ CompiledCode getCode() {
		return code;
	}

	/**
	 * @return the environment
	 */
	/*package*/ Environment getEnvironment() {
		return environment;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<promise " + Integer.toString(
				System.identityHashCode(this), 16) + ">");
	}

}
