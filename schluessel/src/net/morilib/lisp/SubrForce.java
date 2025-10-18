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


public class SubrForce extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		throw new RuntimeException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#getClosureClass(net.morilib.lisp.Environment)
	 */
	@Override
	/*package*/ ClosureClass createClosureClass(Environment env) {
		CompiledCode.Builder bld = new CompiledCode.Builder();
		ClosureClass cl1 = new ClosureClass();
		Symbol promise = Symbol.getSymbol("promise");
		
		bld.addReferSymbol(promise);
		bld.addForce();
		bld.addReturnOp();
		
		cl1.setParameterList(new Cons(promise, Nil.NIL));
		cl1.setCode(bld.getCodeRef());
		
		return cl1;
	}

}
