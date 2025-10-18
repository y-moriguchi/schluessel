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

public class SubrMap2 extends Subr {
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		throw new RuntimeException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#getClosure()
	 */
	@Override
	/*package*/ ClosureClass createClosureClass(Environment env) {
		CompiledCode.Builder bld = new CompiledCode.Builder();
		int lbl1 = bld.allocLabel();
		int lbl2 = bld.allocLabel();
		
		bld.addPush(Symbol.getSymbol("aux-map-valid"));
		bld.addReferSymbol(Symbol.getSymbol("arg"));
		bld.addCallMethod();
		bld.addPop();
		
		bld.addPush(env.findDatum(Symbol.getSymbol("car")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("arg"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addBind(Symbol.getSymbol("f"));
		
		bld.addPush(env.findDatum(Symbol.getSymbol("cdr")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("arg"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addBind(Symbol.getSymbol("l"));
		
		bld.addPush(Nil.NIL);
		bld.addBind(Symbol.getSymbol("res"));
		
		bld.addPush(Symbol.getSymbol("aux-map"));
		bld.addReferSymbol(Symbol.getSymbol("l"));
		bld.addCallMethod();
		bld.addBind(Symbol.getSymbol("l2"));
		
		bld.setCurrentAddressToLabel(lbl1);
		bld.addPush(env.findDatum(Symbol.getSymbol("null?")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("l2"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addJmpIf(lbl2);
		
		bld.addPop();
		bld.addPush(env.findDatum(Symbol.getSymbol("cons")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("f"));
		
		bld.addPush(env.findDatum(Symbol.getSymbol("car")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("l2"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addCall();
		
		bld.addAppendList();
		bld.addReferSymbol(Symbol.getSymbol("res"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addBind(Symbol.getSymbol("res"));
		
		bld.addPush(env.findDatum(Symbol.getSymbol("cdr")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("l2"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addBind(Symbol.getSymbol("l2"));
		bld.addJmp(lbl1);
		
		bld.setCurrentAddressToLabel(lbl2);
		bld.addPop();
		bld.addPush(env.findDatum(Symbol.getSymbol("reverse")));
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("res"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addReturnOp();
		
		ClosureClass cl1 = new ClosureClass();
		cl1.setParameterList(Symbol.getSymbol("arg"));
		cl1.setCode(bld.getCodeRef());
		
		return cl1;
	}
	
}
