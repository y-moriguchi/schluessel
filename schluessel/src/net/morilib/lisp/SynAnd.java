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

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynAnd extends Syntax {

	
	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento, MiscInfo syncased) {
		//List<Datum> lst = LispUtils.consToList(body, mesg);
		Datum p = body;
		int last = build.allocLabel();
		
		build.addPush(LispBoolean.TRUE);
		while(p instanceof Cons) {
			Cons c = (Cons)p;
			
			build.addPop();
			//comp.compile(lst.get(i), env, build, callsym, false);
			comp.compile(
					c.getCar(), env, build, callsym,
					istail && (c.getCdr() == Nil.NIL), symlist,
					exec, memento, syncased);
			build.addJmpUnless(last);
			p = c.getCdr();
		}
		
		if(p != Nil.NIL) {
			throw mesg.getError("err.list");
		}
		build.setCurrentAddressToLabel(last);
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv,
			int ttype) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		List<Datum> res = new ArrayList<Datum>();
		
		for(int i = 0; i < lst.size(); i++) {
			res.add(comp.replaceLocalVals(
					lst.get(i), env, ienv, false, ttype));
		}
		
		return LispUtils.listToCons(res);
	}
	
}
