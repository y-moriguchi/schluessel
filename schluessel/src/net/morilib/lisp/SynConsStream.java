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
import net.morilib.lisp.subr.MakeCons;

public class SynConsStream extends Syntax {
	
	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail, LispMessage mesg, List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		Promise prms;
		
		if(lst.size() != 2) {
			//throw new LispException("invalid cons-stream");
			throw mesg.getError("err.argument");
		}
		
		// コンパイル済みコードを構築する
		comp.compile(
				lst.get(1), env, nbuild, callsym, false, symlist,
				exec, memento, syncased);
		nbuild.addReturnOp();
		prms = new Promise(nbuild.getCodeRef());
		
		// (cons <0> (delay <1>))
		build.addPush(new MakeCons());
		build.addBeginList();
		comp.compile(
				lst.get(0), env, build, callsym, false, symlist,
				exec, memento, syncased);
		build.addAppendList();
		build.addNewPromise(prms);
		build.addAppendList();
		build.addEndList();
		build.addCall();
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
		
		if(lst.size() != 2) {
			//throw new LispException("invalid cons-stream");
			throw mesg.getError("err.argument");
		}
		
		res.add(comp.replaceLocalVals(
				lst.get(0), env, ienv, false, ttype));
		res.add(comp.replaceLocalVals(
				lst.get(1), env, ienv, false, ttype));
		return LispUtils.listToCons(res);
	}
	
	
	public String toString() {
		return "Syntax:cons-stream";
	}

}
