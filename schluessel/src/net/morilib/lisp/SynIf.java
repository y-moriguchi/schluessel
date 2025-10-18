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

public class SynIf extends Syntax {

	
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
			IntStack memento,
			MiscInfo syncased) {
		if(body instanceof Cons) {
			Datum bcdr = ((Cons)body).getCdr();
			if(!(bcdr instanceof Cons)) {
				//throw new LispException("malformed if");
				throw mesg.getError("err.if.malform");
			}
			
			// 使用するラベル
			int l1 = build.allocLabel();
			int l2 = build.allocLabel();
			
			// 条件部分
			comp.compile(
					((Cons)body).getCar(), env, build,
					callsym, false, symlist,
					exec, memento, syncased);
			
			// then部分
			build.addJmpUnless(l1);
			build.addPop();
			comp.compile(
					((Cons)bcdr).getCar(), env, build,
					callsym, istail, symlist,
					exec, memento, syncased);
			build.addJmp(l2);
			
			// else部分
			build.setCurrentAddressToLabel(l1);
			build.addPop();
			Datum bcddr = ((Cons)bcdr).getCdr();
			if(bcddr instanceof Nil) {
				build.addPush(Undef.UNDEF);
			} else if(bcddr instanceof Cons) {
				if(((Cons)bcddr).getCdr() != Nil.NIL) {
					throw mesg.getError("err.if.malform");
				}
				
				comp.compile(
						((Cons)bcddr).getCar(), env, build,
						callsym, istail, symlist,
						exec, memento, syncased);
			} else {
				//throw new LispException("malformed if");
				throw mesg.getError("err.if.malform");
			}
			
			// 終了
			build.setCurrentAddressToLabel(l2);
		} else {
			//throw new LispException("malformed if");
			throw mesg.getError("err.if.malform");
		}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		List<Datum> lst = new ArrayList<Datum>();
		
		if(body instanceof Cons) {
			Datum bcdr = ((Cons)body).getCdr();
			if(!(bcdr instanceof Cons)) {
				//throw new LispException("malformed if");
				throw mesg.getError("err.if.malform");
			}
			
			// 条件部分
			lst.add(comp.replaceLocalVals(
					((Cons)body).getCar(), env, ienv, false, ttype));
			
			// then部分
			lst.add(comp.replaceLocalVals(
					((Cons)bcdr).getCar(), env, ienv, toplv, ttype));
			
			// else部分
			Datum bcddr = ((Cons)bcdr).getCdr();
			if(bcddr instanceof Nil) {
				// do nothing
			} else if(bcddr instanceof Cons) {
				lst.add(comp.replaceLocalVals(
						((Cons)bcddr).getCar(),
						env, ienv, toplv, ttype));
			} else {
				//throw new LispException("malformed if");
				throw mesg.getError("err.if.malform");
			}
		} else {
			//throw new LispException("malformed if");
			throw mesg.getError("err.if.malform");
		}
		
		return LispUtils.listToCons(lst);
	}
	
}
