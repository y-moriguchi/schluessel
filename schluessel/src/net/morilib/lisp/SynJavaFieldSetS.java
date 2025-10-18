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

import java.util.List;

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynJavaFieldSetS extends Syntax {

	@Override
	void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			MiscInfo syncased) {
		Datum method, klass, val;
		
		if(body instanceof Cons) {
			Cons p1 = (Cons)body;
			
			method = p1.getCar();
			if(!IntLispUtils.isSymbolName(method)) {
				throw mesg.getError("err.reqired.symbol", method);
			} else if(p1.getCdr() instanceof Cons) {
				Cons p2 = (Cons)p1.getCdr();
				
				klass = p2.getCar();
				if(p2.getCdr() instanceof Cons) {
					Cons p3 = (Cons)p2.getCdr();
					
					val = p3.getCar();
					if(p3.getCdr() != Nil.NIL) {
						throw mesg.getError(
								"err.java-field-set.malform");
					}
				} else {
					throw mesg.getError("err.java-field-set.malform");
				}
			} else {
				throw mesg.getError("err.java-field-set.malform");
			}
		} else {
			throw mesg.getError("err.java-field-set.malform");
		}
		
		comp.compile(
				klass, env, build, callsym, false, symlist,
				exec, memento, syncased);
		comp.compile(
				val, env, build, callsym, false, symlist,
				exec, memento, syncased);
		build.addJavaFieldSet((SymbolName)method, klass);
	}

	@Override
	Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg, boolean toplv, int ttype) {
		ConsListBuilder bld = new ConsListBuilder();
		Datum method, klass, val;
		
		if(body instanceof Cons) {
			Cons p1 = (Cons)body;
			
			method = p1.getCar();
			if(!(method instanceof Symbol)) {
				throw mesg.getError("err.reqired.symbol", method);
			} else if(p1.getCdr() instanceof Cons) {
				Cons p2 = (Cons)p1.getCdr();
				
				klass = p2.getCar();
				if(p2.getCdr() instanceof Cons) {
					Cons p3 = (Cons)p2.getCdr();
					
					val = p3.getCar();
					if(p3.getCdr() != Nil.NIL) {
						throw mesg.getError(
								"err.java-field-set.malform");
					}
				} else {
					throw mesg.getError("err.java-field-set.malform");
				}
			} else {
				throw mesg.getError("err.java-field-set.malform");
			}
		} else {
			throw mesg.getError("err.java-field-set.malform");
		}
		
		bld.append(method);
		bld.append(comp.replaceLocalVals(
				klass, env, ienv, false, ttype));
		bld.append(comp.replaceLocalVals(
				val, env, ienv, false, ttype));
		
		return bld.get();
	}

}
