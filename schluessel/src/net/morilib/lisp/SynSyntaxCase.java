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

public class SynSyntaxCase extends Syntax {
	
	/*package*/ static final Symbol SYNTAX_CASE =
		Symbol.getSymbol("syntax-case");
	
	private static final Symbol KARI_SYM = Symbol.gensym();
	
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
			IntStack memento, MiscInfo syncased) {
		if(!(body instanceof Cons)) {
			//throw new LispException("malformed define-syntax");
			throw mesg.getError("err.syntaxrule.malform");
		}
		
		Cons b1 = (Cons)body;
		comp.compile(
				b1.getCar(), env, build, callsym,
				false, symlist,
				exec, memento, syncased);
		
		if(b1.getCdr() instanceof Cons) {
			UserSyntax usyn = SyntaxUtils.processSyntaxRules(
					KARI_SYM, b1.getCdr(), env,
					mesg, syncased.getUserSyntax());
			
			build.addPush(usyn);    // record ExecuteEnv
			build.addPop();
			build.addExpandSyntaxRule(usyn);
		} else {
			throw mesg.getError("err.syntaxrule.malform");
		}
	}

	@Override
	Datum replaceLocalVals(
			Datum body, Environment env, LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		//if(ttype == 0) {
		//	return body;
		//} else if(ttype == 1) {
		//	return comp.replaceLocalVals(
		//			body, env, ienv, toplv, ttype);
		//} else {
		//	throw new RuntimeException();
		//}
		//throw mesg.getError("err.syntaxcase.definesyntax");
		//return body;
		ConsListBuilder res = new ConsListBuilder();
		ConsIterator    itr = new ConsIterator(body);
		
		if(!itr.hasNext()) {
			throw mesg.getError("err.syntaxcase.malform");
		}
		res.append(comp.replaceLocalValsOnly(
				itr.next(), env, ienv, false, ttype));
		
		if(!itr.hasNext()) {
			throw mesg.getError("err.syntaxcase.malform");
		}
		res.append(itr.next());
		
		while(itr.hasNext()) {
			ConsListBuilder re2 = new ConsListBuilder();
			ConsIterator    it2 = new ConsIterator(itr.next());
			
			if(!it2.hasNext()) {
				throw mesg.getError("err.syntaxcase.malform");
			}
			re2.append(it2.next());
			
			if(!it2.hasNext()) {
				throw mesg.getError("err.syntaxcase.malform");
			}
			re2.append(comp.replaceLocalValsOnly(
					it2.next(), env, ienv, false, ttype));
			
			if(it2.hasNext()) {
				throw mesg.getError("err.syntaxcase.malform");
			}
			res.append(re2.get());
		}
		return res.get();
	}

}
