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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SynLambda extends Syntax {

	//
	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			Datum bcdr = c.getCdr();

			// validate parameter list
			if(!SyntaxUtils.isValidSymbolList(c.getCar())) {
				throw mesg.getError("err.parameters", c.getCar());
			}

			// create a temporary Closure
			CompiledCode.Builder mbuild = new CompiledCode.Builder();
			Environment  menv = new Environment(env);

			// compile the sequence(list)
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Environment nenv = new Environment(menv);
			SyntaxUtils.compileList(
					bcdr, nenv, comp, nbuild,
					new Cons(), true, mesg,
					new ArrayList<Cons>(),
					exec, memento, syncased);
			//		symlist);

			nbuild.addReturnOp();
			ClosureClass cln = new ClosureClass(
					c.getCar(), nbuild.getCodeRef());

			//build.addPush(cl);
			mbuild.addPush(cln);
			mbuild.addReturnOp();

			// 一時的なClosureを生成する
			ClosureClass clm = new ClosureClass(
					Nil.NIL, mbuild.getCodeRef());

			// 一時的なClosureを呼び出す
			build.addPush(clm);
			build.addBeginList();
			build.addEndList();
			build.addCall();
		} else {
			//throw new LispException("malformed lambda");
			throw mesg.getError("err.lambda.malform");
		}
	}

	//
//	private static final String REPLACE_CDR =
//		"(define (replace-cdr args rep)" +
//		"  (cond ((null? rep) '())" +
//		"        ((atom? rep)" +
//		"          (if (assq rep args)" +
//		"              (cdr (assq rep args))" +
//		"              rep))" +
//		"        (else" +
//		"          (cons (replace-cdr args (car rep))" +
//		"                (replace-cdr args (cdr rep))))))";
//	private static final Scheme INTERNAL;

	//
//	static {
//		INTERNAL = Scheme.newRnRS(5);
//		INTERNAL.set("atom?", new IsAtom());
//		INTERNAL.input(REPLACE_CDR);
//	}

	//
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg,
			boolean toplv, int ttype) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			Datum bcdr = c.getCdr();
			Cons res = new Cons();

			// 環境を新規に作成する
			Environment nenv = new Environment(ienv);

			// 引数をリネームリストに追加する
			res.setCar(SyntaxUtils.addLocalValsAll(
					nenv, c.getCar(), mesg));

			// リストをコンパイルする
			res.setCdr(SyntaxUtils.replaceLocalValsList(
					bcdr, env, comp, nenv, mesg, ttype));

			return res;
		} else {
			//throw new LispException("malformed lambda");
			throw mesg.getError("err.lambda.malform");
		}
	}

}
