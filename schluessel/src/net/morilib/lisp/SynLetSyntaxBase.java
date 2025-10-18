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


public abstract class SynLetSyntaxBase extends Syntax {
	
	
	/*package*/ abstract String getName();
	
	/*package*/ abstract LispException getError(LispMessage mesg);
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			List<Datum> lst = new ArrayList<Datum>();
			
			// 環境を新規に作成する
			Environment nenv = new Environment(ienv);
			
			// ローカル変数の定義
			List<Datum> lst2 = new ArrayList<Datum>();
			
			if(bcar instanceof Cons) {
				Datum d = bcar;
				
				// ローカル変数の評価
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2;
						Cons rc = new Cons();
						Cons r2 = new Cons();
						
						// rcのcdr部をr2にする
						rc.setCdr(r2);
						
						l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);
						
						if(l2.size() != 2) {
							//throw new LispException(
							//		"syntax error: " + getName());
							throw getError(mesg);
						}
						
						// リネームリストに追加する
						rc.setCar(SyntaxUtils.putSymbol(
								nenv, l2.get(0), mesg));
						
						// ローカル構文の値はそのまま
						r2.setCar(l2.get(1));
						
						d = ((Cons)d).getCdr();
						lst2.add(rc);
					} else {
						//throw new LispException(
						//		"malformed " + getName());
						throw getError(mesg);
					}
				}
			} else {
				//throw new LispException("malformed " + getName());
				throw getError(mesg);
			}
			lst.add(LispUtils.listToCons(lst2));
			
			// cdr部を調査する
			Datum cdrx = SyntaxUtils.replaceLocalValsList(
					bcdr, env, comp, nenv, mesg, ttype);
			
			return LispUtils.listToCons(lst, cdrx);
		} else {
			//throw new LispException("malformed " + getName());
			throw getError(mesg);
		}
	}
	
}
