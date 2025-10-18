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
import java.util.Map;

import net.morilib.lisp.LispCompiler.MiscInfo;
import net.morilib.lisp.r6rs.SymbolEnv;

public class SynImport extends Syntax {
	
	
	/*package*/ static void buildImport(
			Map<Symbol, SymbolEnv> mp,
			CompiledCode.Builder build,
			LispMessage mesg) {
		for(Map.Entry<Symbol, SymbolEnv> e : mp.entrySet()) {
			Datum d;
			
			d = e.getValue().getEnvironment().getDatum(e.getKey());
			if(d == null) {
				throw mesg.getError("err.unbound", e.getKey());
			}
			build.addPush(d);
			build.addBind(e.getValue().getSymbol());
		}
	}
	
	
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
		//Map<Symbol, SymbolEnv> mp = LibraryID.compileImport(body);
		
		//buildImport(mp, build, mesg);
		build.addImportLib(body);
		build.addPush(Undef.UNDEF);
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		throw mesg.getError("err.import.definesyntax");
	}
	
}
