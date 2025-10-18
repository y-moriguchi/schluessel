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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.LispCompiler.MiscInfo;
import net.morilib.lisp.r6rs.LibraryID;
import net.morilib.lisp.r6rs.LibraryIDException;
import net.morilib.lisp.r6rs.SymbolEnv;

public class SynLibrary extends Syntax {
	
	private static final Symbol EXPORT = Symbol.getSymbol("export");
	private static final Symbol RENAME = Symbol.getSymbol("rename");
	private static final Symbol IMPORT = Symbol.getSymbol("import");
	
	
	private void torens(
			Datum d, Map<Symbol, Symbol> sym, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(d);
		
		while(itr.hasNext()) {
			Datum o = itr.next();
			
			if(o instanceof Cons) {
				Cons c1, c2;
				
				c1 = (Cons)o;
				if(!(c1.getCdr() instanceof Cons)) {
					throw mesg.getError("err.library.malform");
				}
				c2 = (Cons)c1.getCdr();
				if(c2.getCdr() != Nil.NIL) {
					throw mesg.getError("err.library.malform");
				}
				
				if(!(c1.getCar() instanceof Symbol)) {
					throw mesg.getError(
							"err.require.symbol", c1.getCar());
				} else if(!(c2.getCar() instanceof Symbol)) {
					throw mesg.getError(
							"err.require.symbol", c2.getCar());
				}
				
				sym.put((Symbol)c1.getCar(), (Symbol)c2.getCar());
			} else {
				throw mesg.getError("err.library.malform");
			}
		}
		
		if(itr.getTerminal() != Nil.NIL) {
			throw mesg.getError("err.list.symbol");
		}
	}
	
	private Map<Symbol, Symbol> tosyms(
			Datum d,
			LispMessage mesg,
			Map<Symbol, SymbolEnv> mp) {
		ConsIterator itr = new ConsIterator(d);
		Map<Symbol, Symbol> res = new HashMap<Symbol, Symbol>();
		
		while(itr.hasNext()) {
			Datum o = itr.next();
			
			if(o instanceof Symbol) {
				res.put((Symbol)o, (Symbol)o);
			} else if(o instanceof Cons) {
				Cons c1 = (Cons)o;
				
				if(!c1.getCar().equals(RENAME)) {
					throw mesg.getError("err.list.symbol");
				}
				torens(c1.getCdr(), res, mesg);
			} else {
				throw mesg.getError("err.list.symbol");
			}
		}
		
		if(itr.getTerminal() != Nil.NIL) {
			throw mesg.getError("err.list.symbol");
		}
		
		for(SymbolEnv e : mp.values()) {
			res.put(e.getSymbol(), e.getSymbol());
		}
		return res;
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
		Cons c0, c00, c1, c10;
		Datum c2i = null;
		Map<Symbol, SymbolEnv> mp = new HashMap<Symbol, SymbolEnv>();
		LibraryID libid;
		
		//
		if(!toplevel) {
			throw mesg.getError("err.nottoplevel");
		}
		
		if(!(body instanceof Cons)) {
			throw mesg.getError("err.library.malform");
		}
		c0 = (Cons)body;
		
		if(!(c0.getCar() instanceof Cons)) {
			throw mesg.getError("err.libid");
		}
		c00 = (Cons)c0.getCar();
		
		if(!(c0.getCdr() instanceof Cons)) {
			throw mesg.getError("err.library.malform");
		}
		c1 = (Cons)c0.getCdr();
		
		if(!(c1.getCar() instanceof Cons)) {
			throw mesg.getError("err.library.malform");
		}
		c10 = (Cons)c1.getCar();
		
		if(!c10.getCar().equals(EXPORT)) {
			throw mesg.getError("err.library.malform");
		}
		
		if(c1.getCdr() instanceof Cons) {
			Cons c2 = (Cons)c1.getCdr();
			
			if(c2.getCar() instanceof Cons) {
				Cons c20 = (Cons)c2.getCar();
				
				if(c20.getCar().equals(IMPORT)) {
					c2i = c20.getCdr();
					c1  = c2;
				}
			}
		}
		
		if(!(c00.getCar() instanceof Symbol)) {
			throw mesg.getError("err.libid");
		}
		libid = new LibraryID(c00);
		
		// compile the list
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		Environment nenv = new Environment(env);
		
		if(c2i != null) {
			try {
				mp = LibraryID.compileImport(c2i);
			} catch (LibraryIDException e) {
				throw e.toLispException(mesg);
			}
			SynImport.buildImport(mp, nbuild, mesg);
		}
		SyntaxUtils.compileList(
				c1.getCdr(), nenv,
				comp, nbuild, callsym, false, mesg, symlist,
				exec, memento, toplevel, syncased);
		nbuild.addSetLibID(libid, tosyms(c10.getCdr(), mesg, mp));
		nbuild.addReturnOp();
		ClosureClass cln = new ClosureClass(
				Nil.NIL, nbuild.getCodeRef());
		
		//
		build.addPush(cln);
		build.addBeginList();
		build.addEndList();
		build.addCall();
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		throw mesg.getError("err.library.definesyntax");
	}
	
}
