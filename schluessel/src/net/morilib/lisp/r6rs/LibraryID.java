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
package net.morilib.lisp.r6rs;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SymbolName;
import net.morilib.lisp.UserSyntax;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class LibraryID {

	/**
	 * 
	 */
	public static final LibraryID R5RS = new LibraryID(
			new Cons(Symbol.getSymbol("r5rs"), Nil.NIL));

	//
	private static Map<LibraryID, Environment> namespc =
		new HashMap<LibraryID, Environment>();
	private static Map<LibraryID, Map<Symbol, Symbol>> exports =
		new HashMap<LibraryID, Map<Symbol, Symbol>>();

	//
	private Cons id;

	/**
	 * 
	 * @param dtm
	 */
	public LibraryID(Datum dtm) {
		if(dtm == null) {
			throw new NullPointerException();
		} else if(!(dtm instanceof Cons)) {
			throw new IllegalArgumentException();
		}
		this.id   = (Cons)dtm;
	}

	/**
	 * 
	 * @param id
	 * @return
	 */
	public static Environment getNamespace(LibraryID id) {
		synchronized(LibraryID.class) {
			return namespc.get(id);
		}
	}

	/**
	 * 
	 * @param id
	 * @return
	 */
	public static Map<Symbol, Symbol> getExport(LibraryID id) {
		synchronized(LibraryID.class) {
			Map<Symbol, Symbol> res = exports.get(id);

			return (res != null) ?
					Collections.unmodifiableMap(res) : null;
		}
	}

	/**
	 * 
	 * @param id
	 * @param sp
	 * @param ex
	 */
	public static void putNamespace(
			LibraryID id, Environment sp, Map<Symbol, Symbol> ex) {
		synchronized(LibraryID.class) {
			namespc.put(id, sp);
			exports.put(id, new HashMap<Symbol, Symbol>(ex));
		}
	}

	//
	private static final Symbol LIBRARY = Symbol.getSymbol("library");
	private static final Symbol ONLY    = Symbol.getSymbol("only");
	private static final Symbol EXCEPT  = Symbol.getSymbol("except");
	private static final Symbol PREFIX  = Symbol.getSymbol("prefix");
	private static final Symbol RENAME  = Symbol.getSymbol("rename");

	//
	private static LibraryIDException getError(String s) {
		return new LibraryIDException(s);
	}

	//
	private static LibraryIDException getError(String s, Datum d) {
		return new LibraryIDException(s, d);
	}

	//
	private static Datum car(Datum b) throws LibraryIDException {
		Cons c0;

		if(!(b instanceof Cons)) {
			throw getError("err.import.malform");
		}
		c0 = (Cons)b;
		return c0.getCar();
	}

	//
	private static Cons cxdr(Datum b) throws LibraryIDException {
		Cons c0;

		if(!(b instanceof Cons)) {
			throw getError("err.import.malform");
		}
		c0 = (Cons)b;

		if(!(c0.getCdr() instanceof Cons)) {
			throw getError("err.import.malform");
		}
		return (Cons)c0.getCdr();
	}

	//
	private static Datum cadr(Datum b) throws LibraryIDException {
		return cxdr(b).getCar();
	}

	//
	private static Datum cddr(Datum b) throws LibraryIDException {
		return cxdr(b).getCdr();
	}

	//
	private static Symbol findkey(
			Map<Symbol, SymbolEnv> sym, Symbol s) {
		for(Map.Entry<Symbol, SymbolEnv> e : sym.entrySet()) {
			if(e.getValue().getSymbol().equals(s)) {
				return e.getKey();
			}
		}
		return null;
	}

	//
	private static void putenv(
			LibraryID id,
			Map<Symbol, SymbolEnv> sym
			) throws LibraryIDException {
		Map<Symbol, Symbol> mp = LibraryID.getExport(id);
		Environment env = LibraryID.getNamespace(id);
		Map<SymbolName, Datum> mp2;

		if(mp == null) {
			throw getError("err.library.undefined");
		}

		for(Map.Entry<Symbol, Symbol> e : mp.entrySet()) {
			sym.put(e.getKey(), new SymbolEnv(e.getValue(), env));
		}

		// macro
		mp2 = env.getData();
		for(Map.Entry<SymbolName, Datum> e : mp2.entrySet()) {
			if(e.getValue() instanceof UserSyntax) {
				sym.put(e.getKey().getSymbol(),
						new SymbolEnv(e.getKey().getSymbol(), env));
			}
		}
	}

	//
	private static Map<Symbol, SymbolEnv> only(
			Map<Symbol, SymbolEnv> sym,
			Datum lst) throws LibraryIDException {
		ConsIterator itr = new ConsIterator(lst);
		Map<Symbol, SymbolEnv> res =
			new HashMap<Symbol, SymbolEnv>();

		while(itr.hasNext()) {
			Datum  d = itr.next();
			Symbol e;

			if(!(d instanceof Symbol)) {
				throw getError("err.require.symbol", d);
			} else if((e = findkey(sym, (Symbol)d)) == null) {
				throw getError("err.library.undefined");
			}

			res.put(e, sym.get(e));
		}

		if(itr.getTerminal() != Nil.NIL) {
			throw getError("err.list");
		}
		return res;
	}

	//
	private static Map<Symbol, SymbolEnv> except(
			Map<Symbol, SymbolEnv> sym,
			Datum lst) throws LibraryIDException {
		ConsIterator itr = new ConsIterator(lst);
		Map<Symbol, SymbolEnv> res = sym;

		while(itr.hasNext()) {
			Datum  d = itr.next();
			Symbol e;

			if(!(d instanceof Symbol)) {
				throw getError("err.require.symbol", d);
			} else if((e = findkey(sym, (Symbol)d)) == null) {
				throw getError("err.library.undefined");
			}

			res.remove(e);
		}

		if(itr.getTerminal() != Nil.NIL) {
			throw getError("err.list");
		}
		return res;
	}

	//
	private static Map<Symbol, SymbolEnv> prefix(
			Map<Symbol, SymbolEnv> sym,
			Datum lst) throws LibraryIDException {
		ConsIterator itr = new ConsIterator(lst);
		Map<Symbol, SymbolEnv> res = sym;
		Datum d;

		if(!itr.hasNext()) {
			throw getError("err.import.malform");
		}
		d = itr.next();
		if(!(d instanceof Symbol)) {
			throw getError("err.require.symbol", d);
		} else if(itr.hasNext()) {
			throw getError("err.import.malform");
		} else if(itr.getTerminal() != Nil.NIL) {
			throw getError("err.list");
		}

		for(Map.Entry<Symbol, SymbolEnv> e : sym.entrySet()) {
			String sn = ((Symbol)d).getName();
			Symbol s2 = Symbol.getSymbol(
					sn + e.getValue().getSymbol().getName());

			e.setValue(e.getValue().prototype(s2));
		}
		return res;
	}

	//
	private static Map<Symbol, SymbolEnv> rename(
			Map<Symbol, SymbolEnv> sym,
			Datum lst) throws LibraryIDException {
		ConsIterator itr = new ConsIterator(lst);
		Map<Symbol, SymbolEnv> res = sym;

		while(itr.hasNext()) {
			Datum  d = itr.next();
			Datum  da, dd;
			Symbol e;

			da = car (d);
			dd = cadr(d);
			if(cddr(d) != Nil.NIL) {
				throw getError("err.import.malform");
			}

			if((e = findkey(sym, (Symbol)da)) == null) {
				throw getError("err.library.undefined");
			}

			res.put(e, res.get(e).prototype((Symbol)dd));
		}

		if(itr.getTerminal() != Nil.NIL) {
			throw getError("err.list");
		}
		return res;
	}

	//
	private static Map<Symbol, SymbolEnv> compileImportSet(
			Datum d
			) throws LibraryIDException {
		return compileImportSet0(d, new HashMap<Symbol, SymbolEnv>());
	}

	//
	private static Map<Symbol, SymbolEnv> compileImportSet0(
			Datum d,
			Map<Symbol, SymbolEnv> res
			) throws LibraryIDException {
		if(d instanceof Cons) {
			Cons c0 = (Cons)d;
			Map<Symbol, SymbolEnv> r2;

			if(c0.getCar().equals(LIBRARY)) {
				putenv(new LibraryID(cadr(c0)), res);
			} else if(c0.getCar().equals(ONLY)) {
				r2 = only(compileImportSet(cadr(c0)), cddr(c0));
				res.putAll(r2);
			} else if(c0.getCar().equals(EXCEPT)) {
				r2 = except(compileImportSet(cadr(c0)), cddr(c0));
				res.putAll(r2);
			} else if(c0.getCar().equals(PREFIX)) {
				r2 = prefix(compileImportSet(cadr(c0)), cddr(c0));
				res.putAll(r2);
			} else if(c0.getCar().equals(RENAME)) {
				r2 = rename(compileImportSet(cadr(c0)), cddr(c0));
				res.putAll(r2);
			} else if(c0.getCar() instanceof Symbol) {
				putenv(new LibraryID(c0), res);
			} else {
				throw getError("err.libid");
			}
		} else {
			throw getError("err.libid");
		}

		return res;
	}

	/**
	 * 
	 * @param body
	 * @return
	 * @throws LibraryIDException
	 */
	public static Map<Symbol, SymbolEnv> compileImport(
			Datum body
			) throws LibraryIDException {
		ConsIterator itr = new ConsIterator(body);
		HashMap<Symbol, SymbolEnv> res =
			new HashMap<Symbol, SymbolEnv>();

		while(itr.hasNext()) {
			Datum d = itr.next();

			compileImportSet0(d, res);
		}

		if(itr.getTerminal() != Nil.NIL) {
			throw getError("err.list");
		}
		return Collections.unmodifiableMap(res);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LibraryID) {
			return id.getCar().equals(((LibraryID)o).id.getCar());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return id.getCar().hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return LispUtils.print(id);
	}

}
