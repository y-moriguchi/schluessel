/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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
package net.morilib.lisp.file;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.io.filter.AndFilenameFilter;
import net.morilib.util.io.filter.FilenameFilters;
import net.morilib.util.io.filter.NotFilenameFilter;
import net.morilib.util.io.filter.OrFilenameFilter;
import net.morilib.util.io.filter.WildcardFilenameFilter;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/24
 */
public final class LispFiles {

	//
	/*package*/ static String chdir = null;

	//
	private LispFiles() {}

	/**
	 * 
	 * @param env
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static File getFile(
			Environment env, String s) throws IOException {
		File  f = new File(s.replaceFirst("^~/",
				System.getProperty("user.home") + "/"));

		if(f.isAbsolute()) {
			return f;
		} else if(chdir != null) {
			return new File(chdir, s).getCanonicalFile();
		} else {
			return new File(
					System.getProperty("user.dir"),
					s).getCanonicalFile();
		}
	}

	/**
	 * 
	 * @param env
	 * @param d
	 * @param mesg
	 * @return
	 * @throws IOException 
	 */
	public static File getFile(
			Environment env, Datum d,
			LispMessage mesg) throws IOException {
		return getFile(env, SubrUtils.getString(d, mesg));
	}

	/**
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 * @throws IOException 
	 */
	public static File nextFile(ConsIterator itr, Environment env,
			LispMessage mesg, Datum body) throws IOException {
		return getFile(env, SubrUtils.nextIf(itr, mesg, body), mesg);
	}

	/**
	 * 
	 * @param env
	 * @return
	 */
	public static File pwd(Environment env) {
		if(chdir != null) {
			return new File(chdir);
		} else {
			return new File(System.getProperty("user.dir"));
		}
	}

	//
	private static FilenameFilter[] tofarray(
			ConsIterator itr, Datum d, LispMessage mesg) {
		List<FilenameFilter> l;

		l = new ArrayList<FilenameFilter>();
		while(itr.hasNext()) {
			l.add(compileFilter(itr.next(), mesg));
		}
		if(!itr.getTerminal().isNil()) {
			throw mesg.getError("err.list", d);
		}
		return l.toArray(new FilenameFilter[0]);
	}

	//
	/*package*/ static FilenameFilter compileFilter(
			Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			ConsIterator itr = new ConsIterator(d);
			String lg = SubrUtils.nextSymbolName(itr, mesg, d);

			if(lg.equalsIgnoreCase("and")) {
				return new AndFilenameFilter(tofarray(itr, d, mesg));
			} else if(lg.equalsIgnoreCase("or")) {
				return new OrFilenameFilter(tofarray(itr, d, mesg));
			} else if(lg.equalsIgnoreCase("not")) {
				Datum x = SubrUtils.nextIf(
						itr, mesg, "err.file.invalid.findop");

				if(itr.hasNext()) {
					throw mesg.getError("err.file.invalid.findop");
				}
				return new NotFilenameFilter(compileFilter(x, mesg));
			} else {
				throw mesg.getError("err.file.invalid.findop");
			}
		} else if(d.isNil()) {
			return FilenameFilters.TRUE;
		} else if(d.equals(Symbol.getSymbol("directory"))) {
			return FilenameFilters.IS_DIRECTORY;
		} else if(d.equals(Symbol.getSymbol("file"))) {
			return FilenameFilters.IS_FILE;
		} else if(d.equals(Symbol.getSymbol("exist"))) {
			return FilenameFilters.IS_EXIST;
		} else {
			return new WildcardFilenameFilter(
					SubrUtils.getString(d, mesg));
		}
	}

}
