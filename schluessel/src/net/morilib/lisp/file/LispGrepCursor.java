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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.InputPort;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.RegexPattern;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/30
 */
public class LispGrepCursor extends LispLineCursor {

	//
	private RegexPattern pattern;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/29
	 */
	public static class OpenGrepCursor extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum  p = SubrUtils.nextIf(itr, mesg, body);
			//String f = SubrUtils.nextString(itr, mesg, body);
			Datum  f = SubrUtils.nextIf(itr, mesg, body);
			String e = SubrUtils.nextString(itr, null, mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(p instanceof RegexPattern)) {
				throw mesg.getError("err.regexp.require.regexp", p);
			}

			try {
				if(f instanceof LispString) {
					return new LispGrepCursor(
							LispFiles.getFile(env, f.getString()), e,
							(RegexPattern)p);
				} else if(f instanceof InputPort) {
					return new LispGrepCursor(
							((InputPort)f).getReader(),
							(RegexPattern)p);
				} else {
					throw mesg.getError("err.file.require.pathoriport",
							f);
				}
			} catch (UnsupportedEncodingException e1) {
				throw mesg.getError("err.unsupportedencoding", e);
			} catch (FileNotFoundException e1) {
				throw mesg.getError("err.filenotfound", f);
			} catch (IOException e1) {
				throw mesg.getError("err.io", e1.getMessage());
			}
		}

	}

	/**
	 * @param fn
	 * @param enc
	 * @throws UnsupportedEncodingException
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public LispGrepCursor(File fn, String enc, RegexPattern pat)
			throws UnsupportedEncodingException, FileNotFoundException,
			IOException {
		super(fn, enc);
		pattern = pat;
	}

	/**
	 * 
	 * @param rd
	 * @param enc
	 * @throws UnsupportedEncodingException
	 * @throws IOException
	 */
	public LispGrepCursor(Reader rd0, RegexPattern pat
			) throws UnsupportedEncodingException, IOException {
		super(rd0);
		pattern = pat;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.file.LispLineCursor#test(java.lang.String)
	 */
	@Override
	protected boolean test(String now2) {
		return pattern.matches(now2);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<grep-line-cursor>");
	}

}
