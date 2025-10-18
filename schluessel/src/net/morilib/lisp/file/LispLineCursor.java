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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.InputPort;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/29
 */
public class LispLineCursor extends Datum2 implements ILispLineCursor {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/29
	 */
	public static class OpenLineCursor extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			//String f = SubrUtils.nextString(itr, mesg, body);
			Datum  f = SubrUtils.nextIf(itr, mesg, body);
			String e = SubrUtils.nextString(itr, null, mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			try {
				if(f instanceof LispString) {
					return new LispLineCursor(
							LispFiles.getFile(env, f.getString()), e);
				} else if(f instanceof InputPort) {
					return new LispLineCursor(
							((InputPort)f).getReader());
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

	//
	private BufferedReader rd;

	/**
	 * 
	 */
	protected String now;

	/**
	 * 
	 * @param fn
	 * @param enc
	 * @throws FileNotFoundException 
	 * @throws UnsupportedEncodingException 
	 */
	public LispLineCursor(
			File fn, String enc
			) throws UnsupportedEncodingException, IOException {
		if(fn == null) {
			throw new NullPointerException();
		} else if(enc == null) {
			rd = new BufferedReader(new FileReader(fn));
		} else {
			rd = new BufferedReader(new InputStreamReader(
					new FileInputStream(fn), enc));
		}
		next();
	}

	/**
	 * 
	 * @param rd
	 * @param enc
	 * @throws UnsupportedEncodingException
	 * @throws IOException
	 */
	public LispLineCursor(Reader rd0
			) throws UnsupportedEncodingException, IOException {
		if(rd0 == null) {
			throw new NullPointerException();
		} else {
			rd = new BufferedReader(rd0);
		}
		next();
	}

	/**
	 * 
	 * @return
	 */
	public Datum getLine() {
		return new LispString(now);
	}

	/**
	 * 
	 * @return
	 */
	public boolean hasLine() {
		return now != null;
	}

	/**
	 * 
	 * @throws IOException 
	 */
	public void next() throws IOException {
		while((now = rd.readLine()) != null) {
			if(test(now)) {
				return;
			}
		}
	}

	/**
	 * @param now2
	 * @return
	 */
	protected boolean test(String now2) {
		return true;
	}

	/**
	 * 
	 * @throws IOException
	 */
	public void close() throws IOException {
		rd.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<line-cursor>");
	}

}
