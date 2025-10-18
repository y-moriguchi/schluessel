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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/29
 */
public class LispFileBuffer extends LispLineBuffer {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/29
	 */
	public static class OpenFileBuffer extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			String f = SubrUtils.nextString(itr, mesg, body);
			String e = SubrUtils.nextString(itr, null, mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			try {
				return new LispFileBuffer(
						LispFiles.getFile(env, f), e);
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
	private PrintWriter wr;

	/**
	 * 
	 * @param fn
	 * @param enc
	 * @throws FileNotFoundException 
	 * @throws UnsupportedEncodingException 
	 */
	public LispFileBuffer(File fn, String enc) throws
			UnsupportedEncodingException,
			FileNotFoundException, IOException {
		if(fn == null) {
			throw new NullPointerException();
		} else if(enc == null) {
			wr = new PrintWriter(new BufferedWriter(
					new FileWriter(fn)));
		} else {
			wr = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(
							new FileOutputStream(fn), enc)));
		}
	}

	/**
	 * 
	 * @return
	 */
	public void putLine(Datum s) {
		wr.println(LispUtils.print(s));
	}

	/**
	 * 
	 * @throws IOException
	 */
	public void close() throws IOException {
		wr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<line-buffer>");
	}

}
