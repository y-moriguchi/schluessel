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
package net.morilib.lisp.zip;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.IOs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class ExtractZipEntry extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		Datum c3a = Iterators.nextIf(itr, LispBoolean.FALSE);
		OutputStream ous = null;
		InputStream  ins = null;
		ZipFile zf;
		ZipEntry ze;
		String ss;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(c1a instanceof LispZipFile) {
			zf = ((LispZipFile)c1a).zipfile;
			if(c2a instanceof LispZipEntry) {
				ze = ((LispZipEntry)c1a).entry;
				return SubrUtils.nvl(ze.getComment());
			} else {
				ze = zf.getEntry(SubrUtils.getString(c2a, mesg));
			}

			if(ze == null || ze.isDirectory()) {
				throw mesg.getError("err.zip.entry.notfound", c2a);
			}

			try {
				if(c3a.isTrue()) {
					ous = new FileOutputStream(
							SubrUtils.getString(c3a, mesg));
				} else {
					ss  = ze.getName();
					if(ss.lastIndexOf('/') >= 0) {
						ss  = ss.substring(ss.lastIndexOf('/') + 1);
					}
					ous = new FileOutputStream(
							LispFiles.getFile(env, ss));
				}
				ins = zf.getInputStream(ze);
				IOs.copy(ins, ous);
				return Undef.UNDEF;
			} catch(IOException e) {
				throw mesg.getError("err.io");
			} finally {
				IOs.close(ins);
				IOs.close(ous);
			}
		} else {
			throw mesg.getError("err.zip.require.zipfile", c1a);
		}
	}

}
