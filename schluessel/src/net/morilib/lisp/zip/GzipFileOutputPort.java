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
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.r6rs.io.LispFileOptions;
import net.morilib.lisp.r6rs.io.LispOutputStreamPort;
import net.morilib.lisp.r6rs.io.LispTextualOutputPort;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class GzipFileOutputPort extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum op  = Iterators.nextIf(itr, LispFileOptions.DEFAULT);
		Datum bm  = Iterators.nextIf(itr, LispBufferMode.BLOCK);
		Datum tr  = Iterators.nextIf(itr, LispBoolean.FALSE);
		OutputStream ous;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!(op instanceof LispFileOptions)) {
			throw mesg.getError("err.io.require.fileoptions", op);
		} else if(!(bm instanceof LispBufferMode)) {
			throw mesg.getError("err.io.require.buffermode", bm);
		} else if(tr.isTrue() && !(tr instanceof ILispTranscoder)) {
			throw mesg.getError("err.io.require.transcoder", tr);
		} else {
			try {
				ous = new GZIPOutputStream(new FileOutputStream(
						LispFiles.getFile(env, c1a, mesg)));
				if(tr.isTrue()) {
					return new LispTextualOutputPort(
							((ILispTranscoder)tr).newOutput(ous),
							(ILispTranscoder)tr,
							(LispBufferMode)bm);
				} else {
					return new LispOutputStreamPort(ous,
							(LispBufferMode)bm);
				}
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		}
	}

}
