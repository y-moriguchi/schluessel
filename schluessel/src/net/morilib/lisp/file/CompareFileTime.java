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

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/01
 */
public abstract class CompareFileTime extends BinaryArgs {

	//
	private long getFileTime(
			Datum c1a,
			Environment env,
			LispMessage mesg) throws IOException {
		if(c1a instanceof LispString) {
			return LispFiles.getFile(
					env, c1a.getString()).lastModified();
		} else if(c1a instanceof LispDate) {
			return ((LispDate)c1a).getTime();
		} else if(c1a instanceof LispInteger) {
			return c1a.getLong();
		} else {
			throw mesg.getError("err.file.require", c1a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		try {
			long t1 = getFileTime(c1a, env, mesg);
			long t2 = getFileTime(c2a, env, mesg);
	
			return LispBoolean.getInstance(compare(t1, t2));
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

	/**
	 * @param t1
	 * @param t2
	 * @return
	 */
	protected abstract boolean compare(long t1, long t2);

}
