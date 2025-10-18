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
package net.morilib.lisp.r6rs.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class LispFileOptions extends Datum2 {

	//
	/*package*/ static final int NO_FAIL = 1;
	/*package*/ static final int NO_CREATE = 2;
	/*package*/ static final int NO_TRUNCATE = 4;

	/**
	 * 
	 */
	public static LispFileOptions DEFAULT = new LispFileOptions(0);

	//
	private int options;

	//
	/*package*/ LispFileOptions(int options) {
		this.options = options;
	}

	/**
	 * 
	 * @param file
	 * @return
	 */
	public OutputStream openForOutput(File file,
			LispMessage mesg) throws IOException {
		if(file.isFile() && ((options & NO_FAIL) != 0)) {
			throw mesg.getError("err.io.file.alreadyexists",
					file.toString());
		} else if(!file.isFile() && ((options & NO_CREATE) != 0)) {
			throw mesg.getError("err.io.file.notexist",
					file.toString());
		}
		return new FileOutputStream(file,
				(options & NO_TRUNCATE) != 0);
	}

	/**
	 * 
	 * @param file
	 * @return
	 */
	public InputStream openForInput(File file,
			LispMessage mesg) throws IOException {
		if(!file.isFile()) {
			throw mesg.getError("err.io.file.notexist",
					file.toString());
		}
		return new FileInputStream(file);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<file-option");
		if((options & NO_FAIL)     != 0)  buf.append(" no-fail");
		if((options & NO_CREATE)   != 0)  buf.append(" no-create");
		if((options & NO_TRUNCATE) != 0)  buf.append(" no-truncate");
		buf.append(">");
	}

}
