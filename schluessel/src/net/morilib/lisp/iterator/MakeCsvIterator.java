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
package net.morilib.lisp.iterator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public class MakeCsvIterator extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum  d1  = SubrUtils.nextIf(itr, mesg, body);
		String sep = SubrUtils.nextStringOptional(
				itr, mesg, body, ",");
		int quo = SubrUtils.nextCharacterOptional(
				itr, mesg, body, '\"');
		int nln = SubrUtils.nextCharacterOptional(
				itr, mesg, body, '\n');
		int cmt = SubrUtils.nextCharacterOptional(
				itr, mesg, body, -1);
		Reader rd = null;

		SubrUtils.checkTerminated(itr, body, mesg);
		try {
			rd = new BufferedReader(new FileReader(
					LispFiles.getFile(env, d1, mesg)));
			return new LispCSVIterator(rd, sep, quo, cmt, nln, mesg);
		} catch(IOException e) {
			throw mesg.getError("err.io");
		}
	}

}
