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
import java.io.IOException;
import java.util.Arrays;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;
import net.morilib.util.io.filter.WildcardSyntaxException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/24
 */
public class DirectoryList extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.getString(
				SubrUtils.nextIf(itr, mesg, "err.argument"), mesg);
		File   f;
		Datum  g = Iterators.nextIf(itr, Nil.NIL);

		try {
			f = LispFiles.getFile(env, s);
		} catch (IOException e1) {
			throw mesg.getError("err.io");
		}

		if(f.isDirectory()) {
			String[] ss;
			ConsListBuilder r = new ConsListBuilder();

			try {
				ss = f.list(LispFiles.compileFilter(g, mesg));
			} catch(WildcardSyntaxException e) {
				throw mesg.getError(
						"err.file.invalid.wildcard", g);
			}
			Arrays.sort(ss);
			for(String x : ss) {
				r.append(new LispString(x));
			}
			return r.get();
		} else {
			throw mesg.getError("err.file.notdirectory", s);
		}
	}

}
