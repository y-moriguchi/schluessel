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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.io.DirectoryGlobber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class FindFilenames extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextString(itr, mesg, body);
		String t = SubrUtils.nextString(itr, null, mesg);
		String p = "", q;
		ConsListBuilder b = new ConsListBuilder();
		File[] fs;
		File g;

		try {
			if(t != null) {
				if(s.charAt(0) == '/') {
					g = new File(s);
				} else {
					g = new File(new File(LispFiles.chdir), s);
				}
				fs = DirectoryGlobber.glob(g, p = t);
			} else if(s.charAt(0) == '/') {
				fs = DirectoryGlobber.glob(p = s);
			} else {
				fs = DirectoryGlobber.glob(
						new File(LispFiles.chdir), p = s);
			}
		} catch(IllegalArgumentException e) {
//			e.printStackTrace();
			throw mesg.getError("err.file.pattern.invalid", p);
		}

		for(File f : fs) {
			q = f.getPath();
			if(t == null && s.charAt(0) != '/') {
				q = q.replaceFirst("^" + LispFiles.chdir + "/", "");
			}
			b.append(new LispString(q));
		}
		return b.get();
	}

}
