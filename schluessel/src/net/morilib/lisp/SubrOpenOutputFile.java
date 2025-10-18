/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.List;

import net.morilib.lisp.file.LispFiles;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SubrOpenOutputFile extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);

		if(l.size() == 1) {
			Datum c1a = l.get(0);

			if(c1a instanceof LispString) {
				String fn = ((LispString)c1a).getString();

				try {
					return new OutputPort(
							LispFiles.getFile(env, fn), mesg);
				} catch (IOException e) {
					throw mesg.getError("err.io", e.getMessage());
				}
			} else {
				throw mesg.getError("err.require.string", c1a);
				//throw new LispException("string required");
			}
		} else if(l.size() == 2) {
			Datum c1a = l.get(0);
			Datum c2a = l.get(1);

			if(!(c1a instanceof LispString)) {
				throw mesg.getError("err.require.string", c1a);
			} else if(!(c2a instanceof LispString)) {
				throw mesg.getError("err.require.string", c2a);
			} else {
				String fn  = ((LispString)c1a).getString();
				String enc = c2a.getString();

				try {
					return new OutputPort(
							LispFiles.getFile(env, fn),
							enc, mesg);
				} catch (UnsupportedEncodingException e) {
					throw mesg.getError(
							"err.unsupportedencoding", enc);
				} catch (IOException e) {
					throw mesg.getError("err.io", e.getMessage());
				}
			}
		}
		return null;
	}

}
