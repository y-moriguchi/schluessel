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
package net.morilib.lisp.exlib;

import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispJavaUtils;
import net.morilib.lisp.ParameterNotFoundException;
import net.morilib.lisp.JavaUtils;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/17
 */
public class ApplyJavaStatic extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String c = SubrUtils.nextString(itr, mesg, body);
		String s = SubrUtils.nextSymbolName(itr, mesg, body);
		List<Datum> l = LispUtils.consToList(itr.rest(), mesg);

		try {
			Class<?> cls = Class.forName(c);
	
			try {
				return LispJavaUtils.newInstance(
						JavaUtils.invokeMethod(cls, null, s, l));
			} catch(ParameterNotFoundException e) {
				if(l.size() == 0) {
					try {
						return LispJavaUtils.newInstance(
								JavaUtils.getStaticField(cls, s));
					} catch (NoSuchFieldException e1) {
						// ignore
					}
				}
				throw mesg.getError("err.java.method.notfound", s);
			}
		} catch(ClassNotFoundException e) {
			throw mesg.getError("err.java.class.notfound", c);
		} catch(NullPointerException e) {
			throw mesg.getError("err.java.staticmethod.notfound", s);
		}
	}

}
