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
package net.morilib.lisp.r6rs.enums;

import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/22
 */
public final class R6RSEnumUtils {

	//
	private R6RSEnumUtils() {}

	//
	static ILispR6RSEnum getEnum(Datum d, LispMessage mesg) {
		if(d instanceof ILispR6RSEnum) {
			return (ILispR6RSEnum)d;
		} else {
			throw mesg.getError("err.r6rs.require.enum", d);
		}
	}

	//
	static ILispR6RSEnum nextEnum(Iterator<Datum> itr,
			Datum body, LispMessage mesg) {
		if(itr.hasNext()) {
			return getEnum(itr.next(), mesg);
		} else {
			throw mesg.getError("err.argument", body);
		}
	}

}
