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

import net.morilib.lisp.subr.BinaryArgs;

public class SubrSetCarS extends BinaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		if(c1a instanceof Cons) {
			((Cons)c1a).setCar(c2a);
			return Undef.UNDEF;
		} else {
			//throw new LispException("pair required");
			throw mesg.getError("err.require.pair", c1a);
		}
	}

}
