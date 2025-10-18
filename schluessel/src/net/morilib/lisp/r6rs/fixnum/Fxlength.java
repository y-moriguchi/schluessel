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
package net.morilib.lisp.r6rs.fixnum;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.util.BitUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/29
 */
public class Fxlength extends AbstractFixnumUnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.fixnum.AbstractFixnumUnaryArgs#execute(int, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(int x, Environment env, LispMessage mesg) {
		return LispInteger.valueOf((x >= 0) ?
				BitUtils.getMsb(x) : BitUtils.getMsb(~x));
	}

}
