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
package net.morilib.lisp.r6rs.bytevector;

import java.math.BigInteger;

import net.morilib.lisp.LispMessage;
import net.morilib.util.Endianness2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class SintListToBytevector extends ListToBytevector {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.bytevector.BytevectorSetS#setInt(net.morilib.util.Endianness2, byte[], int, int, java.math.BigInteger)
	 */
	@Override
	protected void setInt(Endianness2 e, byte[] a, int k, int s,
			BigInteger i, LispMessage mesg) {
		e.write(a, k, s, i);
	}

}
