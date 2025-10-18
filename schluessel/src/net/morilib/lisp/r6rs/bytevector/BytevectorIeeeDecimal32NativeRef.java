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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispDecimal32;
import net.morilib.util.Endianness2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class BytevectorIeeeDecimal32NativeRef
extends BytevectorNNativeRef {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.bytevector.BytevectorNRef#getInt(net.morilib.util.Endianness2, byte[], int)
	 */
	@Override
	protected Datum getInt(Endianness2 e, byte[] a, int k) {
		return new LispDecimal32(
				e.readBig(a, k, getSize()).intValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.bytevector.BytevectorNRef#getSize()
	 */
	@Override
	protected int getSize() {
		return 4;
	}

}
