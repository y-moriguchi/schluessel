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
package net.morilib.lisp.r6rs.io.transcd;

import java.io.IOException;

import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public final class Transcoders {

	/**
	 * 
	 * @param tri
	 * @param tro
	 * @throws IOException
	 */
	public static void copy(InputTranscoder tri,
			OutputTranscoder tro) throws IOException {
		int[] b = new int[1024];
		int l;

		while((l = tri.read(b)) >= 0) {
			tro.write(b, 0, l);
		}
	}

	/**
	 * 
	 * @param tri
	 * @param tro
	 * @throws IOException
	 */
	public static void copyWithClose(InputTranscoder tri,
			OutputTranscoder tro) throws IOException {
		try {
			copy(tri, tro);
		} finally {
			IOs.close(tri);
			IOs.close(tro);
		}
	}

}
