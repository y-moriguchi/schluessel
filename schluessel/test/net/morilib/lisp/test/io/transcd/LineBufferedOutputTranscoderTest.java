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
package net.morilib.lisp.test.io.transcd;

import net.morilib.lisp.r6rs.io.transcd.LineBufferedOutputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.StringOutputTranscoder;
import net.morilib.lisp.test.TC;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class LineBufferedOutputTranscoderTest extends TC {

	public void testWrite1() throws Exception {
		StringOutputTranscoder tr1 = new StringOutputTranscoder();
		LineBufferedOutputTranscoder tr =
			new LineBufferedOutputTranscoder(tr1);

		tr.write(UTF16.getInts("Nakano"));
		eq(tr1.toString(), "");
		tr.write('\n');
		eq(tr1.toString(), "Nakano\n");
		tr.write(UTF16.getInts("Azusa"));
		eq(tr1.toString(), "Nakano\n");
		tr.flush();
		eq(tr1.toString(), "Nakano\nAzusa");
		tr.close();
	}

}
