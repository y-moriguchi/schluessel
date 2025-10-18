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

import net.morilib.lisp.r6rs.io.transcd.BlockBufferedOutputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.StringOutputTranscoder;
import net.morilib.lisp.test.TC;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/08
 */
public class BlockBufferedOutputTranscoderTest extends TC {

	public void testWrite1() throws Exception {
		StringOutputTranscoder tr1 = new StringOutputTranscoder();
		BlockBufferedOutputTranscoder tr =
			new BlockBufferedOutputTranscoder(tr1, 21);

		tr.write(UTF16.getInts("MiuraAzusa"));
		eq(tr1.toString(), "");
		tr.write(UTF16.getInts("AkizukiRitsuko"));
		eq(tr1.toString(), "MiuraAzusaAkizukiRits");
		tr.write(UTF16.getInts(
				"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"));
		eq(tr1.toString(),
				"MiuraAzusaAkizukiRitsukoABCDEFGHIJKLMNOPQRSTUVWXYZ" +
				"abcdefghijklm");
		tr.flush();
		eq(tr1.toString(),
				"MiuraAzusaAkizukiRitsukoABCDEFGHIJKLMNOPQRSTUVWXYZ" +
				"abcdefghijklmnopqrstuvwxyz");
		tr.close();
	}

	public void testWrite2() throws Exception {
		StringOutputTranscoder tr1 = new StringOutputTranscoder();
		BlockBufferedOutputTranscoder tr =
			new BlockBufferedOutputTranscoder(tr1, 11);

		tr.write(UTF16.getInts("MiuraAzusa"));
		eq(tr1.toString(), "");
		tr.write('-');
		eq(tr1.toString(), "MiuraAzusa-");
		tr.close();
	}

}
