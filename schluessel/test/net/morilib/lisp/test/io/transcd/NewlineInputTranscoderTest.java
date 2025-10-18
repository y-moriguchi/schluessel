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

import java.io.ByteArrayInputStream;

import net.morilib.lisp.r6rs.io.transcd.LispEolStyle;
import net.morilib.lisp.r6rs.io.transcd.LispErrorHandlingMode;
import net.morilib.lisp.r6rs.io.transcd.NewlineInputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.UTF8InputTranscoder;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/08
 */
public class NewlineInputTranscoderTest extends TC {

	public void testRead1() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"@\r\na".getBytes());
		UTF8InputTranscoder tr1 = new UTF8InputTranscoder(bis,
				LispErrorHandlingMode.IGNORE);
		NewlineInputTranscoder tr = new NewlineInputTranscoder(tr1,
				LispEolStyle.CRLF, '\n');

		eq(tr.read(), '@');
		eq(tr.read(), '\n');
		eq(tr.read(), 'a');
		eq(tr.read(), -1);
		tr.close();
	}

	public void testRead2() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"@\ra".getBytes());
		UTF8InputTranscoder tr1 = new UTF8InputTranscoder(bis,
				LispErrorHandlingMode.IGNORE);
		NewlineInputTranscoder tr = new NewlineInputTranscoder(tr1,
				LispEolStyle.CRLF, '\n');

		eq(tr.read(), '@');
		eq(tr.read(), '\r');
		eq(tr.read(), 'a');
		eq(tr.read(), -1);
		tr.close();
	}

	public void testRead3() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"@\na".getBytes());
		UTF8InputTranscoder tr1 = new UTF8InputTranscoder(bis,
				LispErrorHandlingMode.IGNORE);
		NewlineInputTranscoder tr = new NewlineInputTranscoder(tr1,
				LispEolStyle.LF, '\n');

		eq(tr.read(), '@');
		eq(tr.read(), '\n');
		eq(tr.read(), 'a');
		eq(tr.read(), -1);
		tr.close();
	}

}
