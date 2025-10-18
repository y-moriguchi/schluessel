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

import net.morilib.lisp.r6rs.io.transcd.LispErrorHandlingMode;
import net.morilib.lisp.r6rs.io.transcd.UTF16InputTranscoder;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/08
 */
public class UTF16InputTranscoderTest extends TC {

	public void test_Init1() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfe, (byte)0xff, 0, '@'
		});
		UTF16InputTranscoder tr = new UTF16InputTranscoder(bis,
				LispErrorHandlingMode.IGNORE);

		eq(tr.read(), '@');
		tr.close();
	}

	public void test_Init2() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xff, (byte)0xfe, '@', 0
		});
		UTF16InputTranscoder tr = new UTF16InputTranscoder(bis,
				LispErrorHandlingMode.IGNORE);

		eq(tr.read(), '@');
		tr.close();
	}

	public void test_Init3() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				0, '@'
		});
		UTF16InputTranscoder tr = new UTF16InputTranscoder(bis,
				LispErrorHandlingMode.IGNORE);

		eq(tr.read(), '@');
		tr.close();
	}

}
