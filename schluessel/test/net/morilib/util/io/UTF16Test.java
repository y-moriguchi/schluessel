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
package net.morilib.util.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/08
 */
public class UTF16Test extends TC {

	public void testRead1b() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				0x00, '@'
		});
		eq(UTF16.read(bis, UTF16.BIG_ENDIAN), '@');
	}

	public void testRead1l() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				'@', 0x00
		});
		eq(UTF16.read(bis, UTF16.LITTLE_ENDIAN), '@');
	}

	public void testRead2b() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xD8, (byte)0x00, (byte)0xDF, (byte)0x02
		});
		eq(UTF16.read(bis, UTF16.BIG_ENDIAN), 0x10302);
	}

	public void testRead2l() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0x00, (byte)0xD8, (byte)0x02, (byte)0xDF
		});
		eq(UTF16.read(bis, UTF16.LITTLE_ENDIAN), 0x10302);
	}

	public void testWrite1b() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF16.write(bos, '@', UTF16.BIG_ENDIAN);
		eq(bos.toByteArray(), new byte[] {
			0x00, '@'
		});
	}

	public void testWrite1l() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF16.write(bos, '@', UTF16.LITTLE_ENDIAN);
		eq(bos.toByteArray(), new byte[] {
			'@', 0x00
		});
	}

	public void testWrite2b() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF16.write(bos, 0x10302, UTF16.BIG_ENDIAN);
		eq(bos.toByteArray(), new byte[] {
			(byte)0xD8, (byte)0x00, (byte)0xDF, (byte)0x02
		});
	}

	public void testWrite2l() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF16.write(bos, 0x10302, UTF16.LITTLE_ENDIAN);
		eq(bos.toByteArray(), new byte[] {
			(byte)0x00, (byte)0xD8, (byte)0x02, (byte)0xDF
		});
	}

	public void testGetInts() {
		eq(UTF16.getInts("\ud800\udf02@"), new int[] {
			0x10302, '@'
		});
	}

	public void testToString() {
		eq(UTF16.toString(new int[] {
			0x10302, '@'
		}), "\ud800\udf02@");
	}

}
