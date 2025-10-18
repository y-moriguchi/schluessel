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
import java.io.InputStream;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/07
 */
public class UTF8Test extends TC {

	private void invalid(InputStream ins) throws Exception {
		try {
			UTF8.read(ins);
			fail();
		} catch (IllegalUTFException e) {
		}
	}

	public void testRead1() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"MiuraAzusa".getBytes());

		eq(UTF8.read(bis), 'M');
		eq(UTF8.read(bis), 'i');
		eq(UTF8.read(bis), 'u');
		eq(UTF8.read(bis), 'r');
		eq(UTF8.read(bis), 'a');
		eq(UTF8.read(bis), 'A');
		eq(UTF8.read(bis), 'z');
		eq(UTF8.read(bis), 'u');
		eq(UTF8.read(bis), 's');
		eq(UTF8.read(bis), 'a');
	}

	public void testRead2() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"Schlüssel".getBytes());

		eq(UTF8.read(bis), 'S');
		eq(UTF8.read(bis), 'c');
		eq(UTF8.read(bis), 'h');
		eq(UTF8.read(bis), 'l');
		eq(UTF8.read(bis), 'ü');
		eq(UTF8.read(bis), 's');
		eq(UTF8.read(bis), 's');
		eq(UTF8.read(bis), 'e');
		eq(UTF8.read(bis), 'l');
	}

	public void testRead3() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"如月千早".getBytes());

		eq(UTF8.read(bis), '如');
		eq(UTF8.read(bis), '月');
		eq(UTF8.read(bis), '千');
		eq(UTF8.read(bis), '早');
	}

	public void testRead4() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf0, (byte)0x90, (byte)0x80, (byte)0x80
		});
		eq(UTF8.read(bis), 0x10000);
	}

	public void testRead5() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf8, (byte)0x88, (byte)0x80, (byte)0x80,
				(byte)0x80
		});
		eq(UTF8.read(bis), 0x200000);
	}

	public void testRead6() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0x84, (byte)0x80, (byte)0x80,
				(byte)0x80, (byte)0x80
		});
		eq(UTF8.read(bis), 0x4000000);
	}

	public void testRead7() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xc1, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead8() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xE0, (byte)0x9f, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead9() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf0, (byte)0x8f, (byte)0xbf, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead10() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf8, (byte)0x87, (byte)0xbf, (byte)0xbf,
				(byte)0xbf
		});
		invalid(bis);
	}

	public void testRead11() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0x83, (byte)0xbf, (byte)0xbf,
				(byte)0xbf, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead12() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xc2, (byte)0xcf
		});
		invalid(bis);
	}

	public void testRead13() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xE0, (byte)0xff, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead14() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xE0, (byte)0xbf, (byte)0xff
		});
		invalid(bis);
	}

	public void testRead15() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf0, (byte)0xd0, (byte)0xbf, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead16() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf0, (byte)0x90, (byte)0xff, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead17() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf0, (byte)0x90, (byte)0xbf, (byte)0xff
		});
		invalid(bis);
	}

	public void testRead18() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf8, (byte)0xc8, (byte)0xbf, (byte)0xbf,
				(byte)0xbf
		});
		invalid(bis);
	}

	public void testRead19() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf8, (byte)0x88, (byte)0xff, (byte)0xbf,
				(byte)0xbf
		});
		invalid(bis);
	}

	public void testRead20() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf8, (byte)0x88, (byte)0xbf, (byte)0xff,
				(byte)0xbf
		});
		invalid(bis);
	}

	public void testRead21() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xf8, (byte)0x88, (byte)0xbf, (byte)0xbf,
				(byte)0xff
		});
		invalid(bis);
	}

	public void testRead22() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0xc4, (byte)0xbf, (byte)0xbf,
				(byte)0xbf, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead23() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0x84, (byte)0xff, (byte)0xbf,
				(byte)0xbf, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead24() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0x84, (byte)0xbf, (byte)0xff,
				(byte)0xbf, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead25() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0x84, (byte)0xbf, (byte)0xbf,
				(byte)0xff, (byte)0xbf
		});
		invalid(bis);
	}

	public void testRead26() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[] {
				(byte)0xfc, (byte)0x84, (byte)0xbf, (byte)0xbf,
				(byte)0xbf, (byte)0xff
		});
		invalid(bis);
	}

	public void testWrite1() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF8.write(bos, 'A');
		UTF8.write(bos, 'z');
		UTF8.write(bos, 'u');
		UTF8.write(bos, 's');
		UTF8.write(bos, 'a');
		eq(bos.toByteArray(), "Azusa".getBytes());
	}

	public void testWrite2() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF8.write(bos, 'S');
		UTF8.write(bos, 'c');
		UTF8.write(bos, 'h');
		UTF8.write(bos, 'l');
		UTF8.write(bos, 'ü');
		UTF8.write(bos, 's');
		UTF8.write(bos, 's');
		UTF8.write(bos, 'e');
		UTF8.write(bos, 'l');
		eq(bos.toByteArray(), "Schlüssel".getBytes());
	}

	public void testWrite3() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF8.write(bos, '中');
		UTF8.write(bos, '野');
		UTF8.write(bos, '梓');
		eq(bos.toByteArray(), "中野梓".getBytes());
	}

	public void testWrite4() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF8.write(bos, 0x10000);
		eq(bos.toByteArray(), new byte[] {
				(byte)0xf0, (byte)0x90, (byte)0x80, (byte)0x80
		});
	}

	public void testWrite5() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF8.write(bos, 0x200000);
		eq(bos.toByteArray(), new byte[] {
			(byte)0xf8, (byte)0x88, (byte)0x80, (byte)0x80,
			(byte)0x80
		});
	}

	public void testWrite6() throws Exception {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		UTF8.write(bos, 0x4000000);
		eq(bos.toByteArray(), new byte[] {
			(byte)0xfc, (byte)0x84, (byte)0x80, (byte)0x80,
			(byte)0x80, (byte)0x80
		});
	}

}
