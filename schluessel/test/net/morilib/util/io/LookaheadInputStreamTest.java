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

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/07
 */
public class LookaheadInputStreamTest extends TC {

	//
	private static final byte[] _DAT1 = new byte[] {
		'a', 'b', 'c'
	};

	public void testReadLookahead1() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(_DAT1);
		LookaheadInputStream lis = new LookaheadInputStream(bis);

		eq(lis.lookahead(), 'a');
		eq(lis.read(), 'a');
		eq(lis.lookahead(), 'b');
		eq(lis.read(), 'b');
		eq(lis.lookahead(), 'c');
		eq(lis.read(), 'c');
		eq(lis.lookahead(), -1);
		eq(lis.read(), -1);
		lis.close();
	}

	public void testReadLookahead2() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[0]);
		LookaheadInputStream lis = new LookaheadInputStream(bis);

		eq(lis.lookahead(), -1);
		eq(lis.read(), -1);
		lis.close();
	}

	public void testReadLookahead3() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"MiuraAzusa".getBytes());
		LookaheadInputStream lis = new LookaheadInputStream(bis);
		byte[] b = new byte[5];

		eq(lis.lookahead(), 'M');
		eq(lis.read(b), 5);
		eq(b, "Miura".getBytes());
		eq(lis.lookahead(), 'A');
		eq(lis.read(b), 5);
		eq(b, "Azusa".getBytes());
		eq(lis.lookahead(), -1);
		eq(lis.read(), -1);
		lis.close();
	}

	public void testReadLookahead4() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"FutamiAmiMami".getBytes());
		LookaheadInputStream lis = new LookaheadInputStream(bis);
		byte[] b = new byte[6];

		eq(lis.lookahead(), 'F');
		eq(lis.read(b), 6);
		eq(b, "Futami".getBytes());
		eq(lis.lookahead(), 'A');
		eq(lis.read(b), 6);
		eq(b, "AmiMam".getBytes());
		eq(lis.lookahead(), 'i');
		eq(lis.read(b), 1);
		eq(b[0], 'i');
		eq(lis.lookahead(), -1);
		eq(lis.read(), -1);
		lis.close();
	}

	public void testReadLookahead5() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[0]);
		LookaheadInputStream lis = new LookaheadInputStream(bis);
		byte[] b = new byte[6];

		eq(lis.lookahead(), -1);
		eq(lis.read(b), -1);
		lis.close();
	}

	public void testSkipLookahead3() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"MiuraAzusa".getBytes());
		LookaheadInputStream lis = new LookaheadInputStream(bis);

		eq(lis.lookahead(), 'M');
		eq(lis.skip(5), 5);
		eq(lis.lookahead(), 'A');
		eq(lis.skip(5), 5);
		eq(lis.lookahead(), -1);
		eq(lis.skip(1), 0);
		lis.close();
	}

	public void testSkipLookahead4() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(
				"FutamiAmiMami".getBytes());
		LookaheadInputStream lis = new LookaheadInputStream(bis);

		eq(lis.lookahead(), 'F');
		eq(lis.skip(6), 6);
		eq(lis.lookahead(), 'A');
		eq(lis.skip(6), 6);
		eq(lis.lookahead(), 'i');
		eq(lis.skip(6), 1);
		eq(lis.lookahead(), -1);
		eq(lis.skip(1), 0);
		lis.close();
	}

	public void testSkipLookahead5() throws Exception {
		ByteArrayInputStream bis = new ByteArrayInputStream(new byte[0]);
		LookaheadInputStream lis = new LookaheadInputStream(bis);

		eq(lis.lookahead(), -1);
		eq(lis.skip(1), 0);
		lis.close();
	}

}
