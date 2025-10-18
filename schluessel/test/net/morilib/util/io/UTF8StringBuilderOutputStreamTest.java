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

import java.io.PrintStream;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/23
 */
public class UTF8StringBuilderOutputStreamTest extends TC {

	public void testWrite() throws Exception {
		UTF8StringBuilderOutputStream ut =
			new UTF8StringBuilderOutputStream();
		PrintStream ps = new PrintStream(ut, true, "UTF-8");

		ps.println("Scheme");
		eq(ut.toString(), "Scheme\n");
		ut.clear();
		ps.println("Schlüssel");
		eq(ut.toString(), "Schlüssel\n");
		ut.clear();
		ps.println("âge");
		eq(ut.toString(), "âge\n");
		ut.clear();
		ps.println("らき☆すた けいおん！");
		eq(ut.toString(), "らき☆すた けいおん！\n");
		ut.clear();
		ps.write(0xc0);  ps.write(0xaf);  ps.flush();
		eq(ut.toString(), "");
		ut.clear();
		ps.write(0xe0);  ps.write(0x80);  ps.write(0xaf);  ps.flush();
		eq(ut.toString(), "");
		ut.clear();
		ut.close();
	}

}
