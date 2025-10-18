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
package net.morilib.util.xml;

import java.io.StringReader;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/20
 */
public class TestXMLDeclaration extends TC {

	/**
	 * 
	 * @throws Exception
	 */
	public void testXML1() throws Exception {
		XMLDeclaration d;
		String s = "<xml></xml>";

		d = XMLDeclarationParser.parseDeclaration(
				new StringReader(s));
		eq(d, null);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testXML2() throws Exception {
		XMLDeclaration d;
		String s = "<?xml version=\"1.1\"?>";

		d = XMLDeclarationParser.parseDeclaration(
				new StringReader(s));
		eq(d.encoding, null);
		eq(d.version, "1.1");
		eq(d.standalone, null);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testXML3() throws Exception {
		XMLDeclaration d;
		String s = "<?xml  version=\"1.1\" \t?>";

		d = XMLDeclarationParser.parseDeclaration(
				new StringReader(s));
		eq(d.encoding, null);
		eq(d.version, "1.1");
		eq(d.standalone, null);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testXML4() throws Exception {
		XMLDeclaration d;
		String s = "<?xml version=\"1.1\" encoding='Shift_JIS' ?>";

		d = XMLDeclarationParser.parseDeclaration(
				new StringReader(s));
		eq(d.encoding, "Shift_JIS");
		eq(d.version, "1.1");
		eq(d.standalone, null);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testXML5() throws Exception {
		XMLDeclaration d;
		String s = "<?xml version=\"1.1\" encoding='Shift_JIS'" +
				" standalone='no' ?>";

		d = XMLDeclarationParser.parseDeclaration(
				new StringReader(s));
		eq(d.encoding, "Shift_JIS");
		eq(d.version, "1.1");
		eq(d.standalone, Boolean.FALSE);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testXML6() throws Exception {
		XMLDeclaration d;
		String s = "<?xml version=\"1.1\" standalone='no' ?>";

		d = XMLDeclarationParser.parseDeclaration(
				new StringReader(s));
		eq(d.encoding, null);
		eq(d.version, "1.1");
		eq(d.standalone, Boolean.FALSE);
	}

}
