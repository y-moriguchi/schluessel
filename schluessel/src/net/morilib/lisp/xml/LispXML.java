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
package net.morilib.lisp.xml;

import java.io.IOException;
import java.io.Reader;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import net.morilib.lisp.Datum;
import net.morilib.util.xml.XMLDeclaration;
import net.morilib.util.xml.XMLDeclarationException;
import net.morilib.util.xml.XMLDeclarationParser;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/20
 */
public class LispXML {

	/**
	 * 
	 * @param rd
	 * @param trim
	 * @param comment
	 * @return
	 * @throws XMLParseException
	 * @throws IOException 
	 */
	public static Datum parseSXML(
			Reader rd, boolean trim, boolean comment
			) throws XMLParseException, IOException {
		SAXParser parser;
		SXMLHandler dh;
		XMLDeclaration decl;

		try {
			decl = XMLDeclarationParser.parseDeclaration(rd);
			dh = new SXMLHandler(decl, trim, comment);
			parser = SAXParserFactory.newInstance().newSAXParser();
			parser.setProperty(
					"http://xml.org/sax/properties/lexical-handler",
					dh);
			parser.parse(new InputSource(rd), dh);
			return dh.getResult();
		} catch (ParserConfigurationException e) {
			throw new XMLParseException();
		} catch (SAXException e) {
			throw new XMLParseException();
		} catch (XMLDeclarationException e) {
			throw new XMLParseException();
		}
	}

}
