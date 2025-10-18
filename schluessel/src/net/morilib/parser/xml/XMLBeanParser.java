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
package net.morilib.parser.xml;

import java.io.IOException;
import java.io.InputStream;

import net.morilib.lang.ReflectionException;
import net.morilib.lingua.plural.SimplePluralConverter;
import net.morilib.util.LinkedListStack;
import net.morilib.util.Stack2;
import net.morilib.util.string.StringTransforms;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/02
 */
public abstract class XMLBeanParser {
	
	//
	private static final int _INIT = 0;
	private static final int _STR1 = 1;
	private static final int _STR2 = 2;
	private static final int _LST1 = 3;
	private static final int _ACPT = 4;
	private static final XMLBeanHelper HELPER =
		XMLBeanHelper.getInstance();
	
	//
	private class Handler extends DefaultHandler {
		
		//
		private StringBuilder buf = null;
		private Stack2<Object> pdstk = new LinkedListStack<Object>();
		private int stat;
		private Object result = null;

		//
		public void characters(
				char[] ch,
				int start,
				int length) throws SAXException {
			if(buf == null) {
				buf = new StringBuilder();
			}
			buf.append(new String(ch, start, length));
		}

		//
		public void endElement(
				String uri,
				String localName,
				String qName
				) throws SAXException {
			Object o;
			String q;
			
			switch(stat) {
			case _INIT:
				if(buf != null &&
						!buf.toString().trim().isEmpty()) {
					throw new XMLBeanParseException(
							"extra string appeared between tags");
				}
				o = pdstk.pop();
				q = (String)pdstk.pop();
				if(pdstk.isEmpty()) {
					stat = _ACPT;
					result = o;
				} else {
					HELPER.setField(pdstk.peek(), q, o);
				}
				break;
			case _STR1:
			case _STR2:
				q = (String)pdstk.pop();
				HELPER.setField(
						pdstk.peek(), q,
						buf.toString());
				buf = null;
				stat = _INIT;
				break;
			case _ACPT:
				throw new IllegalStateException();
			}
		}

		//
		public void startElement(
				String uri,
				String localName,
				String qName,
				Attributes attributes) throws SAXException {
			String q = StringTransforms.HYPHEN_TO_CAMEL.f(qName);
			
			try {
				switch(stat) {
				case _STR2:
					String s1 = StringTransforms.CAPITALIZE_FIRST.f(
							(String)pdstk.peek());
					Object o1 = HELPER.newInstance(forName(s1));
					
					pdstk.push(o1);
					// continue
				case _INIT:
					startElementInit(q, attributes);
					break;
				case _STR1:
					throw new XMLBeanParseException(
							"tag " + pdstk.peek() +
							" cannot be nested");
				case _LST1:
					if(!SimplePluralConverter.ENGLISH.isPlural(
							q, (String)pdstk.peek())) {
						throw new XMLBeanParseException(
								"tag " + q +
								" is not the plural of" +
								pdstk.peek());
					}
					pdstk.push(q);
					break;
				case _ACPT:
					throw new IllegalStateException();
				}
			} catch(ClassNotFoundException e) {
				throw new ReflectionException(e);
			}
		}

		//
		private void startElementInit(
				String q,
				Attributes attributes) throws ClassNotFoundException {
			Object o, p;
			
			p = pdstk.isEmpty() ? null : pdstk.peek();
			pdstk.push(q);
			if(p != null &&
					attributes.getLength() == 0 &&
					HELPER.isStringField(p, q)) {
				stat = _STR1;
			} else if(p != null &&
					attributes.getLength() == 0 &&
					HELPER.isStringListField(p, q)) {
				stat = _STR2;
			} else if(p != null &&
					attributes.getLength() == 0 &&
					HELPER.isStringListFieldExtra(p, q)) {
				stat = _LST1;
			} else if(buf != null &&
					!buf.toString().trim().isEmpty()) {
				throw new XMLBeanParseException(
						"extra string appeared between tags");
			} else {
				o = HELPER.newInstance(forName(
						StringTransforms.CAPITALIZE_FIRST.f(q)));
				for(int i = 0; i < attributes.getLength(); i++) {
					HELPER.setField(o,
							attributes.getQName(i),
							attributes.getValue(i));
				}
				pdstk.push(o);
			}
			buf = null;
		}
		
	}
	
	/**
	 * 
	 * @param cls
	 */
	protected XMLBeanParser() {
		// do nothing
	}
	
	/**
	 * Load the class from the given class name.<br>
	 * If you want to load a non-public class,
	 * you override this method and load the class.
	 * 
	 * @param name  a class name
	 * @return  a class correspond to the given name
	 * @throws ClassNotFoundException 
	 */
	protected abstract Class<?> forName(
			String name) throws ClassNotFoundException;
	
	/**
	 * Parse the given InputStream of a XML file.<br>
	 * Warning: This method manipulates accessible flag.
	 * If you use security manager,
	 * you should configure security policy.
	 * 
	 * @param ins  a stream of a XML file
	 * @throws SAXException
	 * @throws IOException
	 */
	public Object parse(
			InputStream ins
			) throws SAXException, IOException {
		XMLReader r = XMLReaderFactory.createXMLReader();
		Handler   h = new Handler();
		
		r.setContentHandler(h);
		r.parse(new InputSource(ins));
		return h.result;
	}
	
}
