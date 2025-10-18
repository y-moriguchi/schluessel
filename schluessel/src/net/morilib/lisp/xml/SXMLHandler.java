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

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Symbol;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;
import net.morilib.util.xml.XMLDeclaration;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.ext.DefaultHandler2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/20
 */
/*package*/ class SXMLHandler extends DefaultHandler2 {

	//
	private Stack2<ConsListBuilder> builder;
	private boolean trim;
	private boolean comment;
	private XMLDeclaration decl;

	/**
	 * 
	 * @param trim
	 * @param comment
	 */
	public SXMLHandler(
			XMLDeclaration decl, boolean trim, boolean comment) {
		this.trim    = trim;
		this.comment = comment;
		this.decl    = decl;
		builder = new ArrayListStack<ConsListBuilder>();
		builder.add(new ConsListBuilder());
	}

	//
	private Datum processAttributes(Attributes attrs) {
		List<Integer> l = new ArrayList<Integer>();
		ConsListBuilder b = new ConsListBuilder();
		ConsListBuilder c = new ConsListBuilder();
		Datum d;

		for(int i = 0; i < attrs.getLength(); i++) {
			String a = attrs.getQName(i);

			if(a.startsWith("xmlns:") || a.equals("xmlns")) {
				l.add(i);
			} else {
				b.append(LispUtils.list(
						Symbol.getSymbol(attrs.getQName(i)),
						new LispString(attrs.getValue(i))));
			}
		}

		if(l.size() > 0) {
			for(int i = 0; i < l.size(); i++) {
				String z;
	
				z = attrs.getQName(l.get(i));
				if(z.equals("xmlns")) {
					z = "*DEFAULT*";
				} else {
					z = z.replaceFirst("^xmlns:", "");
				}
				c.append(LispUtils.list(
						Symbol.getSymbol(z),
						new LispString(attrs.getValue(l.get(i)))));
			}
			d = LispUtils.listDot(
					c.get(),
					Symbol.getSymbol("*NAMESPACES*"));
			d = LispUtils.list(Symbol.getSymbol("@"), d);
			b.append(d);
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#startDocument()
	 */
	@Override
	public void startDocument() throws SAXException {
		builder.peek().append(Symbol.getSymbol("*TOP*"));
		if(decl != null) {
			builder.peek().append(LispUtils.list(
					Symbol.getSymbol("*PI*"),
					Symbol.getSymbol("xml"),
					new LispString(decl.toString())));
		}
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(
			String uri, String localName, String qName,
			Attributes attributes) throws SAXException {
		String q = qName;

		if(uri != null && !uri.equals("")) {
			q = uri + ":" + q;
		}

		builder.add(new ConsListBuilder());
		builder.peek().append(Symbol.getSymbol(q));
		if(attributes.getLength() > 0) {
			builder.peek().append(LispUtils.listDot(
					processAttributes(attributes),
					Symbol.getSymbol("@")));
		}
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void endElement(
			String uri, String localName, String qName
			) throws SAXException {
		ConsListBuilder b = builder.pop();

		builder.peek().append(b.get());
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
	 */
	@Override
	public void characters(
			char[] ch, int start, int length) throws SAXException {
		String s = new String(ch, start, length);

		if(trim) {
			s = s.trim();
		}
		if(!s.isEmpty()) {
			builder.peek().append(new LispString(s));
		}
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#processingInstruction(java.lang.String, java.lang.String)
	 */
	@Override
	public void processingInstruction(
			String target, String data) throws SAXException {
		ConsListBuilder b = new ConsListBuilder();

		b.append(Symbol.getSymbol("*PI*"));
		b.append(Symbol.getSymbol(target));
		b.append(new LispString(data));
		builder.peek().append(b.get());
	}

	/* (non-Javadoc)
	 * @see org.xml.sax.ext.DefaultHandler2#comment(char[], int, int)
	 */
	@Override
	public void comment(
			char[] ch, int start, int length) throws SAXException {
		if(comment) {
			String s = new String(ch, start, length);
	
			builder.peek().append(
					new Cons(Symbol.getSymbol("*COMMENT*"),
					new Cons(new LispString(s), Nil.NIL)));
		}
	}

	/**
	 * 
	 * @return
	 */
	public Datum getResult() {
		return builder.peek().get();
	}

}
