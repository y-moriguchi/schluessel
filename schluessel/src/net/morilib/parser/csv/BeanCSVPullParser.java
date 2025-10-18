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
package net.morilib.parser.csv;

import java.io.Closeable;
import java.io.IOException;
import java.io.Reader;
import java.lang.reflect.Method;
import java.text.Format;
import java.text.ParseException;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.TreeMap;

import net.morilib.parser.BeanIntrospection;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/18
 */
public final class BeanCSVPullParser<T>
implements CSVPullParser<T>, Closeable {

	//
	private class Hndl implements CSVHandler {

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#startFile()
		 */
		public boolean startFile() throws CSVException {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#endFile()
		 */
		public boolean endFile() throws CSVException {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#startLine(int)
		 */
		public boolean startLine(int line) throws CSVException {
			try {
				toreturn = klasse.newInstance();
				return false;
			} catch (InstantiationException e) {
				throw new CSVException(e);
			} catch (IllegalAccessException e) {
				throw new CSVException(e);
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#endLine(int)
		 */
		public boolean endLine(int line) throws CSVException {
			return true;
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#emptyLine(int)
		 */
		public boolean emptyLine(int line) throws CSVException {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#element(java.lang.String, int, int)
		 */
		public boolean element(
				String elm, int line, int field) throws CSVException {
			if(field > format.lastKey()) {
				// field length overflow
				return false;
			} else if(callmtd.get(field) == null) {
				// ignore
				return false;
			}

			try {
				BeanIntrospection.setFormattedProperty(
						callmtd.get(field),
						format.get(field),
						elm,
						toreturn);
				return false;
			} catch (ParseException e) {
				// parse error
				return false;
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#comment(java.lang.String, int)
		 */
		public boolean comment(
				String com, int line) throws CSVException {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.csv.CSVHandler#error(net.morilib.csv.CSVParseException)
		 */
		public boolean error(CSVParseException e) throws CSVException {
			throw e;
		}

	}

	//
	private static final CSVParserContinuation MARKER =
		new CSVParserContinuation() {};

	//
	private final Hndl handler = new Hndl();
	private Class<T> klasse;
	private CSVPushParser parser;
	private CSVParserContinuation cont;
	private T toreturn;
	private SortedMap<Integer, Method> callmtd;
	private SortedMap<Integer, Format> format;
	private Reader stream;

	/**
	 * 
	 * @param rd
	 * @param f
	 * @param klasse
	 */
	public BeanCSVPullParser(
			Reader rd, CSVConfig f, Class<T> klasse) {
		parser  = new SimpleCSVPushParser(handler, f);
		callmtd = new TreeMap<Integer, Method>();
		stream  = rd;

		//
		Method[] mth0 = klasse.getMethods();

		for(Method m : mth0) {
			if(m.isAnnotationPresent(CSVField.class)) {
				CSVField fn = m.getAnnotation(CSVField.class);

				callmtd.put(fn.value(), m);
				if(m.isAnnotationPresent(CSVFormat.class)) {
					CSVFormat   ff = m.getAnnotation(CSVFormat.class);
					Class<?>[] kls = m.getParameterTypes();

					if(kls.length != 1) {
						throw new IllegalArgumentException(
								"Illegal setter");
					} else if(ff != null) {
						format.put(
								fn.value(),
								BeanIntrospection.introspectFormat(
										kls[0], ff.value()));
					} else {
						format.put(fn.value(), null);
					}
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public T get() {
		return toreturn;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public boolean next() throws CSVException, IOException {
		if(cont == null) {
			throw new NoSuchElementException();
		} else if(cont == MARKER) {
			cont = parser.parse(stream);
		}

		if((cont = parser.parse(cont)) == null) {
			stream.close();
			return false;
		} else {
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		if(stream != null) {
			stream.close();
			stream = null;  cont = null;
		}
	}

}
