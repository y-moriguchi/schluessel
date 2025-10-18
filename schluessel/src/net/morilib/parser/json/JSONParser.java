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
package net.morilib.parser.json;

import java.io.IOException;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/21
 */
public class JSONParser {

	//
	private enum State {
		INIT,
		ARRAY,  ARRAY_SEP,
		OBJECT, OBJECT_KEYSEP, OBJECT_VALUE, OBJECT_SEP,
		STRING, STRING_ESCAPE,
		LITERAL,
		NUMBER, NUMBER_FRAC, NUMBER_EXP,
	};

	//
	private static interface Readable {

		//
		public int read() throws IOException;

	}

	//
	private static class JavaHandler1 implements JSONParseHandler {

		//
		/*package*/ Stack<Object> stack = new Stack<Object>();

		public void begin() {
		}

		public Object eof() {
			return stack.pop();
		}

		public void beginArray() {
			stack.push(new ArrayList<Object>());
		}

		public void beginObject() {
			stack.push(new HashMap<Object, Object>());
		}

		@SuppressWarnings("unchecked")
		public void nextArray() {
			Object o = stack.pop();
			List<Object> l = (List<Object>)stack.peek();

			l.add(o);
		}

		public void endArray() {
			nextArray();
		}

		public void nextValue() {
		}

		@SuppressWarnings("unchecked")
		public void nextObject() {
			Object v = stack.pop();
			Object k = stack.pop();
			Map<Object, Object> l = (Map<Object, Object>)stack.peek();

			l.put(k, v);
		}

		public void endObject() {
			nextObject();
		}

		public void setString(String s) {
			stack.push(s);
		}

		public void setTrue() {
			stack.push(Boolean.TRUE);
		}

		public void setFalse() {
			stack.push(Boolean.FALSE);
		}

		public void setNull() {
			stack.push(null);
		}

		public void setInteger(BigInteger x) {
			stack.push(x);
		}

		public void setFraction(BigDecimal x) {
			stack.push(x);
		}

		/* (non-Javadoc)
		 * @see net.morilib.parser.json.JSONParseHandler#emptyArray()
		 */
		public void emptyArray() {
		}

		/* (non-Javadoc)
		 * @see net.morilib.parser.json.JSONParseHandler#emptyObject()
		 */
		public void emptyObject() {
		}

	};

	//
	private static class JavaHandler2 extends JavaHandler1 {

		public void setInteger(BigInteger x) {
			stack.push(x.longValue());
		}

		public void setFraction(BigDecimal x) {
			stack.push(x.doubleValue());
		}

	}

	//
	private static boolean isws(int c) {
		return (c == 0x20 || c == 0x09 || c == 0x0a || c == 0x0d);
	}

	//
	private static boolean isdelimiter(int c) {
		return (c == ']' || c == '}' || c == ',' || c == ':');
	}

	//
	private static int skipws(Readable rd) throws IOException {
		for(int c = 0; (c = rd.read()) >= 0;) {
			if(!isws(c)) {
				return c;
			}
		}
		return -1;
	}

	//
	private static int skipws(int ch, Readable rd) throws IOException {
		return isdelimiter(ch) ? ch : skipws(rd);
	}

	//
	private static int noteof(
			Readable rd) throws JSONParseException, IOException {
		int c = rd.read();

		if(c < 0) {
			throw new JSONParseException("invalid EOF");
		} else {
			return c;
		}
	}

	//
	private static int readhex(
			Readable rd) throws JSONParseException, IOException {
		int c = rd.read();

		if(c < 0) {
			throw new JSONParseException("invalid EOF");
		} else if(c >= '0' && c <= '9') {
			return c - '0';
		} else if(c >= 'a' && c <= 'f') {
			return c - 'a' + 10;
		} else if(c >= 'A' && c <= 'F') {
			return c - 'A' + 10;
		} else {
			throw new JSONParseException(
					"invalid character" + (char)c);
		}
	}

	//
	private static BigInteger getinteger(
			int sign, String s) throws JSONParseException {
		if(s.isEmpty()) {
			throw new JSONParseException("invalid character");
		}
		return (sign < 0) ?
				new BigInteger(s).negate() : new BigInteger(s);
	}

	//
	private static BigDecimal getfraction(
			int sign, String d, String f) throws JSONParseException {
		String s;

		if(f.isEmpty()) {
			throw new JSONParseException("invalid character");
		}
		s = d + "." + f;
		return (sign < 0) ?
				new BigDecimal(s).negate() : new BigDecimal(s);
	}

	//
	private static BigDecimal getfraction(
			int sign, String d, String f,
			int signe, String e) throws JSONParseException {
		String s;

		if(e.isEmpty() || (f.isEmpty() && d.isEmpty())) {
			throw new JSONParseException("invalid character");
		}
		s = ((f.isEmpty() ? d : (d + "." + f)) +
				"e" + ((signe < 0) ? "-" : "+") + e);
		return (sign < 0) ?
				new BigDecimal(s).negate() : new BigDecimal(s);
	}

	//
	private static Object parse(
			Readable rd,
			JSONParseHandler h
			) throws JSONParseException, IOException {
		State st = State.INIT;
		StringBuilder b  = new StringBuilder();
		StringBuilder b2 = new StringBuilder();
		StringBuilder b3 = new StringBuilder();
		Stack<State> ss = new Stack<State>();
		int c = 0, sign = 1, singe = 1, quot = -1;
		boolean empty = false;

		ss.push(State.INIT);
		h.begin();
		while(!ss.isEmpty()) {
			switch(st) {
			case INIT:
				c = skipws(rd);
				if(c < 0) {
					return h.eof();
				} else if(c == '[') {
					h.beginArray();
					st = State.ARRAY;
					ss.push(State.ARRAY_SEP);
					empty = true;
				} else if(c == '{') {
					h.beginObject();
					st = State.OBJECT;
					ss.push(State.OBJECT_KEYSEP);
					empty = true;
				} else if(c == '\"' || c == '\'') {
					quot = c;
					st = State.STRING;
				} else if(c == '-') {
					sign = -1;
					st = State.NUMBER;
				} else if(c >= 0x30 && c <= 0x39) {
					sign = 1;
					b.append((char)c);
					st = State.NUMBER;
				} else if(c == '.') {
					sign = 1;
					st = State.NUMBER_FRAC;
				} else if(Character.isLowerCase(c)) {
					b.append((char)c);
					st = State.LITERAL;
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case ARRAY:
				c = skipws(rd);
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == '[') {
					h.beginArray();
					st = State.ARRAY;
					ss.push(State.ARRAY_SEP);
					empty = true;
				} else if(c == '{') {
					h.beginObject();
					st = State.OBJECT;
					ss.push(State.OBJECT_KEYSEP);
					empty = true;
				} else if(c == '\"' || c == '\'') {
					quot = c;
					st = State.STRING;
				} else if(c == '-') {
					sign = -1;
					st = State.NUMBER;
				} else if(c >= 0x30 && c <= 0x39) {
					sign = 1;
					b.append((char)c);
					st = State.NUMBER;
				} else if(c == '.') {
					sign = 1;
					st = State.NUMBER_FRAC;
				} else if(Character.isLowerCase(c)) {
					b.append((char)c);
					st = State.LITERAL;
				} else if(c == ']' && empty) {
					h.emptyArray();
					ss.pop();
					st = ss.pop();
					c  = rd.read();
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case ARRAY_SEP:
				c = skipws(c, rd);
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == ',') {
					h.nextArray();
					st = State.ARRAY;
					ss.push(State.ARRAY_SEP);
					empty = false;
				} else if(c == ']') {
					h.endArray();
					st = ss.pop();
					c  = rd.read();
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case OBJECT:
				c = skipws(rd);
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == '[') {
					h.beginArray();
					st = State.ARRAY;
					ss.push(State.ARRAY_SEP);
					empty = true;
				} else if(c == '{') {
					h.beginObject();
					st = State.OBJECT;
					ss.push(State.OBJECT_KEYSEP);
					empty = true;
				} else if(c == '\"' || c == '\'') {
					quot = c;
					st = State.STRING;
				} else if(c == '-') {
					sign = -1;
					st = State.NUMBER;
				} else if(c >= 0x30 && c <= 0x39) {
					sign = 1;
					b.append((char)c);
					st = State.NUMBER;
				} else if(c == '.') {
					sign = 1;
					st = State.NUMBER_FRAC;
				} else if(Character.isLowerCase(c)) {
					b.append((char)c);
					st = State.LITERAL;
				} else if(c == '}' && empty) {
					h.emptyObject();
					ss.pop();
					st = ss.pop();
					c  = rd.read();
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case OBJECT_KEYSEP:
				c = skipws(c, rd);
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == ':') {
					h.nextValue();
					st = State.OBJECT_VALUE;
					ss.push(State.OBJECT_SEP);
					empty = false;
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case OBJECT_VALUE:
				c = skipws(rd);
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == '[') {
					h.beginArray();
					st = State.ARRAY;
					ss.push(State.ARRAY_SEP);
					empty = true;
				} else if(c == '{') {
					h.beginObject();
					st = State.OBJECT;
					ss.push(State.OBJECT_KEYSEP);
					empty = true;
				} else if(c == '\"' || c == '\'') {
					quot = c;
					st = State.STRING;
				} else if(c == '-') {
					sign = -1;
					st = State.NUMBER;
				} else if(c >= 0x30 && c <= 0x39) {
					sign = 1;
					b.append((char)c);
					st = State.NUMBER;
				} else if(c == '.') {
					sign = 1;
					st = State.NUMBER_FRAC;
				} else if(Character.isLowerCase(c)) {
					b.append((char)c);
					st = State.LITERAL;
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case OBJECT_SEP:
				c = skipws(c, rd);
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == ',') {
					h.nextObject();
					st = State.OBJECT;
					ss.push(State.OBJECT_KEYSEP);
					empty = false;
				} else if(c == '}') {
					h.endObject();
					st = ss.pop();
					c  = rd.read();
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case STRING:
				c = rd.read();
				if(c < 0) {
					throw new JSONParseException("invalid EOF");
				} else if(c == quot) {
					h.setString(b.toString());
					b = new StringBuilder();
					st = ss.pop();
					quot = -1;
				} else if(c == '\\') {
					st = State.STRING_ESCAPE;
				} else {
					b.append((char)c);
				}
				break;
			case STRING_ESCAPE:
				switch(c = noteof(rd)) {
				case '\"':  b.append('\"');  break;
				case '\\':  b.append('\\');  break;
				case '/':   b.append('/');   break;
				case 'b':   b.append('\u0008');  break;
				case 'f':   b.append('\u000c');  break;
				case 'n':   b.append('\n');  break;
				case 'r':   b.append('\r');  break;
				case 't':   b.append('\u0009'); break;
				case 'u':
					c = readhex(rd);
					c = (c << 4) | readhex(rd);
					c = (c << 4) | readhex(rd);
					c = (c << 4) | readhex(rd);
					b.append((char)c);
					break;
				default:
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				st = State.STRING;
				break;
			case LITERAL:
				c = rd.read();
				if(Character.isLowerCase(c)) {
					b.append((char)c);
				} else {
					String s = b.toString();

					if(s.equals("true")) {
						h.setTrue();
					} else if(s.equals("false")) {
						h.setFalse();
					} else if(s.equals("null")) {
						h.setNull();
					} else {
						throw new JSONParseException(
								"invalid character" + (char)c);
					}
					b = new StringBuilder();
					st = ss.pop();
				}
				break;
			case NUMBER:
				c = rd.read();
				if(c < 0 || isws(c) || isdelimiter(c)) {
					h.setInteger(getinteger(sign, b.toString()));
					b = new StringBuilder();
					st = ss.pop();
				} else if(c >= '0' && c <= '9') {
					b.append((char)c);
				} else if(c == '.') {
					st = State.NUMBER_FRAC;
				} else if(c == 'e' || c == 'E') {
					c = noteof(rd);
					if(c == '+') {
						singe = 1;
					} else if(c == '-') {
						singe = -1;
					} else if(c >= '0' && c <= '9') {
						singe = 1;
						b3.append((char)c);
					} else {
						throw new JSONParseException(
								"invalid charcter");
					}
					st = State.NUMBER_EXP;
				} else {
					throw new JSONParseException("invalid charcter");
				}
				break;
			case NUMBER_FRAC:
				c = rd.read();
				if(c < 0 || isws(c) || isdelimiter(c)) {
					h.setFraction(getfraction(
							sign, b.toString(), b2.toString()));
					b  = new StringBuilder();
					b2 = new StringBuilder();
					st = ss.pop();
				} else if(c >= '0' && c <= '9') {
					b2.append((char)c);
				} else if(c == 'e' || c == 'E') {
					c = noteof(rd);
					if(c == '+') {
						singe = 1;
					} else if(c == '-') {
						singe = -1;
					} else if(c >= '0' && c <= '9') {
						singe = 1;
						b3.append((char)c);
					} else {
						throw new JSONParseException(
								"invalid charcter");
					}
					st = State.NUMBER_EXP;
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			case NUMBER_EXP:
				c = rd.read();
				if(c < 0 || isws(c) || isdelimiter(c)) {
					h.setFraction(getfraction(sign, b.toString(),
							b2.toString(), singe, b3.toString()));
					b  = new StringBuilder();
					b2 = new StringBuilder();
					b3 = new StringBuilder();
					st = ss.pop();
				} else if(c >= '0' && c <= '9') {
					b3.append((char)c);
				} else {
					throw new JSONParseException(
							"invalid character" + (char)c);
				}
				break;
			default:
				throw new RuntimeException("internal error");
			}
		}
		return h.eof();
	}

	/**
	 * 
	 * @param reader
	 * @param handler
	 * @return
	 * @throws JSONParseException
	 * @throws IOException
	 */
	public static Object parse(
			final Reader reader,
			JSONParseHandler handler
			) throws JSONParseException, IOException {
		Readable rd = new Readable() {

			public int read() throws IOException {
				return reader.read();
			}

		};
		return parse(rd, handler);
	}

	/**
	 * 
	 * @param seq
	 * @param handler
	 * @return
	 * @throws JSONParseException
	 * @throws IOException
	 */
	public static Object parse(
			final CharSequence seq,
			JSONParseHandler handler
			) throws JSONParseException {
		Readable rd = new Readable() {

			//
			int p = 0;

			public int read() {
				return (p < seq.length()) ? seq.charAt(p++) : -1;
			}

		};

		try {
			return parse(rd, handler);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * @param reader
	 * @return
	 * @throws JSONParseException
	 * @throws IOException
	 */
	public static Object parseExact(
			Reader reader
			) throws JSONParseException, IOException {
		return parse(reader, new JavaHandler1());
	}

	/**
	 * 
	 * @param reader
	 * @return
	 * @throws JSONParseException
	 * @throws IOException
	 */
	public static Object parseExact(
			CharSequence reader
			) throws JSONParseException {
		return parse(reader, new JavaHandler1());
	}

	/**
	 * 
	 * @param reader
	 * @return
	 * @throws JSONParseException
	 * @throws IOException
	 */
	public static Object parseInexact(
			Reader reader
			) throws JSONParseException, IOException {
		return parse(reader, new JavaHandler2());
	}

	/**
	 * 
	 * @param reader
	 * @return
	 * @throws JSONParseException
	 * @throws IOException
	 */
	public static Object parseInexact(
			CharSequence reader
			) throws JSONParseException {
		return parse(reader, new JavaHandler2());
	}

}
