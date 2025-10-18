/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.format;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

public class LispFormat {
	
	private static final LispFormat DEFAULT_INS = new LispFormat();
	
	//
	private int tilde = '~';
	private int comma = ',';
	private int charf = '\'';
	private int atmrk = '@';
	private int colon = ':';
	private int varargc = 'V';
	private FormatCommandFactory factory =
		MapFormatCommandFactory.getInstance();
	
	
	private LispFormat() {
		// do nothing
	}
	
	
	public static final LispFormat getInstance() {
		return DEFAULT_INS;
	}
	
	
	private static enum State {
		INIT,
		TILDE,
		TILDE_SIGN,
		TILDE_NUM,
		TILDE_COMMA,
		TILDE_CHAR,
		FLAG,
		ACCEPT,
	}
	
	
	private boolean isVarargChar(int c) {
		return Character.toUpperCase(c) == varargc;
	}
	
	private boolean isNum(int c) {
		return c >= '0' && c <= '9';
	}
	
	private void addSimpleStr(
			List<FormatAtom> lst, StringBuilder buf) {
		if(buf.length() > 0) {
			String str = buf.toString();
			
			buf.delete(0, buf.length());
			lst.add(new SimpleString(str));
		}
	}
	
	private int getInteger(StringBuilder buf) {
		String str = buf.toString();
		
		buf.delete(0, buf.length());
		return Integer.parseInt(str);
	}
	
	private void addFormat(
			List<FormatAtom> lst, int c,
			ArgumentTypeBuf argbuf) throws FormatParseException {
		lst.add(factory.newInstance(c, argbuf));
	}
	
	private State trans(
			State st,
			Reader rd,
			List<FormatAtom> lst,
			ArgumentTypeBuf argbuf,
			StringBuilder buf) throws IOException, FormatParseException {
		State res = st;
		int c = rd.read();
		
		switch(st) {
		case INIT:
			if(c < 0) {
				addSimpleStr(lst, buf);
				res = State.ACCEPT;
			} else if(c == tilde) {
				addSimpleStr(lst, buf);
				res = State.TILDE;
			} else {
				buf.append((char)c);
			}
			break;
		case TILDE:
			if(c < 0) {
				throw new FormatParseException();
			} else if(c == '+') {
				res = State.TILDE_SIGN;
			} else if(c == '-') {
				buf.append((char)c);
				res = State.TILDE_SIGN;
			} else if(isNum(c)) {
				buf.append((char)c);
				res = State.TILDE_NUM;
			} else if(isVarargChar(c)) {
				argbuf.setVararg(true);
				res = State.TILDE_COMMA;
			} else if(c == charf) {
				res = State.TILDE_CHAR;
			} else if(c == comma) {
				argbuf.push();
			} else if(c == atmrk) {
				argbuf.setAtmark(true);
				res = State.FLAG;
			} else if(c == colon) {
				argbuf.setColon(true);
				res = State.FLAG;
			} else {
				addFormat(lst, c, argbuf);
				res = State.INIT;
			}
			break;
		case TILDE_SIGN:
			if(isNum(c)) {
				buf.append((char)c);
				res = State.TILDE_NUM;
			} else {
				throw new FormatParseException();
			}
			break;
		case TILDE_NUM:
			if(c < 0) {
				throw new FormatParseException();
			} else if(isNum(c)) {
				buf.append((char)c);
			} else if(c == comma) {
				argbuf.setInt(getInteger(buf));
				argbuf.push();
				res = State.TILDE;
			} else if(c == atmrk) {
				argbuf.setInt(getInteger(buf));
				argbuf.push();
				argbuf.setAtmark(true);
				res = State.FLAG;
			} else if(c == colon) {
				argbuf.setInt(getInteger(buf));
				argbuf.push();
				argbuf.setColon(true);
				res = State.FLAG;
			} else {
				argbuf.setInt(getInteger(buf));
				argbuf.push();
				addFormat(lst, c, argbuf);
				res = State.INIT;
			}
			break;
		case TILDE_COMMA:
			if(c < 0) {
				throw new FormatParseException();
			} else if(c == comma) {
				argbuf.push();
				res = State.TILDE;
			} else if(c == atmrk) {
				argbuf.push();
				argbuf.setAtmark(true);
				res = State.FLAG;
			} else if(c == colon) {
				argbuf.push();
				argbuf.setColon(true);
				res = State.FLAG;
			} else {
				argbuf.push();
				addFormat(lst, c, argbuf);
				res = State.INIT;
			}
			break;
		case TILDE_CHAR:
			if(c < 0) {
				throw new FormatParseException();
			} else {
				argbuf.setChar((char)c);
				res = State.TILDE_COMMA;
			}
			break;
		case FLAG:
			if(c < 0) {
				throw new FormatParseException();
			} else if(c == atmrk) {
				if(argbuf.isAtmark()) {
					throw new FormatParseException();
				} else {
					argbuf.setAtmark(true);
				}
			} else if(c == colon) {
				if(argbuf.isColon()) {
					throw new FormatParseException();
				} else {
					argbuf.setColon(true);
				}
			} else {
				addFormat(lst, c, argbuf);
				res = State.INIT;
			}
			break;
		case ACCEPT:
			throw new FormatParseException();
		default:
			throw new RuntimeException("Internal error:" + st);
		}
		
		return res;
	}
	
	
	public ParsedFormat parse(String fmt) throws FormatParseException {
		Reader rd = new StringReader(fmt);
		State  st = State.INIT;
		ArgumentTypeBuf  argbuf = new ArgumentTypeBuf();
		List<FormatAtom> lst = new ArrayList<FormatAtom>();
		StringBuilder buf = new StringBuilder();
		
		while(true) {
			try {
				st = trans(st, rd, lst, argbuf, buf);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
			
			if(st.equals(State.ACCEPT)) {
				return new ParsedFormat(lst);
			}
		}
	}
	
	
	public String format(
			ParsedFormat pfmt,
			FormatArguments args) throws LispFormatException {
		StringBuilder   buf  = new StringBuilder();
		
		for(FormatAtom atm : pfmt.getAtoms()) {
			buf.append(atm.toString(args));
		}
		return buf.toString();
	}
	
	
	public String format(
			String fmt,
			FormatArguments args) throws LispFormatException {
		return format(parse(fmt), args);
	}
	
}
