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

import java.io.IOException;
import java.io.Reader;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/20
 */
public class XMLDeclarationParser {

	//
	private static final int INIT          =   0;
	private static final int INIT_1        =   1;
	private static final int INIT_2        =   2;
	private static final int INIT_X        =   3;
	private static final int INIT_M        =   4;
	private static final int INIT_L        =   5;
	private static final int VERSION_0     = 101;
	private static final int VERSION_V     = 102;
	private static final int VERSION_E     = 103;
	private static final int VERSION_R     = 104;
	private static final int VERSION_S     = 105;
	private static final int VERSION_I     = 106;
	private static final int VERSION_O     = 107;
	private static final int VERSION_N     = 108;
	private static final int ENC_OR_ST     = 200;
	private static final int ENCODING_E    = 401;
	private static final int ENCODING_N1   = 402;
	private static final int ENCODING_C    = 403;
	private static final int ENCODING_O    = 404;
	private static final int ENCODING_D    = 405;
	private static final int ENCODING_I    = 406;
	private static final int ENCODING_N2   = 407;
	private static final int ENCODING_G    = 408;
	private static final int STANDALONE_0  = 501;
	private static final int STANDALONE_S  = 502;
	private static final int STANDALONE_T  = 503;
	private static final int STANDALONE_A1 = 504;
	private static final int STANDALONE_N1 = 505;
	private static final int STANDALONE_D  = 506;
	private static final int STANDALONE_A2 = 507;
	private static final int STANDALONE_L  = 508;
	private static final int STANDALONE_O  = 509;
	private static final int STANDALONE_N2 = 510;
	private static final int STANDALONE_E  = 511;
	private static final int END_0         = 901;
	private static final int END_1         = 902;

	//
	private static boolean isWhitespace(int c) {
		return (c == ' '  || c == '\t' ||
				c == '\r' || c == '\n');
	}

	//
	private static int skipWhitespace(Reader rd, int bc
			) throws IOException, XMLDeclarationException {
		int c = bc;

		while(isWhitespace(c)) {
			if((c = rd.read()) < 0) {
				throw new XMLDeclarationException();
			}
		}
		return c;
	}

	//
	private static String readString(Reader rd, int bc
			) throws XMLDeclarationException, IOException {
		StringBuilder b = new StringBuilder();
		int c = bc, d;

		skipWhitespace(rd, c);
		if(c != '=') {
			throw new XMLDeclarationException();
		}

		if((c = rd.read()) < 0) {
			throw new XMLDeclarationException();
		}
		skipWhitespace(rd, c);

		d = c;
		if(c != '\'' && c != '\"') {
			throw new XMLDeclarationException();
		}

		do {
			if((c = rd.read()) < 0) {
				throw new XMLDeclarationException();
			}
			b.append((char)c);
		} while(c != '\'' && c != '\"');
		b.deleteCharAt(b.length() - 1);

		if(c != d) {
			throw new XMLDeclarationException();
		}
		return b.toString();
	}

	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 * @throws XMLDeclarationException 
	 */
	public static XMLDeclaration parseDeclaration(
			Reader rd) throws IOException, XMLDeclarationException {
		XMLDeclaration d = XMLDeclaration.newDefault();
		int state = INIT;
		int c;

		while((c = rd.read()) > 0) {
			switch(state) {
			case INIT:
				if(c == '<') {
					state = INIT_1;
					break;
				} else {
					return null;
				}
			case INIT_1:
				if(c == '?') {
					state = INIT_2;
					break;
				} else {
					return null;
				}
			case INIT_2:
				if(c == 'x' || c == 'X') {
					state = INIT_X;
					break;
				} else {
					return null;
				}
			case INIT_X:
				if(c == 'm' || c == 'M') {
					state = INIT_M;
					break;
				} else {
					return null;
				}
			case INIT_M:
				if(c == 'l' || c == 'L') {
					state = INIT_L;
					break;
				} else {
					return null;
				}
			case INIT_L:
				if(!isWhitespace(c)) {
					return null;
				}
				state = VERSION_0;
				break;
			case VERSION_0:
				c = skipWhitespace(rd, c);
				if(c == 'v') {
					state = VERSION_V;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_V:
				if(c == 'e') {
					state = VERSION_E;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_E:
				if(c == 'r') {
					state = VERSION_R;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_R:
				if(c == 's') {
					state = VERSION_S;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_S:
				if(c == 'i') {
					state = VERSION_I;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_I:
				if(c == 'o') {
					state = VERSION_O;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_O:
				if(c == 'n') {
					state = VERSION_N;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case VERSION_N:
				d.version = readString(rd, c);
				state = ENC_OR_ST;
				break;
			case ENC_OR_ST:
				c = skipWhitespace(rd, c);
				if(c == 'e') {
					state = ENCODING_E;
					break;
				} else if(c == 's') {
					state = STANDALONE_S;
					break;
				} else if(c == '?') {
					state = END_1;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_E:
				if(c == 'n') {
					state = ENCODING_N1;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_N1:
				if(c == 'c') {
					state = ENCODING_C;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_C:
				if(c == 'o') {
					state = ENCODING_O;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_O:
				if(c == 'd') {
					state = ENCODING_D;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_D:
				if(c == 'i') {
					state = ENCODING_I;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_I:
				if(c == 'n') {
					state = ENCODING_N2;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_N2:
				if(c == 'g') {
					state = ENCODING_G;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case ENCODING_G:
				d.encoding = readString(rd, c);
				state = STANDALONE_0;
				break;
			case STANDALONE_0:
				c = skipWhitespace(rd, c);
				if(c == 's') {
					state = STANDALONE_S;
					break;
				} else if(c == '?') {
					state = END_1;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_S:
				if(c == 't') {
					state = STANDALONE_T;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_T:
				if(c == 'a') {
					state = STANDALONE_A1;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_A1:
				if(c == 'n') {
					state = STANDALONE_N1;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_N1:
				if(c == 'd') {
					state = STANDALONE_D;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_D:
				if(c == 'a') {
					state = STANDALONE_A2;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_A2:
				if(c == 'l') {
					state = STANDALONE_L;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_L:
				if(c == 'o') {
					state = STANDALONE_O;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_O:
				if(c == 'n') {
					state = STANDALONE_N2;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_N2:
				if(c == 'e') {
					state = STANDALONE_E;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case STANDALONE_E:
				d.standalone = readString(rd, c).equals("yes");
				state = END_0;
				break;
			case END_0:
				c = skipWhitespace(rd, c);
				if(c == '?') {
					state = END_1;
					break;
				} else {
					throw new XMLDeclarationException();
				}
			case END_1:
				if(c == '>') {
					return d;
				} else {
					throw new XMLDeclarationException();
				}
			}
		}
		throw new XMLDeclarationException();
	}

}
