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
package net.morilib.lingua;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/27
 */
public final class GreekUtils {

	//
	private GreekUtils() {}

	/**
	 * 
	 * @param latin
	 * @return
	 */
	public static String latinToGreekAlphabet(String latin) {
		StringBuilder b = new StringBuilder();
		int c, state = 0;

		for(int p = 0; true; p++) {
			c = (p < latin.length()) ? latin.charAt(p) : -1;
			switch(state) {
			case 0:
				switch(c) {
				case 'a':  b.append('α');  break;
				case 'b':  b.append('β');  break;
				case 'c':  state = 10;  break;
				case 'd':  b.append('δ');  break;
				case 'e':  state = 20;  break;
				case 'g':  b.append('γ');  break;
				case 'h':
					if(p != 0)  b.append('h');
					break;
				case 'i':  b.append('ι');  break;
				case 'k':  state = 30;  break;
				case 'l':  b.append('λ');  break;
				case 'm':  b.append('μ');  break;
				case 'n':  state = 80;  break;
				case 'o':  state = 40;  break;
				case 'p':  state = 50;  break;
				case 'r':  state = 60;  break;
				case 's':
					b.append((p == latin.length() - 1) ? 'ς' : 'σ');
					break;
				case 't':  state = 70;  break;
				case 'u':  b.append('υ');  break;
				case 'x':  b.append('ξ');  break;
				case 'y':  b.append('υ');  break;
				case 'z':  state = 90;  break;
				case 'A':  b.append('Α');  break;
				case 'B':  b.append('Β');  break;
				case 'C':  state = 11;  break;
				case 'D':  b.append('Δ');  break;
				case 'E':  state = 21;  break;
				case 'G':  b.append('Γ');  break;
				case 'H':
					if(p != 0)  b.append('H');
					break;
				case 'I':  b.append('Ι');  break;
				case 'K':  state = 31;  break;
				case 'L':  b.append('Λ');  break;
				case 'M':  b.append('Μ');  break;
				case 'N':  state = 81;  break;
				case 'O':  state = 41;  break;
				case 'P':  state = 51;  break;
				case 'R':  state = 61;  break;
				case 'S':  b.append('Σ');  break;
				case 'T':  state = 71;  break;
				case 'U':  b.append('Υ');  break;
				case 'X':  b.append('Ξ');  break;
				case 'Y':  b.append('Υ');  break;
				case 'Z':  state = 91;  break;
				case -1:   break;
				default:   b.append(c);  break;
				}
				break;
			case 10:
				if(c == 'h') {
					b.append('χ');
				} else {
					b.append('c');  p--;
				}
				state = 0;
				break;
			case 20:
				if(c == 'e') {
					b.append('η');
				} else {
					b.append('ε');  p--;
				}
				state = 0;
				break;
			case 30:
				if(c == 'h') {
					b.append('χ');
				} else if(c == 's') {
					b.append('ξ');
				} else {
					b.append('κ');  p--;
				}
				state = 0;
				break;
			case 40:
				if(c == 'o') {
					b.append('ω');
				} else {
					b.append('ο');  p--;
				}
				state = 0;
				break;
			case 50:
				if(c == 'h') {
					b.append('φ');
				} else if(c == 's') {
					b.append('ψ');
				} else {
					b.append('π');  p--;
				}
				state = 0;
				break;
			case 60:
				b.append('ρ');
				if(c != 'h')  p--;
				state = 0;
				break;
			case 70:
				if(c == 'h') {
					b.append('θ');
				} else {
					b.append('τ');  p--;
				}
				state = 0;
				break;
			case 80:
				if(c == 'g') {
					b.append("γγ");
					state = 0;
				} else if(c == 'x') {
					b.append("γξ");
					state = 0;
				} else if(c == 'k') {
					state = 1000;
				} else if(c == 'c') {
					state = 1010;
				} else {
					b.append('ν');  p--;
					state = 0;
				}
				break;
			case 90:
				b.append('ζ');
				if(c != 'd')  p--;
				state = 0;
				break;
			case 1000:
				if(c == 's') {
					b.append("γξ");
				} else if(c == 'h') {
					b.append("γχ");
				} else {
					b.append("νκ");  p--;
				}
				state = 0;
				break;
			case 1010:
				if(c == 'h') {
					b.append("γχ");
				} else {
					b.append("νc");  p--;
				}
				state = 0;
				break;
			case 11:
				if(c == 'H' || c == 'h') {
					b.append('Χ');
				} else {
					b.append('C');  p--;
				}
				state = 0;
				break;
			case 21:
				if(c == 'E' || c == 'e') {
					b.append('Η');
				} else {
					b.append('Ε');  p--;
				}
				state = 0;
				break;
			case 31:
				if(c == 'H' || c == 'h') {
					b.append('Χ');
				} else if(c == 'S' || c == 's') {
					b.append('Ξ');
				} else {
					b.append('Κ');  p--;
				}
				state = 0;
				break;
			case 41:
				if(c == 'O' || c == 'o') {
					b.append('Ω');
				} else {
					b.append('Ο');  p--;
				}
				state = 0;
				break;
			case 51:
				if(c == 'H' || c == 'h') {
					b.append('Φ');
				} else if(c == 'S' || c == 's') {
					b.append('Ψ');
				} else {
					b.append('Π');  p--;
				}
				state = 0;
				break;
			case 61:
				b.append('Ρ');
				if(c != 'H' && c != 'h')  p--;
				state = 0;
				break;
			case 71:
				if(c == 'H' || c == 'h') {
					b.append('Θ');
				} else {
					b.append('Τ');  p--;
				}
				state = 0;
				break;
			case 81:
				if(c == 'g') {
					b.append("Γγ");
					state = 0;
				} else if(c == 'G') {
					b.append("ΓΓ");
					state = 0;
				} else if(c == 'x') {
					b.append("Γξ");
					state = 0;
				} else if(c == 'X') {
					b.append("ΓΞ");
					state = 0;
				} else if(c == 'k') {
					state = 1001;
				} else if(c == 'c') {
					state = 1011;
				} else if(c == 'K') {
					state = 1002;
				} else if(c == 'C') {
					state = 1012;
				} else {
					b.append('Ν');  p--;
					state = 0;
				}
				break;
			case 91:
				b.append('Ζ');
				if(c != 'D' && c != 'd')  p--;
				state = 0;
				break;
			case 1001:
				if(c == 's') {
					b.append("Γξ");
				} else if(c == 'h') {
					b.append("Γχ");
				} else {
					b.append("Νκ");  p--;
				}
				state = 0;
				break;
			case 1011:
				if(c == 'h') {
					b.append("Γχ");
				} else {
					b.append("Νc");  p--;
				}
				state = 0;
				break;
			case 1002:
				if(c == 'S') {
					b.append("ΓΞ");
				} else if(c == 'H') {
					b.append("ΓΧ");
				} else {
					b.append("ΝΚ");  p--;
				}
				state = 0;
				break;
			case 1012:
				if(c == 'H') {
					b.append("ΓΧ");
				} else {
					b.append("ΝC");  p--;
				}
				state = 0;
				break;
			default:  throw new RuntimeException();
			}
			if(c < 0)  return b.toString();
		}
	}

	/**
	 * 
	 * @param greek
	 * @return
	 */
	public static String greekToLatinAlphabet(String greek) {
		StringBuilder b = new StringBuilder();
		int c = -1, q = -1, l;

		for(int p = 0; p < greek.length(); p++) {
			q = c;
			l = (p + 1 < greek.length()) ? greek.charAt(p + 1) : 'A';
			switch(c = greek.charAt(p)) {
			case 'α':  b.append("a");  break;
			case 'β':  b.append("b");  break;
			case 'γ':  b.append("g");  break;
			case 'δ':  b.append("d");  break;
			case 'ε':  b.append("e");  break;
			case 'ζ':  b.append("zd");  break;
			case 'η':  b.append("ee");  break;
			case 'θ':  b.append("th");  break;
			case 'ι':  b.append("i");  break;
			case 'κ':  b.append("k");  break;
			case 'λ':  b.append("l");  break;
			case 'μ':  b.append("m");  break;
			case 'ν':  b.append("n");  break;
			case 'ξ':  b.append("ks");  break;
			case 'ο':  b.append("o");  break;
			case 'π':  b.append("p");  break;
			case 'ρ':  b.append((p == 0) ? "rh" : "r");  break;
			case 'σ':  b.append("s");  break;
			case 'ς':  b.append("s");  break;
			case 'τ':  b.append("t");  break;
			case 'υ':
				if(q == 'ε' || q == 'ο' ||
						q == 'α' || q == 'η' ||
						q == 'Ε' || q == 'Ο' ||
						q == 'Α' || q == 'Η') {
					b.append("u");
				} else if(p == 0) {
					b.append("hy");
				} else {
					b.append("y");
				}
				break;
			case 'φ':  b.append("ph");  break;
			case 'χ':  b.append("ch");  break;
			case 'ψ':  b.append("ps");  break;
			case 'ω':  b.append("oo");  break;
			case 'Α':  b.append("A");  break;
			case 'Β':  b.append("B");  break;
			case 'Γ':  b.append("G");  break;
			case 'Δ':  b.append("D");  break;
			case 'Ε':  b.append("E");  break;
			case 'Ζ':  b.append("ZD");  break;
			case 'Η':
				b.append(Character.isUpperCase(l) ? "EE" : "Ee");
				break;
			case 'Θ':
				b.append(Character.isUpperCase(l) ? "TH" : "Th");
				break;
			case 'Ι':  b.append("I");  break;
			case 'Κ':  b.append("K");  break;
			case 'Λ':  b.append("L");  break;
			case 'Μ':  b.append("M");  break;
			case 'Ν':  b.append("N");  break;
			case 'Ξ':
				b.append(Character.isUpperCase(l) ? "KS" : "Ks");
				break;
			case 'Ο':  b.append("O");  break;
			case 'Π':  b.append("P");  break;
			case 'Ρ':
				if(p == 0) {
					b.append(Character.isUpperCase(l) ? "RH" : "Rh");
				} else {
					b.append("R");
				}
				break;
			case 'Σ':  b.append("S");  break;
			case 'Τ':  b.append("T");  break;
			case 'Υ':
				if(q == 'ε' || q == 'ο' ||
						q == 'α' || q == 'η' ||
						q == 'Ε' || q == 'Ο' ||
						q == 'Α' || q == 'Η') {
					b.append("U");
				} else if(p == 0) {
					b.append(Character.isUpperCase(l) ? "HY" : "Hy");
				} else {
					b.append("Y");
				}
				break;
			case 'Φ':
				b.append(Character.isUpperCase(l) ? "PH" : "Ph");
				break;
			case 'Χ':
				b.append(Character.isUpperCase(l) ? "CH" : "Ch");
				break;
			case 'Ψ':
				b.append(Character.isUpperCase(l) ? "PS" : "Ps");
				break;
			case 'Ω':
				b.append(Character.isUpperCase(l) ? "OO" : "Oo");
				break;
			}
		}
		return b.toString();
	}

}
