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
package net.morilib.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.lang.Hashes;

/**
 * <i>USEful Implements</i> for strings.
 * <p>文字列に関する便利な関数である.
 * 
 * @author MORIGUCHI, Yuichiro 2004/12/11
 */
public final class Strings {

	//
	private static final int PADDING_CHAR = ' ';
	
	//
	private static final String BLANK_CHARS = " \t";
	
	/**
	 * the constant used in
	 * {@link Strings#find(String, String, int, int, boolean)}.
	 * <p>{@link Strings#find(String, String, int, int, boolean)}
	 * にて使用される
	 * 定数である.
	 * 
	 * @see Strings#find(String, String, int, int, boolean)
	 */
	public static final boolean UNTIL = true;
	
	/**
	 * the constant used in
	 * {@link Strings#find(String, String, int, int, boolean)}.
	 * <p>{@link Strings#find(String, String, int, int, boolean)}
	 * にて使用される
	 * 定数である.
	 * 
	 * @see Strings#find(String, String, int, int, boolean)
	 */
	public static final boolean WHILE = false;
	
	//
	private Strings() {}

	// 
	static StringBuffer appendFill(
			StringBuffer buf, int c, int len) {
		// fill blanks
		for(int i = 0; i < len; i++) {
			buf.append((char)c);
		}
		return buf;
	}

	//
	static StringBuffer buildFill(int c, int len) {
		return appendFill(new StringBuffer(), c, len);
	}

	//
	static StringBuffer buildFill(String s, int c, int len) {
		return appendFill(new StringBuffer(s), c, len);
	}

	/**
	 * finds the index at which a character of the second argument
	 * set appears in the first string.
	 * <p>If the argument <i>type</i> is {@link #UNTIL}, returns
	 * the index at which a character of the character set
	 * <i>appears</i> first.
	 * If <i>type</i> is {@link #WHILE}, returns
	 * the index at which a character <i>does not</i> appear first.
	 * 
	 * <p>第2引数の文字列にある文字が第1引数の文字列に現れる場所を見つける.
	 * 引数<i>type</i>が{@link #UNTIL}のときは、文字セット中の文字が
	 * 最初に<i>現れる</i>場所を返す.
	 * 引数<i>type</i>が{@link #WHILE}のときは、文字セット中の文字が
	 * 最初に<i>現れない</i>場所を返す.
	 * <p>文字列の検索範囲は引数beginから引数end - 1までの範囲である.
	 * <p>検索する文字列がnullのときは無条件で-1を返す.
	 * 
	 * @param line  a string to find
	 * @param str   a character set
	 * @param begin a index from which the set is find, inclusive
	 * @param end   a index to which the set is find, exclusive
	 * @return  the index at which the characters was found,
	 * &lt 0 if could not found
	 */
	public static int find(
			String line, String str, int begin, int end, boolean type) {
		if(begin < 0 || end < 0 || begin > end) {
			throw new IllegalArgumentException(
					"illegal index: from " + begin + " to " + end);
		} else if(line == null) {
			return -1;
		}
		
		end = Math.min(end, line.length());
		for(int i = begin; i < end; i++) {
			if((str.indexOf(line.charAt(i)) < 0) ^ type) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * finds the index at which a character of the second argument
	 * set appears in the first string.
	 * <p>If the argument <i>type</i> is {@link #UNTIL}, returns
	 * the index at which a character of the character set
	 * <i>appears</i> first.
	 * If <i>type</i> is {@link #WHILE}, returns
	 * the index at which a character <i>does not</i> appear first.
	 * 
	 * <p>第2引数の文字列にある文字が第1引数の文字列に現れる場所を見つける.
	 * 引数<i>type</i>が{@link #UNTIL}のときは、文字セット中の文字が
	 * 最初に<i>現れる</i>場所を返す.
	 * 引数<i>type</i>が{@link #WHILE}のときは、文字セット中の文字が
	 * 最初に<i>現れない</i>場所を返す.
	 * <p>検索する文字列がnullのときは無条件で-1を返す.
	 * 
	 * @param line  a string to find
	 * @param str   a character set
	 * @param begin a index from which the set is find, inclusive
	 */
	public static int find(
			String line, String str, int begin, boolean type) {
		return find(line, str, begin, line.length(), type);
	}

	/**
	 * finds the index at which a character of the second argument
	 * set appears in the first string.
	 * <p>If the argument <i>type</i> is {@link #UNTIL}, returns
	 * the index at which a character of the character set
	 * <i>appears</i> first.
	 * If <i>type</i> is {@link #WHILE}, returns
	 * the index at which a character <i>does not</i> appear first.
	 * 
	 * <p>第2引数の文字列にある文字が第1引数の文字列に現れる場所を見つける.
	 * 引数<i>type</i>が{@link #UNTIL}のときは、文字セット中の文字が
	 * 最初に<i>現れる</i>場所を返す.
	 * 引数<i>type</i>が{@link #WHILE}のときは、文字セット中の文字が
	 * 最初に<i>現れない</i>場所を返す.
	 * <p>検索する文字列がnullのときは無条件で-1を返す.
	 * 
	 * @param line   a string to find
	 * @param string a character set
	 */
	public static int find(String line, String string, boolean type) {
		return find(line, string, 0, line.length(), type);
	}
	
	/**
	 * gets a character at the given index from a string.
	 * <p>If the string is null, this returns -1.
	 * If the index is negative or more than the length of the string,
	 * this also returns -1.
	 * <p>指定された場所の文字を文字列から取り出す.
	 * 文字列がnullのときは-1を返す.
	 * 引数が負数または文字列の長さより大きいときも-1を返す.
	 * 
	 * @param line  a string
	 * @param i     a index
	 * @return  the character at the given index
	 */
	public static int charAt(String line, int i) {
		return ((line == null || i < 0 || i >= line.length()) ?
				-1 : line.charAt(i));
	}


	/**
	 * validates that all characters of the given string are spaces and
	 * tabs.
	 * <p>引数の文字列が全て空白かタブであるならばtrueを返す.
	 * nullのときはfalseである.
	 * 
	 * @param x  a string to validate
	 * @return  true if all characters of the string are blanks
	 */
	public static boolean isBlank(String x) {
		if(x == null) {
			return false;
		} else {
			return find(x, BLANK_CHARS, 1, x.length(), WHILE) < 0;
		}
	}
	
	/**
	 * returns true if the given string is null or empty.
	 * <p>引数の文字列がnullかから文字列のときにtrueを得る.
	 * 
	 * @param x  a string to validate
	 */
	public static boolean isEmpty(String x) {
		return x == null || x.equals("");
	}
	
	/**
	 * returns true if the given strings are equal each other,
	 * or both of the strings are null.
	 * <p>引数の文字列が互いに等しいまたは両方ともnullのときにtrueを得る.
	 * 
	 * @param x1  a string to be tested
	 * @param x2  another string to be tested
	 */
	public static boolean equals(String x1, String x2) {
		/*return
		 (x1 == null && x2 == null)   ||
		 (x1 != null && x1.equals(x2));*/
		return Objects.equals(x1, x2);
	}

	// from Oracle
	/**
	 * trims leftside blanks of the given string.
	 * <p>文字列の左側の空白を削ります.
	 * nullのときはなにもしない.
	 * 
	 * @param line  a string to trim
	 */
	public static String ltrim(String line) {
		if(line == null) {
			return null;
		} else {
			int x = find(line, BLANK_CHARS, WHILE);
			
			return (x < 0) ? "" : right(line, line.length() - x);
		}
	}
	
	/**
	 * pads left side by the given character.
	 * <p>文字列を与えられた文字で左詰する.
	 * 結果は引数のStringBufferに追加される.
	 * 詰める前の文字列がnullのときはなにもしない.
	 * 
	 * @param buf  a StringBuffer to be appended the result
	 * @param s    a string to be padded
	 * @param len  length of the result
	 * @return  the padded string
	 */
	private static StringBuffer lpad(
			StringBuffer buf, String s, int len, int c) {
		if(len < 0) {
			throw new IllegalArgumentException(
					"negative length: " + len);
		} else {
			int l = len - s.length();
			
			if(l > 0) {
				appendFill(buf, c, l);
			}
			return buf.append(s);
		}
	}

	/**
	 * pads left side by the given character.
	 * <p>文字列を与えられた文字で右詰する.
	 * 結果は引数のStringBufferに追加される.
	 * 詰める前の文字列がnullのときはなにもしない.
	 * 
	 * @param buf  a StringBuffer to be appended the result
	 * @param s    a string to be padded
	 * @param len  length of the result
	 * @return  the padded string
	 */
	private static StringBuffer rpad(
			StringBuffer buf, String s, int len, int c) {
		if(len < 0) {
			throw new IllegalArgumentException(
					"negative length: " + len);
		} else {
			int l = len - s.length();
			
			buf.append(s);
			if(l > 0) {
				appendFill(buf, c, l);
			}
			return buf;
		}
	}
	
	/**
	 * pads left side by the given character.
	 * <p>文字列を与えられた文字で左詰する.
	 * 詰める前の文字列がnullのときはnullを返す.
	 * <pre>
	 * lpad(null,  *, *)   = null
	 * lpad("",    2, '0') = "00"
	 * lpad("abc", 5, '0') = "00abc"
	 * lpad("abc", 3, '0') = "abc"
	 * lpad("abc", 2, '0') = "abc"
	 * </pre>
	 * 
	 * @param s    a string to be padded
	 * @param len  length of the result
	 * @return  the padded string
	 */
	public static String lpad(String s, int len, int c) {
		if(s == null) {
			return null;
		} else {
			return Strings.lpad(
					new StringBuffer(), s, c, len).toString();
		}
	}
	
	/**
	 * pads left side by the given character.
	 * <p>文字列を与えられた文字で右詰する.
	 * 詰める前の文字列がnullのときはnullを返す.
	 * <pre>
	 * lpad(null,  any, any)  = null
	 * lpad("",    2, '0')    = "00"
	 * lpad("abc", 5, '0')    = "abc00"
	 * lpad("abc", 3, '0')    = "abc"
	 * lpad("abc", 2, '0')    = "abc"
	 * </pre>
	 * 
	 * @param s    a string to be padded
	 * @param len  length of the result
	 * @return  the padded string
	 */
	public static String rpad(String s, int len, int c) {
		if(s == null) {
			return null;
		} else {
			return Strings.rpad(
					new StringBuffer(), s, c, len).toString();
		}
	}
	
	/**
	 * pads left side by half-width space.
	 * <p>文字列を半角スペースで左詰する.
	 * 詰める前の文字列がnullのときはなにもしない.
	 * 
	 * @param s    a string to be padded
	 * @param len  length of the result
	 * @return  the padded string
	 */
	public static String lpad(String s, int len) {
		return lpad(s, PADDING_CHAR, len);
	}
	
	/**
	 * pads left side by half-width space.
	 * <p>文字列を半角スペースで右詰する.
	 * 詰める前の文字列がnullのときはなにもしない.
	 * 
	 * @param s    a string to be padded
	 * @param len  length of the result
	 * @return  the padded string
	 */
	public static String rpad(String s, int len) {
		return rpad(s, PADDING_CHAR, len);
	}
	
	/**
	 * if the given string is not null, this method returns the string
	 * itself; if the string is null, returns an empty string.
	 * <p>引数の文字列がnullのときは引数の文字列そのものを返す.
	 * 文字列がnullのときは空文字列を返す.
	 * 
	 * @param src  the string
	 */
	public static String nvl(String src) {
		return (src != null) ? src : "";
	}
	
	/**
	 * if the first string is not null, this method returns the string
	 * itself; if the first string is null, returns the second string.
	 * <p>第1引数の文字列がnullのときは第1引数の文字列そのものを返す.
	 * 文字列がnullのときはから第2引数を返す.
	 * 
	 * @param src     the string
	 * @param ifnull  the return value if the first string is null
	 */
	public static String nvl(String src, String ifnull) {
		return (src != null) ? src : ifnull;
	}
	
	// from BASIC
	/**
	 * gets the substring from left of the given string.
	 * <p>指定された長さの文字列を左から取得する.
	 * 引数がnullのときはnullを返す.
	 * <pre>
	 * right(null,  any) = null
	 * right("abc", 0)   = ""
	 * right("abc", 1)   = "a"
	 * right("abc", 3)   = "abc"
	 * right("abc", 4)   = "abc"
	 * </pre>
	 * 
	 * @param line   a string
	 * @param length      length of the result substring
	 */
	public static String left(String line, int length) {
		if(length < 0) {
			throw new IllegalArgumentException(
					"the given length is negative:" + length);
		} else if(line == null) {
			return null;
		} else if(length == 0) {
			return "";
		} else {
			return ((line.length() > length) ?
					line.substring(0, length) : line);
		}
	}

	/**
	 * gets the substring from left of the given string.
	 * <p>指定された長さの文字列を左から取得する.
	 * 引数がnullのときはnullを返す.
	 * <pre>
	 * right(null,  any) = null
	 * right("abc", 0)   = ""
	 * right("abc", 1)   = "c"
	 * right("abc", 3)   = "abc"
	 * right("abc", 4)   = "abc"
	 * </pre>
	 * 
	 * @param line   a string
	 * @param length      length of the result substring
	 */
	public static String right(String line, int length) {
		if(length < 0) {
			throw new IllegalArgumentException(
					"the given length is negative:" + length);
		} else if(line == null) {
			return null;
		} else if(length == 0) {
			return "";
		} else {
			int l = line.length();
			
			return (l > length) ? line.substring(l - length, l) : line;
		}
	}

	// from Perl
	/**
	 * removes the last character from the given string.
	 * <p>与えられた文字列の最後の文字を取り除く.
	 * nullのときはnullを返す.
	 * 
	 * @param line  a string to chop
	 */
	public static String chop(String line) {
		if(line == null) {
			return null;
		} else {
			int x = line.length();
			
			return (x > 0) ? line.substring(0, x - 1) : "";
		}
	}
	
	/**
	 * if a newline at end of the given string, this remove it.
	 * <p>与えられた文字列の最後に与えられた文字があるとき、それを取り除く.
	 * nullのときはnullを返す.
	 * 
	 * @param line       a string to chomp
	 * @param separator  a separator to be removed
	 */
	public static String chomp(String line, int separator) {
		if(line == null) {
			return null;
		} else {
			int x = line.length();
			
			return (x > 0 && line.charAt(x - 1) == separator) ?
			 		line.substring(0, x - 1) : line;
		}
	}
	
	/**
	 * if a newline at end of the given string, this remove it.
	 * <p>与えられた文字列の最後に改行文字があるとき、それを取り除く.
	 * nullのときはnullを返す.
	 * 
	 * @param line       a string to chomp
	 */
	public static String chomp(String line) {
		return chomp(line, '\n');
	}
	
	//
	/**
	 * splits the first argument by a character of the second argument.
	 * <p>第1引数を引数の第2引数の文字で分割し、そのキーと値の組を得る.
	 * 文字列に文字セットの文字が含まれないときはキーを引数の文字列
	 * そのものとし、値を<i>空文字列</i>とする.
	 * セパレータが含まれないときは、第1引数そのものをキーとして
	 * 第2引数を値とする.
	 * 
	 * @param line     a string to be splitted
	 * @param charset  characters to split
	 * @return  the key-value pair
	 */
	public static Tuple2<String, String> getKeyValue(
			String line, String charset) {
		String k = null, v = null;
		int b, e;
		
		// 
		if(line == null || charset == null) {
			throw new IllegalArgumentException(
					"the argument(s) is null");
		}
		
		// 
		b = Strings.find(line, charset, Strings.UNTIL);
		if(b >= 0) {
			k = Strings.left(line, b);
			e = Strings.find(line, charset, b + 1, Strings.WHILE);
			v = (e < 0) ? "" : line.substring(e);
		} else {
			k = line;
			v = "";
		}
		return new Tuple2<String, String>(k, v);
	}
	
	/**
	 * joins the given key and value with the given delimiter.
	 * <p>与えられたデリミタ文字列でキーと値を結合します.
	 * 
	 * @param key       a key
	 * @param value     a value
	 * @param delimiter a delimiter to join
	 * @return  the joined string
	 */
	public static String putKeyValue(
			Object key, Object value, String delimiter) {
		if(key == null || delimiter == null) {
			throw new IllegalArgumentException(
					"the argument(s) is null");
		}
		
		return key + delimiter + Objects.toString(value);
	}

	/**
	 * joins the given key and value with the given delimiter.
	 * <p>与えられたデリミタ文字列でキーと値を結合します.
	 * 
	 * @param entry      a pair of key-value
	 * @param delimiter  a delimiter to join
	 * @return  the joined string
	 */
	public static String putKeyValue(
			Tuple2<String, String> entry, String delimiter) {
		if(entry == null || delimiter == null) {
			throw new IllegalArgumentException(
					"the argument(s) is null");
		}
		
		return putKeyValue(entry.getA(), entry.getB(), delimiter);
	}

	/**
	 * joins the given key and value with &quot;=&quot;.
	 * <p>&quot;=&quot;でキーと値を結合します.
	 * 
	 * @param key       a key
	 * @param value     a value
	 * @return  the joined string
	 */
	public static String putKeyValue(Object key, Object value) {
		return putKeyValue(key, value, "=");
	}

	/**
	 * joins the given key and value with &quot;=&quot;.
	 * <p>&quot;=&quot;でキーと値を結合します.
	 * 
	 * @param entry      a pair of key-value
	 * @return  the joined string
	 */
	public static String putKeyValue(Tuple2<String, String> entry) {
		return putKeyValue(entry, "=");
	}
	
	// methods for build strings
	/**
	 * creates a instance of string which consists of only one character.
	 * <p>1文字だけの文字列のインスタンスを生成する.
	 * 
	 * @param c  a character
	 */
	public static String newString(int c) {
		char[] r = new char[1];
		
		r[0] = (char)c;
		return new String(r);
	}

	//
	private static void checkRange(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		if(s1 == null || s2 == null) {
			throw new NullPointerException();
		} else if(b1 < 0 || b1 >= s1.length()) {
			throw new StringIndexOutOfBoundsException(b1);
		} else if(e1 < 0 || e1 > s1.length()) {
			throw new StringIndexOutOfBoundsException(e1);
		} else if(b2 < 0 || b2 >= s2.length()) {
			throw new StringIndexOutOfBoundsException(b2);
		} else if(e2 < 0 || e2 > s2.length()) {
			throw new StringIndexOutOfBoundsException(e2);
		} else if(e1 < b1) {
			throw new IllegalArgumentException();
		} else if(e2 < b2) {
			throw new IllegalArgumentException();
		}
	}

	//
	private static void checkRange(CharSequence s, int b, int e) {
		if(s == null) {
			throw new NullPointerException();
		} else if(b < 0 || b >= s.length()) {
			throw new StringIndexOutOfBoundsException(b);
		} else if(e < 0 || e > s.length()) {
			throw new StringIndexOutOfBoundsException(e);
		} else if(e < b) {
			throw new IllegalArgumentException();
		}
	}

	//
	private static void checkRange(char[] s, int b, int e) {
		if(s == null) {
			throw new NullPointerException();
		} else if(b < 0 || b >= s.length) {
			throw new ArrayIndexOutOfBoundsException(b);
		} else if(e < 0 || e > s.length) {
			throw new ArrayIndexOutOfBoundsException(e);
		} else if(e < b) {
			throw new IllegalArgumentException();
		}
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static boolean equals(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		checkRange(s1, s2, b1, e1, b2, e2);
		if(e1 - b1 != e2 - b2) {
			return false;
		} else {
			int i = b1, j = b2;

			while(i < e1) {
				if(s1.charAt(i++) != s2.charAt(j++)) {
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int compareTo(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = b1, j = b2;

		checkRange(s1, s2, b1, e1, b2, e2);
		while(i < e1 && j < e2) {
			char c1 = s1.charAt(i++);
			char c2 = s2.charAt(j++);

			if(c1 < c2) {
				return -1;
			} else if(c1 > c2) {
				return 1;
			}
		}
		return (j < e2) ? -1 : (i < e1) ? 1 : 0;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static boolean equalsIgnoreCase(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		checkRange(s1, s2, b1, e1, b2, e2);
		if(e1 - b1 != e2 - b2) {
			return false;
		} else {
			int i = b1, j = b2;

			while(i < e1) {
				char c1 = s1.charAt(i++);
				char c2 = s2.charAt(j++);

				c1 = Character.toLowerCase(Character.toUpperCase(c1));
				c2 = Character.toLowerCase(Character.toUpperCase(c2));
				if(c1 != c2) {
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int compareToIgnoreCase(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = b1, j = b2;

		checkRange(s1, s2, b1, e1, b2, e2);
		while(i < e1 && j < e2) {
			char c1 = s1.charAt(i++);
			char c2 = s2.charAt(j++);

			c1 = Character.toLowerCase(Character.toUpperCase(c1));
			c2 = Character.toLowerCase(Character.toUpperCase(c2));
			if(c1 < c2) {
				return -1;
			} else if(c1 > c2) {
				return 1;
			}
		}
		return (j < e2) ? -1 : (i < e1) ? 1 : 0;
	}

	/**
	 * 
	 * @param s
	 * @param bound
	 * @param b
	 * @param e
	 * @return
	 */
	public static int hashCode(
			CharSequence s, int bound, int b, int e) {
		int r = Hashes.INIT;

		checkRange(s, b, e);
		if(bound > e - b) {
			throw new IllegalArgumentException();
		}

		for(int i = b; i < b + bound; i++) {
			r = (r + (int)s.charAt(i)) * Hashes.A;
		}
		return r;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int hashCodeIgnoreCase(String s) {
		if(s == null) {
			throw new NullPointerException();
		}
		return s.toUpperCase().toLowerCase().hashCode();
	}

	/**
	 * 
	 * @param s
	 * @param bound
	 * @param b
	 * @param e
	 * @return
	 */
	public static int hashCodeIgnoreCase(
			CharSequence s, int bound, int b, int e) {
		int r = Hashes.INIT;

		checkRange(s, b, e);
		if(bound > e - b) {
			throw new IllegalArgumentException();
		}

		for(int i = b; i < b + bound; i++) {
			r = (r + (int)Character.toLowerCase(
					Character.toUpperCase(s.charAt(i)))) * Hashes.A;
		}
		return r;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int prefixLength(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = b1, j = b2;

		checkRange(s1, s2, b1, e1, b2, e2);
		for(; i < e1 && j < e2; i++, j++) {
			char c1 = s1.charAt(i);
			char c2 = s2.charAt(j);

			if(c1 != c2) {
				return i - b1;
			}
		}
		return i - b1;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int prefixLengthIgnoreCase(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = b1, j = b2;

		checkRange(s1, s2, b1, e1, b2, e2);
		for(; i < e1 && j < e2; i++, j++) {
			char c1 = s1.charAt(i);
			char c2 = s2.charAt(j);

			c1 = Character.toLowerCase(Character.toUpperCase(c1));
			c2 = Character.toLowerCase(Character.toUpperCase(c2));
			if(c1 != c2) {
				return i - b1;
			}
		}
		return i - b1;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int suffixLength(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = e1 - 1, j = e2 - 1;

		checkRange(s1, s2, b1, e1, b2, e2);
		for(; i >= b1 && j >= b2; i--, j--) {
			char c1 = s1.charAt(i);
			char c2 = s2.charAt(j);

			if(c1 != c2) {
				return e1 - 1 - i;
			}
		}
		return e1 - 1 - i;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int suffixLengthIgnoreCase(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = e1 - 1, j = e2 - 1;

		checkRange(s1, s2, b1, e1, b2, e2);
		for(; i >= b1 && j >= b2; i--, j--) {
			char c1 = s1.charAt(i);
			char c2 = s2.charAt(j);

			c1 = Character.toLowerCase(Character.toUpperCase(c1));
			c2 = Character.toLowerCase(Character.toUpperCase(c2));
			if(c1 != c2) {
				return e1 - 1 - i;
			}
		}
		return e1 - 1 - i;
	}

	/**
	 * 
	 * @param s2
	 * @param s1
	 * @param b2
	 * @param e2
	 * @param b1
	 * @param e1
	 * @return
	 */
	public static boolean startsWith(
			CharSequence s2, CharSequence s1,
			int b2, int e2, int b1, int e1) {
		int i = b2, j = b1;

		checkRange(s2, s1, b2, e2, b1, e1);
		for(; i < e2 && j < e1; i++, j++) {
			char c2 = s2.charAt(i);
			char c1 = s1.charAt(j);

			if(c2 != c1) {
				return false;
			}
		}
		return j >= e1;
	}

	/**
	 * 
	 * @param s2
	 * @param s1
	 * @param b2
	 * @param e2
	 * @param b1
	 * @param e1
	 * @return
	 */
	public static boolean startsWithIgnoreCase(
			CharSequence s2, CharSequence s1,
			int b2, int e2, int b1, int e1) {
		int i = b2, j = b1;

		checkRange(s2, s1, b2, e2, b1, e1);
		for(; i < e2 && j < e1; i++, j++) {
			char c2 = s2.charAt(i);
			char c1 = s1.charAt(j);

			c2 = Character.toLowerCase(Character.toUpperCase(c2));
			c1 = Character.toLowerCase(Character.toUpperCase(c1));
			if(c2 != c1) {
				return false;
			}
		}
		return j >= e1;
	}

	/**
	 * 
	 * @param s2
	 * @param s1
	 * @param b2
	 * @param e2
	 * @param b1
	 * @param e1
	 * @return
	 */
	public static boolean endsWith(
			CharSequence s2, CharSequence s1,
			int b2, int e2, int b1, int e1) {
		int i = e2 - 1, j = e1 - 1;

		checkRange(s2, s1, b2, e2, b1, e1);
		for(; i >= b2 && j >= b1; i--, j--) {
			char c2 = s2.charAt(i);
			char c1 = s1.charAt(j);

			if(c2 != c1) {
				return false;
			}
		}
		return j < b1;
	}

	/**
	 * 
	 * @param s2
	 * @param s1
	 * @param b2
	 * @param e2
	 * @param b1
	 * @param e1
	 * @return
	 */
	public static boolean endsWithIgnoreCase(
			CharSequence s2, CharSequence s1,
			int b2, int e2, int b1, int e1) {
		int i = e2 - 1, j = e1 - 1;

		checkRange(s2, s1, b2, e2, b1, e1);
		for(; i >= b2 && j >= b1; i--, j--) {
			char c2 = s2.charAt(i);
			char c1 = s1.charAt(j);

			c2 = Character.toLowerCase(Character.toUpperCase(c2));
			c1 = Character.toLowerCase(Character.toUpperCase(c1));
			if(c2 != c1) {
				return false;
			}
		}
		return j < b1;
	}

	//
	private static char charAtCi(CharSequence s, int at) {
		return Character.toLowerCase(
				Character.toUpperCase(s.charAt(at)));
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int contains(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = b1, j = b2, r;

		checkRange(s1, s2, b1, e1, b2, e2);
		if(e2 - b2 == 0) {
			return b1;
		}

		while(s1.charAt(i) != s2.charAt(j)) {
			if(++i >= e1) {
				return -1;
			}
		}
		r = i;

		for(; i < e1 && j < e2; i++, j++) {
			char c1 = s1.charAt(i);
			char c2 = s2.charAt(j);

			if(c1 != c2) {
				return -1;
			}
		}
		return j >= e2 ? r : -1;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static int containsIgnoreCase(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		int i = b1, j = b2, r;

		checkRange(s1, s2, b1, e1, b2, e2);
		if(e2 - b2 == 0) {
			return b1;
		}

		while(charAtCi(s1, i) != charAtCi(s2, j)) {
			if(++i >= e1) {
				return -1;
			}
		}
		r = i;

		for(; i < e1 && j < e2; i++, j++) {
			char c1 = charAtCi(s1, i);
			char c2 = charAtCi(s2, j);

			if(c1 != c2) {
				return -1;
			}
		}
		return j >= e2 ? r : -1;
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static char[] toTitleCase(char[] cs, int b, int e) {
		int stat = 0;

		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			switch(stat) {
			case 0:
//				if(Character.isLowerCase(cs[i])) {
//					cs[i] = Character.toUpperCase(cs[i]);
//					stat = 1;
//				}
				cs[i] = Character.toUpperCase(cs[i]);  stat = 1;
				break;
			case 1:
				if(Character.isUpperCase(cs[i]) ||
						cs[i] == '\'' ||
						cs[i] == '’') {
					cs[i] = Character.toLowerCase(cs[i]);
				} else if(!Character.isLetter(cs[i])) {
					stat = 0;
				}
				break;
			default:
				throw new IllegalStateException();
			}
		}
		return cs;
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static String toTitleCase(CharSequence cs, int b, int e) {
		StringBuilder bf = new StringBuilder();
		int stat = 0;

		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			char ch = cs.charAt(i);

			switch(stat) {
			case 0:
//				if(Character.isLowerCase(ch)) {
//					ch = Character.toUpperCase(ch);
//					stat = 1;
//				}
				ch = Character.toUpperCase(ch);  stat = 1;
				bf.append(ch);
				break;
			case 1:
				if(Character.isUpperCase(ch) ||
						ch == '\'' ||
						ch == '’') {
					ch = Character.toLowerCase(ch);
				} else if(!Character.isLetter(ch) ||
						Character.isDigit(ch)) {
					stat = 0;
				}
				bf.append(ch);
				break;
			default:
				throw new IllegalStateException();
			}
		}
		return bf.toString();
	}

	/**
	 * 
	 * @param cs
	 * @return
	 */
	public static String toTitleCase(CharSequence cs) {
		return toTitleCase(cs, 0, cs.length());
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static char[] toUpperCase(char[] cs, int b, int e) {
		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			cs[i] = Character.toUpperCase(cs[i]);
		}
		return cs;
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static char[] toLowerCase(char[] cs, int b, int e) {
		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			cs[i] = Character.toLowerCase(cs[i]);
		}
		return cs;
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static String toUpperCase(CharSequence cs, int b, int e) {
		StringBuilder bf = new StringBuilder();

		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			bf.append(Character.toUpperCase(cs.charAt(i)));
		}
		return bf.toString();
	}


	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static String toLowerCase(CharSequence cs, int b, int e) {
		StringBuilder bf = new StringBuilder();

		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			bf.append(Character.toLowerCase(cs.charAt(i)));
		}
		return bf.toString();
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static char[] reverse(char[] cs, int b, int e) {
		checkRange(cs, b, e);
		for(int i = b; i < (b + e) / 2; i++) {
			char c = cs[i];

			cs[i] = cs[b + e - i - 1];
			cs[b + e - i - 1] = c;
		}
		return cs;
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static String reverse(String cs, int b, int e) {
		try {
			return new String(reverse(cs.toCharArray(), b, e));
		} catch(ArrayIndexOutOfBoundsException e1) {
			throw new StringIndexOutOfBoundsException(e1.getMessage());
		}
	}

	//
	private static int mod(int x, int m) {
		int r = x % m;

		return (r < 0) ? r + m : r;
	}

	/**
	 * 
	 * @param cs
	 * @param from
	 * @param to
	 * @param b
	 * @param e
	 * @return
	 */
	public static String xsubstring(
			CharSequence cs, int from, int to, int b, int e) {
		StringBuilder bl = new StringBuilder();

		checkRange(cs, b, e);
		for(int i = from; i < to; i++) {
			bl.append(cs.charAt(mod(i, e - b) + b));
		}
		return bl.toString();
	}

	/**
	 * 
	 * @param tg
	 * @param ts
	 * @param cs
	 * @param from
	 * @param to
	 * @param b
	 * @param e
	 * @return
	 */
	public static char[] xcopy(
			char[] tg, int ts, CharSequence cs,
			int from, int to, int b, int e) {
		checkRange(cs, b, e);
		if(to - from + ts > tg.length) {
			throw new IllegalArgumentException();
		} else if(ts >= tg.length) {
			throw new ArrayIndexOutOfBoundsException(ts);
		}

		for(int i = from, j = ts; i < to; i++, j++) {
			tg[j] = cs.charAt(mod(i, e - b) + b);
		}
		return tg;
	}

	/**
	 * 
	 * @param s1
	 * @param s2
	 * @param b1
	 * @param e1
	 * @param b2
	 * @param e2
	 * @return
	 */
	public static String replace(
			CharSequence s1, CharSequence s2,
			int b1, int e1, int b2, int e2) {
		StringBuilder bf = new StringBuilder();

		bf.append(s1.subSequence(0, b1));
		bf.append(s2.subSequence(b2, e2));
		bf.append(s1.subSequence(e1, s1.length()));
		return bf.toString();
	}

	/**
	 * 
	 * @param dest
	 * @param re
	 * @return
	 */
	public static String[] matchRegexp(String dest, String re) {
		Pattern pt = Pattern.compile(re);
		Matcher mt = pt.matcher(dest);
		String[] res;

		res = new String[mt.groupCount() + 1];
		for(int i = 0; i <= mt.groupCount(); i++) {
			res[i] = mt.group(i);
		}
		return res;
	}

	/**
	 * 
	 * @param dest
	 * @param re
	 * @param grp
	 * @return
	 */
	public static String matchRegexp(String dest, String re, int grp) {
		Pattern pt = Pattern.compile(re);
		Matcher mt = pt.matcher(dest);

		return mt.group(grp);
	}

	/**
	 * @param s
	 * @return
	 */
	public static String toHyphen(CharSequence cs, int b, int e) {
		StringBuilder bf = new StringBuilder();
		int stat = 0;

		checkRange(cs, b, e);
		for(int i = b; i < e; i++) {
			char ch = cs.charAt(i);

			switch(stat) {
			case 0:
				bf.append(Character.toLowerCase(ch));
				stat = 1;
				break;
			case 1:
				if(Character.isUpperCase(ch)) {
					bf.append('-');
					bf.append(Character.toLowerCase(ch));
				} else {
					bf.append(ch);
				}
				break;
			default:
				throw new IllegalStateException();
			}
		}
		return bf.toString();
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static String escapeHtml(String s) {
		String r = s;

		r = r.replaceAll("&(?!#)",  "&amp;");
		r = r.replaceAll("\"", "&quot;");
		r = r.replaceAll("<",  "&lt;");
		r = r.replaceAll(">",  "&gt;");
		return r;
	}

	/**
	 * 
	 * @param tok
	 * @return
	 */
	public static List<String> toList(StringTokenizer tok) {
		List<String> r = new ArrayList<String>();

		while(tok.hasMoreTokens()) {
			r.add(tok.nextToken());
		}
		return r;
	}

	/**
	 * 
	 * @param tok
	 * @return
	 */
	public static Iterator<String> toIterator(StringTokenizer tok) {
		final StringTokenizer t = tok;

		return new Iterator<String>() {

			public boolean hasNext() {
				return t.hasMoreTokens();
			}

			public String next() {
				return t.nextToken();
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static boolean isUpperCase(String s) {
		int c;

		for(int i = 0; i < s.length(); i++) {
			c = s.codePointAt(i);
			if(!Character.isUpperCase(c)) {
				return false;
			} else if(c > 0xffff) {
				i++;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static boolean isLowerCase(String s) {
		int c;

		for(int i = 0; i < s.length(); i++) {
			c = s.codePointAt(i);
			if(!Character.isLowerCase(c)) {
				return false;
			} else if(c > 0xffff) {
				i++;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static boolean isTitleCase(String s) {
		int c;

		for(int i = 0; i < s.length(); i++) {
			c = s.codePointAt(i);
			if((i == 0 && !Character.isUpperCase(c)) ||
					(i > 0 && !Character.isLowerCase(c))) {
				return false;
			} else if(c > 0xffff) {
				i++;
			}
		}
		return true;
	}

}
