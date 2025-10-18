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
package net.morilib.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PushbackInputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import net.morilib.lang.string.ByteString;
import net.morilib.util.SimpleMap;
import net.morilib.util.primitive.ByteArrayVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/22
 */
public final class ParserUtils {

	//
	private ParserUtils() { }

	/**
	 * 
	 * @param ins
	 * @return
	 * @throws IOException
	 */
	public static Reader readEncoding(
			InputStream ins) throws IOException {
		PushbackInputStream ins2 = new PushbackInputStream(ins);
		String enc = null;
		int cb;

		cb = ins2.read();
		if(cb == '#') {
			ByteArrayVector v = new ByteArrayVector();

			cb = ins2.read();
			if(cb != '?') {
				enc = "US-ASCII";
				ins2.unread(cb);
			}

			// read the encoding
			while(true) {
				cb = ins2.read();
				if(cb == '\n' || cb == '\r') {
					break;
				} else if(cb < 0) {
					throw new ParsingException();
				}
				v.addByte((byte)cb);
			}

			// skip the newline/line feed
			while(true) {
				cb = ins2.read();
				if(cb < 0) {
					throw new ParsingException();
				} else if(cb != '\n' && cb != '\r') {
					break;
				}
			}
			enc = (enc == null) ?
					new ByteString(v.toByteArray()).toString() : enc;
		} else {
			enc = "US-ASCII";
		}

		ins2.unread(cb);
		return new InputStreamReader(ins2, enc);
	}

	/**
	 * 
	 * @param <T>
	 * @param ins
	 * @param map
	 * @param comment
	 * @return
	 */
	public static<T> List<T> parseToList(
			InputStream ins,
			String comment,
			SimpleMap<String, T> map) {
		List<T> res = new ArrayList<T>();
		BufferedReader rd;

		try {
			String s;

			rd = new BufferedReader(readEncoding(ins));
			while((s = rd.readLine()) != null) {
				if(!s.equals("") && !s.startsWith(comment)) {
					res.add(map.map(s));
				}
			}
			return res;
		} catch (IOException e) {
			throw new ParsingException(e);
		}
	}

}
