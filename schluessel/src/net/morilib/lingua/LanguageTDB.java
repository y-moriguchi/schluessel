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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.Closeable;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/16
 */
public class LanguageTDB {

	static int RECORD_LEN = 96;
	private static final String DB_FILE =
			"/net/morilib/lingua/iso639.bin";
	private static final String ISO_FILE =
			"/net/morilib/lingua/iso639.properties";
	private static final int[] ISO_TAB;
	private static final SortedMap<String, String> ISO_MR;
	private static final int NIL = 26 * 26 - 1;
	private static File tmpfile = null;

	static {
		SortedMap<String, String> m = new TreeMap<String, String>();
		BufferedReader rd = null;
		String[] a;
		String s;
		int[] t;
		int i, n;

		try {
			rd = new BufferedReader(new InputStreamReader(
					Language.class.getResourceAsStream(ISO_FILE)));

			// read number of lines
			if((s = skipcm(rd)) == null) {
				throw new RuntimeException("missing lines");
			} else {
				t = new int[n = Integer.parseInt(s)];
			}

			for(i = 0; i < n; i++) {
				if((s = skipcm(rd)) == null) {
					throw new RuntimeException();
				}
				t[i] = encodeISO639_3(s.toLowerCase());

				if(s.length() == 6) {
					a = s.split("=");
					m.put(a[1], a[0]);
				}
			}
			ISO_TAB = t;
			ISO_MR  = m;
		} catch(IOException e) {
			throw new RuntimeException(e);
		} finally {
			closeq(rd);
		}
	}

	private static void closeq(Closeable c) {
		try {
			if(c != null)  c.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	private static int val(String s, int i) {
		char c;

		if((c = s.charAt(i)) < 'a' || c > 'z') {
			throw new RuntimeException();
		}
		return c - 'a';
	}

	static int encodeISO639_3(String s) {
		int x = 0;

		if(s.length() == 3) {
			x = val(s, 0);
			x = x * 26 + val(s, 1);
			return (x * 26 + val(s, 2)) * 26 * 26 + NIL;
		} else if(s.length() == 6 && s.charAt(3) == '=') {
			x = val(s, 0);
			x = x * 26 + val(s, 1);
			x = x * 26 + val(s, 2);
			x = x * 26 + val(s, 4);
			return x * 26 + val(s, 5);
		} else {
			throw new RuntimeException();
		}
	}

	private static String skipcm(
			BufferedReader rd) throws IOException {
		String s;

		while((s = rd.readLine()) != null && s.startsWith("#"));
		return s;
	}

	private static int getk(int i) {
		return ISO_TAB[i] / 26 / 26;
	}

	private static int getv(int i) {
		return ISO_TAB[i] % (26 * 26);
	}

	private static int srch(int k, int b, int e) {
		int x;

		if(b == e) {
			return (getk(b) == k) ? b : -1;
		} else {
			x = (b + e) / 2;
			return (k <= getk(x)) ? srch(k, b, x) : srch(k, x + 1, e);
		}
	}

	static int searchIndex(int k) {
		return srch(k, 0, ISO_TAB.length - 1);
	}

	static String find(int k) {
		char[] a = new char[2];
		int x;

		if((x = searchIndex(k)) < 0) {
			return null;
		} else if((x = getv(x)) == NIL) {
			return "";
		} else {
			a[0] = (char)(x / 26 + 'a');
			a[1] = (char)(x % 26 + 'a');
			return new String(a);
		}
	}

	public static String findISO639_3(String s) {
		if(s.length() != 3)  return null;
		return find(encodeISO639_3(s) / 26 / 26);
	}

	public static String findISO639_1(String s) {
		return ISO_MR.get(s);
	}

	private synchronized static File initdb() throws IOException {
		BufferedInputStream r = null;
		BufferedOutputStream w = null;
		byte[] b = new byte[4096];
		int l;

		if(tmpfile == null) {
			tmpfile = File.createTempFile("iso639", ".tmp");
			tmpfile.deleteOnExit();
			try {
				r = new BufferedInputStream(
						Language.class.getResourceAsStream(DB_FILE));
				w = new BufferedOutputStream(
						new FileOutputStream(tmpfile));
				while((l = r.read(b)) >= 0)  w.write(b, 0, l);
			} finally {
				closeq(r);  closeq(w);
			}
		}
		return tmpfile;
	}

	static LanguageTDBRecord getRecord(int i) {
		byte[] b = new byte[RECORD_LEN];
		RandomAccessFile a = null;

		try {
			a = new RandomAccessFile(initdb(), "r");
			a.seek(i * RECORD_LEN);
			a.read(b);
			return new LanguageTDBRecord(b);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			closeq(a);
		}
	}

	static LanguageTDBRecord findRecord(int k) {
		int i = searchIndex(k);

		return i < 0 ? null : getRecord(i);
	}

	public static LanguageTDBRecord findRecord(String s) {
		if(s.length() != 3)  return null;
		return findRecord(encodeISO639_3(s) / 26 / 26);
	}

}
