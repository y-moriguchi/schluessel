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
package net.morilib.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.Stack;

/**
 * An utility class for I/O.
 * <p>I/Oのためのユーティリティクラスである。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class IOs {

	private IOs() {}

	/**
	 * close the given Closeable quietly.
	 * <p>与えられたCloseableを例外をキャッチせずに閉じます。
	 * 
	 * @param ins Closeable to be closed
	 */
	public static void close(Closeable ins) {
		if(ins != null) {
			try {
				ins.close();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	/**
	 * The null InputStream, which has only EOF.
	 * <p>ダミーのInputStreamである.EOFのみをもつ.
	 */
	public static final InputStream NULL_INPUT = new InputStream() {

		//
		public int read() throws IOException {
			return -1;
		}

		//
		public int available() throws IOException {
			return 0;
		}

		//
		public void close() throws IOException {
			// do nothing
		}

		//
		public synchronized void mark(int readlimit) {
			// do nothing
		}

		//
		public boolean markSupported() {
			return false;
		}

		//
		public int read(byte[] b, int off, int len) throws IOException {
			return -1;
		}

		//
		public int read(byte[] b) throws IOException {
			return -1;
		}

		//
		public synchronized void reset() throws IOException {
			// do nothing
		}

		//
		public long skip(long n) throws IOException {
			return 0;
		}

	};

	/**
	 * The null OutputStream, which does nothing.
	 * <p>ダミーのOutputStreamである.
	 */
	public static final OutputStream NULL_OUTPUT = new OutputStream() {

		//
		public void write(byte[] b, int off, int len) {
			// do nothing
		}

		//
		public void write(byte[] b) {
			// do nothing
		}

		//
		public void write(int b) {
			// do nothing
		}

		//
		public void close() throws IOException {
			// do nothing
		}

		//
		public void flush() throws IOException {
			// do nothing
		}

	};

	//
	private static final int BUFFER_SIZE = 4096;

	/**
	 * closes the given stream.
	 * <p>与えられたストリームをクローズする.
	 * 
	 * @param stream  the stream to be closed
	 */
	public static void close(InputStream stream) {
		if(stream != null) try {
			stream.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * closes the given stream.
	 * <p>与えられたストリームをクローズする.
	 * 
	 * @param stream  the stream to be closed
	 */
	public static void close(OutputStream stream) {
		if(stream != null) try {
			stream.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * closes the given stream.
	 * <p>与えられたストリームをクローズする.
	 * 
	 * @param stream  the stream to be closed
	 */
	public static void close(Reader stream) {
		if(stream != null) try {
			stream.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * closes the given stream.
	 * <p>与えられたストリームをクローズする.
	 * 
	 * @param stream  the stream to be closed
	 */
	public static void close(Writer stream) {
		if(stream != null) try {
			stream.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * creates a BufferedReader of the given InputStream
	 * with the given encoding.
	 * <p>与えられたInputStreamのBufferedReaderを生成する.
	 * 
	 * @param ins       an InputStream to be open
	 * @param encoding  an encoding of the file
	 * @return          new BufferedReader
	 * @throws IOException  occurs an IO error
	 */
	public static BufferedReader newBufferedReader(
			InputStream ins, String encoding) throws IOException {
		return new BufferedReader(new InputStreamReader(ins, encoding));
	}

	/**
	 * creates a BufferedReader of the given file with the given encoding.
	 * <p>与えられたファイルのBufferedReaderを生成する.
	 * 
	 * @param file      a file object to be open
	 * @param encoding  an encoding of the file
	 * @return          new BufferedReader
	 * @throws IOException  occurs an IO error
	 */
	public static BufferedReader newBufferedReader(
			File file, String encoding) throws IOException {
		return new BufferedReader(new InputStreamReader(
				new FileInputStream(file), encoding));
	}

	/**
	 * creates a BufferedReader of the given file
	 * with the default encoding.
	 * <p>与えられたファイルのBufferedReaderを生成する.
	 * 
	 * @param file      a file object to be open
	 * @return          new BufferedReader
	 * @throws IOException  occurs an IO error
	 */
	public static BufferedReader newBufferedReader(
			File file) throws IOException {
		return new BufferedReader(new FileReader(file));
	}

	/**
	 * creates a BufferedReader of the given file name
	 * with the given encoding.
	 * <p>与えられたファイルのBufferedReaderを生成する.
	 * 
	 * @param filename  a file name to be open
	 * @param encoding  an encoding of the file
	 * @return          new BufferedReader
	 * @throws IOException  occurs an IO error
	 */
	public static BufferedReader newBufferedReader(
			String filename, String encoding) throws IOException {
		return new BufferedReader(new InputStreamReader(
				new FileInputStream(filename), encoding));
	}

	/**
	 * creates a BufferedReader of the given file name
	 * with the default encoding.
	 * <p>与えられたファイル名のBufferedReaderを生成する.
	 * 
	 * @param filename  a file name to be open
	 * @return          new BufferedReader
	 * @throws IOException  occurs an IO error
	 */
	public static BufferedReader newBufferedReader(
			String filename) throws IOException {
		return new BufferedReader(new FileReader(filename));
	}

	/**
	 * creates a PrintWriter of the given file name
	 * with the given encoding.
	 * <p>与えられたファイル名のPrintWriterを生成する.
	 * 
	 * @param filename  a file name to be open
	 * @param encoding  an encoding of the file
	 * @return          new PrintWriter
	 * @throws IOException  occurs an IO error
	 */
	public static PrintWriter newPrintWriter(
			String filename, String encoding) throws IOException {
		return new PrintWriter(new BufferedWriter(
				new OutputStreamWriter(
						new FileOutputStream(filename), encoding)));
	}

	/**
	 * creates a PrintWriter of the given file name
	 * with the default encoding.
	 * <p>与えられたファイル名のPrintWriterを生成する.
	 * 
	 * @param filename  a file name to be open
	 * @return          new PrintWriter
	 * @throws IOException  occurs an IO error
	 */
	public static PrintWriter newPrintWriter(
			String filename) throws IOException {
		return new PrintWriter(
				new BufferedWriter(new FileWriter(filename)));
	}

	/**
	 * read a string from the given Reader until a character read from
	 * the Reader is equal to the given character.
	 * <p>与えられたReaderから文字列を、与えられた文字に一致するまで
	 * 読み込む.
	 * 
	 * @param rd  the reader
	 * @param ch  the character
	 * @return    the string
	 * @throws IOException  occurs an IO error
	 */
	public static String readUntil(
			Reader rd, int ch) throws IOException {
		StringBuffer buf = new StringBuffer();

		int c = rd.read();
		while(c > 0 && c != ch) {
			buf.append(c);
		}
		return buf.toString();
	}

	/**
	 * returns true if the given encoding is supported.
	 * <p>文字コードがサポートされているかをチェックする.
	 * 
	 * @param encoding  the name of encoding to be tested
	 * @throws UnsupportedEncodingException  unsupported encoding
	 */
	public static void checkSupported(
			String encoding) throws UnsupportedEncodingException {
		new InputStreamReader(NULL_INPUT, encoding);
	}

	//
	private static Stack<PrintStream> outstack =
		new Stack<PrintStream>(); 

	/**
	 * 
	 * @param ps
	 */
	public static void pushOut(PrintStream ps) {
		outstack.push(System.out);
		System.setOut(ps);
	}

	/**
	 * 
	 * @return
	 */
	public static PrintStream peekOut() {
		return outstack.peek();
	}

	/**
	 * 
	 */
	public static PrintStream popOut() {
		PrintStream r = outstack.pop();

		System.setOut(r);
		return r;
	}

	//
	private static Stack<PrintStream> errstack =
		new Stack<PrintStream>(); 

	/**
	 * 
	 * @param ps
	 */
	public static void pushErr(PrintStream ps) {
		errstack.push(System.err);
		System.setOut(ps);
	}

	/**
	 * 
	 * @return
	 */
	public static PrintStream peekErr() {
		return errstack.peek();
	}

	/**
	 * 
	 */
	public static PrintStream popErr() {
		PrintStream r = errstack.pop();

		System.setErr(r);
		return r;
	}

	//
	private static Stack<InputStream> instack =
		new Stack<InputStream>(); 

	/**
	 * 
	 * @param ps
	 */
	public static void pushIn(InputStream ps) {
		instack.push(System.in);
		System.setIn(ps);
	}

	/**
	 * 
	 * @return
	 */
	public static InputStream peekIn() {
		return instack.peek();
	}

	/**
	 * 
	 */
	public static InputStream popIn() {
		InputStream r = instack.pop();

		System.setIn(r);
		return r;
	}

	/**
	 * 
	 * @param ins
	 * @param ous
	 * @throws IOException
	 */
	public static void copy(
			InputStream ins, OutputStream ous) throws IOException {
		BufferedInputStream  bi = new BufferedInputStream(ins);
		BufferedOutputStream bo = new BufferedOutputStream(ous);
		byte[] cs = new byte[BUFFER_SIZE];
		int l;

		while((l = bi.read(cs)) >= 0) {
			bo.write(cs, 0, l);
		}
		bo.flush();
	}

	/**
	 * 
	 * @param rd
	 * @param wr
	 * @throws IOException
	 */
	public static void copy(Reader rd, Writer wr) throws IOException {
		BufferedReader bi = new BufferedReader(rd);
		BufferedWriter bo = new BufferedWriter(wr);
		char[] cs = new char[BUFFER_SIZE];
		int l;

		while((l = bi.read(cs)) >= 0) {
			bo.write(cs, 0, l);
		}
		bo.flush();
	}

	/**
	 * 
	 * @param fs
	 * @param fd
	 * @throws IOException
	 */
	public static void copy(File fs, File fd) throws IOException {
		InputStream  ins = null;
		OutputStream ous = null;

		try {
			ins = new FileInputStream(fs);
			ous = new FileOutputStream(fd);
			copy(ins, ous);
		} finally {
			close(ins);
			close(ous);
		}
	}

	/**
	 * 
	 * @param f
	 * @param enc
	 * @return
	 * @throws IOException
	 */
	public static String toString(File f,
			String enc) throws IOException {
		InputStream ins = null;
		BufferedReader rd = null;
		StringBuilder b = new StringBuilder();
		String s, dlm = "";

		try {
			ins = new FileInputStream(f);
			if(enc != null) {
				rd = new BufferedReader(
						new InputStreamReader(ins, enc));
			} else {
				rd = new BufferedReader(new InputStreamReader(ins));
			}

			while((s = rd.readLine()) != null) {
				b.append(dlm).append(s);
				dlm = "\n";
			}
			return b.toString();
		} finally {
			close(ins);
		}
	}

	/**
	 * 
	 * @param f
	 * @param enc
	 * @return
	 * @throws IOException
	 */
	public static void fromString(File f, String s,
			String enc) throws IOException {
		OutputStream ous = null;
		Writer wr = null;
		StringReader rd = new StringReader(s);

		try {
			ous = new FileOutputStream(f);
			if(enc != null) {
				wr = new OutputStreamWriter(ous, enc);
			} else {
				wr = new OutputStreamWriter(ous);
			}
			copy(rd, wr);
		} finally {
			close(ous);
		}
	}

	/**
	 * 
	 * @param ins
	 * @param buf
	 * @return
	 * @throws IOException
	 */
	public static boolean isStreamSerialized(InputStream ins,
			byte[] buf) throws IOException {
		return (ins.read(buf, 0, 2) == 2 &&
				buf[0] == (byte)0xac &&
				buf[1] == (byte)0xed);
	}

}
