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
package net.morilib.lisp;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import net.morilib.lisp.exlib.ILispLineReadable;
import net.morilib.lisp.r6rs.io.ILispSimpleInputPort;
import net.morilib.lisp.r6rs.io.ILispTextualInputPort;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.util.io.CharUnreadBuffer;
import net.morilib.util.tape.CharTape;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class InputPort extends Datum
implements CharTape, CharUnreadBuffer, ILispSimpleInputPort,
ReadUnreadable, ILispTextualInputPort, ILispLineReadable {

	//
	private Reader reader;
	private boolean standard;
	private Parser parser;
	private boolean reachedEOF = false;
	private boolean closed = false;
	private LispMessage msg;
	private int unreadbuf = -1;

	//
	private InputPort(Reader rd, boolean std, LispMessage msg) {
		this.reader     = rd;
		this.standard   = std;
		this.msg        = msg;
		this.parser     = new Parser(this, msg);
	}

	/**
	 * 
	 * @param rd
	 * @param msg
	 */
	public InputPort(Reader rd, LispMessage msg) {
		this(rd, false, msg);
	}

	/**
	 * 
	 * @param fname
	 * @param msg
	 */
	public InputPort(File fname, LispMessage msg) {
		try {
			Reader rd;

			rd = new InputStreamReader(new FileInputStream(fname));
			this.reader     = new PushbackReader(rd);
			this.standard   = false;
			this.msg        = msg;
			this.parser     = new Parser(this, msg);
		} catch (FileNotFoundException e) {
			throw new LispIOException(e);
		}

	}

	/**
	 * 
	 * @param fname
	 * @param msg
	 * @throws UnsupportedEncodingException 
	 */
	public InputPort(String fname, String enconding,
			LispMessage msg) throws UnsupportedEncodingException {
		try {
			Reader rd;

			rd = new InputStreamReader(
					new FileInputStream(fname), enconding);
			this.reader     = new PushbackReader(rd);
			this.standard   = false;
			this.msg        = msg;
			this.parser     = new Parser(this, msg);
		} catch (FileNotFoundException e) {
			throw new LispIOException(e);
		}

	}

	/**
	 * 
	 * @param msg
	 * @return
	 */
	public static InputPort getStandard(LispMessage msg) {
		return new InputPort(
				new InputStreamReader(System.in), true, msg);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public int getc() throws IOException {
		int ch;

		if(unreadbuf == -1) {
			ch = reader.read();
		} else {
			ch = unreadbuf;
			unreadbuf = -1;
		}
		return ch;
	}

	/**
	 * 
	 * @return
	 */
	public Datum readChar() {
		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return EOFObject.EOF;
		}

		try {
			int ch = getc();

			if(ch < 0) {
				reachedEOF = true;
				return EOFObject.EOF;
			} else {
				return new LispCharacter((char)ch);
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	/**
	 * 
	 * @return
	 */
	public Datum peekChar() {
		int c = readc();

		return (c != -1) ? new LispCharacter((char)c) : EOFObject.EOF;
	}

	/**
	 * 
	 * @return
	 */
	public String readLine() {
		StringBuffer b = new StringBuffer();
		int c;

		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return null;
		} else {
			try {
				while((c = getc()) >= 0 && c != '\n') {
					b.appendCodePoint(c);
				}
				return b.toString();
			} catch (IOException e) {
				throw new LispIOException(e);
			}
		}
	}

	/**
	 * 
	 */
	public Datum readS() {
		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return EOFObject.EOF;
		}

		try {
			/*String line;

			parser.clear();

			line = lineReader.readLine();
			if(line == null) {
				reachedEOF = true;
				return EOFObject.EOF;
			}

			do {
				parser.read(line);
				if(parser.parse()) {
					return parser.getDatum();
				}
			} while((line = lineReader.readLine()) != null);

			// lineがnull
			reachedEOF = true;
			throw new ReadException("unexpected EOF");*/
			parser.clear();
			if(parser.parse()) {
				Datum res = parser.getDatum();

				if(res != null) {
					return res;
				} else {
					reachedEOF = true;
					return EOFObject.EOF;
				}
			} else {
				reachedEOF = true;
				if(!parser.isReadBefore()) {
					return EOFObject.EOF;
				} else {
					throw msg.getReadError("err.read.eof");
					//return EOFObject.EOF;
				}
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	/**
	 * 
	 */
	public void close() {
		try {
			if(!closed && !standard) {
				reader.close();
				closed = true;
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	//
	/*package*/ void skipShebang() {
		try {
			reachedEOF = !IntLispUtils.skipShebang(reader, this);
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isStandard() {
		return standard;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isReachedEOF() {
		return reachedEOF;
	}

	/**
	 * 
	 * @return
	 */
	public Reader getReader() {
		return reader;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypePort()
	 */
	public boolean isTypePort() {
		return true;
	}

	//
	/*package*/ Parser getParser() {
		return parser;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#read()
	 */
	public Integer read() {
		return readc();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#write(java.lang.Object)
	 */
	public boolean write(Integer symbol) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#moveRight()
	 */
	public boolean moveRight() {
		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return false;
		}

		try {
			int ch = getc();

			if(ch < 0) {
				reachedEOF = true;
				return false;
			} else {
				return true;
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#moveLeft()
	 */
	public boolean moveLeft() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.CharTape#readc()
	 */
	public int readc() {
		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return -1;
		}

		try {
			int ch = getc();

			if(ch < 0) {
				reachedEOF = true;
				return -1;
			} else {
				unread((char)ch);
				return ch;
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.CharTape#writec(int)
	 */
	public boolean writec(int c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#mark()
	 */
	public int mark() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#back()
	 */
	public int back() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.io.CharUnreadBuffer#unread(char)
	 */
	public void unread(int c) {
		unreadbuf = c;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.io.CharUnreadBuffer#unread(char)
	 */
	public void unread(char c) {
		unreadbuf = c;
	}

	/**
	 * 
	 * @return
	 */
	public boolean ready() {
		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return false;
		} else {
			try {
				return reader.ready();
			} catch (IOException e) {
				throw new LispIOException(e);
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChar()
	 */
	public int getChar() throws IOException {
		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return -1;
		}

		try {
			int ch = getc();

			if(ch < 0) {
				reachedEOF = true;
				return ch;
			} else {
				return ch;
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#lookaheadChar()
	 */
	public int lookaheadChar() throws IOException {
		return readc();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getString(int)
	 */
	public String getString(int n) throws IOException {
		StringBuilder b = new StringBuilder();
		int c;

		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return null;
		} else {
			for(int i = 0; i < n; i++) {
				if((c = getChar()) >= 0) {
					b.appendCodePoint(c);
				} else if(i == 0) {
					reachedEOF = true;
					return null;
				} else {
					break;
				}
			}
			return b.toString();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChars(int[], int, int)
	 */
	public int getChars(int[] buf,
			int start, int end) throws IOException {
		int c;

		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return -1;
		} else {
			for(int i = start; i < end; i++) {
				if((c = getChar()) >= 0) {
					buf[i] = c;
				} else if(i == start) {
					reachedEOF = true;
					return -1;
				} else {
					break;
				}
			}
			return end - start;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getStringAll()
	 */
	public String getStringAll() throws IOException {
		StringBuilder b = new StringBuilder();
		int c;

		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return null;
		} else {
			while(true) {
				if((c = getChar()) >= 0) {
					b.appendCodePoint(c);
				} else if(b.length() == 0) {
					reachedEOF = true;
					return null;
				} else {
					break;
				}
			}
			return b.toString();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getLine()
	 */
	public String getLine() throws IOException {
		StringBuilder b = new StringBuilder();
		int c;

		if(closed) {
			throw msg.getError("err.port.closed");
		} else if(reachedEOF) {
			return null;
		} else {
			while(true) {
				if((c = getChar()) == '\n') {
					break;
				} else if(c >= 0) {
					b.appendCodePoint(c);
				} else if(b.length() == 0) {
					reachedEOF = true;
					return null;
				} else {
					break;
				}
			}
			return b.toString();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getDatum()
	 */
	public Datum getDatum() throws IOException {
		return readS();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSInputPort#isPortEof()
	 */
	public boolean isPortEof() {
		return reachedEOF;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#supportsPosition()
	 */
	public boolean hasPortPosition() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getPortPosition()
	 */
	public Datum getPortPosition() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#supportsSetPosition()
	 */
	public boolean hasSetPortPosition() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#setPortPosition(net.morilib.lisp.Datum)
	 */
	public void setPortPosition(Datum pos) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<iport>");
	}

}
