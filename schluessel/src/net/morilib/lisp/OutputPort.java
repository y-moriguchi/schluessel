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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import net.morilib.lisp.r6rs.io.ILispSimpleOutputPort;
import net.morilib.lisp.r6rs.io.ILispTextualOutputPort;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.util.io.UTF8StringBuilderOutputStream;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class OutputPort extends Datum
implements ILispSimpleOutputPort, ILispTextualOutputPort {

	//
	private PrintStream lineWriter;
	private boolean standard;
	private boolean closed = false;
	private UTF8StringBuilderOutputStream stringPort;
	private LispMessage msg;

	//
	private OutputPort(OutputStream wr, boolean std, LispMessage msg) {
		if(wr instanceof PrintStream) {
			this.lineWriter = (PrintStream)wr;
		} else {
			this.lineWriter = new PrintStream(wr);
		}
		this.standard   = std;
		this.msg        = msg;

		if(wr instanceof UTF8StringBuilderOutputStream) {
			this.stringPort = (UTF8StringBuilderOutputStream)wr;
		} else {
			this.stringPort = null;
		}
	}

	/**
	 * 
	 * @param wr
	 * @param msg
	 */
	public OutputPort(PrintStream wr, LispMessage msg) {
		this(wr, false, msg);
	}

	/**
	 * 
	 * @param wr
	 * @param msg
	 */
	public OutputPort(OutputStream wr, LispMessage msg) {
		this(wr, false, msg);
	}

	/**
	 * 
	 * @param fname
	 * @param msg
	 */
	public OutputPort(File fname, LispMessage msg) {
		try {
			OutputStream wr;

			wr = new FileOutputStream(fname);
			this.lineWriter = new PrintStream(wr);
			this.standard   = false;
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
	public OutputPort(File fname, String encoding,
			LispMessage msg) throws UnsupportedEncodingException {
		try {
			OutputStream wr;

			wr = new FileOutputStream(fname);
			this.lineWriter = new PrintStream(wr, true, encoding);
			this.standard   = false;
		} catch (FileNotFoundException e) {
			throw new LispIOException(e);
		}

	}

	//
	/*package*/ static OutputPort getStringPort(LispMessage msg) {
		return new OutputPort(
				new UTF8StringBuilderOutputStream(), false, msg);
	}

	/**
	 * 
	 * @param msg
	 * @return
	 */
	public static OutputPort getStandard(LispMessage msg) {
		return new OutputPort(System.out, true, msg);
	}

	/**
	 * 
	 * @param ch
	 */
	public void writeChar(LispCharacter ch) {
		if(closed) {
			throw msg.getError("err.port.closed");
		}

		if(standard) {
			System.out.print(ch.getCharacter());
		} else {
			lineWriter.print(ch.getCharacter());
		}
	}

	/**
	 * 
	 * @param d
	 */
	public void write(Datum d) {
		if(closed) {
			throw msg.getError("err.port.closed");
		}

		if(standard) {
			System.out.print(LispUtils.getResultWithoutSS(d));
		} else {
			lineWriter.print(LispUtils.getResultWithoutSS(d));
		}
	}

	/**
	 * 
	 * @param d
	 */
	public void display(Datum d) {
		if(closed) {
			throw msg.getError("err.port.closed");
		}

		if(standard) {
			System.out.print(LispUtils.print(d));
		} else {
			lineWriter.print(LispUtils.print(d));
		}
	}

	/**
	 * 
	 * @param d
	 */
	public void writeWithSS(Datum d) {
		if(closed) {
			throw msg.getError("err.port.closed");
		}

		if(standard) {
			System.out.print(LispUtils.getResult(d));
		} else {
			lineWriter.print(LispUtils.getResult(d));
		}
	}

	/**
	 * 
	 */
	public void newline() {
		if(closed) {
			throw msg.getError("err.port.closed");
		}

		if(standard) {
			System.out.println();
		} else {
			lineWriter.println();
		}
	}

	/**
	 * 
	 */
	public void close() {
		if(!closed && !standard) {
			lineWriter.close();
			closed = true;
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isStandard() {
		return standard;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypePort()
	 */
	public boolean isTypePort() {
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isStringPort() {
		return stringPort != null;
	}

	/**
	 * 
	 * @return
	 */
	public Writer getWriter() {
		return new OutputStreamWriter(lineWriter);
	}

	/**
	 * 
	 * @return
	 */
	public PrintStream getPrintStream() {
		return lineWriter;
	}

	/**
	 * 
	 * @return
	 */
	public String getOutputString() {
		if(isStringPort()) {
			return new String(stringPort.toString());
		} else {
			throw new IllegalStateException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return null;
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
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#flush()
	 */
	public void flush() throws IOException {
		lineWriter.flush();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putChar(int)
	 */
	public void putChar(int c) throws IOException {
		writeChar(LispCharacter.valueOf(c));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String)
	 */
	public void putString(String s) throws IOException {
		lineWriter.print(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int)
	 */
	public void putString(String s, int start) throws IOException {
		lineWriter.print(s.substring(start));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int, int)
	 */
	public void putString(String s,
			int start, int end) throws IOException {
		lineWriter.print(s.substring(start, end));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putDatum(net.morilib.lisp.Datum)
	 */
	public void putDatum(Datum d) throws IOException {
		writeWithSS(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#getBufferMode()
	 */
	public LispBufferMode getBufferMode() {
		return LispBufferMode.NONE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<oport>");
	}

}
