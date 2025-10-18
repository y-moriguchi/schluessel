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
package net.morilib.lisp.iterator;

import java.io.Closeable;
import java.io.IOException;
import java.io.Reader;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.parser.csv.CSVConfig;
import net.morilib.parser.csv.CSVException;
import net.morilib.parser.csv.StringCSVPullParser;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public class LispCSVIterator extends Datum2
implements ILispIterator, Closeable {

	//
	private StringCSVPullParser parser;
	private Datum list;
	private LispMessage mesg;

	/**
	 * 
	 * @param delimiters
	 * @param quote
	 * @param comment
	 * @param newline
	 */
	public LispCSVIterator(Reader rd, String delimiters,
			int quote, int comment, int newline, LispMessage mesg) {
		CSVConfig conf = new CSVConfig(delimiters, quote, false,
				comment, "", newline, comment > 0);

		parser = new StringCSVPullParser(rd, conf);
		read();
	}

	//
	private void read() {
		ConsListBuilder bld = new ConsListBuilder();

		try {
			if(parser.next()) {
				for(String s : parser.get()) {
					bld.append(new LispString(s));
				}
				list = bld.get();
			} else {
				list = null;
			}
		} catch (CSVException e) {
			try {
				close();
			} catch (IOException e1) {
				throw mesg.getError("err.io");
			}
			throw mesg.getError("err.iterator.csv.invalid");
		} catch (IOException e) {
			try {
				close();
			} catch (IOException e1) {
				throw mesg.getError("err.io");
			}
			throw mesg.getError("err.io");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		return list == null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		read();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		return list;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		parser.close();
		list = null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<csv-iterator>");
	}

}
