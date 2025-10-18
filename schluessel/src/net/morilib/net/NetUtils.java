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
package net.morilib.net;

import java.io.IOException;
import java.io.InputStream;
import java.net.ProtocolException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public final class NetUtils {

	//
	private NetUtils() {}

	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int readnum(InputStream rd) throws IOException {
		int c;
	
		c = rd.read();
		if(c < '0' || c > '9') {
			throw new ProtocolException();
		} else {
			return c - '0';
		}
	}

	/**
	 * 
	 * @param rd
	 * @param ok
	 * @param okto
	 * @return
	 * @throws IOException
	 */
	public static int readCode(InputStream rd,
			int ok, int okto) throws IOException {
		int r;
		
		r  = readnum(rd) * 100;
		r += readnum(rd) * 10;
		r += readnum(rd);
		if(r < ok || r > okto) {
			throw new ProtocolException(r + getReason(rd));
		}
		return r;
	}

	/**
	 * 
	 * @param rd
	 * @param ok
	 * @return
	 * @throws IOException
	 */
	public static int readCode(InputStream rd,
			int ok) throws IOException {
		return readCode(rd, ok, ok + 99);
	}

	/**
	 * 
	 * @param rd
	 * @param ok
	 * @param okto
	 * @return
	 * @throws IOException
	 */
	public static int getCode(InputStream rd,
			int ok, int okto) throws IOException {
		int r;
	
		r  = readnum(rd) * 100;
		r += readnum(rd) * 10;
		r += readnum(rd);
		if(r < ok || r > okto) {
			throw new ProtocolException(r + getReason(rd));
		} else {
			for(int c = rd.read(); true; c = rd.read()) {
				if(c == '\r') {
					c = rd.read();
					if(c < 0) {
						throw new ProtocolException();
					} else if(c == '\n') {
						return r;
					}
				}
//				System.out.print((char)c);
			}
		}
	}

	/**
	 * 
	 * @param rd
	 * @param ok
	 * @return
	 * @throws IOException
	 */
	public static int getCode(InputStream rd,
			int ok) throws IOException {
		return getCode(rd, ok, ok + 99);
	}

	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int getCode(InputStream rd) throws IOException {
		int r;
	
		r  = readnum(rd) * 100;
		r += readnum(rd) * 10;
		r += readnum(rd);
		for(int c = rd.read(); true; c = rd.read()) {
			if(c == '\r') {
				c = rd.read();
				if(c < 0) {
					throw new ProtocolException();
				} else if(c == '\n') {
					return r;
				}
			}
//				System.out.print((char)c);
		}
	}

	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static String getReason(InputStream rd) throws IOException {
		StringBuilder b = new StringBuilder();
	
		for(int c = rd.read(); true; c = rd.read()) {
			if(c == '\r') {
				c = rd.read();
				if(c < 0) {
					throw new ProtocolException();
				} else if(c == '\n') {
					return b.toString();
				}
			} else {
				b.append((char)c);
			}
		}
	}

}
