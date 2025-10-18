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
package net.morilib.net.ntp;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public final class NTP {

	//
	private static void readbts(DataInputStream din,
			byte[] b) throws IOException {
		if(din.read(b) != b.length) {
			throw new EOFException();
		}
	}

	//
	private static NTPTimestamp gettm(long ntp) {
		return new NTPTimestamp(ntp >>> 32, ntp & 0xffffffffl);
	}

	//
	private static long gettm(NTPTimestamp ntp) {
		if(ntp == null) {
			return 0;
		} else {
			return ntp.getSeconds() << 32 | ntp.getFraction();
		}
	}

	/**
	 * 
	 * @param bts
	 * @return
	 * @throws NTPException
	 */
	public static NTPInfo parsePacket(
			byte[] bts) throws NTPException {
		ByteArrayInputStream bis = new ByteArrayInputStream(bts);
		DataInputStream din = new DataInputStream(bis);
		NTPInfo r = new NTPInfo();
		int b, layer;
		byte y[];

		try {
			b = din.readUnsignedByte();
			r.setLeapIndicator(NTPLeapIndicator.getFromNTPBits(b));
			r.setVersion((byte)((b >> 3) & 7));
			r.setMode(NTPMode.getFromNTPBits(b));
	
			layer = din.readUnsignedByte();
	
			r.setPollingInterval(din.readByte());
			r.setPrecision(din.readByte());
			r.setRouteDelay(din.readInt());
			r.setRouteDispersion(din.readInt());
	
			if(layer == 0) {
				y = new byte[4];
				readbts(din, y);
				r.setReferenceInfo(new NTPReferenceInfo.Unknown(
						layer, new String(y)));
			} else if(layer == 1) {
				y = new byte[4];
				readbts(din, y);
				r.setReferenceInfo(new NTPReferenceInfo.Primary(
						new String(y)));
			} else if(layer >= 2 && layer <= 15) {
				y = new byte[4];
				readbts(din, y);
				r.setReferenceInfo(new NTPReferenceInfo.Secondary(
						layer, InetAddress.getByAddress(y)));
			} else {
				b = din.readInt();
				r.setReferenceInfo(new NTPReferenceInfo(layer));
			}
	
			r.setReferenceTime(gettm(din.readLong()));
			r.setOriginateTime(gettm(din.readLong()));
			r.setReceiveTime(gettm(din.readLong()));
			r.setTransmitTime(gettm(din.readLong()));
			r.setKeyIdentifier(din.readInt());
	
			y = new byte[16];
			readbts(din, y);
			r.setMessageDigest(y);
			return r;
		} catch(EOFException e) {
			throw new NTPException(e);
		} catch(IOException e) {
			throw new NTPException(e);
		}
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public static byte[] toBytes(NTPInfo r) {
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		DataOutputStream dot = new DataOutputStream(b);
		int c;

		try {
			c  = r.getLeapIndicator().getCode() << 6;
			c |= r.getVersion() << 3;
			c |= r.getMode().getCode();
			dot.writeByte(c);
			dot.writeByte(0);
			dot.writeByte(r.getPollingInterval());
			dot.writeByte(r.getPrecision());
			dot.writeInt(r.getRouteDelay());
			dot.writeInt(r.getRouteDispersion());
			dot.writeInt(0);
			dot.writeLong(gettm(r.getReferenceTime()));
			dot.writeLong(gettm(r.getOriginateTime()));
			dot.writeLong(gettm(r.getReceiveTime()));
			dot.writeLong(gettm(r.getTransmitTime()));
			dot.writeInt(r.getKeyIdentifier());
			dot.write(new byte[16]);
			return b.toByteArray();
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPInfo receive(InetAddress addr,
			int port) throws IOException, NTPException {
		DatagramSocket sok = null;
		DatagramPacket pkt, lpk;
		NTPInfo si = new NTPInfo(), ri;
		byte[] y, z;

		try {
			sok = new DatagramSocket();
			sok.setSoTimeout(5000);

			si.setVersion((byte)3);
			si.setMode(NTPMode.CLIANT);
			si.setTransmitTime(NTPTimestamp.localCurrentTime());
			y   = toBytes(si);
			pkt = new DatagramPacket(y, y.length, addr, port);
			z   = new byte[1024];
			lpk = new DatagramPacket(z, z.length);

			sok.send(pkt);
			sok.receive(lpk);
			ri  = parsePacket(lpk.getData());
			ri.setOriginateLocalTime(si.getTransmitTime());
			return ri;
		} finally {
			if(sok != null) {
				sok.close();
			}
		}
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPInfo receive(String host,
			int port) throws IOException, NTPException {
		return receive(InetAddress.getByName(host), port);
	}

	/**
	 * 
	 * @param addr
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPInfo receive(
			InetAddress addr) throws IOException, NTPException {
		return receive(addr, 123);
	}

	/**
	 * 
	 * @param host
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPInfo receive(
			String host) throws IOException, NTPException {
		return receive(InetAddress.getByName(host), 123);
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPTimestamp receiveTime(InetAddress addr,
			int port) throws IOException, NTPException {
		NTPTimestamp r;
		NTPInfo ri;
		long t, s;

		ri = receive(addr, port);
		t = ri.getOriginateLocalTime().subtract(ri.getOriginateTime());
		r = NTPTimestamp.localCurrentTime();
		s = ri.getReceiveTime().subtract(r);
		return r.add((t + s) / 2);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPTimestamp receiveTime(String host,
			int port) throws IOException, NTPException {
		return receiveTime(InetAddress.getByName(host), port);
	}

	/**
	 * 
	 * @param addr
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPTimestamp receiveTime(
			InetAddress addr) throws IOException, NTPException {
		return receiveTime(addr, 123);
	}

	/**
	 * 
	 * @param host
	 * @return
	 * @throws IOException
	 * @throws NTPException
	 */
	public static NTPTimestamp receiveTime(
			String host) throws IOException, NTPException {
		return receiveTime(InetAddress.getByName(host), 123);
	}

}
