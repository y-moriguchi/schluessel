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
package net.morilib.net.snmp;

import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class ASN1Tag {

	public static final ASN1Tag BOOLEAN = new ASN1TagBoolean();
	public static final ASN1Tag INTEGER = new ASN1TagInteger();
	public static final ASN1Tag BIT_STRING =
		new ASN1Tag(0, false, (byte)3);
	public static final ASN1Tag OCTET_STRING = new ASN1TagString(4);
	public static final ASN1Tag NULL = new ASN1TagNull();
	public static final ASN1Tag OID = new ASN1TagOID();
	public static final ASN1Tag OBJECT_DESCRIPTOR = 
		new ASN1TagString(7);
	public static final ASN1Tag REAL =
		new ASN1Tag(0, false, (byte)9);
	public static final ASN1Tag UTF8STRING = new ASN1TagString(0xc);
	public static final ASN1Tag SEQUENCE =
		new ASN1Tag(0, true, (byte)0x10);
	public static final ASN1Tag SET = new ASN1TagSet();
	public static final ASN1Tag NUMERIC_STRING =
		new ASN1TagString(0x12);
	public static final ASN1Tag PRINTABLE_STRING =
		new ASN1TagString(0x13);
	public static final ASN1Tag TELETEX_STRING =
		new ASN1TagString(0x14);
	public static final ASN1Tag IA5STRING = new ASN1TagString(0x16);
	public static final ASN1Tag UTC_TIME = new ASN1TagUTCTime();
	public static final ASN1Tag GENERALIZED_TIME =
		new ASN1TagGeneralizedTime();
	public static final ASN1Tag IP_ADDRESS = new ASN1TagIpAddress();
	public static final ASN1Tag COUNTER32 = new ASN1TagCounter32();
	public static final ASN1Tag UNSIGNED32 = new ASN1TagUnsigned32();
	public static final ASN1Tag TIME_TICKS = new ASN1TagTimeTicks();
	public static final ASN1Tag COUNTER64 = new ASN1TagCounter64();

	//
	private static final ASN1Tag[] _POOL = new ASN1Tag[] {
		null, BOOLEAN, INTEGER, BIT_STRING, OCTET_STRING,
		NULL, OID, OBJECT_DESCRIPTOR, null,
		REAL, null, null, null,
		UTF8STRING, null, null, null,
		SEQUENCE, SET, NUMERIC_STRING, PRINTABLE_STRING,
		TELETEX_STRING, null, IA5STRING, UTC_TIME,
		GENERALIZED_TIME, null, null, null,
		null, null, null, null
	};

	//
	private static final String[] _POOL_STR = new String[] {
		null, "BOOLEAN", "INTEGER", "BIT_STRING", "OCTET_STRING",
		"NULL", "OBJECT_IDENTIFIER", "ObjectDescriptor", null,
		"REAL", null, null, null,
		"UTF8String", null, null, null,
		"SEQUENCE", "SET", "NumericString", "PrintableString",
		"TeletexString", null, "IA5String", "UTCTime",
		"GeneralizedTime", null, null, null,
		null, null, null, null
	};

	//
	private static final ASN1Tag[] _POOL1 = new ASN1Tag[] {
		IP_ADDRESS, COUNTER32, UNSIGNED32, TIME_TICKS,
		null, null, COUNTER64, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null
	};

	//
	private static final String[] _POOL_STR1 = new String[] {
		"IpAddress", "Counter32", "Unsigned32", "TimeTicks",
		null, null, "Counter64", null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null,
		null, null, null, null
	};

	//
	private static final String[] _CLASS_STR = new String[] {
		"GENERIC:", "APPLICATION:", "CONTEXT:", "PRIVATE:"
	};

	//
	private byte no;
	private byte[] extendedNo;
	private int asnClass;
	private boolean structured;

	/**
	 * 
	 * @param structured
	 * @param no
	 */
	public ASN1Tag(int asnClass, boolean structured, byte[] no) {
		if(no == null) {
			throw new NullPointerException();
		} else if(asnClass < 0 || asnClass > 3) {
			throw new IllegalArgumentException();
		}
		this.asnClass = asnClass;
		this.structured = structured;
		this.no = 0x1f;
		this.extendedNo = new byte[no.length];
		System.arraycopy(no, 0, this.no, 0, no.length);
	}

	//
	ASN1Tag(int asnClass, boolean structured, byte no) {
		if(no < 0 || no >= 0x1f) {
			throw new IllegalArgumentException();
		} else if(asnClass < 0 || asnClass > 3) {
			throw new IllegalArgumentException();
		}
		this.asnClass = asnClass;
		this.structured = structured;
		this.extendedNo = null;
		this.no = no;
	}

	/**
	 * 
	 * @param structured
	 * @param no
	 * @return
	 */
	public static ASN1Tag getInstance(int asnClass,
			boolean structured, int no) {
		if(no < 0 || no >= _POOL.length) {
			throw new IllegalArgumentException();
		} else if(asnClass == 0 && _POOL[no] != null) {
			return _POOL[no];
		} else if(asnClass == 1 && _POOL1[no] != null) {
			return _POOL1[no];
		} else {
			return new ASN1Tag(asnClass, structured, (byte)no);
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isStructured() {
		return structured;
	}

	/**
	 * 
	 * @param bts
	 * @return
	 */
	protected Object analyse(byte bts[]) throws ASN1Exception {
		return structured ? ASN1.readSequence(bts) : bts;
	}

	@Override
	public boolean equals(Object o) {
		if(o instanceof ASN1Tag) {
			ASN1Tag t = (ASN1Tag)o;

			return (asnClass == t.asnClass && no == t.no &&
					Arrays.equals(extendedNo, t.extendedNo));
		}
		return false;
	}

	@Override
	public int hashCode() {
		int r = 31;

		r = 17 * r + asnClass;
		r = 17 * r + no;
		r = 17 * r + Arrays.hashCode(extendedNo);
		return r;
	}

	@Override
	public String toString() {
		StringBuilder b;

		b = new StringBuilder();
		b.append(_CLASS_STR[asnClass]);
		if(asnClass == 0 && _POOL_STR[no] == null) {
			b.append(_POOL_STR[no]);
		} else if(asnClass == 1 && _POOL1[no] != null) {
			b.append(_POOL_STR1[no]);
		} else {
			b.append((int)no);
		}
		return b.toString();
	}

}
