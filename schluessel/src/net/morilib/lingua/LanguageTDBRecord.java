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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/16
 */
public class LanguageTDBRecord {

	public static enum Scope {
		INDIVIDUAL, MACROLANGUAGE, SPECIAL
	}

	public static enum Type {
		ANCIENT, CONSTRUCTED, EXTINCT, HISTORICAL, LIVING, SPECIAL
	}

	private static int R_ISO3 = 0;
	private static int R_ISO2B = 3;
	private static int R_ISO2A = 6;
	private static int R_ISO1 = 9;
	private static int R_SCOPE = 11;
	private static int R_LANG_TYPE = 12;
	private static int R_DESC = 13;
	private static int L_ISO3 = R_ISO2B - R_ISO3;
	private static int L_ISO2B = R_ISO2A - R_ISO2B;
	private static int L_ISO2A = R_ISO1 - R_ISO2A;
	private static int L_ISO1 = R_SCOPE - R_ISO1;

	private byte[] record;

	LanguageTDBRecord(byte[] record) {
		this.record = record;
	}

	public String getISO3() {
		return new String(record, R_ISO3, L_ISO3);
	}

	public String getISO2b() {
		return record[R_ISO2B] != 0 ?
				new String(record, R_ISO2B, L_ISO2B) : null;
	}

	public String getISO2a() {
		return record[R_ISO2A] != 0 ?
				new String(record, R_ISO2A, L_ISO2A) : null;
	}

	public String getISO1() {
		return record[R_ISO1] != 0 ?
				new String(record, R_ISO1, L_ISO1) : null;
	}

	public boolean isIndividual() {
		return (char)record[R_SCOPE] == 'I';
	}

	public boolean isMacrolanguage() {
		return (char)record[R_SCOPE] == 'M';
	}

	public boolean isSpecial() {
		return (char)record[R_SCOPE] == 'S';
	}

	public Scope getScope() {
		switch((char)record[R_SCOPE]) {
		case 'I':  return Scope.INDIVIDUAL;
		case 'M':  return Scope.MACROLANGUAGE;
		case 'S':  return Scope.SPECIAL;
		default:   throw new RuntimeException();
		}
	}

	public boolean isAncient() {
		return (char)record[R_LANG_TYPE] == 'A';
	}

	public boolean isConstructed() {
		return (char)record[R_LANG_TYPE] == 'C';
	}

	public boolean isExtinct() {
		return (char)record[R_LANG_TYPE] == 'E';
	}

	public boolean isHistorical() {
		return (char)record[R_LANG_TYPE] == 'H';
	}

	public boolean isLiving() {
		return (char)record[R_LANG_TYPE] == 'L';
	}

	public Type getLanguageType() {
		switch((char)record[R_LANG_TYPE]) {
		case 'A':  return Type.ANCIENT;
		case 'C':  return Type.CONSTRUCTED;
		case 'E':  return Type.EXTINCT;
		case 'H':  return Type.HISTORICAL;
		case 'L':  return Type.LIVING;
		case 'S':  return Type.SPECIAL;
		default:   throw new RuntimeException();
		}
	}

	public String getDescription() {
		StringBuffer b = new StringBuffer();

		for(int i = R_DESC; record[i] != 0; i++) {
			b.append((char)record[i]);
		}
		return b.toString();
	}

}
