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
package net.morilib.net.misc;

import java.net.ProtocolException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public class TFTPErrorPacketException extends ProtocolException {

	public static enum TFTPError {

		UNDEFINED, FILE_NOT_FOUND, ACCESS_VIOLATION,
		OUT_OF_SPACE, ILLEGAL_OPERATION, UNKNOWN_TID,
		FILE_EXISTS, NO_SUCH_USER, OPTION_NEGOTIATION_ERROR;

	}

	//
	private TFTPError code;

	/**
	 * 
	 * @param code
	 */
	public TFTPErrorPacketException(String str, TFTPError code) {
		super(str);
		this.code = code;
	}

	/**
	 * 
	 * @param str
	 * @param ec
	 */
	public TFTPErrorPacketException(String str, int ec) {
		super(str);
		this.code = getErrorCode(ec);
	}

	/**
	 * 
	 * @param code
	 * @return
	 */
	public static TFTPError getErrorCode(int code) {
		switch(code) {
		case 1:   return TFTPError.FILE_NOT_FOUND;
		case 2:   return TFTPError.ACCESS_VIOLATION;
		case 3:   return TFTPError.OUT_OF_SPACE;
		case 4:   return TFTPError.ILLEGAL_OPERATION;
		case 5:   return TFTPError.UNKNOWN_TID;
		case 6:   return TFTPError.FILE_EXISTS;
		case 7:   return TFTPError.NO_SUCH_USER;
		case 8:   return TFTPError.OPTION_NEGOTIATION_ERROR;
		default:  return TFTPError.UNDEFINED;
		}
	}

	/**
	 * 
	 * @return
	 */
	public TFTPError getErrorCode() {
		return code;
	}

}
