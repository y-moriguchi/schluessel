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
package net.morilib.lisp.format;

import java.util.List;

public final class FormatCommandA extends FormatCommand {

	public FormatCommandA(
			List<ArgumentType> parameters,
			boolean atmark, boolean colon) {
		super(parameters, atmark, colon);
	}
	
	
	public String toString(
			FormatArguments args) throws LispFormatException {
		int  mincol  = getInt(0, args, 0);
		int  colinc  = getInt(1, args, 1);
		int  minpad  = getInt(2, args, 0);
		char padchar = getChar(3, args, ' ');
		
		if(mincol < 0) {
			throw new LispFormatException();
		} else if(colinc < 1) {
			throw new LispFormatException();
		} else if(minpad < 0) {
			throw new LispFormatException();
		}
		
		return FormatUtils.pad(
				args.shiftString(),
				mincol, colinc, minpad, padchar,
				isAtmark());
	}

}
