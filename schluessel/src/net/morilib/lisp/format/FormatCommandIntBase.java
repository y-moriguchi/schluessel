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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispUtils;

public abstract class FormatCommandIntBase extends FormatCommand {

	protected FormatCommandIntBase(
			List<ArgumentType> parameters, boolean atmark,
			boolean colon) {
		super(parameters, atmark, colon);
	}

	protected String toString(
			FormatArguments args,
			int radix) throws LispFormatException {
		int  mincol    = getInt (0, args, 0);
		char padchar   = getChar(1, args, ' ');
		char commachar = getChar(2, args, ',');
		int  commaintv = getInt (3, args, 0);
		
		if(mincol < 0) {
			throw new LispFormatException();
		} else if(commaintv < 1 && !get(3).isDefault()) {
			throw new LispFormatException();
		}
		
		if(isColon() && commaintv < 1) {
			commaintv = 3;
		}
		
		Object arg = args.shift();
		if(arg instanceof LispInteger) {
			return FormatUtils.padInteger(
					((LispInteger)arg).getBigInteger(),
					mincol, padchar, commachar, commaintv,
					isAtmark(), radix);
		} else if(arg instanceof Datum) {
			return LispUtils.print((Datum)arg);
		} else {
			return (arg == null) ? "null" : arg.toString();
		}
	}

}
