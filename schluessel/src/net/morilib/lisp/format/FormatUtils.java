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

import java.math.BigInteger;

import net.morilib.util.IntMath;

public final class FormatUtils {
	
	private FormatUtils() {
		// do nothing
	}
	
	
	public static String pad(
			String src,
			int mincol,
			int colinc,
			int minpad,
			char padchar,
			boolean rightpad) {
		if(mincol < 0) {
			throw new IllegalArgumentException();
		} else if(colinc < 1) {
			throw new IllegalArgumentException();
		} else if(minpad < 0) {
			throw new IllegalArgumentException();
		}
		
		int padlen = IntMath.natsub(mincol, src.length());
		StringBuilder buf = new StringBuilder();
		
		padlen = IntMath.tomult(padlen, colinc);
		if(padlen < minpad) {
			padlen = minpad;
		}
		
		if(!rightpad) {
			buf.append(src);
		}
		for(int i = 0; i < padlen; i++) {
			buf.append(padchar);
		}
		if(rightpad) {
			buf.append(src);
		}
		return buf.toString();
	}
	
	
	private static void padComma(
			StringBuilder vstr,
			int sign0, char commachar, int commaintv) {
		int l0 = vstr.length();
		
		for(int i = vstr.length() - 1; i > sign0; i--) {
			int l = l0 - i;
			
			if(l % commaintv == 0) {
				vstr.insert(i, commachar);
			}
		}
	}
	
	public static String padInteger(
			BigInteger val,
			int mincol,
			char padchar,
			char commachar,
			int commaintv,
			boolean signp,
			int radix) {
		if(mincol < 0) {
			throw new IllegalArgumentException();
		}
		
		StringBuilder vstr;
		int c0, sign0;
		
		vstr = new StringBuilder(val.toString(radix));
		if(signp && val.signum() >= 0) {
			vstr.insert(0, '+');
		}
		
		c0    = vstr.charAt(0);
		sign0 = (c0 == '+' || c0 == '-') ? 1 : 0;
		if(commaintv > 0 && padchar == '0') {
			int nl0 = vstr.length();
			int minlen = mincol - (mincol - 1) / commaintv;
			int rpt = minlen - nl0 + (
					(((minlen + 1) % commaintv) == 0) ? 1 : 0);
			
			//numlen = nl0 + (nl0 - 1) / commaintv + sign0;
			for(int i = 0; i < rpt; i++) {
				vstr.insert(sign0, padchar);
			}
			
			padComma(vstr, sign0, commachar, commaintv);
		} else if(padchar == '0') {
			int l0 = vstr.length();
			for(int i = 0; i < mincol - l0; i++) {
				vstr.insert(sign0, padchar);
			}
		} else {
			if(commaintv > 0) {
				padComma(vstr, sign0, commachar, commaintv);
			}
			
			int l0 = vstr.length();
			for(int i = 0; i < mincol - l0; i++) {
				vstr.insert(0, padchar);
			}
		}
		return vstr.toString();
	}
	
}
