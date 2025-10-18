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
package net.morilib.lisp.print;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.HashAttributeSet;
import javax.print.attribute.standard.Chromaticity;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.MediaSizeName;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/31
 */
public final class LispPrintUtils {

	//
	private static final Symbol CHROMATICITY =
		Symbol.getSymbol("chromaticity");
	private static final Symbol COPIES       =
		Symbol.getSymbol("copies");
	private static final Symbol JOB_NAME     =
		Symbol.getSymbol("job-name");
	private static final Symbol MEDIA_SIZE   =
		Symbol.getSymbol("media-size");
	private static final Symbol MONOCHROME   =
		Symbol.getSymbol("monochrome");
	private static final Symbol COLOR        =
		Symbol.getSymbol("color");
	private static final Map<String, Attribute> MEDIA_SIZES;

	//
	static {
		Map<String, Attribute> mp = new HashMap<String, Attribute>();

		mp.put("executive", MediaSizeName.EXECUTIVE);
		mp.put("folio", MediaSizeName.FOLIO);
		mp.put("invoice", MediaSizeName.INVOICE);
		mp.put("iso-a0", MediaSizeName.ISO_A0);
		mp.put("iso-a1", MediaSizeName.ISO_A1);
		mp.put("iso-a2", MediaSizeName.ISO_A2);
		mp.put("iso-a3", MediaSizeName.ISO_A3);
		mp.put("iso-a4", MediaSizeName.ISO_A4);
		mp.put("iso-a5", MediaSizeName.ISO_A5);
		mp.put("iso-a6", MediaSizeName.ISO_A6);
		mp.put("iso-a7", MediaSizeName.ISO_A7);
		mp.put("iso-a8", MediaSizeName.ISO_A8);
		mp.put("iso-a9", MediaSizeName.ISO_A9);
		mp.put("iso-a10", MediaSizeName.ISO_A10);
		mp.put("iso-b0", MediaSizeName.ISO_B0);
		mp.put("iso-b1", MediaSizeName.ISO_B1);
		mp.put("iso-b2", MediaSizeName.ISO_B2);
		mp.put("iso-b3", MediaSizeName.ISO_B3);
		mp.put("iso-b4", MediaSizeName.ISO_B4);
		mp.put("iso-b5", MediaSizeName.ISO_B5);
		mp.put("iso-b6", MediaSizeName.ISO_B6);
		mp.put("iso-b7", MediaSizeName.ISO_B7);
		mp.put("iso-b8", MediaSizeName.ISO_B8);
		mp.put("iso-b9", MediaSizeName.ISO_B9);
		mp.put("iso-b10", MediaSizeName.ISO_B10);
		mp.put("iso-c0", MediaSizeName.ISO_C0);
		mp.put("iso-c1", MediaSizeName.ISO_C1);
		mp.put("iso-c2", MediaSizeName.ISO_C2);
		mp.put("iso-c3", MediaSizeName.ISO_C3);
		mp.put("iso-c4", MediaSizeName.ISO_C4);
		mp.put("iso-c5", MediaSizeName.ISO_C5);
		mp.put("iso-c6", MediaSizeName.ISO_C6);
		mp.put("iso-designated-long", MediaSizeName.ISO_DESIGNATED_LONG);
		mp.put("italy-envelope", MediaSizeName.ITALY_ENVELOPE);
		mp.put("japanese-double-postcard", MediaSizeName.JAPANESE_DOUBLE_POSTCARD);
		mp.put("japanese-postcard", MediaSizeName.JAPANESE_POSTCARD);
		mp.put("jis-b0", MediaSizeName.JIS_B0);
		mp.put("jis-b1", MediaSizeName.JIS_B1);
		mp.put("jis-b2", MediaSizeName.JIS_B2);
		mp.put("jis-b3", MediaSizeName.JIS_B3);
		mp.put("jis-b4", MediaSizeName.JIS_B4);
		mp.put("jis-b5", MediaSizeName.JIS_B5);
		mp.put("jis-b6", MediaSizeName.JIS_B6);
		mp.put("jis-b7", MediaSizeName.JIS_B7);
		mp.put("jis-b8", MediaSizeName.JIS_B8);
		mp.put("jis-b9", MediaSizeName.JIS_B9);
		mp.put("jis-b10", MediaSizeName.JIS_B10);
		mp.put("ledger", MediaSizeName.LEDGER);
		mp.put("monarch-envelope", MediaSizeName.MONARCH_ENVELOPE);
		mp.put("na-10x13-envelope", MediaSizeName.NA_10X13_ENVELOPE);
		mp.put("na-10x14-envelope", MediaSizeName.NA_10X14_ENVELOPE);
		mp.put("na-10x15-envelope", MediaSizeName.NA_10X15_ENVELOPE);
		mp.put("na-5x7", MediaSizeName.NA_5X7);
		mp.put("na-6x9-envelope", MediaSizeName.NA_6X9_ENVELOPE);
		mp.put("na-7x9-envelope", MediaSizeName.NA_7X9_ENVELOPE);
		mp.put("na-8x10", MediaSizeName.NA_8X10);
		mp.put("na-9x11-envelope", MediaSizeName.NA_9X11_ENVELOPE);
		mp.put("na-9x12-envelope", MediaSizeName.NA_9X12_ENVELOPE);
		mp.put("na-legal", MediaSizeName.NA_LEGAL);
		mp.put("na-letter", MediaSizeName.NA_LETTER);
		mp.put("na-number-10-envelope", MediaSizeName.NA_NUMBER_10_ENVELOPE);
		mp.put("na-number-11-envelope", MediaSizeName.NA_NUMBER_11_ENVELOPE);
		mp.put("na-number-12-envelope", MediaSizeName.NA_NUMBER_12_ENVELOPE);
		mp.put("na-number-14-envelope", MediaSizeName.NA_NUMBER_14_ENVELOPE);
		mp.put("na-number-9-envelope", MediaSizeName.NA_NUMBER_9_ENVELOPE);
		mp.put("personal-envelope", MediaSizeName.PERSONAL_ENVELOPE);
		mp.put("quarto", MediaSizeName.QUARTO);
		mp.put("tabloid", MediaSizeName.TABLOID);
		mp.put("a0", MediaSizeName.ISO_A0);
		mp.put("a1", MediaSizeName.ISO_A1);
		mp.put("a2", MediaSizeName.ISO_A2);
		mp.put("a3", MediaSizeName.ISO_A3);
		mp.put("a4", MediaSizeName.ISO_A4);
		mp.put("a5", MediaSizeName.ISO_A5);
		mp.put("a6", MediaSizeName.ISO_A6);
		mp.put("a7", MediaSizeName.ISO_A7);
		mp.put("a8", MediaSizeName.ISO_A8);
		mp.put("a9", MediaSizeName.ISO_A9);
		mp.put("a10", MediaSizeName.ISO_A10);
		mp.put("b0", MediaSizeName.ISO_B0);
		mp.put("b1", MediaSizeName.ISO_B1);
		mp.put("b2", MediaSizeName.ISO_B2);
		mp.put("b3", MediaSizeName.ISO_B3);
		mp.put("b4", MediaSizeName.ISO_B4);
		mp.put("b5", MediaSizeName.ISO_B5);
		mp.put("b6", MediaSizeName.ISO_B6);
		mp.put("b7", MediaSizeName.ISO_B7);
		mp.put("b8", MediaSizeName.ISO_B8);
		mp.put("b9", MediaSizeName.ISO_B9);
		mp.put("b10", MediaSizeName.ISO_B10);
		mp.put("c0", MediaSizeName.ISO_C0);
		mp.put("c1", MediaSizeName.ISO_C1);
		mp.put("c2", MediaSizeName.ISO_C2);
		mp.put("c3", MediaSizeName.ISO_C3);
		mp.put("c4", MediaSizeName.ISO_C4);
		mp.put("c5", MediaSizeName.ISO_C5);
		mp.put("c6", MediaSizeName.ISO_C6);
		MEDIA_SIZES = Collections.unmodifiableMap(mp);
	}

	//
	private LispPrintUtils() {}

	/**
	 * 
	 * @param d
	 * @return
	 * @throws IllegalArgumentException
	 */
	public AttributeSet toAttribute(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		AttributeSet atr = new HashAttributeSet();
		Attribute a;
		Datum d, g;
		Cons c;

		while(itr.hasNext()) {
			d = itr.next();
			if(!(d instanceof Cons)) {
				throw mesg.getError("err.print.attribute.invalid");
			} else if((c = (Cons)d).getCar().equals(COPIES)) {
				atr.add(new Copies(SubrUtils.getSmallInt(
						SubrUtils.cadr(c, mesg),
						mesg)));
			} else if(c.getCar().equals(CHROMATICITY)) {
				if((g = SubrUtils.cadr(c, mesg)).equals(COLOR)) {
					atr.add(Chromaticity.COLOR);
				} else if(g.equals(MONOCHROME)) {
					atr.add(Chromaticity.MONOCHROME);
				} else {
					// ignore
				}
			} else if(c.getCar().equals(JOB_NAME)) {
				atr.add(new JobName(SubrUtils.getString(
						SubrUtils.cadr(c, mesg), mesg),
						mesg.getLocale()));
			} else if(c.getCar().equals(MEDIA_SIZE)) {
				a = MEDIA_SIZES.get(SubrUtils.getSymbolName(
						SubrUtils.cadr(c, mesg), mesg));
				if(a != null) {
					atr.add(a);
				}
			} else {
				// ignore
			}
		}
		return atr;
	}

}
