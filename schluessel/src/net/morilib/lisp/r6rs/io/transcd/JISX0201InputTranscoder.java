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
package net.morilib.lisp.r6rs.io.transcd;

import java.io.IOException;
import java.io.InputStream;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class JISX0201InputTranscoder extends AbstractInputTranscoder {

	//
	private static final char[] KANAS = new char[] {
		// 0     1     2     3    4     5     6     7
		0,    '。', '「', '」', '、', '・', 'ヲ', 'ァ', // A0
		'ィ', 'ゥ', 'ェ', 'ォ', 'ャ', 'ュ', 'ョ', 'ッ', // A8
		'ー', 'ア', 'イ', 'ウ', 'エ', 'オ', 'カ', 'キ', // B0
		'ク', 'ケ', 'コ', 'サ', 'シ', 'ス', 'セ', 'ソ', // B8
		'タ', 'チ', 'ツ', 'テ', 'ト', 'ナ', 'ニ', 'ヌ', // C0
		'ネ', 'ノ', 'ハ', 'ヒ', 'フ', 'ヘ', 'ホ', 'マ', // C8
		'ミ', 'ム', 'メ', 'モ', 'ヤ', 'ユ', 'ヨ', 'ラ', // D0
		'リ', 'ル', 'レ', 'ロ', 'ワ', 'ン', '゛', '゜', // D8
	};

	//
	private InputStream ins;
	private LispErrorHandlingMode mode;

	/**
	 * 
	 * @param ins
	 */
	public JISX0201InputTranscoder(InputStream ins,
			LispErrorHandlingMode mode) {
		this.ins = ins;
		this.mode = mode;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.InputTranscoder#read()
	 */
	public int read() throws IOException {
		while(true) {
			int c = ins.read(), s;
	
			if(c < 0x80) {
				return c;
			} else if(c >= 0xa1 && c < 0xe0) {
				return KANAS[c - 0xa0];
			} else if((s = mode.ifRead(c, '?')) >= 0) {
				return s;
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		ins.close();
	}

}