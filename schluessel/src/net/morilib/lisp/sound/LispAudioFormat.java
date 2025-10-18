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
package net.morilib.lisp.sound;

import java.util.List;

import javax.sound.sampled.AudioFormat;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/13
 */
public class LispAudioFormat extends Datum2 {

	//
	private static final
	OneToOneSet<Symbol, AudioFormat.Encoding>
	ENCODINGS = new HashOneToOneSet<Symbol, AudioFormat.Encoding>
	(new Object[][] {
			{ 
				Symbol.getSymbol("alaw"),
				AudioFormat.Encoding.ALAW
			},
			{
				Symbol.getSymbol("pcm-signed"),
				AudioFormat.Encoding.PCM_SIGNED
			},
			{
				Symbol.getSymbol("pcm-unsigned"),
				AudioFormat.Encoding.PCM_UNSIGNED
			},
			{
				Symbol.getSymbol("ulaw"),
				AudioFormat.Encoding.ULAW
			}
	});

	//
	private static final OneToOneSet<Symbol, Boolean>
	ENDIAN = new HashOneToOneSet<Symbol, Boolean>
	(new Object[][] {
			{ 
				Symbol.getSymbol("big-endian"),
				Boolean.TRUE
			},
			{
				Symbol.getSymbol("little-endian"),
				Boolean.FALSE
			}
	});

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/13
	 */
	public static class MakeAudioFormat extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			AudioFormat.Encoding e;
			Boolean b;

			if(l.size() != 7) {
				throw mesg.getError("err.argument", body);
			} else if((e = ENCODINGS.getValue(l.get(0))) == null) {
				throw mesg.getError(
						"err.sound.invalidencoding", l.get(0));
			} else if(!(l.get(1) instanceof LispReal)) {
				throw mesg.getError("err.require.real", l.get(1));
			} else if(!(l.get(2) instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", l.get(2));
			} else if(l.get(2).getInt() <= 0 ||
					l.get(2).getInt() > 32) {
				throw mesg.getError(
						"err.sound.invalidsamplingbits", l.get(2));
			} else if(!(l.get(3) instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", l.get(3));
			} else if(!(l.get(4) instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", l.get(4));
			} else if(!(l.get(5) instanceof LispReal)) {
				throw mesg.getError("err.require.real", l.get(5));
			} else if((b = ENDIAN.getValue(l.get(6))) == null) {
				throw mesg.getError(
						"err.sound.invalidendian", l.get(6));
			}

			return new LispAudioFormat(
					e,
					(float)l.get(1).getRealDouble(),
					l.get(2).getInt(),
					l.get(3).getInt(),
					l.get(4).getInt(),
					(float)l.get(5).getRealDouble(),
					b.booleanValue()
			);
		}

	}

	//
	/*package*/ AudioFormat audioFormat;

	/**
	 * 
	 * @param encoding
	 * @param sampleRate
	 * @param sampleSizeInBits
	 * @param channels
	 * @param frameSize
	 * @param frameRate
	 * @param bigEndian
	 */
	public LispAudioFormat(
			AudioFormat.Encoding encoding,
			float sampleRate,
			int sampleSizeInBits,
			int channels,
			int frameSize,
			float frameRate,
			boolean bigEndian) {
		audioFormat = new AudioFormat(
				encoding,
				sampleRate,
				sampleSizeInBits,
				channels,
				frameSize,
				frameRate,
				bigEndian);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<audio-format ")
		.append(audioFormat.toString()).append(">");
	}

}
