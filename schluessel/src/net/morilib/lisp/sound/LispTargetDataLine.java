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

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.TargetDataLine;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.uvector.LispF32Vector;
import net.morilib.util.Endianness2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/13
 */
public class LispTargetDataLine extends Datum2
implements LispDataLine {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/13
	 */
	public static class MakeTargetDataLine extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			LispAudioFormat f;
			DataLine.Info info;
			TargetDataLine t;

			if(!(c1a instanceof LispAudioFormat)) {
				throw mesg.getError(
						"err.sound.require.audioformat", c1a);
			}

			try {
				f = (LispAudioFormat)c1a;
				info = new DataLine.Info(
						TargetDataLine.class, f.audioFormat);
				t = (TargetDataLine)AudioSystem.getLine(info);
				return new LispTargetDataLine(t, f.audioFormat);
			} catch (LineUnavailableException e) {
				throw mesg.getError("err.sound.lineunavailable");
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/14
	 */
	public static class ReadDataLineRelative extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			LispTargetDataLine t;
			LispF32Vector[] res;
			AudioFormat af;
			Endianness2 end;
			byte[] buf;
			int size, bts;

			if(!(c1a instanceof LispTargetDataLine)) {
				throw mesg.getError(
						"err.sound.require.dataline.target", c1a);
			} else if(!(c2a instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", c2a);
			} else if((size = c2a.getInt()) < 0) {
				throw mesg.getError(
						"err.require.int.nonnegative", c2a);
			}

			t   = (LispTargetDataLine)c1a;
			af  = t.audioFormat;
			bts = af.getSampleSizeInBits() >> 3;
			buf = new byte[size * af.getChannels() * bts];
			res = new LispF32Vector[af.getChannels()];
			end = af.isBigEndian() ?
					Endianness2.BIG : Endianness2.LITTLE;
			t.targetLine.read(buf, 0, buf.length);
			for(int j = 0; j < af.getChannels(); j++) {
				res[j] = LispF32Vector.malloc(size);
			}

			for(int i = 0; i < size; i++) {
				for(int j = 0; j < af.getChannels(); j++) {
					long ll;
					double f;

					if(af.getEncoding().equals(
							AudioFormat.Encoding.PCM_UNSIGNED)) {
						ll = end.readu(buf,
								(i * af.getChannels() + j) * bts,
								bts);
						f = (double)ll / (double)(1 << (bts * 8));
					} else {
						ll = end.read(buf,
								(i * af.getChannels() + j) * bts,
								bts);
						f = (double)ll / (double)(1 << (bts * 8 - 1));
					}
					res[j].set(i, f);
				}
			}
			return MultiValues.newValues(res);
		}

	}

	//
	private TargetDataLine targetLine;
	private AudioFormat audioFormat;

	//
	private LispTargetDataLine(
			TargetDataLine target, AudioFormat format) {
		this.targetLine  = target;
		this.audioFormat = format;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#open()
	 */
	public void open() throws LineUnavailableException {
		targetLine.open(audioFormat);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#start()
	 */
	public void start() {
		targetLine.start();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#stop()
	 */
	public void stop() {
		targetLine.stop();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#flush()
	 */
	public void flush() {
		targetLine.flush();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#close()
	 */
	public void close() {
		targetLine.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<target-data-line>");
	}

}
