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
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.uvector.HomogeneousFloatArray;
import net.morilib.util.Endianness2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/13
 */
public class LispSourceDataLine extends Datum2
implements LispDataLine {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/13
	 */
	public static class MakeSourceDataLine extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			LispAudioFormat f;
			DataLine.Info info;
			SourceDataLine t;

			if(!(c1a instanceof LispAudioFormat)) {
				throw mesg.getError(
						"err.sound.require.audioformat", c1a);
			}

			try {
				f = (LispAudioFormat)c1a;
				info = new DataLine.Info(
						SourceDataLine.class, f.audioFormat);
				t = (SourceDataLine)AudioSystem.getLine(info);
				return new LispSourceDataLine(t, f.audioFormat);
			} catch (LineUnavailableException e) {
				throw mesg.getError("err.sound.lineunavailable");
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/13
	 */
	public static class WriteDataLineRelative extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			HomogeneousFloatArray[] as;
			LispSourceDataLine s;
			Endianness2 end;
			AudioFormat af;
			int size = -1;
			int bts, cnl;
			byte[] buf;

			if(l.size() < 2) {
				throw mesg.getError("err.argument", body);
			} else if(!(l.get(0) instanceof LispSourceDataLine)) {
				throw mesg.getError(
						"err.sound.require.dataline.source", l.get(0));
			}

			s  = (LispSourceDataLine)l.get(0);
			as = new HomogeneousFloatArray[l.size() - 1];
			for(int i = 1; i < l.size(); i++) {
				HomogeneousFloatArray a;

				if(!(l.get(i) instanceof HomogeneousFloatArray)) {
					throw mesg.getError("err.uvector.require.float");
				}
				a = (HomogeneousFloatArray)l.get(i);

				if(size >= 0 && size != a.size()) {
					throw mesg.getError("err.sound.notsamevectors");
				}
				size = a.size();
				as[i - 1] = a;
			}

			af  = s.audioFormat;
			bts = af.getSampleSizeInBits() >> 3;
			cnl = af.getChannels();
			buf = new byte[size * cnl * bts];
			end = af.isBigEndian() ?
					Endianness2.BIG : Endianness2.LITTLE;
			for(int i = 0; i < size; i++) {
				for(int j = 0; j < cnl; j++) {
					long ll;
					double f;

					f = as[j].get(i).getRealDouble();
					if(af.getEncoding().equals(
							AudioFormat.Encoding.PCM_UNSIGNED)) {
//						if(f < 0.0 || f > 1.0) {
//							throw mesg.getError(
//									"err.sound.relative." +
//									"outofrange.unsigned",
//									as[j].get(i));
//						}
						if(f < 0.0) {
							f = 0.0;
						} else if(f > 1.0) {
							f = 1.0;
						}
						ll = (long)(f * ((1 << (bts * 8)) - 1));
					} else {
//						if(f < -1.0 || f > 1.0) {
//							throw mesg.getError(
//									"err.sound.relative." +
//									"outofrange.signed",
//									as[j].get(i));
//						}
						if(f < -1.0) {
							f = -1.0;
						} else if(f > 1.0) {
							f = 1.0;
						}
						ll = (long)(f * ((1 << (bts * 8 - 1)) - 1));
					}
					end.write(buf, (i * bts + j) * cnl, bts, ll);
				}
			}

			s.sourceLine.write(buf, 0, buf.length);
			return LispInteger.valueOf(size);
		}

	}

	//
	private SourceDataLine sourceLine;
	private AudioFormat audioFormat;

	//
	private LispSourceDataLine(
			SourceDataLine source, AudioFormat format) {
		this.sourceLine  = source;
		this.audioFormat = format;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#open()
	 */
	public void open() throws LineUnavailableException {
		sourceLine.open(audioFormat);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#start()
	 */
	public void start() {
		sourceLine.start();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#stop()
	 */
	public void stop() {
		sourceLine.stop();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#flush()
	 */
	public void flush() {
		sourceLine.flush();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sound.LispDataLine#close()
	 */
	public void close() {
		sourceLine.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<source-data-line>");
	}

}
