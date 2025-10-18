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

import java.io.File;
import java.io.IOException;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.UnsupportedAudioFileException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/10
 */
public class Playable extends Datum2 {

	//
	private static final int WAIT_MILLISEC = 2000;

	//
	private int repeat;
	private PlayThread thread = null;
	private SourceDataLine line;
	private int samplePerTime;
	private AudioFormat format;
	private File file;
	private AudioInputStream stream;

	//
	private class PlayThread extends Thread {

		//
		private boolean playing = true;
		private boolean paused = false;
		private boolean first = true;

		/* (non-Javadoc)
		 * @see java.lang.Thread#run()
		 */
		@Override
		public void run() {
			try {
				while(playing) {
					byte[] wave = new byte[samplePerTime];
					int l = stream.read(wave);
					int s;

					if(paused) {
						synchronized(this) {
							wait();
						}
					} else if(l > 0) {
						line.write(wave, 0, l);
						if(first) {
							first = false;
						} else {
							s = (int)(l * 8 * 1000 /
									format.getChannels() /
									format.getSampleRate() /
									format.getSampleSizeInBits());
							if(s > 0) {
								Thread.sleep(s);
							}
						}
					} else {
						if(--repeat == 0) {
							break;
						}
						stream = AudioSystem.getAudioInputStream(
								file);
						stream = AudioSystem.getAudioInputStream(
								AudioFormat.Encoding.PCM_SIGNED,
								stream);
					}
				}
				line.stop();
				line.close();
			} catch (InterruptedException e) {
				// ignore it
			} catch (IOException e) {
				//e.printStackTrace();
				// ignore it
			} catch (UnsupportedAudioFileException e) {
				//e.printStackTrace();
				// ignore it
			}
		}

	}

	
	public static class MakePlayable extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			Playable res = new Playable();
			String fln = SubrUtils.getString(c1a, mesg);
			File fl;
			AudioInputStream ais;
			AudioFormat fmt;
			DataLine.Info dif;

			if(!c2a.isTrue()) {
				res.repeat = -1;
			} else if(c2a instanceof LispSmallInt) {
				res.repeat = c2a.getInt();
			}

			try {
				fl  = res.file = LispFiles.getFile(env, fln);
				ais = AudioSystem.getAudioInputStream(fl);
				ais = res.stream = AudioSystem.getAudioInputStream(
						AudioFormat.Encoding.PCM_SIGNED, ais);
				fmt = res.format = ais.getFormat();
				dif = new DataLine.Info(SourceDataLine.class, fmt,
						AudioSystem.NOT_SPECIFIED);
				res.line = (SourceDataLine)AudioSystem.getLine(dif);
				res.samplePerTime = (int)(WAIT_MILLISEC *
						fmt.getChannels() * fmt.getSampleRate() *
						fmt.getSampleSizeInBits() / 8 / 1000);
				return res;
			} catch (IOException e) {
				throw mesg.getError("err.io");
			} catch (UnsupportedAudioFileException e) {
				throw mesg.getError("err.sound.file.invalid", c1a);
			} catch (LineUnavailableException e) {
				throw mesg.getError("err.sound.lineunavailable");
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/10
	 */
	public static class PlaySound extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Playable) {
				Playable p = (Playable)c1a;

				if(p.thread == null) {
					try {
						p.thread = p.new PlayThread();
						p.line.open();
						p.line.flush();
						p.line.start();
						p.thread.start();
					} catch (LineUnavailableException e) {
						throw mesg.getError(
								"err.sound.lineunavailable");
					}
					return LispBoolean.TRUE;
				} else if(!p.thread.isAlive()) {
					return LispBoolean.FALSE;
				} else if(p.thread.paused) {
					synchronized(p.thread) {
						p.thread.playing = true;
						p.thread.paused = false;
						p.thread.notifyAll();
					}
					return LispBoolean.TRUE;
				} else if(!p.thread.playing) {
					p.thread.playing = true;
					return LispBoolean.TRUE;
				} else {
					return LispBoolean.FALSE;
				}
			} else {
				throw mesg.getError("err.sound.require.playable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/10
	 */
	public static class StopPlaySound extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Playable) {
				Playable p = (Playable)c1a;

				if(p.thread == null || !p.thread.isAlive()) {
					return LispBoolean.FALSE;
				} else if(p.thread.paused) {
					synchronized(p.thread) {
						p.thread.playing = false;
						p.thread.paused = false;
						p.thread.notifyAll();
					}
					return LispBoolean.TRUE;
				} else if(p.thread.playing) {
					p.thread.playing = false;
					return LispBoolean.TRUE;
				} else {
					return LispBoolean.FALSE;
				}
			} else {
				throw mesg.getError("err.sound.require.playable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/10
	 */
	public static class PausePlaySound extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Playable) {
				Playable p = (Playable)c1a;

				if(p.thread == null || !p.thread.isAlive()) {
					return LispBoolean.FALSE;
				} else if(!p.thread.paused) {
					p.thread.paused = true;
					return LispBoolean.TRUE;
				} else {
					return LispBoolean.FALSE;
				}
			} else {
				throw mesg.getError("err.sound.require.playable", c1a);
			}
		}

	}

	//
	private Playable() {
		// do nothing
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<playable>");
	}

}
