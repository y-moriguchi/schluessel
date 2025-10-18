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
package net.morilib.lisp.process;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SymbolName;
import net.morilib.util.IOs;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public final class ProcessSubrUtils {

	//
	private static final Writer stdout =
		new OutputStreamWriter(System.out);
	private static final Writer stderr =
		new OutputStreamWriter(System.err);

	//
	private ProcessSubrUtils() {}

	/**
	 * 
	 * @param r
	 * @param ins
	 * @param ous
	 * @param err
	 * @param e2o
	 * @return
	 * @throws IOException
	 */
	public static int execProcessWithoutThread(List<String[]> r,
			Map<String, String> env, String dir,
			IntegerCollection e2o,
			Reader ins, Writer ous, Writer err) throws IOException {
		ProcessBuilder[] pb = new ProcessBuilder[r.size()];
		Process[] pip = new Process[r.size()];
		Writer[] ppo = new Writer[r.size() + 1];
		Reader[] ppi = new Reader[r.size()];
		Reader[] ppe = new Reader[r.size()];
		char[] b = new char[1024];
		int l = 0, res = 72;

		for(int i = 0; i < r.size(); i++) {
			pb[i] = new ProcessBuilder(r.get(i));
			pb[i] = pb[i].redirectErrorStream(e2o.containsInt(i));
			pb[i].environment().putAll(env);
			pb[i].directory(new File(dir));
			pip[i] = pb[i].start();
			ppo[i] = new BufferedWriter(new OutputStreamWriter(
					pip[i].getOutputStream()));
			ppi[i] = new BufferedReader(new InputStreamReader(
					pip[i].getInputStream()));
			ppe[i] = new BufferedReader(new InputStreamReader(
					pip[i].getErrorStream()));
		}
		ppo[r.size()] = ((ous != null) ? ous : stdout);

		if(ins != null) {
			while((l = ins.read(b)) >= 0) {
				ppo[0].write(b, 0, l);
				ppo[0].flush();
			}
			IOs.close(ppo[0]);
		}

		for(int i = 0; i < r.size(); i++) {
			while(true) {
				if((l = ppi[i].read(b)) >= 0) {
					ppo[i + 1].write(b, 0, l);
					ppo[i + 1].flush();
				} else if(l < 0) {
					if(i < r.size() - 1) {
						IOs.close(ppo[i + 1]);
					}
					ppo[i + 1] = null;
				}

				if((l = ppe[i].read(b)) >= 0) {
					((err != null) ? err : stderr).write(b, 0, l);
					((err != null) ? err : stderr).flush();
				}

				try {
					res = pip[i].exitValue();
					if(ppo[i + 1] == null) {
						break;
					}
				} catch(IllegalThreadStateException e) {}

//				try {
//					Thread.sleep(91);
//				} catch(InterruptedException e) {}
			}
		}
		return res;
	}

	/**
	 * 
	 * @param r
	 * @param e2o
	 * @param ins
	 * @param ous
	 * @param err
	 * @return
	 * @throws IOException
	 */
	public static int execProcess(final List<String[]> r,
			Map<String, String> env, String dir,
			IntegerCollection e2o, final Reader ins, Writer ous,
			final Writer err) throws IOException {
		ProcessBuilder[] pb = new ProcessBuilder[r.size()];
		final Process[] pip = new Process[r.size()];
		final Writer[] ppo = new Writer[r.size() + 1];
		final Reader[] ppi = new Reader[r.size()];
		final Reader[] ppe = new Reader[r.size()];
		final IOException[] lock = new IOException[1];

		for(int i = 0; i < r.size(); i++) {
			pb[i] = new ProcessBuilder(r.get(i));
			pb[i] = pb[i].redirectErrorStream(e2o.containsInt(i));
			pb[i].environment().putAll(env);
			pb[i].directory(new File(dir));
			pip[i] = pb[i].start();
			ppo[i] = new BufferedWriter(new OutputStreamWriter(
					pip[i].getOutputStream()));
			ppi[i] = new BufferedReader(new InputStreamReader(
					pip[i].getInputStream()));
			ppe[i] = new BufferedReader(new InputStreamReader(
					pip[i].getErrorStream()));
		}
		ppo[r.size()] = ((ous != null) ? ous : stdout);

		new Thread(new Runnable() {

			public void run() {
				char[] b = new char[1024];
				int l = 0;

				try {
					if(ins != null) {
						while((l = ins.read(b)) >= 0) {
							ppo[0].write(b, 0, l);
							ppo[0].flush();
						}
						IOs.close(ppo[0]);
					}
				} catch(IOException e) {
					synchronized(lock) {
						lock[0] = e;
						lock.notifyAll();
					}
				}
			}

		}).start();

		for(int ii = 0; ii < r.size(); ii++) {
			final int i = ii;
			Runnable run;

			run = new Runnable() {

				public void run() {
					char[] b = new char[1024];
					int l = 0;
					Writer err0 = (err != null) ? err : stderr;

					try {
						while(true) {
							if((l = ppi[i].read(b)) >= 0) {
								ppo[i + 1].write(b, 0, l);
								ppo[i + 1].flush();
							} else if(l < 0) {
								if(i < r.size() - 1) {
									IOs.close(ppo[i + 1]);
								}
								ppo[i + 1] = null;
							}
	
							if((l = ppe[i].read(b)) >= 0) {
								err0.write(b, 0, l);
								err0.flush();
							}
	
							try {
								pip[i].exitValue();
								if(ppo[i + 1] == null) {
									if(i == r.size() - 1) {
										synchronized(lock) {
											lock[0] = null;
											lock.notifyAll();
										}
									}
									break;
								}
							} catch(IllegalThreadStateException e) {}
	
	//						try {
	//							Thread.sleep(91);
	//						} catch(InterruptedException e) {}
						}
					} catch(IOException e) {
						synchronized(lock) {
							lock[0] = e;
							lock.notifyAll();
						}
					}
				}

			};
			new Thread(run).start();
		}

		try {
			synchronized(lock) {
				lock.wait();
				if(lock[0] != null) {
					throw lock[0];
				}
			}
			return pip[r.size() - 1].exitValue();
		} catch(InterruptedException e) {
			return Integer.MIN_VALUE;
		}
	}

	/**
	 * 
	 * @param itr
	 * @return
	 * @throws IOException
	 */
	public static int runProcess(Iterator<Datum> itr,
			Map<Symbol, Datum> env, String dir,
			Reader console) throws IOException {
		Map<String, String> e2 = new HashMap<String, String>();
		List<String[]> t = new ArrayList<String[]>();
		List<String> r = new ArrayList<String>();
		IntegerVector e2o = new IntegerArrayVector();
		Writer ous = null;
		Writer err = null;
		Reader ins = console;
		String s;
		int state = 0;
		Datum d;

		while(itr.hasNext()) {
			d = itr.next();
			if(state == 1) {
				ins = new BufferedReader(new InputStreamReader(
						new FileInputStream(LispUtils.print(d))));
				state = 100;
			} else if(state == 2) {
				ous = new BufferedWriter(new OutputStreamWriter(
						new FileOutputStream(LispUtils.print(d))));
				state = 100;
			} else if(state == 3) {
				err = new BufferedWriter(new OutputStreamWriter(
						new FileOutputStream(LispUtils.print(d))));
				state = 100;
			} else if(state == 4) {
				ous = new BufferedWriter(new OutputStreamWriter(
						new FileOutputStream(LispUtils.print(d),
								true)));
				state = 100;
			} else if(state == 5) {
				err = new BufferedWriter(new OutputStreamWriter(
						new FileOutputStream(LispUtils.print(d),
								true)));
				state = 100;
			} else if(!(d instanceof SymbolName)) {
				if(state == 0) {
					r.add(LispUtils.print(d));
				} else {
					throw new ProcessSyntaxException();
				}
			} else if((s = ((SymbolName)d).getName()).equals("<")) {
				state = 1;
			} else if(s.equals(">")) {
				state = 2;
			} else if(s.equals("2>")) {
				state = 3;
			} else if(s.equals(">>")) {
				state = 4;
			} else if(s.equals("2>>")) {
				state = 5;
			} else if(s.equals("2>&1")) {
				e2o.add(t.size());
			} else if(state == 0 && s.equals("|")) {
				t.add(r.toArray(new String[0]));
				r.clear();
			} else if(state == 0) {
				r.add(s);
			} else {
				throw new ProcessSyntaxException();
			}
		}

		if(env != null) {
			for(Map.Entry<Symbol, Datum> e : env.entrySet()) {
				e2.put(e.getKey().getName(),
						LispUtils.print(e.getValue()));
			}
		}

		try {
			t.add(r.toArray(new String[0]));
			return execProcess(t, e2, dir, e2o, ins, ous, err);
		} finally {
			IOs.close(ins);
			IOs.close(ous);
			IOs.close(err);
		}
	}

}
