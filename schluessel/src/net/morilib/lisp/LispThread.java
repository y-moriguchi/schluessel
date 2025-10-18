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
package net.morilib.lisp;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.datetime.LispTime;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/07
 */
public class LispThread extends Datum implements Runnable {

	//
	private static interface CallTh {

		//
		public void run(long millis) throws InterruptedException;

		//
		public void run(
				long millis, int nanos) throws InterruptedException;

	}

	//
	private static class Mutex extends Datum {

		//
		private boolean locked = false;
		private boolean wait = false;
		private String name;
		private Datum specific = Nil.NIL;
		private LispThread owner = null;

		//
		private Mutex() { }

		//
		private Mutex(String s) {
			this.name = s;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<mutex>");
		}

	}

	//
	private static class ConditionVariable extends Datum {

		//
		private boolean wait = false;
		private String name;
		private Datum specific = Nil.NIL;

		//
		private ConditionVariable() { }

		//
		private ConditionVariable(String s) {
			this.name = s;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<condition-variable>");
		}

	}
	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class CurrentThread extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			if(!body.equals(Nil.NIL)) {
				throw mesg.getError("err.argument", body);
			}
			return currentThread();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class IsThread extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispThread);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class MakeThread extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			String name = null;
			CompiledCode c;
			Environment e2;

			if(!(body instanceof Cons)) {
				throw mesg.getError("err.argument", body);
			} else if(((Cons)body).getCar() instanceof Closure) {
				Closure cl = (Closure)((Cons)body).getCar();

				c = cl.getCode();
				e2 = cl.getEnvironment();
			} else {
				throw mesg.getError(
						"err.require.closure",
						((Cons)body).getCar());
			}

			Datum d1 = ((Cons)body).getCdr();
			if(d1 instanceof Cons) {
				Cons d1c = (Cons)d1;

				if(d1c.getCar() instanceof Symbol) {
					name = ((Symbol)d1c.getCar()).getName();
					d1 = d1c.getCdr();
				} else {
					throw mesg.getError(
							"err.require.symbol", d1c.getCar());
				}
			}

			if(!d1.equals(Nil.NIL)) {
				throw mesg.getError("err.argument", body);
			}

			return new LispThread(name, mesg, c, e2);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class ThreadName extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispThread) {
				return Symbol.getSymbol(
						((LispThread)c1a).getThreadName());
			}
			throw mesg.getError("err.srfi18.require.thread", c1a);
		}
		
	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class ThreadSpecific extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispThread) {
				synchronized(c1a) {
					return ((LispThread)c1a).specific;
				}
			}
			throw mesg.getError("err.srfi18.require.thread", c1a);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class ThreadSpecificSetS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispThread) {
				synchronized(c1a) {
					((LispThread)c1a).specific = c2a;
					return Undef.UNDEF;
				}
			} else {
				throw mesg.getError("err.srfi18.require.thread", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class ThreadStartS extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispThread) {
				((LispThread)c1a).start();
				return c1a;
			}
			throw mesg.getError("err.srfi18.require.thread", c1a);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class ThreadYieldS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			if(body.equals(Nil.NIL)) {
				Thread.yield();
				return Undef.UNDEF;
			}
			throw mesg.getError("err.argument", body);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ThreadSleepS extends UnaryArgs {

		//
		private static final CallTh TH1 = new CallTh() {

			public void run(long millis) throws InterruptedException {
				Thread.sleep(millis);
			}

			public void run(long millis,
					int nanos) throws InterruptedException {
				Thread.sleep(millis, nanos);
			}

		};

		/* (non-Javadoc)
		 * @see net.morilib.lisp.datetime.TimeUnaryArgs#execute(net.morilib.lisp.datetime.LispTime, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			try {
				callTime(c1a, TH1, mesg);
			} catch(InterruptedException e) {
				throw mesg.getError("err.srfi18.interrupted");
			}
			return null;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ThreadTerminateS extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispThread) {
				synchronized(terminatedThreads) {
					terminatedThreads.add(((LispThread)c1a).thread);
				}
				return Undef.UNDEF;
			}
			throw mesg.getError("err.srfi18.require.thread", c1a);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ThreadJoinS extends Subr {

		//
		private Datum getResult(LispThread d, LispMessage mesg) {
			if(d.uncaughtException != null) {
				if(d.uncaughtException instanceof LispException) {
					LispException le =
						(LispException)d.uncaughtException;

					if(le.getErrorCode().equals(
							"err.srfi18.threadterminated")) {
						throw le;
					} else {
						throw mesg.getUncaughtException(le);
					}
				} else {
					throw new RuntimeException(d.uncaughtException);
				}
			} else {
				return resultField.get(d);
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);

			try {
				if(l.size() < 1 || l.size() > 3) {
					throw mesg.getError("err.argument", body);
				} else if(!(l.get(0) instanceof LispThread)) {
					throw mesg.getError(
							"err.srfi18.require.thread", l.get(0));
				} else if(l.size() == 1) {
					((LispThread)l.get(0)).thread.join();
					return getResult((LispThread)l.get(0), mesg);
				}

				//
				final LispThread d = (LispThread)l.get(0);
				CallTh ct = new CallTh() {

					public void run(long millis)
							throws InterruptedException {
						d.thread.join(millis);
					}

					public void run(long millis,
							int nanos
							) throws InterruptedException {
						d.thread.join(millis, nanos);
					}
					
				};

				callTime(l.get(1), ct, mesg);
				if(d.thread.isAlive()) {
					if(l.size() == 3) {
						return l.get(2);
					} else {
						throw mesg.getError("err.srfi18.jointimeout");
					}
				} else {
					return getResult((LispThread)l.get(0), mesg);
				}
			} catch(InterruptedException e) {
				throw mesg.getError("err.srfi18.interrupted");
			}
		}
		
	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class IsMutex extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof Mutex);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MakeMutex extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);

			if(l.size() == 0) {
				return new Mutex();
			} else if(!(l.get(0) instanceof Symbol)) {
				throw mesg.getError("err.require.symbol", l.get(0));
			} else if(l.size() == 1) {
				return new Mutex(((Symbol)l.get(0)).getName());
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MutexName extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Mutex) {
				return Symbol.getSymbol(((Mutex)c1a).name);
			} else {
				throw mesg.getError("err.srfi18.require.mutex", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MutexSpecific extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Mutex) {
				return ((Mutex)c1a).specific;
			} else {
				throw mesg.getError("err.srfi18.require.mutex", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MutexSpecificSetS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Mutex) {
				((Mutex)c1a).specific = c2a;
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.srfi18.require.mutex", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MutexState extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Mutex) {
				Mutex m = (Mutex)c1a;

				if(m.owner != null && !m.owner.isAlive()) {
					return Symbol.getSymbol("abandoned");
				} else if(m.locked && m.owner != null) {
					return m.owner;
				} else if(m.locked) {
					return Symbol.getSymbol("not-owned");
				} else {
					return Symbol.getSymbol("not-abandoned");
				}
			} else {
				throw mesg.getError("err.srfi18.require.mutex", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MutexLockS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			Datum time = null;
			LispThread lt = currentThread();

			if(l.size() < 1 || l.size() > 3) {
				throw mesg.getError("err.argument", body);
			} else if(!(l.get(0) instanceof Mutex)) {
				throw mesg.getError(
						"err.srfi18.require.mutex", l.get(0));
			} else if(l.size() == 1) {
				// do nothing
			} else if(l.size() == 2) {
				if(l.get(1).isTrue()) {
					time = l.get(1);
				}
			} else if(!l.get(2).isTrue()) {
				time = l.get(1);
			} else if(l.get(2) instanceof LispThread) {
				time = l.get(1);
				lt = (LispThread)l.get(2);
			} else {
				throw mesg.getError(
						"err.srfi18.require.thread", l.get(2));
			}

			boolean res = true;
			Mutex m = (Mutex)l.get(0);
			if(m.locked) {
				m.wait = true;
				synchronized(m) {
					try {
						if(time == null) {
							m.wait();
						} else {
							_wait(m, time, mesg);
						}
						res = !m.wait;
					} catch(InterruptedException e) {
						throw mesg.getError("err.srfi18.interrupted");
					}
				}
				m.wait = false;
			} else if(m.owner != null && !m.owner.isAlive()) {
				throw mesg.getError("err.srfi18.abandonedmutex");
			}
			m.locked = true;
			m.owner  = lt;
			return LispBoolean.getInstance(res);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MutexUnlockS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			ConditionVariable cv = null;

			if(l.size() < 1 || l.size() > 3) {
				throw mesg.getError("err.argument", body);
			} else if(!(l.get(0) instanceof Mutex)) {
				throw mesg.getError(
						"err.srfi18.require.mutex", l.get(0));
			} else if(l.size() == 1) {
				// do nothing
			} else if(!(l.get(1) instanceof ConditionVariable)) {
				throw mesg.getError(
						"err.srfi18.require.condition", l.get(0));
			} else {
				cv = (ConditionVariable)l.get(1);
			}

			//
			Mutex m = (Mutex)l.get(0);
			boolean res = true;

			m.locked = false;
			if(cv != null) {
				try {
					cv.wait = true;
					synchronized(cv) {
						if(l.size() == 2) {
							cv.wait();
						} else {
							_wait(cv, l.get(2), mesg);
						}
						res = !cv.wait;
					}
					cv.wait = false;
				} catch(InterruptedException e) {
					throw mesg.getError("err.srfi18.interrupted");
				}
			}
			synchronized(m) {
				m.wait = false;
				m.notify();
			}
			return LispBoolean.getInstance(res);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class IsConditionVariable extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof ConditionVariable);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class MakeConditionVariable extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);

			if(l.size() == 0) {
				return new ConditionVariable();
			} else if(!(l.get(0) instanceof Symbol)) {
				throw mesg.getError("err.require.symbol", l.get(0));
			} else if(l.size() == 1) {
				return new ConditionVariable(
						((Symbol)l.get(0)).getName());
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ConditionVariableName extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof ConditionVariable) {
				return Symbol.getSymbol(((ConditionVariable)c1a).name);
			} else {
				throw mesg.getError(
						"err.srfi18.require.condition", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ConditionVariableSpecific extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof ConditionVariable) {
				return ((ConditionVariable)c1a).specific;
			} else {
				throw mesg.getError(
						"err.srfi18.require.condition", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ConditionVariableSpecificSetS
	extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof ConditionVariable) {
				((ConditionVariable)c1a).specific = c2a;
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.srfi18.require.condition", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ConditionVariableSignalS extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof ConditionVariable) {
				ConditionVariable cv = (ConditionVariable)c1a;

				synchronized(cv) {
					cv.wait = false;
					cv.notify();
				}
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.srfi18.require.condition", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/10
	 */
	public static class ConditionVariableBroadcastS extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof ConditionVariable) {
				ConditionVariable cv = (ConditionVariable)c1a;

				synchronized(cv) {
					cv.wait = false;
					cv.notifyAll();
				}
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.srfi18.require.condition", c1a);
			}
		}

	}

	/*
	 * current-time, time? -> SRFI-19
	 */

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class SecondsToTime extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispReal) {
				return new LispTime(
						LispTime.TimeType.TIME_UTC,
						(LispReal)c1a);
			}
			throw mesg.getError("err.require.real", c1a);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static class TimeToSeconds extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispTime) {
				return ((LispTime)c1a).getSecond2();
			}
			throw mesg.getError("err.srfi19.require.time", c1a);
		}

	}

	//
	/*package*/ static final
	ClosureClass DEFAULT_CURRENT_EXCEPTION_HANDLER;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public static class CurrentExceptionHandler extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			throw new RuntimeException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#createClosureClass(net.morilib.lisp.Environment)
		 */
		@Override
		/*package*/ ClosureClass createClosureClass(Environment env) {
			CompiledCode.Builder bld = new CompiledCode.Builder();
			ClosureClass cl1 = new ClosureClass();

			bld.addGetCurrentExceptionHandler();
			bld.addReturnOp();
			cl1.setParameterList(Nil.NIL);
			cl1.setCode(bld.getCodeRef());
			return cl1;
		}

	}

	/*
	 * with-exception-handler, raise -> SRFI-34
	 */

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public static class IsJoinTimeoutException
	extends ExceptionObject.IsExceptionType {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.ExceptionObject.IsExceptionType#validate(java.lang.String)
		 */
		@Override
		protected boolean validate(String errorCode) {
			return errorCode.equals("err.srfi18.jointimeout");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public static class IsAbandonedMutexException
	extends ExceptionObject.IsExceptionType {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.ExceptionObject.IsExceptionType#validate(java.lang.String)
		 */
		@Override
		protected boolean validate(String errorCode) {
			return errorCode.equals("err.srfi18.abandonedmutex");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public static class IsTerminatedThreadException
	extends ExceptionObject.IsExceptionType {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.ExceptionObject.IsExceptionType#validate(java.lang.String)
		 */
		@Override
		protected boolean validate(String errorCode) {
			return errorCode.equals("err.srfi18.threadterminated");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public static class IsUncaughtException
	extends ExceptionObject.IsExceptionType {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.ExceptionObject.IsExceptionType#validate(java.lang.String)
		 */
		@Override
		protected boolean validate(String errorCode) {
			return errorCode.equals("err.srfi18.uncaught");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public static class UncaughtExceptionReason extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof ExceptionObject) {
				LispException le =
					((ExceptionObject)c1a).getException();
				Throwable r = le.getCause();

				if(le.getErrorCode().equals("err.srfi18.uncaught")) {
					if(r instanceof SRFI34.RaisedException) {
						return ((SRFI34.RaisedException)r).raised;
					} else {
						return new ExceptionObject((LispException)r);
					}
				}
			}
			throw mesg.getError("err.srfi18.require.uncaught", c1a);
		}

	}

	//
	private static class ThSp {
		private LispThread lispThread;
		//private Datum specific = Undef.UNDEF;
	}

	//
	private static ThreadLocal<ThSp>
	threadLocal = new ThreadLocal<ThSp>() {

		@Override
		protected ThSp initialValue() {
			return new ThSp();
		}

	};
	private static Set<Thread> terminatedThreads =
		new HashSet<Thread>();
	private static Map<LispThread, Datum> resultField =
		new HashMap<LispThread, Datum>();

	//
	private Thread thread;
	private Throwable uncaughtException = null;
	private Datum specific = Undef.UNDEF;

	//
	static {
		threadLocal.get().lispThread =
			new LispThread(Thread.currentThread());

		CompiledCode.Builder bld = new CompiledCode.Builder();
		ClosureClass cl1 = new ClosureClass();
		Cons c1 = new Cons();

		bld.addPush(new SRFI34.Raise());
		bld.addBeginList();
		bld.addReferSymbol(Symbol.getSymbol("obj"));
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addReturnOp();

		c1.setCar(Symbol.getSymbol("obj"));
		cl1.setParameterList(c1);
		cl1.setCode(bld.getCodeRef());
		DEFAULT_CURRENT_EXCEPTION_HANDLER = cl1;
	}

	//
	private LispThread(Thread th) {
		thread = th;
	}

	//
	/*package*/ LispThread(
			String name,
			final LispMessage msg,
			final CompiledCode code,
			final Environment env) {
		Runnable run = new Runnable() {

			public void run() {
				CodeExecutor exe;
				IntStack m;

				threadLocal.get().lispThread = LispThread.this;
				exe = CodeExecutorFactory.getInstance(msg);
				m = exe.newMemento();
				try {
					resultField.put(
							LispThread.this,
							exe.exec(code, new Environment(env), m));
				} catch(LispException e) {
					String tra = m.getStackTrace();

					System.err.println(
							msg.get("err.repl.err") +
							e.getMessage());
					if(tra != null && !tra.equals("")) {
						System.err.println(msg.get("err.stacktrace"));
						System.err.print(tra);
					}
					uncaughtException = e;
				}
			}

		};

		if(code == null) {
			throw new NullPointerException();
		} else if(env == null) {
			throw new NullPointerException();
		} else if(msg == null) {
			throw new NullPointerException();
		} else if(name != null) {
			this.thread = new Thread(run, name);
		} else {
			this.thread = new Thread(run);
		}
	}

	//
	private static void callTime(Datum c1a, CallTh th,
			LispMessage mesg) throws InterruptedException {
		if(c1a instanceof LispReal) {
			double d = c1a.getRealDouble();

			if(d < 0.001) {
				// do nothing
			} else if(d > Long.MAX_VALUE / 1000) {
				th.run(Long.MAX_VALUE);
			} else {
				th.run((long)(d * 1000),
						(int)(d * 1000000000) % 1000000);
			}
		} else if(c1a instanceof LispTime) {
			LispTime d = (LispTime)c1a;

			switch(d.getTimeType()) {
			case TIME_DURATION:
				long sec = d.getSecond();
				int  nanos = d.getNanosecond();

				if(sec + nanos / 1000000 >= 1) {
					th.run(sec + nanos / 1000000, nanos % 1000000);
				}
				break;
			default:
				long td;

				td  = d.toUTCTime().getTimeMillis();
				td -= System.currentTimeMillis();
				if(td >= 1) {
					th.run(td);
				}
			}
		} else if(!c1a.isTrue()) {
			th.run(0);
		} else {
			throw mesg.getError(
					"err.srfi18.require.realortime", c1a);
		}
	}

	//
	private static void _wait(
			final Object m, Datum time,
			LispMessage mesg) throws InterruptedException {
		CallTh ct = new CallTh() {

			public void run(
					long millis) throws InterruptedException {
				m.wait(millis);
			}

			public void run(
					long millis, int nanos
					) throws InterruptedException {
				m.wait(millis, nanos);
			}

		};

		callTime(time, ct, mesg);
	}

	/**
	 * 
	 */
	public void start() {
		thread.start();
	}

	/**
	 * 
	 * @return
	 */
	public static LispThread currentThread() {
		return threadLocal.get().lispThread;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isAlive() {
		return thread.isAlive();
	}

	/**
	 * 
	 * @return
	 */
	public static boolean isTerminated() {
		return terminatedThreads.contains(Thread.currentThread());
	}

	/**
	 * 
	 */
	/*package*/ static void acceptTerminate() {
		synchronized(terminatedThreads) {
			terminatedThreads.remove(Thread.currentThread());
		}
	}

	/**
	 * 
	 * @return
	 */
	public String getThreadName() {
		return thread.getName();
	}

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		thread.run();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<thread>");
	}

}
