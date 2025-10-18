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
package net.morilib.lisp;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.PushbackReader;
import java.io.Reader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import net.morilib.lisp.exlib.CommandLine;
import net.morilib.lisp.r6rs.LibraryID;
import net.morilib.lisp.util.LogEnv;
import net.morilib.parser.ParserUtils;
import net.morilib.util.Maps;
import net.morilib.util.SimpleMap;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Scheme {

	/**
	 * 
	 */
	public static final int SCHEME_VERSION = 5;

	/**
	 * 
	 */
	public static final String SCHLUESSEL_VERSION = "0.4.4";

	//
	private static Logger _log;

	//
	static {
		//
		_log = LogEnv.init("schlush.main");

		//
		Environment env = Scheme.newRnRSEnv(5);
		List<Symbol> sym;
		InputStream ins = LibraryID.class.getResourceAsStream(
				"/net/morilib/lisp/init/R5RSenvs.txt");
		SimpleMap<String, Symbol> map;

		map = new SimpleMap<String, Symbol>() {

			public Symbol map(String key) {
				return Symbol.getSymbol(key);
			}

		};
		sym = ParserUtils.parseToList(ins, "#", map);
		LibraryID.putNamespace(
				LibraryID.R5RS, env,
				Maps.identityMapOf(new HashSet<Symbol>(sym)));
	}

	//
	private static PrintWriter transcript = null;

	//
	private Environment  global;
	private LispCompiler comp;
	private CodeExecutor exec;
	private IntStack     memento;
	private Parser       parser;
	private LispMessage  message;
	private String       stackTrace;

	//
	private Scheme(LispMessage msg) {
		if(msg == null) {
			throw new NullPointerException();
		}

		global  = new Environment();
		message = msg;
		comp    = CompilerFactory.getInstance(message);
		exec    = CodeExecutorFactory.getInstance(message);
		memento = exec.newMemento();
		parser  = new Parser(global, message);
	}

	/**
	 * 
	 * @param env
	 * @param msg
	 */
	public Scheme(Environment env, LispMessage msg) {
		if(env == null) {
			throw new NullPointerException();
		} else if(msg == null) {
			throw new NullPointerException();
		}

		global  = env;
		message = msg;
		comp    = CompilerFactory.getInstance(message);
		exec    = CodeExecutorFactory.getInstance(message);
		memento = exec.newMemento();
		parser  = new Parser(global, message);
	}

	/**
	 * 
	 * @param lc
	 * @return
	 */
	public static Scheme newInstance(Locale lc) {
		Scheme res = new Scheme(LispMessage.getInstance(lc));

		InitSubrLoader.load(res.global);
		InitVarLoader.load(res.global);
		res.parser.clear();
		InitLispLoader.load(res);
		res.parser.clear();
		return res;
	}

	/**
	 * 
	 * @return
	 */
	public static Scheme newInstance() {
		return newInstance(Locale.getDefault());
	}

	/**
	 * 
	 * @param lc
	 * @return
	 * @throws InitLoadException 
	 */
	public static Scheme newInstance(
			String[] pkg, Locale lc) throws InitLoadException {
		Scheme res = new Scheme(LispMessage.getInstance(lc));

		for(String s : pkg)  InitSubrLoader.load(s, res.global);
		InitVarLoader.load(res.global);
		res.parser.clear();
		InitLispLoader.load(res);
		res.parser.clear();
		return res;
	}

	/**
	 * 
	 * @return
	 * @throws InitLoadException 
	 */
	public static Scheme newInstance(
			String[] pkg) throws InitLoadException {
		return newInstance(pkg, Locale.getDefault());
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public static Environment newNullEnv(int ver) {
//		Scheme res = new Scheme(LispMessage.getInstance());
//
//		InitSubrLoader.loadNullEnv(res.global, ver);
//		InitVarLoader.loadNullEnv(res.global, ver);
//		res.parser.clear();
//		InitLispLoader.loadNullEnv(res, ver);
//		res.parser.clear();
//		return res.global;
		return newNull(ver).global;
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public static Environment newRnRSEnv(int ver) {
//		Scheme res = new Scheme(LispMessage.getInstance());
//
//		InitSubrLoader.loadRnRSEnv(res.global, ver);
//		InitVarLoader.loadRnRSEnv(res.global, ver);
//		res.parser.clear();
//		InitLispLoader.loadRnRSEnv(res, ver);
//		res.parser.clear();
//		return res.global;
		return newRnRS(ver).global;
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public static Scheme newEmpty() {
		return new Scheme(LispMessage.getInstance());
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public static Scheme newNull(int ver) {
		return newNull(ver, null);
	}

	/**
	 * 
	 * @param ver
	 * @param lc
	 * @return
	 */
	public static Scheme newNull(int ver, Locale lc) {
		Scheme res = new Scheme(LispMessage.getInstance(lc));

		InitSubrLoader.loadNullEnv(res.global, ver);
		InitVarLoader.loadNullEnv(res.global, ver);
		res.parser.clear();
		InitLispLoader.loadNullEnv(res, ver);
		res.parser.clear();
		return res;
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public static Scheme newRnRS(int ver) {
		return newRnRS(ver, null);
	}

	/**
	 * 
	 * @param ver
	 * @param lc
	 * @return
	 */
	public static Scheme newRnRS(int ver, Locale lc) {
		Scheme res = new Scheme(LispMessage.getInstance(lc));

		InitSubrLoader.loadRnRSEnv(res.global, ver);
		InitVarLoader.loadRnRSEnv(res.global, ver);
		res.parser.clear();
		InitLispLoader.loadRnRSEnv(res, ver);
		res.parser.clear();
		return res;
	}

	/**
	 * 
	 * @param pw
	 */
	public static void setTranscript(PrintWriter pw) {
		synchronized(Scheme.class) {
			if(transcript != null) {
				transcript.close();
			}
			transcript = pw;
		}
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public void loadNull(int ver) {
		InitSubrLoader.loadNullEnv(global, ver);
		InitVarLoader.loadNullEnv(global, ver);
		parser.clear();
		InitLispLoader.loadNullEnv(this, ver);
		parser.clear();
	}

	/**
	 * 
	 * @param ver
	 * @return
	 */
	public void loadRnRS(int ver) {
		InitSubrLoader.loadRnRSEnv(global, ver);
		InitVarLoader.loadRnRSEnv(global, ver);
		parser.clear();
		InitLispLoader.loadRnRSEnv(this, ver);
		parser.clear();
	}

	/**
	 * 
	 * @param sexp
	 * @return
	 */
	public Datum input(Datum sexp) {
		try {
			Datum d = sexp;
			CompiledCode.Builder b = new CompiledCode.Builder();
			long t;

			// macro extension
			t = System.currentTimeMillis();
			d = comp.expandMacro(d, global, exec, memento);
			//System.out.println(LispUtils.getResult(d));
			IntLispUtils.timelog(_log, "Expand Macro : ", t);

			// compile
			t = System.currentTimeMillis();
			comp.compile(
					d, global, b, true, new Cons(), true,
					new LinkedList<Cons>(), exec, memento,
					new LispCompiler.MiscInfo(null));
			b.addReturnOp();
			IntLispUtils.timelog(_log, "Compile : ", t);

			// execute
			//System.out.println(b.getCodeRef().toString());
			t = System.currentTimeMillis();
			Datum res = exec.exec(b.getCodeRef(), global, memento);
			IntLispUtils.timelog(_log, "Execute : ", t);

			// display the stack
			_log.fine(memento.toString());

			//parser.clear();
			return res;
		} finally {
			parser.clear();
			stackTrace = memento.getStackTrace();
			memento    = exec.newMemento();
		}
	}

	/**
	 * 
	 * @param rd
	 * @return
	 */
	public Datum input(String rd) {
		try {
			parser.read(rd);
			if(!parser.parse()) {
				return null;
			}

			Datum d = parser.getDatum();
			if(d != null) {
				return input(d);
			} else {
				parser.clear();
				return null;
			}
		} catch(ReadException e) {
			parser.clear();
			throw e;
		} catch(IOException e) {
			throw new RuntimeException();
		}
	}

	/**
	 * 
	 * @param rd
	 * @return
	 */
	public Datum exec(String rd) {
		Datum res = input(rd);

		if(res == null) {
			throw new IllegalArgumentException(
					"S-exp is not completed");
		}
		return res;
	}

	/**
	 * 
	 * @param rd1
	 * @throws IOException
	 */
	public void readFile(Reader rd1) throws IOException {
		PushbackReader rd2 = new PushbackReader(rd1);
		BufferedReader rd;
		int lineno = 0;

		IntLispUtils.skipShebang(rd2);
		rd = new BufferedReader(rd2);
		while(true) {
			try {
				String read = rd.readLine();
				lineno++;

				if(read == null) {
					break;
				}
				input(read);
			} catch(ReadException e) {
				String ln = message.get("err.lineno");
				throw new ReadFileException(
						lineno + ln + ":" + e.getMessage(), e);
			} catch(LispException e) {
				String ln = message.get("err.lineno");
				throw new ReadFileException(
						lineno + ln + ":" + e.getMessage(), e);
			} catch(JavaException e) {
				String ln = message.get("err.lineno");
				throw new ReadFileException(
						lineno + ln + ":" + e.getMessage(), e);
			}
		}
		parser.clear();
	}

	/**
	 * 
	 * @param rd1
	 * @throws IOException
	 */
	public void readEvalPrintLoop(Reader rd1) throws IOException {
		BufferedReader rd = new BufferedReader(rd1);
		//int lineno = 0;

		reploop:
		while(true) {
			String prompt = " >";
			Datum res = null;
			String rep;

			try {
				while(res == null) {
					System.out.print(prompt);
					String r2 = rd.readLine();

					if(r2 == null) {
						break reploop;
					} else if(transcript != null) {
						synchronized(Scheme.class) {
							if(transcript != null) {
								transcript.print(prompt);
								transcript.println(r2);
							}
						}
					}
					res = input(r2);
					prompt = ">>";
				}

				// print the result
				rep = LispUtils.getResult(res);
				System.out.println(rep);
				if(transcript != null) {
					synchronized(Scheme.class) {
						if(transcript != null) {
							transcript.println(rep);
						}
					}
				}

				// print the stack
				//System.out.println(memento);
			} catch(ReadException e) {
				System.err.println(
						message.get("err.repl.read") +
						e.getMessage());
				System.err.println(message.get("info.condition") +
						e.getCondition().describeShort());
				//e.printStackTrace();
				//parser.clear();
				//memento = exec.newMemento();
			} catch(LispException e) {
				String tra = stackTrace;

				System.err.println(
						message.get("err.repl.err") +
						e.getMessage());
				if(tra != null && !tra.equals("")) {
					System.err.println(message.get("err.stacktrace"));
					System.err.print(tra);
				}
				if(e.getException() != null) {
					e.getException().printStackTrace();
				}
				System.err.println(message.get("info.condition") +
						e.getCondition().describeShort());
				_log.fine("Stack trace\n" + tra);
				//e.printStackTrace();
				//parser.clear();
				//memento = exec.newMemento();
			} catch(JavaException e) {
				System.err.println(
						message.get("err.repl.err") +
						e.getMessage());
				//e.printStackTrace();
				//parser.clear();
				//memento = exec.newMemento();
			}
		}
	}

	/**
	 * 
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		Scheme eval;
		List<Datum> d1 = new ArrayList<Datum>();
		int opt;

		d1.add(new LispString("schluessel"));
		for(String s : args) {
			d1.add(new LispString(s));
		}
		CommandLine.commandLine = LispUtils.listToCons(d1);

//		eval = Scheme.newInstance();
		eval = SchemeOptions.preparseOption(args);
		opt  = SchemeOptions.parseOption(args, eval);
		if(args.length == opt) {
			eval.readEvalPrintLoop(new InputStreamReader(System.in));
			System.exit(0);
		} else {
			InputStream ins;

			if(args[opt].equals("-")) {
				ins = System.in;
			} else {
				ins = new FileInputStream(args[opt]);
			}

			try {
				eval.readFile(new InputStreamReader(ins));
				System.exit(0);
			} catch(ReadFileException e) {
				System.err.println(e.getMessage());
				System.exit(2);
			}
		}
	}

	/**
	 * 
	 * @param var
	 * @param o
	 */
	public void set(String var, Object o) {
		global.bindDatum(Symbol.getSymbol(var), LispUtils.toDatum(o));
	}

	/**
	 * 
	 * @param var
	 * @param cdr
	 * @param lst
	 */
	public void setDotList(String var, Object cdr, Object... lst) {
		ConsListBuilder b = new ConsListBuilder();

		for(Object o : lst) {
			b.append(LispUtils.toDatum(o));
		}
		global.bindDatum(
				Symbol.getSymbol(var), b.get(LispUtils.toDatum(cdr)));
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 */
	public void setList(String var, Object... lst) {
		setDotList(var, Nil.NIL, lst);
	}

	/**
	 * 
	 * @param var
	 * @param o
	 */
	public void setJavaInstance(String var, Object o) {
		global.bindDatum(Symbol.getSymbol(var), new JavaInstance(o));
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public Datum get(String var) {
		return global.findDatum(Symbol.getSymbol(var));
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public String getString(String var) {
		Datum d = get(var);

		if(d instanceof LispString) {
			return ((LispString)d).getString();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public BigInteger getExactInteger(String var) {
		Datum d = get(var);

		if(d instanceof LispInteger) {
			return d.getBigInteger();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public int getExactInt(String var) {
		return getExactInteger(var).intValue();
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public long getExactLong(String var) {
		return getExactInteger(var).longValue();
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public LispNumber getNumber(String var) {
		Datum d = get(var);

		if(d instanceof LispNumber) {
			return (LispNumber)d;
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public LispReal getReal(String var) {
		LispNumber n = getNumber(var);

		if(n.isReal()) {
			return n.getReal();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public double getDouble(String var) {
		return getReal(var).getRealDouble();
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public int getInt(String var) {
		return getReal(var).getBigInteger().intValue();
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public long getLong(String var) {
		return getReal(var).getBigInteger().longValue();
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public Object getJavaInstance(String var) {
		Datum d = get(var);

		if(d instanceof JavaInstance) {
			return ((JavaInstance)d).getJavaInstance();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @param lst
	 * @return
	 */
	public static Datum call(
			Procedure proc, Environment env, LispMessage mesg,
			Datum lst) {
		if(proc instanceof Subr) {
			if(((Subr)proc).createClosureClass(env) == null) {
				return ((Subr)proc).eval(lst, env, mesg);
			}
		} else if(proc instanceof ISubr) {
			return ((ISubr)proc).eval(lst, env, mesg);
		}

		//
		CodeExecutor exec = CodeExecutorFactory.getInstance(mesg);
		CompiledCode.Builder cd = new CompiledCode.Builder();

		cd.addPush((Datum)proc);
		cd.addPush(lst);
		cd.addCall();
		cd.addReturnOp();
		return exec.exec(cd.getCodeRef(), env, exec.newMemento());
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @param lst
	 * @return
	 */
	public static Datum callva(
			Procedure proc, Environment env, LispMessage mesg,
			Datum... lst) {
		return call(proc, env, mesg, LispUtils.toConsList(lst));
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @param lst
	 * @return
	 */
	public static Datum call(
			Datum proc, Environment env, LispMessage mesg,
			Datum lst) {
		if(proc instanceof Procedure) {
			return call((Procedure)proc, env, mesg, lst);
		} else {
			throw mesg.getError("err.require.procedure", proc);
		}
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @param lst
	 * @return
	 */
	public static Datum callva(
			Datum proc, Environment env, LispMessage mesg,
			Datum... lst) {
		return call(proc, env, mesg, LispUtils.toConsList(lst));
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public Datum call(String var, Object... lst) {
		CompiledCode.Builder cd = new CompiledCode.Builder();
		Datum fn = get(var);

		cd.addPush(fn);
		cd.addPush(LispUtils.toConsList(lst));
		cd.addCall();
		cd.addReturnOp();
		return exec.exec(cd.getCodeRef(), global, memento);
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public String callString(String var, Object... lst) {
		Datum d = call(var, lst);

		if(d instanceof LispString) {
			return ((LispString)d).getString();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public BigInteger callExactInteger(String var, Object... lst) {
		Datum d = call(var, lst);

		if(d instanceof LispInteger) {
			return d.getBigInteger();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public int callExactInt(String var, Object... lst) {
		return callExactInteger(var, lst).intValue();
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public long callExactLong(String var, Object... lst) {
		return callExactInteger(var, lst).longValue();
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public LispNumber callNumber(String var, Object... lst) {
		Datum d = call(var, lst);

		if(d instanceof LispNumber) {
			return (LispNumber)d;
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public LispReal callReal(String var, Object... lst) {
		LispNumber n = callNumber(var, lst);

		if(n.isReal()) {
			return n.getReal();
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public double callDouble(String var, Object... lst) {
		return callReal(var, lst).getRealDouble();
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public int callInt(String var, Object... lst) {
		return callReal(var, lst).getBigInteger().intValue();
	}

	/**
	 * 
	 * @param var
	 * @param lst
	 * @return
	 */
	public long callLong(String var, Object... lst) {
		return callReal(var, lst).getBigInteger().longValue();
	}

	/**
	 * 
	 * @param rd
	 * @return
	 */
	public InputPort createInputPort(Reader rd) {
		return new InputPort(rd, message);
	}

	/**
	 * 
	 * @param wr
	 * @return
	 */
	public OutputPort createOutputPort(OutputStream wr) {
		return new OutputPort(wr, message);
	}

	/**
	 * 
	 * @param cl
	 * @param data
	 * @return
	 */
	public Datum call(Datum cl, Datum... data) {
		CompiledCode.Builder cd = new CompiledCode.Builder();

		if(!(cl instanceof Callable)) {
			throw new ClassCastException();
		}
		cd.addPush(cl);
		cd.addPush(LispUtils.toConsList(data));
		cd.addCall();
		cd.addReturnOp();
		return exec.exec(cd.getCodeRef(), global, memento);
	}

	/**
	 * @return the message
	 */
	public LispMessage getMessage() {
		return message;
	}

}
