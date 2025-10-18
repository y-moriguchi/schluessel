package net.morilib.lisp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;


public class ParserT5 {
	
	private static Parser parser;
	
	public static void main(String[] args) throws IOException {
		BufferedReader rd = new BufferedReader(
				new InputStreamReader(System.in));
		Environment  global  = new Environment();
		LispMessage  msg     = LispMessage.getInstance();
		LispCompiler comp    = CompilerFactory.getInstance(msg);
		//CodeExecutor exec    = CodeExecutorFactory.getInstance();
		//Object memento = exec.newMemento();
		
		parser = new Parser(msg);
		while(true) {
			try {
				System.out.println("パターン:");
				System.out.print(" >");
				String read = rd.readLine();
				
				if(read.equals("exit")) {
					return;
				}
				
				parser.clear();
				parser.read(read);
				while(!parser.parse()) {
					System.out.print(">>");
					String r2 = rd.readLine();
					parser.read(r2);
				}
				
				Set<Symbol> prmSet = new HashSet<Symbol>();
				Datum d = parser.getDatum();
				PatternDepthMap mp0 = new PatternDepthMap();
				Datum src = PatternMatch.compilePattern(
						d, mp0, prmSet, new HashSet<Symbol>());
				System.out.println(src);
				
				// 
				System.out.println("マッチング:");
				System.out.print(" >");
				String read2 = rd.readLine();
				
				if(read2.equals("exit")) {
					return;
				}
				
				parser.clear();
				parser.read(read2);
				while(!parser.parse()) {
					System.out.print(">>");
					String r2 = rd.readLine();
					parser.read(r2);
				}
				
				PatternDepthMap mp = new PatternDepthMap(prmSet);
				Datum p = parser.getDatum();
				Set<Symbol> r = new HashSet<Symbol>();
				boolean mtc = PatternMatch.match(src, p, mp, r);
				//System.out.println(mp);
				
				if(!mtc) {
					System.out.println("unmatch");
				}
				//System.out.println(mtc);
				//System.out.println(mp);
				
				//
				System.out.println("テンプレート:");
				System.out.print(" >");
				String read3 = rd.readLine();
				
				if(read3.equals("exit")) {
					return;
				}
				
				parser.clear();
				parser.read(read3);
				while(!parser.parse()) {
					System.out.print(">>");
					String r3 = rd.readLine();
					parser.read(r3);
				}
				
				// ローカル変数置き換え
				Datum t = parser.getDatum();
				Environment menv = new Environment();
				
				t = comp.replaceLocalVals(t, global, menv, true, 0);
				
				// テンプレート展開
				Datum tpl = PatternMatch.compileTemplate(t, mp);
				//System.out.println(tpl);
				
				Datum res = PatternMatch.expand(
						tpl, mp, null,
						new HashMap<Symbol, Symbol>(), global, false);
				
				System.out.println(LispUtils.getResult(res));
			} catch(ReadException e) {
				e.printStackTrace();
				//memento = exec.newMemento();
			} catch(LispException e) {
				e.printStackTrace();
				//memento = exec.newMemento();
			} catch (PatternDepthException e) {
				e.printStackTrace();
			} catch (PatternEllipsisException e) {
				e.printStackTrace();
			}
		}
	}
	
}
