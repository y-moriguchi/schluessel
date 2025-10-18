package net.morilib.lisp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Set;


public class ParserT4 {
	
	private static Parser parser;
	
	public static void main(String[] args) throws IOException {
		BufferedReader rd = new BufferedReader(
				new InputStreamReader(System.in));
		//LispCompiler comp    = CompilerFactory.getInstance();
		//CodeExecutor exec    = CodeExecutorFactory.getInstance();
		//Object memento = exec.newMemento();
		
		parser = new Parser(LispMessage.getInstance());
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
				
				System.out.println(mtc);
				System.out.println(mp);
			} catch(ReadException e) {
				e.printStackTrace();
				//memento = exec.newMemento();
			} catch(LispException e) {
				e.printStackTrace();
				//memento = exec.newMemento();
			} catch (PatternEllipsisException e) {
				e.printStackTrace();
			}
		}
	}
	
}
