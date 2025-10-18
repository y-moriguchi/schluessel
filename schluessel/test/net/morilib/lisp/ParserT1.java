package net.morilib.lisp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

public class ParserT1 {
	
	private static Parser parser;
	
	public static void print1(Datum d) {
		if(d instanceof Cons) {
			System.out.print("( ");
			print1(((Cons)d).getCar());
			System.out.print(" . ");
			print1(((Cons)d).getCdr());
			System.out.print(" )");
		} else {
			System.out.print(d);
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader rd = new BufferedReader(
				new InputStreamReader(System.in));
		
		parser = new Parser(LispMessage.getInstance());
		while(true) {
			System.out.print(" >");
			String read = rd.readLine();
			
			if(read.equals("exit")) {
				return;
			}
			try {
				parser.clear();
				parser.read(read);
				while(!parser.parse()) {
					System.out.print(">>");
					String r2 = rd.readLine();
					parser.read(r2);
				}
				
				List<Datum> l = parser.getData();
				System.out.println("Results: " + l.size());
				for(int i = 0; i < l.size(); i++) {
					print1(l.get(i));
					System.out.println();
				}
			} catch(ReadException e) {
				e.printStackTrace();
			}
		}
	}
	
}
