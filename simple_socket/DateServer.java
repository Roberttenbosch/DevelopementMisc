
import java.net.*;
import java.io.*;


public class DateServer
{
	public static void main(String[] args)
	{
		printThis("Start DateServer");
		try{
			ServerSocket sock = new ServerSocket(6013);
			printThis("listening " + sock.toString());
			while(true){
				Socket client = sock.accept();
				printThis(client.toString());
				PrintWriter pout = new PrintWriter(client.getOutputStream(), true);
				pout.println(new java.util.Date().toString());
				client.close();
			}
		}
		catch(Exception e)
		{
			System.err.println(e);
		}
	}

	private static void printThis(String input)
	{
		System.out.println("[*] "+ input);
	}
}
