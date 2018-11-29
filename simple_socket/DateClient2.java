import java.net.*;
import java.io.*;

public class DateClient2
{
	public static void main(String[] args)
	{
		try
		{
			for(int i = 1000000; 0 < i;i--)
			{
				Socket sock = new Socket("127.0.0.1", 6013);
				System.out.println("start");
				callServer(sock);
				sock.close();
			}

		}
		catch(Exception e)
		{
			System.err.println(e);
		}
	}

	private static void callServer(Socket sock) throws IOException
	{
		InputStream input = sock.getInputStream();
		BufferedReader bin = new BufferedReader(new InputStreamReader(input));
		String line;
		while((line = bin.readLine()) != null)
			System.out.println(line);

	}
}
