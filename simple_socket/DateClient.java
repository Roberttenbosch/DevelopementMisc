import java.net.*;
import java.io.*;

public class DateClient
{
	public static void main(String[] args)
	{
		try
		{
			Socket sock = new Socket("0.0.0.0", 9034);
			InputStream input = sock.getInputStream();
			BufferedReader bin = new BufferedReader(new InputStreamReader(input));
			String line;
			while((line = bin.readLine()) != null)
			{
				System.out.println(line);
			}
			sock.close();
		}
		catch(Exception e)
		{
			System.err.println(e);
		}
	}
}
