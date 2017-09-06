import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;


public class LaunchMe {
	
	// Write partition into a csv file named fileName
	private static void WritePartition(Partition partition, String fileName) throws IOException{
		BufferedWriter bw = new BufferedWriter( new FileWriter(new File ( "./" + fileName + ".csv" ), false));
		partition.Write(bw);
		bw.flush();
		bw.close();		
	}

	// Write a sanitized partition into a csv file named fileName
	private static void WriteSanitizedPartitions(List<Partition> partitions) throws IOException{
		BufferedWriter bw = new BufferedWriter(new FileWriter(new File(String.format("./sanitized data with %d-anonymity.csv", privacyParam))));
		for (Partition partition : partitions)
			partition.WriteSanitizedPartition(bw);
		bw.flush();
		bw.close();		
	}

	
	// Recursive function Mondrian
	private static List<Partition> Mondrian(Partition partition)
	{
		List<Partition> result = new ArrayList<Partition>();
		int largestGidSize = partition.GetLargestQidSize();
		if(largestGidSize < 2*privacyParam)
		{
			// Stopping clause: if a partition is small enough, don't split it
			result.add(partition);
		}
		else
		{
			// 1 - Splits the partition in two
			List<Partition> splits = partition.GreedySplit();
			
			// 2 - Recursive calls to Mondrian on two sub partitions
			List<Partition> r1 = Mondrian(splits.get(0));
			List<Partition> r2 = Mondrian(splits.get(1));
			
			// 3 - Adds the resulting calls to Mondrian in the result list
			result.addAll(r1);
			result.addAll(r2);
		}
		return result;
	}
	
	static int datasetSize, minQuid1, maxQuid1, minQuid2, maxQuid2, privacyParam;
	
	static Random random = new Random();
	
	// Default values if program called without args
	private static void DefaultInitialisation()
	{
		datasetSize = 100;
		minQuid1 = 1;
		maxQuid1 = 99;
		minQuid2 = 1920;
		maxQuid2 = 2016;
		privacyParam = 4;
	}
	
	public static void main(String args[]) {
		
		if (args != null && args.length > 0) {
		    try {
		    	datasetSize = Integer.parseInt(args[0]);
		    	minQuid1 = Integer.parseInt(args[1]);
		    	maxQuid1 = Integer.parseInt(args[2]);
		    	minQuid2 = Integer.parseInt(args[3]);
		    	maxQuid2 = Integer.parseInt(args[4]);
		    	privacyParam = Integer.parseInt(args[5]);
		        
		    } catch (NumberFormatException e) {
		    	System.out.println("One parameter is invalid");
		    	System.exit(1);
		    }
		}
		else
		{
			// Default values if program called without args
			DefaultInitialisation();
		}
		
		// Sensitive data
		String[] sensitiveData = { "Cholera", "Enteric duplication cyst", "Giardiasis", "Pancreatitis", "Peptic ulcer disease", "Yellow fever", "Hemorrhoids", "Anal fissures", "Anal fistula" };
		
		List<Tuple> tuples = new ArrayList<Tuple>();
				
		for(int index = 0 ; index < datasetSize ; index++)
		{
			// Creates random tuples
			tuples.add(Tuple.of(random.nextInt(maxQuid1 - minQuid1 + 1) + minQuid1, random.nextInt(maxQuid2 - minQuid2 + 1) + minQuid2, sensitiveData[random.nextInt(sensitiveData.length)]));
		}
		
		try {
			
			Partition partition = Partition.CreateFormTupleList(tuples);
			
			// Writes the initial data in Initial_data.csv
			WritePartition(partition, "Initial_data");
			
			// Starts Mondrian
			List<Partition> results = Mondrian(partition);
			
			// Writes the sanitized data
			WriteSanitizedPartitions(results);
			
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}		
	}
}
