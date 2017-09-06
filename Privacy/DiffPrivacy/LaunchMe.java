import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;


public class LaunchMe {

	static int n, m;
	static double epsilon;
	static Laplace laplace;
	static String question;
	static int[] values;
	
	// Default values if program called without args
	private static void DefaultInitialisation()
	{
		question = "question4";
	}
	
	public static void main(String args[]) throws Exception {
		
		if(args == null || args.length == 0)
			DefaultInitialisation();
		else
			question = args[0];
		
		laplace = new Laplace(1.0);

		GenerateData();
		
		switch(question)
		{
			case "question4":
				Question4();
				break;
			case "question5":
				Question5();
				break;
			case "question6":
				Question6();
				break;
			default:
				break;
		}
			
	}

	// Populates values with n random numbers ranging from 0 to m-1
	private static void GenerateData() {
		
		values = new int[n];
		
		for(int i = 0; i < n ; i++)
		{
			values[i] = laplace.random.nextInt(m);
		}
	}
	
	// Tests the Laplace.genNoise function as requested in question 4
	// results are stored in a file used afterwards in R to compare
	// the density against the Laplace distribution computed in R
	private static void Question4() throws Exception
	{
		laplace.setTestMode(true);

		BufferedWriter bw = new BufferedWriter( new FileWriter(new File ( "./question4.csv" ), false));
		
		for(int i = 0 ; i < 10000 ; i++)
			bw.append(String.format("%f\n", laplace.genNoise(10, 5.0)));
		
		bw.flush();
		bw.close();
	}
	
	
	private static void Question5() throws Exception
	{
		
		laplace.setTestMode(true);

		n = 1000;
		m = 100;
		epsilon = 0.0001;
		
		GenerateData();

		BufferedWriter bw = new BufferedWriter( new FileWriter(new File ( "./question5.csv" ), false));
		
		// Simulation of a COUNT request that returns all values above 10
		int count = 0;
    	for (int value : values)
    	{
    		if(value >= 10)
    			count++;
    	}
		
		// Loop to generate 10, 100, 1000, 10000, 100000, 1000000 perturbations
    	bw.append(String.format("%d;%d\n", 0, count));
		for(int pow = 1 ; pow <= 6 ; pow++)
		{
			double power = Math.pow(10.0, pow);
			double average = 0.0;
			for(int perturb = 0 ; perturb < power ; perturb++)
				// The sensibility of a COUNT request is one
				average += n + laplace.genNoise(1, epsilon);
			average /= power;
			bw.append(String.format("%d;%f\n", pow, average));
		}
			
		bw.flush();
		bw.close();		
	}
	
	private static void Question6() throws Exception
	{
		laplace.setTestMode(true);
		
		n = 1000;
		epsilon = 0.01;
		m = 1000;
		
		// 1. computes the average error for 1000 SUM requests
    	double averageError = 0.0;
    	
    	for(int i = 0 ; i <= 1000 ; i++)
		{
    		// The sensibility of a SUM request is the max spread between two values. Here m-0 = m
    		averageError += laplace.genNoise(m, epsilon);
		}
    	
    	averageError /= 1000;
		
    	// Prints the average error for 1000 SUM requests
		System.out.println(String.format("Average error for 1000 SUM requests: %f", averageError));

    	// Prints the the ratio between the average error and the Laplace scale factor
		System.out.println(String.format("Ratio between the average error and the Laplace scale factor: %f", averageError/(m/epsilon)));
		
		
		BufferedWriter bw = new BufferedWriter( new FileWriter(new File ( "./question6.csv" ), false));
		
		// Loop to generate 10², 10³, 10⁴, 10⁵, 10⁶ perturbations
		for(int pow = 2 ; pow <= 6 ; pow++)
		{
			double power = Math.pow(10.0, pow);
			n = (int)power;
			
			// generates a new set of data
			GenerateData();

			// Simulation of a SUM request that returns the sum of all values
			int sum = 0;
	    	for (int value : values)
	    	{
				sum += value;
	    	}

			bw.append(String.format("%d;%f\n", pow, averageError/sum));
		}
		
		bw.flush();
		bw.close();		    	
	}
}