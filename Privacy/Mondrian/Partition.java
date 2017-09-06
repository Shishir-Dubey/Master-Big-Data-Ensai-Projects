import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

public class Partition {
    public static Partition of(List<Integer> qid1List, List<Integer> qid2List, List<Tuple> tuples) {
        return new Partition(qid1List, qid2List, tuples);
    }

    private final List<Tuple> tuples;
	private final List<Integer> qid1List;
	private final List<Integer> qid2List;
	
	// Private constructor
	private Partition(List<Integer> qid1List, List<Integer> qid2List, List<Tuple> tuples) {
        this.qid1List = qid1List;
        this.qid2List = qid2List;
        this.tuples = tuples;
    }
	
    public List<Integer> getQid1List() {
        return qid1List;
    }

    public List<Integer> getQid2List() {
        return qid2List;
    }

    public List<Tuple> getTuples() {
    	return tuples;
    }
    
    // Returns the median value of a given list of integer
    private double getMedian(List<Integer> l) {
    	Collections.sort(l);
    	if(l.size() % 2 == 0)
    	{
    		// Even number N of elements in the class. The median is in the middle of l[N/2] and l[N/2-1]
    		return (l.get(l.size()/2) + l.get(l.size()/2-1)) / 2.0;
    	}
    	else
    	{
    		// Odd number N of elements. The median is l[N/2]
    		return l.get(l.size()/2);
    	}
    }
    
    // Creates a partition from a list of tuples
    public static Partition CreateFormTupleList(List<Tuple> tuples)
    {
    	// We are using HashSet to avoid duplicate values
    	HashSet<Integer> qid1 = new HashSet<Integer>();
    	HashSet<Integer> qid2 = new HashSet<Integer>();
    	for (Tuple tuple : tuples)
    	{
    		qid1.add(tuple.getQid1());
    		qid2.add(tuple.getQid2());
    	}
    	return Partition.of(new ArrayList<Integer>(qid1), new ArrayList<Integer>(qid2), tuples);
    }
    
    // Split the partition according to the median of a given qid
    // and returns a list containing the 2 new partitions
    private List<Partition> SplitQid(double median, int qid) {
    	List<Tuple> l1 = new ArrayList<Tuple>();
    	List<Tuple> l2 = new ArrayList<Tuple>();
    	for (Tuple tuple : this.tuples)
    	{
    		if(tuple.getQid(qid) <= median)
    			l1.add(tuple);
    		else
    			l2.add(tuple);
    	}
    	
    	List<Partition> result = new ArrayList<Partition>();
    	result.add(CreateFormTupleList(l1));
    	result.add(CreateFormTupleList(l2));
    	return result;
    }
    
    // Splits the partition in two with a greedy algorithm
    // 1/ take the QID that has the greater number of elements
    // 2/ Take its median to perform the split
    public List<Partition> GreedySplit() {
    	double median;
    	int qidId;
    	if(qid1List.size()>qid2List.size())
    	{
    		median = getMedian(qid1List);
    		qidId = 1;
    	}
    	else
    	{
    		median = getMedian(qid2List);
    		qidId = 2;    		
    	}
    	return SplitQid(median, qidId);
    }
    
    // Returns the size of the largest QID list
    public int GetLargestQidSize() {
       	if(qid1List.size()>qid2List.size())
       		return qid1List.size();
       	return qid2List.size();
    }
    
    // Writes the partition into a file
    public void Write(BufferedWriter bf) throws IOException
    {
    	for (Tuple tuple : this.tuples)
    		tuple.Write(bf);
    }
    
    // Write the partition in an sanitized way
    public void WriteSanitizedPartition(BufferedWriter bf)  throws IOException
    {
    	// Sorts the 2 qid list before writing them.
    	// This is not required, we just do it to make partitions more readable
    	Collections.sort(qid1List);
    	Collections.sort(qid2List);
    	String sanitizedQid1 = Arrays.toString(qid1List.toArray());
    	String sanitizedQid2 = Arrays.toString(qid2List.toArray());
    	for (Tuple tuple : this.tuples)
    		bf.append(String.format("\"%s\", \"%s\", %s\n", sanitizedQid1 , sanitizedQid2, tuple.getSensitiveData()));
    }
}

