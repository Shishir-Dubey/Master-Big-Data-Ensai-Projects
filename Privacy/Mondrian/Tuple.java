import java.io.BufferedWriter;
import java.io.IOException;

public class Tuple {
    public static Tuple of(Integer qid1, Integer qid2, String sd) {
        return new Tuple(qid1, qid2, sd);
    }

    private final Integer qid1;
    private final Integer qid2;
    private final String sd;

    private Tuple(Integer quid1, Integer quid2, String sd) {
        this.qid1 = quid1;
        this.qid2 = quid2;
        this.sd = sd;
    }

    public Integer getQid1() {
        return this.qid1;
    }

    public Integer getQid2() {
        return this.qid2;
    }

    public Integer getQid(int id) {
    	if(id == 1)
    		return this.qid1;
    	return this.qid2;
    }
    
    public String getSensitiveData() {
    	return this.sd;
    }
    
    public void Write(BufferedWriter bf) throws IOException
    {
    	bf.append(String.format("%d, %d, %s\n", getQid1(), getQid2(), getSensitiveData()));
    }
}