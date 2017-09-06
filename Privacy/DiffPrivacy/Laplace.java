import java.util.Random;

public class Laplace {
	
	private boolean testMode = false;

	public final Random random = new Random();
	
	private double derKapital;
	
	public Laplace(double totalEpsilon)
	{
		this.derKapital = totalEpsilon;
	}
			
	private double genLaplace(double coef)
	{
		// as seen on https://en.wikipedia.org/wiki/Laplace_distribution#Generating_random_variables_according_to_the_Laplace_distribution
		// U needs to be uniform on ]−1/2, 1/2[
		double U = random.nextDouble() - 0.5;
		// TODO: check that U != 0.5 otherwise ln(0) will throw error
		return - coef * Math.signum(U) * Math.log(1.0 - 2.0 * Math.abs(U));
	}
	
	public void setTestMode(boolean testMode)
	{
		this.testMode = testMode;
	}
	
	public double genNoise(int sensitivity, double epsilon) throws Exception {
		
		if(!this.testMode)
		{
			// If test mode is off, the epsilon capital decreases
			this.derKapital -= epsilon;
			if(this.derKapital <= 0.0)
				// Throw an exception is we run out of epsilon capital 
				throw new Exception("der kapital ist erschöpft");
		}
		
		return genLaplace(Math.abs(sensitivity)/epsilon);
	}
}
