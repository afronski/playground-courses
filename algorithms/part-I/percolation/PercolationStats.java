public class PercolationStats {
   private double[] stats;
   private int numberOfSimulations;
    
   // Perform T independent computational experiments on an N-by-N grid.
   public PercolationStats(int N, int T) {
       if (N <= 0 || T <= 0) {
           throw new java.lang.IllegalArgumentException("N or T have invalid value");
       }
       
       numberOfSimulations = T;
       stats = new double[T];
       
       long currentTime = System.currentTimeMillis();
       java.util.Random r = new java.util.Random(currentTime);
       
       for (int s = 0; s < T; ++s) {
           Percolation simulation = new Percolation(N);
           double threshold = 0.0;
           
           do {
               int i = r.nextInt(N) + 1;
               int j = r.nextInt(N) + 1;

               if (!simulation.isOpen(i, j)) {
                   simulation.open(i, j);
                   ++threshold;
               }
               
           } while(!simulation.percolates());

           stats[s] = threshold / (double) (N * N);
       }
   }
   
   // Sample mean of percolation threshold.
   public double mean()                    
   {
       double sum = 0.0;
       
       for (int i = 0; i < numberOfSimulations; ++i) {
           sum += stats[i];
       }
       
       return sum / numberOfSimulations;
   }
   
   // Sample standard deviation of percolation threshold.
   public double stddev()                   
   {
       double mean = mean();
       double stddev = 0.0;
       
       for (int i = 0; i < numberOfSimulations; ++i) {
           stddev += (stats[i] - mean) * (stats[i] - mean);
       }
       
       
       return Math.sqrt(stddev / (numberOfSimulations - 1));
   }
   
   // returns lower bound of the 95% confidence interval
   public double confidenceLo()             
   {
       return mean() - ((1.96 * stddev()) / Math.sqrt(numberOfSimulations));
   }
   
   // Returns upper bound of the 95% confidence interval.
   public double confidenceHi()             
   {
       return mean() + ((1.96 * stddev()) / Math.sqrt(numberOfSimulations));
   }
   
   public static void main(String[] args)
   {
       if (args.length < 2) {
           throw new java.lang.IllegalArgumentException("Too small amount of args");
       }
       
       int N = 0;
       int T = 0;
       
       try {
           N = Integer.parseInt(args[0]);
           T = Integer.parseInt(args[1]);
       } catch (java.lang.NumberFormatException nfe) {
           throw new java.lang.IllegalArgumentException("N or T have invalid val");
       }
       
       PercolationStats stats = new PercolationStats(N, T);
       
       StdOut.println("mean                    = " + stats.mean());
       StdOut.println("stddev                  = " + stats.stddev());
       StdOut.println("95% confidence interval = " + stats.confidenceLo() + ", " 
                                                   + stats.confidenceHi());
   }
}
